#!/usr/bin/env Rscript
# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: scripts/background_data_sync.R
# Purpose: Standalone, persistent background data harvester for CPCB CAAQM data.
#          Can be executed via Windows Task Scheduler or cron for continuous 24/7
#          longitudinal data accumulation without requiring the Shiny UI to be open.
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
})

# ------------------------------------------------------------------------------
# 1. Robust Working Directory Resolution
# ------------------------------------------------------------------------------
get_script_dir <- function() {
  # Check commandArgs for --file= argument (standard Rscript invocation)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  # Fallback to sys.frame
  sf <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) "")
  if (nzchar(sf)) {
    return(dirname(sf))
  }
  # Default to current working directory
  return(getwd())
}

script_dir <- get_script_dir()
# If script is inside 'scripts/', project root is parent directory
if (basename(script_dir) == "scripts") {
  proj_dir <- dirname(script_dir)
  setwd(proj_dir)
} else if (file.exists("R/data_collector.R")) {
  proj_dir <- getwd()
} else {
  proj_dir <- script_dir
  setwd(proj_dir)
}

# ------------------------------------------------------------------------------
# 2. Setup Persistent Logging (logs/background_sync.log)
# ------------------------------------------------------------------------------
log_dir <- file.path(proj_dir, "logs")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(log_dir, "background_sync.log")

log_msg <- function(msg, to_file = TRUE) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S IST")
  line <- sprintf("[%s] %s", ts, msg)
  cat(line, "\n")
  if (to_file) {
    tryCatch({
      cat(line, "\n", file = log_file, append = TRUE)
    }, error = function(e) NULL)
  }
}

log_msg("==============================================================================")
log_msg("CPCB CAAQM BACKGROUND AIR QUALITY DATA SYNC WORKER")
log_msg(sprintf("Working Directory: %s", proj_dir))
log_msg(sprintf("Log File:          %s", log_file))

# Verify collector script exists
collector_script <- file.path(proj_dir, "R", "data_collector.R")
if (!file.exists(collector_script)) {
  log_msg(sprintf("ERROR: Cannot find '%s'. Please execute from the project root.", collector_script))
  quit(status = 1)
}

source(collector_script)

# ------------------------------------------------------------------------------
# 3. Read Configuration & Execute Update
# ------------------------------------------------------------------------------
cfg_file <- file.path(proj_dir, "config", "data_collection_config.json")
cfg <- get_data_collection_config(config_file = cfg_file)

custom_key <- if (!is.null(cfg$custom_api_key) && nzchar(str_trim(cfg$custom_api_key))) {
  str_trim(cfg$custom_api_key)
} else if (nzchar(Sys.getenv("DATA_GOV_IN_API_KEY"))) {
  Sys.getenv("DATA_GOV_IN_API_KEY")
} else {
  NULL
}

archive_dir <- file.path(proj_dir, "data", "archive")
log_msg(sprintf("Target Archive Directory: %s", archive_dir))
log_msg(sprintf("Custom API Key Configured: %s", if (!is.null(custom_key)) "YES (Masked)" else "NO (Using public key/fallback)"))

log_msg(">>> Initiating validated API collection update...")
res <- execute_data_collection_update(
  api_key = custom_key, 
  archive_dir = archive_dir, 
  max_records = 50, 
  force = FALSE,
  config_file = cfg_file
)

# ------------------------------------------------------------------------------
# 4. Process Outcome & Update Logs
# ------------------------------------------------------------------------------
if (isTRUE(res$success)) {
  log_msg(sprintf("[SUCCESS] %s", res$message))
  log_msg(sprintf("  Archived Snapshot: %s", basename(res$archived_file)))
  log_msg(sprintf("  New Records Added: %d", res$new_records))
} else {
  log_msg(sprintf("[STATUS: %s] %s", res$error_type, res$message))
  if (!is.na(res$status_code)) {
    log_msg(sprintf("  HTTP Status Code:  %d", res$status_code))
  }
}

log_msg("Sync worker execution finished cleanly.")
log_msg("==============================================================================\n")
