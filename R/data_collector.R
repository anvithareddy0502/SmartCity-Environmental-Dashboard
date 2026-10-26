# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/data_collector.R
# Purpose: Manages interaction with the official data.gov.in CPCB endpoint,
#          handles rate-limits, timeouts, and server errors safely,
#          and archives genuine new snapshots without altering baseline data.
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(lubridate)
})

API_ENDPOINT <- "https://api.data.gov.in/resource/3b01bcb8-0b14-4abf-b6f2-c1bfd384ba69"

#' Test connectivity and query the official CPCB data endpoint
#' 
#' @param api_key data.gov.in API key. If NULL, checks the default public test key.
#' @param timeout_sec Network timeout in seconds.
#' @return A list with retrieval status, error diagnostics, and raw payload if successful.
fetch_cpcb_live_snapshot <- function(api_key = NULL, timeout_sec = 10) {
  # If no custom key provided, use public demo key
  key_to_use <- if (!is.null(api_key) && nzchar(api_key)) {
    api_key
  } else {
    "579b464db66ec23bdd000001cdd3946e44ce4aad7209ff7b23ac571b"
  }
  
  query_params <- list(
    "api-key" = key_to_use,
    "format" = "json",
    "offset" = 0,
    "limit" = 5
  )
  
  collection_timestamp <- Sys.time()
  
  res <- tryCatch({
    GET(
      API_ENDPOINT,
      query = query_params,
      timeout(timeout_sec),
      user_agent("VIT-CSI3005-Academic-Dashboard/1.0 (R httr)")
    )
  }, error = function(e) {
    return(list(error = TRUE, message = e$message))
  })
  
  # Network or DNS error
  if (!is.null(res$error) && res$error) {
    return(list(
      success = FALSE,
      status_code = NA_integer_,
      error_type = "Network/DNS Failure",
      message = res$message,
      collection_time = collection_timestamp,
      is_live = FALSE
    ))
  }
  
  code <- status_code(res)
  
  # HTTP status handling
  if (code == 200) {
    content_raw <- content(res, as = "text", encoding = "UTF-8")
    parsed_json <- tryCatch(fromJSON(content_raw), error = function(e) NULL)
    
    return(list(
      success = TRUE,
      status_code = 200,
      error_type = NULL,
      message = "Successfully connected to data.gov.in endpoint",
      collection_time = collection_timestamp,
      is_live = TRUE,
      records = parsed_json$records,
      total_count = parsed_json$total
    ))
  } else if (code == 429) {
    return(list(
      success = FALSE,
      status_code = 429,
      error_type = "Rate Limited",
      message = "data.gov.in API rate limit reached. Reverting to static snapshot.",
      collection_time = collection_timestamp,
      is_live = FALSE
    ))
  } else if (code == 500) {
    return(list(
      success = FALSE,
      status_code = 500,
      error_type = "Upstream Server Error",
      message = "Upstream data.gov.in server returned HTTP 500 (documented Slim/OAuth backend issue). Reverting to baseline dataset.",
      collection_time = collection_timestamp,
      is_live = FALSE
    ))
  } else {
    return(list(
      success = FALSE,
      status_code = code,
      error_type = paste("HTTP Error", code),
      message = paste("Unexpected status code returned from data.gov.in:", code),
      collection_time = collection_timestamp,
      is_live = FALSE
    ))
  }
}

#' Archive a newly retrieved snapshot without overwriting baseline data
archive_new_snapshot <- function(records_df, archive_dir = "data/archive") {
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
  }
  
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archive_file <- file.path(archive_dir, paste0("snapshot_", timestamp_str, ".csv"))
  
  # Tag collection time if not already present
  if (!"collection_time" %in% colnames(records_df)) {
    records_df$collection_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }
  
  write_csv(records_df, archive_file)
  return(archive_file)
}

#' Ingest and merge all genuine historical snapshots from data/archive
#' Combines with baseline data and deduplicates on composite key: (station, pollutant_id, observation_time)
load_historical_repository <- function(base_df = BASE_DATA, archive_dir = "data/archive") {
  all_dfs <- list(base_df)
  
  if (dir.exists(archive_dir)) {
    archive_files <- list.files(archive_dir, pattern = "\\.csv$", full.names = TRUE)
    for (f in archive_files) {
      tryCatch({
        raw <- read_csv(f, show_col_types = FALSE, trim_ws = TRUE)
        req_cols <- c("country", "state", "city", "station", "last_update", "latitude", "longitude", 
                      "pollutant_id", "pollutant_min", "pollutant_max", "pollutant_avg")
        if (all(req_cols %in% colnames(raw))) {
          f_mod_time <- file.info(f)$mtime
          clean_f <- raw %>%
            mutate(
              country = str_trim(country),
              state = str_trim(state),
              city = str_trim(city),
              station = str_trim(station),
              pollutant_id = str_to_upper(str_trim(pollutant_id)),
              last_update_raw = last_update,
              observation_time = parse_date_time(last_update, orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM", "d-m-Y H:M:S", "d-m-Y H:M"), tz = "Asia/Kolkata"),
              collection_time = if ("collection_time" %in% colnames(raw)) as.POSIXct(raw$collection_time) else f_mod_time
            ) %>%
            filter(!is.na(observation_time))
          
          if (nrow(clean_f) > 0) {
            all_dfs[[length(all_dfs) + 1]] <- clean_f
          }
        }
      }, error = function(e) {
        warning(paste("Could not parse archived file:", f, "-", e$message))
      })
    }
  }
  
  merged_df <- bind_rows(all_dfs)
  
  # Deduplicate based on composite key: station + pollutant_id + observation_time
  # Preserves earliest collection_time for each unique observation
  deduped_df <- merged_df %>%
    arrange(observation_time, collection_time) %>%
    distinct(station, pollutant_id, observation_time, .keep_all = TRUE)
  
  # Unique sorted timestamps
  unique_times <- sort(unique(na.omit(deduped_df$observation_time)))
  T_count <- length(unique_times)
  
  list(
    data = deduped_df,
    timestamps = unique_times,
    distinct_timestamps_count = T_count,
    has_multi_timestamps = (T_count >= 2),
    total_records = nrow(deduped_df),
    distinct_stations = n_distinct(deduped_df$station),
    distinct_cities = n_distinct(deduped_df$city),
    distinct_states = n_distinct(deduped_df$state),
    archive_files_count = if (dir.exists(archive_dir)) length(list.files(archive_dir, pattern = "\\.csv$")) else 0,
    earliest_time = if (T_count > 0) min(unique_times) else NA,
    latest_time = if (T_count > 0) max(unique_times) else NA,
    supported_analyses = if (T_count < 2) {
      "Baseline Cross-Sectional Spatial Analysis only. Gated: Temporal Trajectory Lines, Multi-Hour Rate of Change (requires ≥2 timestamps)."
    } else if (T_count < 24) {
      paste0("Supported: Multi-Timestamp Trajectories, Station Deltas (Δ), Trend Shifts across ", T_count, " observation timestamps.")
    } else {
      paste0("Supported: Full 24-hr Diurnal Progression Curves, Rolling Averages, Longitudinal Shifts across ", T_count, " timestamps.")
    }
  )
}

#' Validate and import an uploaded CPCB CSV snapshot into data/archive/
import_cpcb_snapshot <- function(temp_path, original_name = "snapshot.csv", archive_dir = "data/archive") {
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
  
  uploaded_df <- tryCatch({
    read_csv(temp_path, show_col_types = FALSE, trim_ws = TRUE)
  }, error = function(e) {
    return(list(success = FALSE, message = paste("CSV read error:", e$message)))
  })
  
  req_cols <- c("country", "state", "city", "station", "last_update", "latitude", "longitude", 
                "pollutant_id", "pollutant_min", "pollutant_max", "pollutant_avg")
  missing_cols <- setdiff(req_cols, colnames(uploaded_df))
  if (length(missing_cols) > 0) {
    return(list(
      success = FALSE,
      message = paste("Invalid schema. Missing required CPCB columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  # Validate timestamps
  parsed_times <- parse_date_time(uploaded_df$last_update, orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM", "d-m-Y H:M:S", "d-m-Y H:M"), tz = "Asia/Kolkata")
  if (all(is.na(parsed_times))) {
    return(list(
      success = FALSE,
      message = "Could not parse valid observation timestamps from last_update column."
    ))
  }
  
  # Tag collection time
  collection_time <- Sys.time()
  uploaded_df$collection_time <- format(collection_time, "%Y-%m-%d %H:%M:%S")
  
  # Generate unique archive filename
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  safe_name <- gsub("[^a-zA-Z0-9_-]", "_", tools::file_path_sans_ext(original_name))
  dest_file <- file.path(archive_dir, paste0("snapshot_", safe_name, "_", timestamp_str, ".csv"))
  
  write_csv(uploaded_df, dest_file)
  
  obs_times <- unique(na.omit(parsed_times))
  
  list(
    success = TRUE,
    file_path = dest_file,
    records_count = nrow(uploaded_df),
    stations_count = n_distinct(uploaded_df$station),
    observation_timestamps = obs_times,
    message = paste0("Successfully imported genuine CPCB snapshot with ", nrow(uploaded_df), 
                     " observations across ", n_distinct(uploaded_df$station), " stations.")
  )
}
