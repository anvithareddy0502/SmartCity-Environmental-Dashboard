# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/mod_trends.R
# Purpose: Shiny module for Historical Trends, Ingestion Pipeline & Time-Series Analysis
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(lubridate)
  library(DT)
  library(readr)
  library(stringr)
  library(scales)
})

# UI Module for Historical Trends
mod_trends_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid py-3",
      
      # Clean Page Header
      div(
        class = "page-header-container mb-3",
        h4(class = "page-header-title", "Historical Trends"),
        p(class = "page-header-sub", "Longitudinal observations, multi-timestamp trajectories, and rate-of-change analysis.")
      ),
      
      # 2. Historical Telemetry KPI Bar
      div(
        class = "row g-2 mb-3",
        div(
          class = "col-md-3",
          div(
            class = "card p-3 shadow-sm border-0 h-100 bg-white",
            div(class = "text-muted small fw-bold", icon("calendar-days", class = "me-1 text-primary"), "Observation Timestamps"),
            h4(class = "fw-bold text-primary my-1", textOutput(ns("telemetry_timestamps_count"), inline = TRUE)),
            div(class = "small text-muted", textOutput(ns("telemetry_timestamps_range"), inline = TRUE))
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "card p-3 shadow-sm border-0 h-100 bg-white",
            div(class = "text-muted small fw-bold", icon("database", class = "me-1 text-primary"), "Total Observations"),
            h4(class = "fw-bold text-primary my-1", textOutput(ns("telemetry_records_total"), inline = TRUE)),
            div(class = "small text-muted", textOutput(ns("telemetry_stations_count"), inline = TRUE))
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "card p-3 shadow-sm border-0 h-100 bg-white",
            div(class = "text-muted small fw-bold", icon("box-archive", class = "me-1 text-primary"), "Archived Snapshots"),
            h4(class = "fw-bold text-primary my-1", textOutput(ns("telemetry_archive_count"), inline = TRUE)),
            div(class = "small text-muted font-monospace", "data/archive/*.csv")
          )
        ),
        div(
          class = "col-md-3",
          div(
            class = "card p-3 shadow-sm border-0 h-100 bg-white",
            div(class = "text-muted small fw-bold", icon("chart-line", class = "me-1 text-primary"), "Analysis Mode"),
            div(class = "my-1", uiOutput(ns("telemetry_mode_badge"))),
            div(class = "small text-muted", textOutput(ns("telemetry_supported_brief"), inline = TRUE))
          )
        )
      ),
      
      # 3. Snapshot Ingestion & Archival Command Center
      div(
        class = "card p-3 mb-3 shadow-sm border-0 bg-white",
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          h6(class = "card-title m-0 fw-bold", "Historical Data Collection"),
          span(class = "badge bg-light text-secondary border", "Deduplication Enforced")
        ),
        p(class = "small text-muted mb-3",
          "Expand the longitudinal dataset by importing genuine CPCB CSV snapshots or querying the official data.gov.in endpoint. ",
          "Every observation is deduplicated using the composite primary key ", tags$code("(station, pollutant_id, observation_time)"), " to guarantee data integrity."
        ),
        fluidRow(
          # Pathway A: Manual CSV Upload
          column(
            width = 6,
            div(
              class = "p-3 bg-light rounded border h-100",
              h6(class = "fw-bold small text-dark mb-1", "Import CSV snapshot"),
              p(class = "small text-muted mb-2", "Upload genuine CPCB CSV files downloaded from data.gov.in. Records are automatically validated, timestamped, and merged."),
              fileInput(
                inputId = ns("import_cpcb_csv"),
                label = NULL,
                accept = c(".csv"),
                buttonLabel = "Browse CSV...",
                placeholder = "Select genuine CPCB CSV snapshot...",
                width = "100%"
              ),
              uiOutput(ns("import_status_ui"))
            )
          ),
          
          # Pathway B: Automated API Query & Scheduled Updates
          column(
            width = 6,
            div(
              class = "p-3 bg-light rounded border h-100",
              h6(class = "fw-bold small text-dark mb-1", "Automated & Live API Data Harvester"),
              p(class = "small text-muted mb-2", "Query the official CPCB CAAQM endpoint (Resource ID: 3b01bcb8) with automatic deduplication, rate-limit defense, and in-session scheduling."),
              div(
                class = "row g-2 mb-2",
                div(
                  class = "col-sm-6",
                  selectInput(
                    inputId = ns("collection_interval"),
                    label = "Auto-Poll Interval:",
                    choices = c(
                      "Disabled (Manual Only)" = 0,
                      "Every 15 Minutes" = 15,
                      "Every 30 Minutes" = 30,
                      "Every 1 Hour" = 60,
                      "Every 2 Hours" = 120,
                      "Every 6 Hours" = 360
                    ),
                    selected = 0,
                    width = "100%"
                  )
                ),
                div(
                  class = "col-sm-6 d-flex align-items-end",
                  div(
                    class = "d-flex gap-1 w-100 mb-1",
                    actionButton(
                      inputId = ns("btn_harvest_api"),
                      label = "Refresh Now",
                      icon = icon("cloud-arrow-down"),
                      class = "btn-primary btn-sm flex-fill"
                    ),
                    actionButton(
                      inputId = ns("btn_scan_archive"),
                      label = "Re-scan",
                      icon = icon("rotate"),
                      class = "btn-outline-secondary btn-sm"
                    )
                  )
                )
              ),
              # Collapsible API Key settings
              tags$details(
                class = "mb-2 small text-muted",
                tags$summary(
                  class = "fw-semibold text-secondary",
                  style = "cursor: pointer;",
                  icon("key", class = "me-1 text-primary"),
                  "Configure Custom API Key (Optional)"
                ),
                div(
                  class = "mt-2 p-2 bg-white rounded border",
                  passwordInput(
                    inputId = ns("custom_api_key"),
                    label = "data.gov.in API Key:",
                    placeholder = "Paste custom API key or leave blank for default public key...",
                    width = "100%"
                  ),
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    span(class = "text-muted", style = "font-size: 0.75rem;", "Saved safely without hardcoding in codebase."),
                    actionButton(ns("btn_save_key"), "Save Key", class = "btn-outline-primary btn-sm py-0 px-2")
                  )
                )
              ),
              # Collapsible Windows Task Scheduler 24/7 Setup Guide
              tags$details(
                class = "mb-2 small text-muted",
                tags$summary(
                  class = "fw-semibold text-secondary",
                  style = "cursor: pointer;",
                  icon("terminal", class = "me-1 text-primary"),
                  "24/7 Windows Background Scheduler Guide (Click to expand)"
                ),
                div(
                  class = "mt-2 p-2 bg-white rounded border",
                  uiOutput(ns("scheduler_guide_ui"))
                )
              ),
              uiOutput(ns("api_status_ui")),
              div(
                class = "small text-muted mt-2 border-top pt-2",
                style = "font-size: 0.75rem;",
                icon("clock", class = "me-1 text-info"),
                tags$b("In-Session Scheduler: "),
                "Runs while dashboard is active in your browser. For 24/7 background collection without browser dependency, schedule ",
                tags$code("scripts/background_data_sync.R"), " via Windows Task Scheduler or cron."
              )
            )
          )
        )
      ),
      
      # 4. Main Dynamic Container (Switches between Data Gating Notice and Trend Studio)
      uiOutput(ns("trends_main_container"))
    )
  )
}

# Server Module for Historical Trends
mod_trends_server <- function(id, shared_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cfg_init <- get_data_collection_config()
    
    # Reactive value to trigger re-reads of the archive store & track collection telemetry
    rv <- reactiveValues(
      refresh_counter = 0,
      last_import_msg = NULL,
      last_import_success = NULL,
      last_api_msg = cfg_init$last_error_message,
      last_api_code = if (identical(cfg_init$last_status, "Success")) 200L else if (identical(cfg_init$last_status, "Rate Limited")) 429L else NULL,
      last_success_time = cfg_init$last_success_time,
      is_busy = FALSE
    )
    
    # --------------------------------------------------------------------------
    # 1. Reactive Historical Repository Ingestion
    # --------------------------------------------------------------------------
    historical_repo <- reactive({
      input$btn_scan_archive
      rv$refresh_counter
      if (!is.null(shared_trigger)) shared_trigger()
      
      load_historical_repository(base_df = BASE_DATA, archive_dir = "data/archive")
    })
    
    # --------------------------------------------------------------------------
    # 2. Automated In-Session Polling Timer
    # --------------------------------------------------------------------------
    auto_timer <- reactiveTimer(30000) # Checks every 30 seconds
    last_auto_poll_time <- reactiveVal(Sys.time())
    
    observeEvent(auto_timer(), {
      if (is.null(input$collection_interval)) return()
      interval_mins <- suppressWarnings(as.numeric(input$collection_interval))
      if (length(interval_mins) == 0 || is.na(interval_mins) || interval_mins <= 0) return()
      
      now <- Sys.time()
      elapsed_sec <- as.numeric(difftime(now, last_auto_poll_time(), units = "secs"))
      
      if (elapsed_sec >= (interval_mins * 60)) {
        last_auto_poll_time(now)
        
        # Concurrency guard
        if (isTRUE(rv$is_busy)) return()
        rv$is_busy <- TRUE
        on.exit({ rv$is_busy <- FALSE }, add = TRUE)
        
        custom_k <- if (!is.null(input$custom_api_key) && nzchar(str_trim(input$custom_api_key))) str_trim(input$custom_api_key) else NULL
        res <- execute_data_collection_update(api_key = custom_k, archive_dir = "data/archive", max_records = 50, force = FALSE)
        
        if (isTRUE(res$success)) {
          rv$last_api_msg <- res$message
          rv$last_api_code <- 200L
          rv$last_success_time <- format(now, "%Y-%m-%d %H:%M:%S")
          rv$refresh_counter <- rv$refresh_counter + 1
          if (!is.null(shared_trigger)) shared_trigger(shared_trigger() + 1)
          showNotification(paste0("Auto-Update: ", res$message), type = "message", duration = 6)
        } else {
          rv$last_api_msg <- paste0(res$error_type, ": ", res$message)
          rv$last_api_code <- res$status_code
        }
      }
    })
    
    # --------------------------------------------------------------------------
    # 3. Custom API Key Save Handler
    # --------------------------------------------------------------------------
    observeEvent(input$btn_save_key, {
      key <- input$custom_api_key
      curr_cfg <- get_data_collection_config()
      curr_cfg$custom_api_key <- if (nzchar(str_trim(key))) str_trim(key) else NULL
      save_data_collection_config(curr_cfg)
      showNotification("API key configuration saved for this session.", type = "message", duration = 4)
    })
    
    # --------------------------------------------------------------------------
    # 4. Ingestion Event Handlers
    # --------------------------------------------------------------------------
    # 4.1 Manual CSV Upload Handler
    observeEvent(input$import_cpcb_csv, {
      req(input$import_cpcb_csv)
      
      file_info <- input$import_cpcb_csv
      res <- import_cpcb_snapshot(
        temp_path = file_info$datapath,
        original_name = file_info$name,
        archive_dir = "data/archive"
      )
      
      if (res$success) {
        rv$last_import_msg <- res$message
        rv$last_import_success <- TRUE
        rv$refresh_counter <- rv$refresh_counter + 1
        if (!is.null(shared_trigger)) shared_trigger(shared_trigger() + 1)
        showNotification(res$message, type = "message", duration = 6)
      } else {
        rv$last_import_msg <- res$message
        rv$last_import_success <- FALSE
        showNotification(res$message, type = "error", duration = 8)
      }
    })
    
    # 4.2 API Harvest / Refresh Now Button Handler
    observeEvent(input$btn_harvest_api, {
      if (isTRUE(rv$is_busy)) {
        showNotification("A data collection query is already in progress. Please wait...", type = "warning", duration = 3)
        return()
      }
      
      rv$is_busy <- TRUE
      on.exit({ rv$is_busy <- FALSE }, add = TRUE)
      
      custom_k <- if (!is.null(input$custom_api_key) && nzchar(str_trim(input$custom_api_key))) str_trim(input$custom_api_key) else NULL
      res <- execute_data_collection_update(api_key = custom_k, archive_dir = "data/archive", max_records = 50, force = TRUE)
      
      if (isTRUE(res$success)) {
        rv$last_api_msg <- res$message
        rv$last_api_code <- 200L
        rv$last_success_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        rv$refresh_counter <- rv$refresh_counter + 1
        if (!is.null(shared_trigger)) shared_trigger(shared_trigger() + 1)
        showNotification(res$message, type = "message", duration = 6)
      } else {
        rv$last_api_msg <- paste0(res$error_type, ": ", res$message)
        rv$last_api_code <- res$status_code
        showNotification(rv$last_api_msg, type = "warning", duration = 8)
      }
    })
    
    # 2.3 Manual Archive Directory Rescan Handler
    observeEvent(input$btn_scan_archive, {
      invalidate_historical_cache()
      rv$refresh_counter <- rv$refresh_counter + 1
      if (!is.null(shared_trigger)) shared_trigger(shared_trigger() + 1)
      showNotification("Archive directory rescanned and cache updated.", type = "message", duration = 3)
    })
    
    # --------------------------------------------------------------------------
    # 3. Status Badges & Telemetry Outputs
    # --------------------------------------------------------------------------
    output$telemetry_timestamps_count <- renderText({
      repo <- historical_repo()
      paste0(repo$distinct_timestamps_count, " Timestamp", if (repo$distinct_timestamps_count != 1) "s" else "")
    })
    
    output$telemetry_timestamps_range <- renderText({
      repo <- historical_repo()
      if (repo$distinct_timestamps_count == 1) {
        paste0("Snapshot: ", format(repo$earliest_time, "%d-%b-%Y %H:%M"))
      } else if (repo$distinct_timestamps_count > 1) {
        paste0(format(repo$earliest_time, "%d-%b %H:%M"), " to ", format(repo$latest_time, "%d-%b %H:%M"))
      } else {
        "No observations"
      }
    })
    
    output$telemetry_records_total <- renderText({
      repo <- historical_repo()
      scales::comma(repo$total_records)
    })
    
    output$telemetry_stations_count <- renderText({
      repo <- historical_repo()
      paste0(repo$distinct_stations, " Stations | ", repo$distinct_states, " States/UTs")
    })
    
    output$telemetry_archive_count <- renderText({
      repo <- historical_repo()
      paste0(repo$archive_files_count, " Snapshot File", if (repo$archive_files_count != 1) "s" else "")
    })
    
    output$telemetry_mode_badge <- renderUI({
      repo <- historical_repo()
      if (repo$has_multi_timestamps) {
        span(class = "badge bg-success text-white py-1 px-2", icon("unlock", class = "me-1"), "Longitudinal Mode Unlocked")
      } else {
        span(class = "badge bg-warning text-dark py-1 px-2", icon("lock", class = "me-1"), "Baseline Snapshot Mode")
      }
    })
    
    output$telemetry_supported_brief <- renderText({
      repo <- historical_repo()
      if (repo$has_multi_timestamps) {
        "Time-series & deltas active"
      } else {
        "Awaiting ≥2 timestamps"
      }
    })
    
    output$import_status_ui <- renderUI({
      if (is.null(rv$last_import_msg)) return(NULL)
      cls <- if (isTRUE(rv$last_import_success)) "text-success" else "text-danger"
      ico <- if (isTRUE(rv$last_import_success)) "circle-check" else "circle-exclamation"
      div(class = paste("small mt-2", cls), icon(ico, class = "me-1"), rv$last_import_msg)
    })
    
    output$scheduler_guide_ui <- renderUI({
      r_path <- if (exists("get_system_rscript_path")) get_system_rscript_path() else "Rscript.exe"
      proj_dir <- tryCatch(normalizePath(getwd(), winslash = "\\"), error = function(e) getwd())
      script_path <- file.path(proj_dir, "scripts", "background_data_sync.R")
      log_path <- file.path(proj_dir, "logs", "background_sync.log")
      
      tagList(
        p(class = "mb-1 fw-bold text-dark", "Continuous 24/7 Background Ingestion:"),
        p(class = "mb-2 text-muted", "Collects genuine CPCB observations automatically even when RStudio and your browser are closed."),
        div(class = "fw-semibold text-dark mb-1", "1. Test One Manual Collection:"),
        tags$pre(
          class = "bg-light p-2 rounded border font-monospace text-dark mb-2",
          style = "font-size: 0.72rem; white-space: pre-wrap; word-break: break-all;",
          sprintf('& "%s" "%s"', r_path, script_path)
        ),
        div(class = "fw-semibold text-dark mb-1", "2. Schedule Hourly in Windows Task Scheduler:"),
        tags$pre(
          class = "bg-light p-2 rounded border font-monospace text-dark mb-2",
          style = "font-size: 0.72rem; white-space: pre-wrap; word-break: break-all;",
          sprintf('schtasks /create /tn "CPCB_AirQuality_Sync" /tr "\\"\\"%s\\"\\" \\"\\"%s\\"\\"" /sc hourly /mo 1', r_path, script_path)
        ),
        div(class = "fw-semibold text-dark mb-1", "3. Inspect Background Execution Log:"),
        tags$pre(
          class = "bg-light p-2 rounded border font-monospace text-dark mb-2",
          style = "font-size: 0.72rem; white-space: pre-wrap; word-break: break-all;",
          sprintf('Get-Content -Path "%s" -Tail 20', log_path)
        ),
        div(class = "fw-semibold text-dark mb-1", "4. Stop or Remove Task:"),
        tags$pre(
          class = "bg-light p-2 rounded border font-monospace text-dark mb-0",
          style = "font-size: 0.72rem; white-space: pre-wrap; word-break: break-all;",
          'schtasks /delete /tn "CPCB_AirQuality_Sync" /f'
        )
      )
    })

    output$api_status_ui <- renderUI({
      repo <- historical_repo()
      bg_info <- if (exists("get_background_sync_log_summary")) get_background_sync_log_summary() else list(is_configured = FALSE)
      
      last_coll_str <- if (!is.null(rv$last_success_time)) {
        format(as.POSIXct(rv$last_success_time), "%d-%b-%Y %H:%M:%S IST")
      } else if (!is.na(repo$last_collection_time)) {
        format(repo$last_collection_time, "%d-%b-%Y %H:%M:%S IST")
      } else {
        "Offline Baseline Snapshot"
      }
      
      latest_obs_str <- if (!is.na(repo$latest_time)) {
        format(repo$latest_time, "%d-%b-%Y %H:%M IST")
      } else {
        "23-Oct-2026 18:00 IST"
      }
      
      interval_val <- if (!is.null(input$collection_interval)) suppressWarnings(as.numeric(input$collection_interval)) else 0
      is_session_polling <- (!is.na(interval_val) && interval_val > 0)
      is_rate_limited <- identical(rv$last_api_code, 429L) || (!is.null(rv$last_api_msg) && grepl("rate limit|cooldown", tolower(rv$last_api_msg)))
      is_error <- (!is.null(rv$last_api_code) && rv$last_api_code != 200L && !is_rate_limited)
      is_synced <- identical(rv$last_api_code, 200L)
      
      if (is_rate_limited) {
        state_badge <- span(class = "badge bg-warning text-dark py-1 px-2", icon("triangle-exclamation", class = "me-1"), "Rate Limited")
        next_action_box <- div(
          class = "small mt-2 p-2 rounded border bg-light text-secondary border-warning",
          icon("lightbulb", class = "me-1 text-warning"),
          strong("Next Action: "),
          "Public OGD quota reached. Configure a private API key in the drawer above or wait 3 minutes for cooldown before refreshing."
        )
      } else if (is_error) {
        state_badge <- span(class = "badge bg-danger text-white py-1 px-2", icon("circle-xmark", class = "me-1"), paste("Error", rv$last_api_code))
        next_action_box <- div(
          class = "small mt-2 p-2 rounded border bg-light text-danger border-danger",
          icon("circle-exclamation", class = "me-1 text-danger"),
          strong("Next Action: "),
          "Upstream server or network issue. Baseline snapshot remains protected. Verify internet connectivity and retry."
        )
      } else if (is_synced) {
        state_badge <- span(class = "badge bg-success text-white py-1 px-2", icon("circle-check", class = "me-1"), "Synchronized")
        next_action_box <- div(
          class = "small mt-2 p-2 rounded border bg-light text-success border-success",
          icon("check", class = "me-1 text-success"),
          strong("Next Action: "),
          sprintf("Genuine observations successfully ingested (%d archived files). Trajectories and station deltas are active below.", repo$archive_files_count)
        )
      } else if (is_session_polling) {
        state_badge <- span(class = "badge bg-primary text-white py-1 px-2", icon("arrows-rotate", class = "me-1"), paste0("In-Session Polling (", interval_val, "m)"))
        next_action_box <- div(
          class = "small mt-2 p-2 rounded border bg-light text-primary border-primary",
          icon("clock", class = "me-1 text-primary"),
          strong("Next Action: "),
          "Auto-polling active. Keep this browser tab open to continue collecting snapshots, or register the Windows Task Scheduler worker."
        )
      } else if (isTRUE(bg_info$is_configured)) {
        state_badge <- span(class = "badge bg-info text-dark py-1 px-2", icon("server", class = "me-1"), "Background Sync Enabled")
        next_action_box <- div(
          class = "small mt-2 p-2 rounded border bg-light text-dark border-info",
          icon("circle-info", class = "me-1 text-info"),
          strong("Next Action: "),
          sprintf("Background worker active (%s). Click 'Re-scan' anytime to load new snapshot files into the dashboard.", bg_info$last_status)
        )
      } else if (repo$has_multi_timestamps) {
        state_badge <- span(class = "badge bg-success text-white py-1 px-2", icon("layer-group", class = "me-1"), "Longitudinal Active")
        next_action_box <- div(
          class = "small mt-2 p-2 rounded border bg-light text-dark border-success",
          icon("chart-line", class = "me-1 text-success"),
          strong("Next Action: "),
          "Multi-timestamp history available. Review temporal trajectories below or switch to AI Forecast & Alerts for predictions."
        )
      } else {
        state_badge <- span(class = "badge bg-secondary text-white py-1 px-2", icon("database", class = "me-1"), "Baseline Snapshot (T=1)")
        next_action_box <- div(
          class = "small mt-2 p-2 rounded border bg-light text-dark border-secondary",
          icon("circle-info", class = "me-1 text-secondary"),
          strong("Next Action: "),
          "Single snapshot loaded (23-Oct-2026 18:00 IST). Click 'Refresh Now' or drop an exported CSV to begin historical accumulation."
        )
      }
      
      div(
        class = "mt-2 pt-2 border-top",
        div(
          class = "d-flex justify-content-between align-items-center mb-1",
          span(class = "small text-muted fw-semibold", "Live Sync Telemetry:"),
          state_badge
        ),
        div(
          class = "small text-muted d-flex justify-content-between py-1",
          span("Last Collection Attempt:"),
          span(class = "fw-semibold text-dark font-monospace", last_coll_str)
        ),
        div(
          class = "small text-muted d-flex justify-content-between py-1",
          span("Latest Source Observation:"),
          span(class = "fw-semibold text-dark font-monospace", latest_obs_str)
        ),
        next_action_box
      )
    })
    
    # --------------------------------------------------------------------------
    # 4. Main Dynamic Container Rendering (Gating vs. Full Trend Studio)
    # --------------------------------------------------------------------------
    output$trends_main_container <- renderUI({
      repo <- historical_repo()
      
      if (!repo$has_multi_timestamps) {
        # ----------------------------------------------------------------------
        # STATE A: Gating Notice & Ingestion Instructions (T < 2)
        # ----------------------------------------------------------------------
        div(
          class = "row g-3",
          div(
            class = "col-md-8",
            div(
              class = "card p-4 shadow-sm border-0 h-100 bg-white",
              div(
                class = "d-flex align-items-center mb-3",
                div(
                  class = "p-3 rounded-circle bg-light text-secondary me-3 border",
                  icon("clock-rotate-left", class = "fs-4 text-primary")
                ),
                div(
                  h5(class = "card-title m-0 fw-bold", "Single Timestamp Baseline"),
                  p(class = "small text-muted m-0", "Additional timestamped snapshots required for longitudinal curves")
                )
              ),
              p(class = "text-secondary",
                "The CPCB baseline dataset (", tags$code("data/air_quality.csv"), ") records air quality measurements for a single synchronized observation timestamp: ",
                strong("23-10-2026 18:00 IST"), " across 526 continuous ambient air quality stations."
              ),
              div(
                class = "interpretation-box p-3 mb-3",
                h6(class = "fw-bold mb-1", "Historical Trend Analysis"),
                p(class = "small m-0",
                  "Currently, the dataset contains observations for a single timestamp (", strong("23 Oct 2026, 18:00 IST"), "). ",
                  "Trajectory curves and rate-of-change metrics will automatically appear once additional timestamped snapshots are imported. ",
                  "Synthetic or simulated time points are never fabricated."
                )
              ),
              h6(class = "fw-bold text-secondary mb-2", "Supported vs. Gated Analyses:"),
              tags$ul(
                class = "small text-muted ps-3 mb-3",
                tags$li(strong("Currently supported (1 timestamp): "), "Cross-sectional spatial analysis, criteria pollutant distribution tracking, CPCB sub-index categorization, and sensor anomaly detection."),
                tags$li(strong("Gated (requires ≥2 timestamps): "), "Station trajectory curves, temporal shift rankings (Δ accumulation/dispersion), state progression tracking, and diurnal comparisons.")
              ),
              p(class = "small text-muted mb-0",
                icon("lightbulb", class = "me-1 text-primary"),
                "To unlock historical trajectories: Use ", strong("Import CSV snapshot"), " above to add any genuine timestamped CPCB CSV snapshot."
              )
            )
          ),
          
          div(
            class = "col-md-4",
            div(
              class = "card p-4 shadow-sm border-0 h-100 bg-white",
              h6(class = "card-title text-secondary mb-3 fw-bold", "Longitudinal Telemetry"),
              tags$table(
                class = "table table-sm table-borderless small mb-3",
                tags$tr(
                  tags$td("Baseline Snapshot:"),
                  tags$td(class = "text-end fw-bold", "23-10-2026 18:00 IST")
                ),
                tags$tr(
                  tags$td("Baseline Records:"),
                  tags$td(class = "text-end fw-bold", "3,682 Records")
                ),
                tags$tr(
                  tags$td("Archival Folder:"),
                  tags$td(class = "text-end font-monospace", "data/archive/")
                ),
                tags$tr(
                  tags$td("Distinct Timestamps:"),
                  tags$td(class = "text-end fw-bold text-warning", "1 / 2 (Required)")
                ),
                tags$tr(
                  tags$td("Archived Snapshots:"),
                  tags$td(class = "text-end fw-bold text-primary", paste(repo$archive_files_count, "Files"))
                ),
                tags$tr(
                  tags$td("API Status:"),
                  tags$td(class = "text-end", span(class = "badge bg-light text-secondary border", "Offline Baseline"))
                )
              ),
              div(
                class = "p-3 rounded bg-light border small text-muted",
                icon("circle-info", class = "me-1 text-primary"),
                "When genuine snapshots are placed into ", tags$code("data/archive/"), " or imported via the file picker, this page reactively unlocks multi-timestamp trajectory analysis."
              )
            )
          )
        )
      } else {
        # ----------------------------------------------------------------------
        # STATE B: Multi-Timestamp Interactive Historical Studio (T >= 2)
        # ----------------------------------------------------------------------
        tagList(
          # Filter Bar for Historical Data
          div(
            class = "card p-3 mb-3 shadow-sm border-0 bg-white",
            fluidRow(
              column(
                width = 3,
                selectInput(
                  inputId = ns("trend_filter_state"),
                  label = "State / UT:",
                  choices = c("All States", sort(unique(repo$data$state))),
                  selected = "All States",
                  width = "100%"
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = ns("trend_filter_city"),
                  label = "City:",
                  choices = c("All Cities"),
                  selected = "All Cities",
                  width = "100%"
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = ns("trend_filter_pollutant"),
                  label = "Pollutant:",
                  choices = c("PM2.5", "PM10", "NO2", "SO2", "CO", "OZONE", "NH3"),
                  selected = "PM2.5",
                  width = "100%"
                )
              ),
              column(
                width = 3,
                div(
                  class = "d-flex align-items-end h-100 pb-1 gap-2",
                  actionButton(
                    inputId = ns("trend_btn_reset"),
                    label = "Reset filters",
                    icon = icon("rotate-left"),
                    class = "btn-outline-secondary btn-sm w-100"
                  )
                )
              )
            )
          ),
          
          # Historical Analytics Tabset
          navset_card_tab(
            # TAB 1: Time-Series Trajectory Tracking
            nav_panel(
              title = "Temporal Trajectories",
              icon = icon("chart-line"),
              div(
                class = "py-2",
                div(
                  class = "d-flex justify-content-between align-items-center mb-1",
                  h6(class = "card-title m-0 fw-bold", textOutput(ns("trajectory_chart_title"), inline = TRUE)),
                  span(class = "badge bg-light text-secondary border", textOutput(ns("trajectory_badge"), inline = TRUE))
                ),
                p(class = "small text-muted mb-2", "Chronological concentration trajectories across genuine observation timestamps. Markers denote exact sensor measurements."),
                plotlyOutput(ns("trajectory_line_chart"), height = "400px"),
                uiOutput(ns("trajectory_interpretation"))
              )
            ),
            
            # TAB 2: Station Delta (Shift) Ranking
            nav_panel(
              title = "Temporal Shifts (Δ)",
              icon = icon("arrow-trend-up"),
              div(
                class = "py-2",
                div(
                  class = "d-flex justify-content-between align-items-center mb-1",
                  h6(class = "card-title m-0 fw-bold", textOutput(ns("delta_chart_title"), inline = TRUE)),
                  span(class = "badge bg-light text-secondary border", textOutput(ns("delta_badge"), inline = TRUE))
                ),
                p(class = "small text-muted mb-2", "Rate of change calculated as (Latest Reading - Baseline Reading). Red indicates pollution accumulation; Green indicates atmospheric dispersion."),
                plotlyOutput(ns("station_delta_chart"), height = "400px"),
                uiOutput(ns("delta_interpretation"))
              )
            ),
            
            # TAB 3: State Progression Tracking
            nav_panel(
              title = "State Progression",
              icon = icon("chart-column"),
              div(
                class = "py-2",
                div(
                  class = "d-flex justify-content-between align-items-center mb-1",
                  h6(class = "card-title m-0 fw-bold", textOutput(ns("state_progression_title"), inline = TRUE)),
                  span(class = "badge bg-light text-secondary border", "State Means Across Timestamps")
                ),
                p(class = "small text-muted mb-2", "Mean pollutant concentration across all reporting stations within each state, compared across chronological timestamps."),
                plotlyOutput(ns("state_progression_chart"), height = "400px"),
                uiOutput(ns("state_progression_interpretation"))
              )
            ),
            
            # TAB 4: Longitudinal Data Directory & Export
            nav_panel(
              title = "Longitudinal Data Directory",
              icon = icon("table"),
              div(
                class = "py-2",
                div(
                  class = "d-flex justify-content-between align-items-center mb-2",
                  h6(class = "card-title m-0 fw-bold", "Multi-Timestamp Observations Directory"),
                  downloadButton(
                    outputId = ns("btn_dl_longitudinal_csv"),
                    label = "Download Longitudinal Data (CSV)",
                    class = "btn-primary btn-sm"
                  )
                ),
                p(class = "small text-muted mb-2", "Searchable record directory preserving both observation timestamps (sensor reading time) and collection timestamps (ingestion time)."),
                DTOutput(ns("longitudinal_data_table"))
              )
            )
          )
        )
      }
    })
    
    # --------------------------------------------------------------------------
    # 5. Cascading Filter Logics & Reactive Subsets (for State B)
    # --------------------------------------------------------------------------
    curr_state <- reactive({
      if (is.null(input$trend_filter_state) || !nzchar(input$trend_filter_state)) "All States" else input$trend_filter_state
    })
    
    curr_city <- reactive({
      if (is.null(input$trend_filter_city) || !nzchar(input$trend_filter_city)) "All Cities" else input$trend_filter_city
    })
    
    curr_pollutant <- reactive({
      if (is.null(input$trend_filter_pollutant) || !nzchar(input$trend_filter_pollutant)) "PM2.5" else input$trend_filter_pollutant
    })
    
    # Cascading City Choices
    observeEvent(input$trend_filter_state, {
      req(input$trend_filter_state)
      repo <- historical_repo()
      st <- curr_state()
      
      cities <- if (identical(st, "All States")) {
        sort(unique(repo$data$city))
      } else {
        sort(unique(repo$data$city[repo$data$state == st]))
      }
      
      updateSelectInput(session, "trend_filter_city", choices = c("All Cities", cities), selected = "All Cities")
    })
    
    # Reset Filters
    observeEvent(input$trend_btn_reset, {
      updateSelectInput(session, "trend_filter_state", selected = "All States")
      updateSelectInput(session, "trend_filter_city", selected = "All Cities")
      updateSelectInput(session, "trend_filter_pollutant", selected = "PM2.5")
    })
    
    # Filtered Long Dataset
    filtered_trend_data <- reactive({
      repo <- historical_repo()
      df <- repo$data
      st <- curr_state()
      ct <- curr_city()
      pol <- curr_pollutant()
      
      df <- df %>% filter(pollutant_id == pol)
      if (!identical(st, "All States")) df <- df %>% filter(state == st)
      if (!identical(ct, "All Cities")) df <- df %>% filter(city == ct)
      
      df
    })
    
    # --------------------------------------------------------------------------
    # 6. Chart Renderers for Multi-Timestamp State
    # --------------------------------------------------------------------------
    
    # 6.1 TAB 1: Trajectory Time-Series Chart
    output$trajectory_chart_title <- renderText({
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      paste0("Temporal Trajectory Analysis: ", pol, " (", unit, ")")
    })
    
    output$trajectory_badge <- renderText({
      df <- filtered_trend_data()
      paste0(n_distinct(df$station), " Stations | ", length(unique(df$observation_time)), " Timestamps")
    })
    
    output$trajectory_line_chart <- renderPlotly({
      df <- filtered_trend_data() %>% filter(!is.na(pollutant_avg))
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      if (nrow(df) == 0 || length(unique(df$observation_time)) < 2) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Insufficient valid time-series observations in current selection.", color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Select top reporting stations to display clean trajectories
      top_stns <- df %>%
        group_by(station) %>%
        filter(n_distinct(observation_time) >= 2) %>%
        ungroup() %>%
        count(station, sort = TRUE) %>%
        head(8) %>%
        pull(station)
      
      plot_df <- df %>%
        filter(station %in% top_stns) %>%
        arrange(observation_time) %>%
        mutate(
          time_label = format(observation_time, "%d-%b %H:%M"),
          hover_txt = paste0(
            "Station: ", station, "\n",
            "City: ", city, ", ", state, "\n",
            "Time: ", time_label, " IST\n",
            pol, ": ", pollutant_avg, " ", unit
          )
        )
      
      p <- ggplot(plot_df, aes(x = observation_time, y = pollutant_avg, color = station, group = station)) +
        geom_line(linewidth = 1, alpha = 0.85) +
        suppressWarnings(geom_point(aes(text = hover_txt), size = 2.5, alpha = 0.9)) +
        scale_color_viridis_d(option = "turbo") +
        scale_x_datetime(date_labels = "%d-%b\n%H:%M", breaks = scales::pretty_breaks(n = 6)) +
        labs(
          x = "Observation Timestamp (IST)",
          y = paste0("Reported Concentration (", unit, ")"),
          color = "Station"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 9, color = "#1E293B"),
          axis.title = element_text(size = 10, color = "#475569"),
          legend.position = "bottom"
        )
      
      suppressWarnings(ggplotly(p, tooltip = "text")) %>%
        layout(
          margin = list(l = 10, r = 10, t = 10, b = 20),
          legend = list(orientation = "h", x = 0.1, y = -0.2)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Understand Trajectory Line Chart
    output$trajectory_interpretation <- renderUI({
      df <- filtered_trend_data() %>% filter(!is.na(pollutant_avg))
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      num_times <- length(unique(df$observation_time))
      
      findings <- if (num_times < 2) {
        "Currently operating on the official single-timestamp CPCB baseline (23-Oct-2026 18:00 IST). Multi-timestamp line trajectories remain gated until additional genuine snapshots are imported into the archive, preventing the fabrication of synthetic trends."
      } else {
        time_summary <- df %>%
          group_by(observation_time) %>%
          summarise(mean_val = mean(pollutant_avg, na.rm = TRUE), .groups = "drop") %>%
          arrange(observation_time)
        first_val <- round(time_summary$mean_val[1], 1)
        last_val <- round(time_summary$mean_val[nrow(time_summary)], 1)
        diff_val <- round(last_val - first_val, 1)
        direction <- if (diff_val > 0) "increased" else if (diff_val < 0) "decreased" else "remained constant"
        
        paste0(
          "Across the monitored observation window (", num_times, " timestamps), the mean concentration of ", pol,
          " ", direction, " by ", abs(diff_val), " ", unit,
          " (from ", first_val, " ", unit, " at ", format(time_summary$observation_time[1], "%d-%b %H:%M"),
          " to ", last_val, " ", unit, " at ", format(time_summary$observation_time[nrow(time_summary)], "%d-%b %H:%M"), ")."
        )
      }
      
      build_understand_chart(
        chart_type = "Time-Series Multi-Line Chart",
        what_shows = "This chart tracks chronological changes in ambient pollutant concentrations over time across individual monitoring stations, uncovering diurnal variations, meteorological dispersion, or pollution accumulation episodes.",
        how_to_read = "The horizontal x-axis represents observation timestamps in Indian Standard Time (IST). The vertical y-axis plots pollutant concentration in its native unit (µg/m³ or mg/m³ for CO). Each line traces a specific monitoring station across successive readings. Hover over any point to view exact timestamp and concentration.",
        colors_meaning = "Distinct categorical colours identify individual monitoring stations as defined in the chart legend.",
        dynamic_findings = findings,
        important_note = "Historical trajectories require genuine chronological snapshots. Interpolation or synthetic trend lines are strictly prohibited under CPCB data integrity standards."
      )
    })
    
    # 6.2 TAB 2: Station Delta Ranking Chart
    output$delta_chart_title <- renderText({
      pol <- curr_pollutant()
      paste0("Station Shift Ranking (Δ ", pol, ": Latest vs. Baseline)")
    })
    
    output$delta_badge <- renderText({
      "Positive Δ = Worsening | Negative Δ = Improving"
    })
    
    output$station_delta_chart <- renderPlotly({
      df <- filtered_trend_data() %>% filter(!is.na(pollutant_avg))
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      times <- sort(unique(df$observation_time))
      if (length(times) < 2) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Requires at least 2 observation timestamps to calculate delta shifts.", color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      t_first <- times[1]
      t_last <- times[length(times)]
      
      # Calculate delta per station
      deltas <- df %>%
        filter(observation_time %in% c(t_first, t_last)) %>%
        group_by(station, city, state) %>%
        filter(n_distinct(observation_time) == 2) %>%
        summarise(
          val_initial = pollutant_avg[observation_time == t_first],
          val_final = pollutant_avg[observation_time == t_last],
          delta = val_final - val_initial,
          .groups = "drop"
        )
      
      if (nrow(deltas) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No co-located stations reported valid measurements for both timestamps.", color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Top 5 degrading (highest positive delta) and top 5 improving (lowest negative delta)
      top_worse <- deltas %>% arrange(desc(delta)) %>% head(6)
      top_better <- deltas %>% arrange(delta) %>% head(6)
      plot_df <- bind_rows(top_worse, top_better) %>%
        distinct(station, .keep_all = TRUE) %>%
        mutate(
          status = ifelse(delta > 0, "Accumulation (+Δ)", "Dispersion (-Δ)"),
          station_label = paste0(substr(station, 1, 26), "... (", city, ")"),
          station_label = reorder(station_label, delta),
          hover_txt = paste0(
            "Station: ", station, "\n",
            "City: ", city, ", ", state, "\n",
            "Baseline: ", round(val_initial, 1), " ", unit, "\n",
            "Latest: ", round(val_final, 1), " ", unit, "\n",
            "Shift (Δ): ", ifelse(delta > 0, "+", ""), round(delta, 1), " ", unit
          )
        )
      
      p <- ggplot(plot_df, aes(x = delta, y = station_label, fill = status)) +
        geom_col(width = 0.65, alpha = 0.85) +
        geom_vline(xintercept = 0, color = "#64748B", linetype = "dashed", linewidth = 0.7) +
        scale_fill_manual(values = c("Accumulation (+Δ)" = "#DC2626", "Dispersion (-Δ)" = "#059669")) +
        labs(
          x = paste0("Concentration Shift Δ (", unit, ")"),
          y = NULL,
          fill = "Shift Type"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 9, color = "#1E293B"),
          axis.title = element_text(size = 9.5, color = "#475569")
        )
      
      suppressWarnings(ggplotly(p, tooltip = "hover_txt")) %>%
        layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Understand Station Delta Ranking Chart
    output$delta_interpretation <- renderUI({
      df <- filtered_trend_data() %>% filter(!is.na(pollutant_avg))
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      times <- sort(unique(df$observation_time))
      
      findings <- if (length(times) < 2) {
        "Requires at least two distinct chronological observation snapshots to calculate net station-level shifts. Currently displaying the single-timestamp baseline (23-Oct-2026 18:00 IST)."
      } else {
        t_first <- times[1]
        t_last <- times[length(times)]
        
        deltas <- df %>%
          filter(observation_time %in% c(t_first, t_last)) %>%
          group_by(station) %>%
          filter(n_distinct(observation_time) == 2) %>%
          summarise(
            delta = pollutant_avg[observation_time == t_last] - pollutant_avg[observation_time == t_first],
            .groups = "drop"
          )
        
        if (nrow(deltas) == 0) {
          "No co-located stations reported valid measurements across both observation timestamps."
        } else {
          inc_count <- sum(deltas$delta > 0)
          dec_count <- sum(deltas$delta < 0)
          tot <- nrow(deltas)
          paste0(
            "Among ", tot, " paired monitoring stations between ", format(t_first, "%d-%b %H:%M"),
            " and ", format(t_last, "%d-%b %H:%M"), ": ",
            inc_count, " stations (", round(100 * inc_count / tot, 1), "%) recorded increasing concentrations, while ",
            dec_count, " stations (", round(100 * dec_count / tot, 1), "%) recorded decreasing concentrations."
          )
        }
      }
      
      build_understand_chart(
        chart_type = "Diverging Horizontal Bar Chart",
        what_shows = "This diverging bar chart ranks monitoring stations by their net concentration change (Δ = Latest Reading − Baseline Reading), highlighting the greatest worsening (spikes) and greatest improvements (dispersion).",
        how_to_read = "The vertical y-axis lists individual stations. The horizontal x-axis measures the magnitude of change (Δ) in pollutant concentration. Bars extending to the right of zero indicate increasing pollution; bars extending to the left indicate decreasing pollution.",
        colors_meaning = "Crimson / coral bars represent positive deltas (worsening air quality), while emerald green bars represent negative deltas (improving air quality).",
        dynamic_findings = findings,
        important_note = "Delta calculations require identical, collocated monitoring stations reporting valid measurements across both observation timestamps."
      )
    })
    
    # 6.3 TAB 3: State-Level Progression Chart
    output$state_progression_title <- renderText({
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      paste0("State-Level Progression: Mean ", pol, " Concentration (", unit, ")")
    })
    
    output$state_progression_chart <- renderPlotly({
      df <- filtered_trend_data() %>% filter(!is.na(pollutant_avg))
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      if (nrow(df) == 0 || length(unique(df$observation_time)) < 2) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Insufficient time-series observations to calculate state progression.", color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Top states by station count
      top_states <- df %>% count(state, sort = TRUE) %>% head(8) %>% pull(state)
      
      state_agg <- df %>%
        filter(state %in% top_states) %>%
        group_by(state, observation_time) %>%
        summarise(mean_val = round(mean(pollutant_avg, na.rm = TRUE), 1), .groups = "drop") %>%
        arrange(observation_time) %>%
        mutate(
          time_label = format(observation_time, "%d-%b %H:%M"),
          hover_txt = paste0("State: ", state, "\nTime: ", time_label, " IST\nMean ", pol, ": ", mean_val, " ", unit)
        )
      
      p <- ggplot(state_agg, aes(x = observation_time, y = mean_val, color = state, group = state)) +
        geom_line(linewidth = 1, alpha = 0.85) +
        suppressWarnings(geom_point(aes(text = hover_txt), size = 2.5)) +
        scale_color_viridis_d(option = "plasma") +
        scale_x_datetime(date_labels = "%d-%b\n%H:%M", breaks = scales::pretty_breaks(n = 6)) +
        labs(
          x = "Observation Timestamp (IST)",
          y = paste0("State Mean Concentration (", unit, ")"),
          color = "State / UT"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 9, color = "#1E293B"),
          axis.title = element_text(size = 10, color = "#475569")
        )
      
      suppressWarnings(ggplotly(p, tooltip = "text")) %>%
        layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Understand State-Level Progression Chart
    output$state_progression_interpretation <- renderUI({
      df <- filtered_trend_data() %>% filter(!is.na(pollutant_avg))
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      times <- sort(unique(df$observation_time))
      
      findings <- if (length(times) < 2) {
        "Requires multiple observation timestamps to compute state-level chronological trajectories. Currently gated in single-timestamp mode (23-Oct-2026 18:00 IST)."
      } else {
        top_states <- df %>% count(state, sort = TRUE) %>% head(8) %>% pull(state)
        state_agg <- df %>%
          filter(state %in% top_states) %>%
          group_by(state, observation_time) %>%
          summarise(mean_val = round(mean(pollutant_avg, na.rm = TRUE), 1), .groups = "drop") %>%
          arrange(observation_time)
        
        latest_time <- max(times)
        latest_ranks <- state_agg %>%
          filter(observation_time == latest_time) %>%
          arrange(desc(mean_val))
        
        highest_state <- latest_ranks$state[1]
        highest_val <- latest_ranks$mean_val[1]
        
        paste0(
          "Tracking ", length(top_states), " high-density states across ", length(times), " observation timestamps. ",
          "At the latest snapshot (", format(latest_time, "%d-%b %H:%M"), " IST), ", highest_state,
          " reported the highest mean concentration of ", pol, " at ", highest_val, " ", unit, "."
        )
      }
      
      build_understand_chart(
        chart_type = "Multi-Series Trend Line Chart",
        what_shows = "This chart tracks the aggregate mean concentration of the selected pollutant across top reporting states over successive observation timestamps, comparing regional-scale air quality trajectories.",
        how_to_read = "The horizontal x-axis marks chronological timestamps. The vertical y-axis displays the state-wide mean pollutant concentration. Each colored line represents a distinct state or Union Territory. Hover over markers to see exact state averages at each timestamp.",
        colors_meaning = "A plasma multi-hue palette assigns a unique color to each state for visual distinction.",
        dynamic_findings = findings,
        important_note = "State-wide averages aggregate heterogeneous urban and rural stations within a jurisdiction and should not be used as a proxy for localized personal exposure."
      )
    })
    
    # 6.4 TAB 4: Longitudinal Data Table & Export
    output$longitudinal_data_table <- renderDT({
      df <- filtered_trend_data()
      
      table_df <- df %>%
        select(
          Station = station,
          City = city,
          State = state,
          Pollutant = pollutant_id,
          `Avg Reading` = pollutant_avg,
          `Observation Time` = last_update_raw,
          `Collection Time` = collection_time
        )
      
      datatable(
        table_df,
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
        rownames = FALSE,
        class = "display compact stripe hover"
      )
    })
    
    output$btn_dl_longitudinal_csv <- downloadHandler(
      filename = function() {
        paste0("cpcb_longitudinal_dataset_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- filtered_trend_data()
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
