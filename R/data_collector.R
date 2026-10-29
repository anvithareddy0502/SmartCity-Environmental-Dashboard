# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/data_collector.R
# Purpose: Manages interaction with the official data.gov.in CPCB endpoint,
#          handles rate-limits, timeouts, and server errors safely,
#          standardizes incoming schemas, enforces rigorous deduplication,
#          and archives genuine new snapshots without altering baseline data.
# ==============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
})

API_ENDPOINT <- "https://api.data.gov.in/resource/3b01bcb8-0b14-4abf-b6f2-c1bfd384ba69"

#' Canonical CPCB Schema Normalization
#' Maps raw API or CSV fields to standard CPCB columns with strict data typing
normalize_cpcb_schema <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  
  # Remove duplicate column names if already present
  df <- df[, !duplicated(colnames(df)), drop = FALSE]
  
  # Map API-specific field names to standard schema names
  col_map <- c(
    "min_value" = "pollutant_min",
    "max_value" = "pollutant_max",
    "avg_value" = "pollutant_avg"
  )
  
  new_names <- colnames(df)
  for (i in seq_along(colnames(df))) {
    low <- tolower(colnames(df)[i])
    if (low %in% names(col_map)) {
      new_names[i] <- col_map[[low]]
    }
  }
  colnames(df) <- new_names
  df <- df[, !duplicated(colnames(df)), drop = FALSE]
  
  req_cols <- c("country", "state", "city", "station", "last_update", "latitude", "longitude",
                "pollutant_id", "pollutant_min", "pollutant_max", "pollutant_avg")
  for (rc in req_cols) {
    if (!rc %in% colnames(df)) {
      df[[rc]] <- NA
    }
  }
  
  raw_update <- if ("last_update_raw" %in% colnames(df)) df$last_update_raw else df$last_update
  
  df %>%
    mutate(
      country = str_trim(as.character(country)),
      state = str_trim(as.character(state)),
      city = str_trim(as.character(city)),
      station = str_trim(as.character(station)),
      pollutant_id = str_to_upper(str_trim(as.character(pollutant_id))),
      last_update_raw = as.character(raw_update),
      latitude = suppressWarnings(as.numeric(latitude)),
      longitude = suppressWarnings(as.numeric(longitude)),
      pollutant_min = suppressWarnings(as.numeric(pollutant_min)),
      pollutant_max = suppressWarnings(as.numeric(pollutant_max)),
      pollutant_avg = suppressWarnings(as.numeric(pollutant_avg)),
      observation_time = parse_date_time(
        last_update, 
        orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM", "d-m-Y H:M:S", "d-m-Y H:M", "Y-m-d H:M:S"), 
        tz = "Asia/Kolkata"
      ),
      collection_time = if ("collection_time" %in% colnames(df)) as.POSIXct(df$collection_time) else Sys.time()
    ) %>%
    filter(!is.na(observation_time))
}

#' Test connectivity and query the official CPCB data endpoint
#' 
#' @param api_key data.gov.in API key. If NULL, checks environment variables then default public key.
#' @param max_records Maximum records to retrieve (paginates in batches of 10).
#' @param timeout_sec Network timeout in seconds.
#' @return A list with retrieval status, error diagnostics, and normalized payload if successful.
fetch_cpcb_live_snapshot <- function(api_key = NULL, max_records = 50, timeout_sec = 10) {
  # Resolve API key in priority: explicit argument -> environment variable -> public fallback key
  key_to_use <- if (!is.null(api_key) && nzchar(str_trim(api_key))) {
    str_trim(api_key)
  } else if (nzchar(Sys.getenv("DATA_GOV_IN_API_KEY"))) {
    Sys.getenv("DATA_GOV_IN_API_KEY")
  } else if (nzchar(Sys.getenv("CPCB_API_KEY"))) {
    Sys.getenv("CPCB_API_KEY")
  } else {
    "579b464db66ec23bdd000001cdd3946e44ce4aad7209ff7b23ac571b"
  }
  
  collection_timestamp <- Sys.time()
  all_records <- list()
  offset <- 0
  limit_per_call <- 10 # data.gov.in public cap per request
  
  # Fetch initial page to verify connectivity and total available records
  res <- tryCatch({
    GET(
      API_ENDPOINT,
      query = list("api-key" = key_to_use, "format" = "json", "offset" = offset, "limit" = limit_per_call),
      timeout(timeout_sec),
      user_agent("VIT-CSI3005-Academic-Dashboard/1.0 (R httr)")
    )
  }, error = function(e) {
    return(list(error = TRUE, message = e$message))
  })
  
  # Network or DNS failure
  if (!is.null(res$error) && res$error) {
    return(list(
      success = FALSE,
      status_code = NA_integer_,
      error_type = "Network/DNS Failure",
      message = paste0("Network error connecting to data.gov.in: ", res$message),
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = 0
    ))
  }
  
  code <- status_code(res)
  
  if (code == 429) {
    return(list(
      success = FALSE,
      status_code = 429,
      error_type = "Rate Limited",
      message = "data.gov.in API rate limit reached (HTTP 429). Reverting to local archive.",
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = 0
    ))
  } else if (code == 500) {
    return(list(
      success = FALSE,
      status_code = 500,
      error_type = "Upstream Server Error",
      message = "Upstream data.gov.in server returned HTTP 500 (documented Slim/OAuth backend issue). Reverting to baseline dataset.",
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = 0
    ))
  } else if (code == 401 || code == 403) {
    return(list(
      success = FALSE,
      status_code = code,
      error_type = "Authentication Error",
      message = paste0("Unauthorized API key (HTTP ", code, "). Please provide a valid data.gov.in API key in settings."),
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = 0
    ))
  } else if (code != 200) {
    return(list(
      success = FALSE,
      status_code = code,
      error_type = paste("HTTP Error", code),
      message = paste("Unexpected status code returned from data.gov.in:", code),
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = 0
    ))
  }
  
  content_raw <- content(res, as = "text", encoding = "UTF-8")
  parsed_json <- tryCatch(fromJSON(content_raw), error = function(e) NULL)
  
  if (is.null(parsed_json)) {
    return(list(
      success = FALSE,
      status_code = 200,
      error_type = "Malformed JSON",
      message = "data.gov.in returned an unparseable or malformed response body.",
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = 0
    ))
  }
  
  # Check for error message encoded inside JSON body (e.g. {"error": "Rate limit exceeded"})
  if (!is.null(parsed_json$error)) {
    err_msg <- as.character(parsed_json$error)
    is_rate_limit <- grepl("rate limit", tolower(err_msg))
    return(list(
      success = FALSE,
      status_code = if (is_rate_limit) 429 else 200,
      error_type = if (is_rate_limit) "Rate Limited" else "API Error",
      message = paste0("data.gov.in reported: ", err_msg),
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = 0
    ))
  }
  
  rec_first <- parsed_json$records
  first_count <- if (is.data.frame(rec_first)) nrow(rec_first) else length(rec_first)
  
  if (is.null(rec_first) || first_count == 0) {
    return(list(
      success = FALSE,
      status_code = 200,
      error_type = "Empty Payload",
      message = "data.gov.in returned an empty record payload (0 observations).",
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = if (!is.null(parsed_json$total)) as.numeric(parsed_json$total) else 0
    ))
  }
  
  total_available <- if (!is.null(parsed_json$total)) as.numeric(parsed_json$total) else 0
  all_records[[1]] <- if (is.data.frame(rec_first)) rec_first else as.data.frame(rec_first)
  offset <- offset + first_count
  
  # Fetch subsequent pages up to max_records with courtesy delay
  while (offset < max_records && offset < total_available) {
    Sys.sleep(0.12) # Courteous delay to respect data.gov.in rate limits
    res_page <- tryCatch({
      GET(
        API_ENDPOINT,
        query = list("api-key" = key_to_use, "format" = "json", "offset" = offset, "limit" = limit_per_call),
        timeout(timeout_sec),
        user_agent("VIT-CSI3005-Academic-Dashboard/1.0 (R httr)")
      )
    }, error = function(e) NULL)
    
    if (is.null(res_page) || status_code(res_page) != 200) break
    
    page_txt <- content(res_page, as = "text", encoding = "UTF-8")
    page_json <- tryCatch(fromJSON(page_txt), error = function(e) NULL)
    rec_page <- if (!is.null(page_json)) page_json$records else NULL
    page_count <- if (is.data.frame(rec_page)) nrow(rec_page) else length(rec_page)
    if (is.null(page_json) || is.null(rec_page) || page_count == 0) break
    
    all_records[[length(all_records) + 1]] <- if (is.data.frame(rec_page)) rec_page else as.data.frame(rec_page)
    offset <- offset + page_count
  }
  
  raw_batch <- bind_rows(all_records)
  norm_batch <- normalize_cpcb_schema(raw_batch)
  
  # Validate that valid observations were parsed
  if (nrow(norm_batch) == 0) {
    return(list(
      success = FALSE,
      status_code = 200,
      error_type = "Invalid Schema",
      message = "Response was received but contained no valid observations matching the CPCB criteria pollutant schema.",
      collection_time = collection_timestamp,
      is_live = FALSE,
      records = NULL,
      total_count = total_available
    ))
  }
  
  # Tag source
  norm_batch$source <- "CPCB / data.gov.in CAAQM (Resource: 3b01bcb8)"
  
  list(
    success = TRUE,
    status_code = 200,
    error_type = NULL,
    message = paste0("Successfully harvested ", nrow(norm_batch), " genuine records from data.gov.in (Total available: ", total_available, ")"),
    collection_time = collection_timestamp,
    is_live = TRUE,
    records = norm_batch,
    total_count = total_available
  )
}

#' Archive a newly retrieved snapshot without overwriting baseline data
#' Validates input data and refuses to create empty or corrupt snapshot files.
archive_new_snapshot <- function(records_df, archive_dir = "data/archive") {
  if (!dir.exists(archive_dir)) {
    dir.create(archive_dir, recursive = TRUE)
  }
  
  if (is.null(records_df) || nrow(records_df) == 0) {
    return(NULL)
  }
  
  norm_df <- normalize_cpcb_schema(records_df)
  if (nrow(norm_df) == 0) return(NULL)
  
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  unique_suffix <- paste(sample(c(0:9, letters), 4, replace = TRUE), collapse = "")
  archive_file <- file.path(archive_dir, paste0("snapshot_", timestamp_str, "_", unique_suffix, ".csv"))
  
  # Format collection_time as string for CSV portability
  if (!"collection_time" %in% colnames(norm_df)) {
    norm_df$collection_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  } else {
    norm_df$collection_time <- format(norm_df$collection_time, "%Y-%m-%d %H:%M:%S")
  }
  
  # Ensure source is recorded
  if (!"source" %in% colnames(norm_df)) {
    norm_df$source <- "CPCB / data.gov.in CAAQM (Resource: 3b01bcb8)"
  }
  
  write_csv(norm_df, archive_file)
  invalidate_historical_cache()
  return(archive_file)
}
# Global in-memory cache environment for historical repository
.historical_repo_cache <- new.env(parent = emptyenv())
.historical_repo_cache$data <- NULL
.historical_repo_cache$signature <- ""

#' Compute signature of archive directory for fast staleness detection
compute_archive_signature <- function(archive_dir = "data/archive") {
  if (!dir.exists(archive_dir)) return("no_dir")
  files <- list.files(archive_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return("empty")
  f_info <- file.info(files)
  paste(basename(files), f_info$size, as.numeric(f_info$mtime), collapse = ";")
}

#' Invalidate the in-memory historical repository cache
invalidate_historical_cache <- function() {
  .historical_repo_cache$data <- NULL
  .historical_repo_cache$signature <- ""
}

#' Ingest and merge all genuine historical snapshots from data/archive
#' Combines with baseline data and deduplicates on composite key: (station, pollutant_id, observation_time)
#' Uses signature-based in-memory caching to avoid redundant disk I/O.
load_historical_repository <- function(base_df = BASE_DATA, archive_dir = "data/archive", force_refresh = FALSE) {
  # Fast in-memory cache check
  curr_signature <- compute_archive_signature(archive_dir)
  if (!force_refresh && !is.null(.historical_repo_cache$data) && identical(.historical_repo_cache$signature, curr_signature)) {
    return(.historical_repo_cache$data)
  }

  base_clean <- normalize_cpcb_schema(base_df)
  if (!"source" %in% colnames(base_clean)) {
    base_clean$source <- "CPCB Baseline Snapshot (data/air_quality.csv)"
  }
  all_dfs <- list(base_clean)
  archive_files_count <- 0
  archive_filenames <- character(0)
  
  if (dir.exists(archive_dir)) {
    archive_files <- list.files(archive_dir, pattern = "\\.csv$", full.names = TRUE)
    archive_files_count <- length(archive_files)
    archive_filenames <- basename(archive_files)
    
    for (f in archive_files) {
      tryCatch({
        raw <- read_csv(f, show_col_types = FALSE, trim_ws = TRUE)
        clean_f <- normalize_cpcb_schema(raw)
        if (nrow(clean_f) > 0) {
          if (!"source" %in% colnames(clean_f)) {
            clean_f$source <- paste0("Archived Snapshot (", basename(f), ")")
          }
          all_dfs[[length(all_dfs) + 1]] <- clean_f
        }
      }, error = function(e) {
        warning(paste("Could not parse archived file:", f, "-", e$message))
      })
    }
  }
  
  total_raw_rows <- sum(sapply(all_dfs, nrow))
  merged_df <- bind_rows(all_dfs)
  
  # Deduplicate based on composite key: station + pollutant_id + observation_time
  # Preserves earliest collection_time for each unique observation
  deduped_df <- merged_df %>%
    arrange(observation_time, collection_time) %>%
    distinct(station, pollutant_id, observation_time, .keep_all = TRUE)
  
  duplicates_removed <- total_raw_rows - nrow(deduped_df)
  
  # Unique sorted timestamps
  unique_times <- sort(unique(na.omit(deduped_df$observation_time)))
  T_count <- length(unique_times)
  
  # Calculate temporal depth per (station, pollutant_id) pair
  depth_df <- deduped_df %>%
    group_by(station, pollutant_id) %>%
    summarise(
      n_obs = n_distinct(observation_time),
      has_valid_readings = sum(!is.na(pollutant_avg)),
      earliest_obs = min(observation_time, na.rm = TRUE),
      latest_obs = max(observation_time, na.rm = TRUE),
      .groups = "drop"
    )
  
  stations_with_multi_timestamps <- sum(depth_df$n_obs >= 2)
  stations_with_ge_10_timestamps <- sum(depth_df$n_obs >= 10)
  stations_with_ge_12_timestamps <- sum(depth_df$n_obs >= 12)
  stations_with_ge_24_timestamps <- sum(depth_df$n_obs >= 24)
  max_station_depth <- if (nrow(depth_df) > 0) max(depth_df$n_obs) else 0
  
  # Readiness assessment
  has_multi <- (T_count >= 2)
  has_station_time_series <- (stations_with_multi_timestamps > 0)
  has_sufficient_history <- (stations_with_ge_10_timestamps > 0 && max_station_depth >= 10)
  
  readiness_status <- if (T_count < 2) {
    "INSUFFICIENT_HISTORY: Single-timestamp baseline snapshot only (1 observation per station). Temporal trajectory lines and time-series forecasting are scientifically gated to prevent fabricating synthetic predictions."
  } else if (!has_station_time_series) {
    "DISJOINT_SNAPSHOTS: Multiple timestamps exist across the dataset, but no individual monitoring station has observations across multiple timestamps. Co-located time-series continuity is absent."
  } else if (!has_sufficient_history) {
    paste0("PARTIAL_LONGITUDINAL: ", stations_with_multi_timestamps, " station-pollutant pairs have 2 to ", max_station_depth, " timestamps. Sufficient for delta comparison (Δ) and trajectory visualization, but insufficient for statistical model training (requires ≥10 consecutive observations).")
  } else {
    paste0("FORECAST_READY: ", stations_with_ge_10_timestamps, " station series possess sufficient longitudinal history (≥10 timestamps; max depth ", max_station_depth, ") for statistical and baseline model evaluation.")
  }
  
  # Extract latest collection time
  valid_coll_times <- na.omit(deduped_df$collection_time)
  last_coll_time <- if (length(valid_coll_times) > 0) max(valid_coll_times) else NA
  
  repo_result <- list(
    data = deduped_df,
    timestamps = unique_times,
    distinct_timestamps_count = T_count,
    has_multi_timestamps = has_multi,
    has_station_time_series = has_station_time_series,
    has_sufficient_history_for_forecasting = has_sufficient_history,
    stations_with_multi_timestamps = stations_with_multi_timestamps,
    stations_with_ge_10_timestamps = stations_with_ge_10_timestamps,
    stations_with_ge_12_timestamps = stations_with_ge_12_timestamps,
    stations_with_ge_24_timestamps = stations_with_ge_24_timestamps,
    max_station_temporal_depth = max_station_depth,
    station_temporal_depth = depth_df,
    total_records = nrow(deduped_df),
    duplicates_removed = duplicates_removed,
    distinct_stations = n_distinct(deduped_df$station),
    distinct_cities = n_distinct(deduped_df$city),
    distinct_states = n_distinct(deduped_df$state),
    archive_files_count = archive_files_count,
    archive_filenames = archive_filenames,
    earliest_time = if (T_count > 0) min(unique_times) else NA,
    latest_time = if (T_count > 0) max(unique_times) else NA,
    last_collection_time = last_coll_time,
    readiness_status = readiness_status,
    supported_analyses = if (T_count < 2) {
      "Baseline Cross-Sectional Spatial Analysis only. Gated: Temporal Trajectory Lines, Multi-Hour Rate of Change, and Time-Series Forecasting (requires ≥2 timestamps)."
    } else if (!has_sufficient_history) {
      paste0("Supported: Multi-Timestamp Trajectories and Station Deltas (Δ) across ", T_count, " observation timestamps. Gated: Auto-ARIMA & ML Forecasting (requires ≥10 consecutive timestamps).")
    } else {
      paste0("Supported: Full Time-Series Modeling, Auto-ARIMA, Naive Baseline Forecasting, Diurnal Curves, and Longitudinal Trajectories across ", T_count, " timestamps.")
    }
  )

  # Update in-memory cache
  .historical_repo_cache$data <- repo_result
  .historical_repo_cache$signature <- curr_signature

  repo_result
}

#' Validate and import an uploaded CPCB CSV snapshot into data/archive/
import_cpcb_snapshot <- function(temp_path, original_name = "snapshot.csv", archive_dir = "data/archive") {
  if (!dir.exists(archive_dir)) dir.create(archive_dir, recursive = TRUE)
  
  uploaded_df <- tryCatch({
    read_csv(temp_path, show_col_types = FALSE, trim_ws = TRUE)
  }, error = function(e) {
    return(list(success = FALSE, message = paste("CSV read error:", e$message)))
  })
  
  clean_df <- normalize_cpcb_schema(uploaded_df)
  if (nrow(clean_df) == 0) {
    return(list(
      success = FALSE,
      message = "Invalid schema or unparseable timestamps. Required columns: station, pollutant_id, pollutant_avg (or avg_value), last_update."
    ))
  }
  
  # Generate unique archive filename
  timestamp_str <- format(Sys.time(), "%Y%m%d_%H%M%S")
  safe_name <- gsub("[^a-zA-Z0-9_-]", "_", tools::file_path_sans_ext(original_name))
  dest_file <- file.path(archive_dir, paste0("snapshot_", safe_name, "_", timestamp_str, ".csv"))
  
  # Format collection_time for CSV portability
  clean_df$collection_time <- format(clean_df$collection_time, "%Y-%m-%d %H:%M:%S")
  clean_df$source <- paste0("Manual Upload (", original_name, ")")
  
  write_csv(clean_df, dest_file)
  invalidate_historical_cache()
  obs_times <- unique(na.omit(clean_df$observation_time))
  
  list(
    success = TRUE,
    file_path = dest_file,
    records_count = nrow(clean_df),
    stations_count = n_distinct(clean_df$station),
    observation_timestamps = obs_times,
    message = paste0("Successfully imported genuine CPCB snapshot with ", nrow(clean_df), 
                     " observations across ", n_distinct(clean_df$station), " stations.")
  )
}

#' Retrieve Data Collection Configuration
get_data_collection_config <- function(config_file = "config/data_collection_config.json") {
  if (file.exists(config_file)) {
    cfg <- tryCatch(jsonlite::fromJSON(config_file), error = function(e) NULL)
    if (!is.null(cfg)) return(cfg)
  }
  list(
    collection_interval_minutes = 0, # 0 = Disabled (Manual only)
    cooldown_seconds = 180,         # 3 minutes cooldown between public API requests
    max_records_per_fetch = 50,
    last_attempt_time = NULL,
    last_success_time = NULL,
    last_status = "Idle",
    last_error_message = NULL,
    custom_api_key = NULL
  )
}

#' Save Data Collection Configuration
save_data_collection_config <- function(cfg, config_file = "config/data_collection_config.json") {
  dir_name <- dirname(config_file)
  if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)
  
  json_txt <- jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json_txt, config_file)
  invisible(TRUE)
}

#' Unified Air Quality Data Collection Execution & Archive Orchestrator
#' Manages concurrency locks, cooldown periods, API retrieval, archive appending,
#' and telemetry reporting.
execute_data_collection_update <- function(api_key = NULL, archive_dir = "data/archive", 
                                           max_records = 50, force = FALSE, config_file = "config/data_collection_config.json") {
  cfg <- get_data_collection_config(config_file = config_file)
  now <- Sys.time()
  
  # Lock file check to prevent overlapping jobs
  lock_file <- file.path(tempdir(), "cpcb_collection.lock")
  if (file.exists(lock_file)) {
    lock_age <- as.numeric(difftime(now, file.info(lock_file)$mtime, units = "secs"))
    if (lock_age < 60) {
      return(list(
        success = FALSE,
        status_code = 409,
        error_type = "Job In Progress",
        message = "A data collection query is already currently executing. Please wait for completion.",
        collection_time = now,
        is_live = FALSE,
        new_records = 0,
        archived_file = NULL
      ))
    } else {
      unlink(lock_file)
    }
  }
  
  # Cooldown check (prevent excessive requests to public API)
  cooldown_sec <- if (!is.null(cfg$cooldown_seconds)) cfg$cooldown_seconds else 180
  if (!force && !is.null(cfg$last_attempt_time)) {
    last_attempt <- tryCatch(as.POSIXct(cfg$last_attempt_time), error = function(e) NULL)
    if (!is.null(last_attempt) && !is.na(last_attempt)) {
      elapsed <- as.numeric(difftime(now, last_attempt, units = "secs"))
      if (elapsed < cooldown_sec) {
        remaining <- ceiling(cooldown_sec - elapsed)
        return(list(
          success = FALSE,
          status_code = 429,
          error_type = "Cooldown Active",
          message = sprintf("Cooldown active (%d sec remaining). Please wait before querying again to avoid rate limits.", remaining),
          collection_time = now,
          is_live = FALSE,
          new_records = 0,
          archived_file = NULL
        ))
      }
    }
  }
  
  # Create lock file
  writeLines(as.character(Sys.getpid()), lock_file)
  on.exit(if (file.exists(lock_file)) unlink(lock_file), add = TRUE)
  
  # Execute fetch
  key_to_use <- if (!is.null(api_key) && nzchar(str_trim(api_key))) {
    str_trim(api_key)
  } else if (!is.null(cfg$custom_api_key) && nzchar(str_trim(cfg$custom_api_key))) {
    str_trim(cfg$custom_api_key)
  } else {
    NULL
  }
  
  api_res <- fetch_cpcb_live_snapshot(api_key = key_to_use, max_records = max_records)
  
  # Update attempt timestamp in config
  cfg$last_attempt_time <- format(now, "%Y-%m-%d %H:%M:%S")
  
  if (isTRUE(api_res$success) && !is.null(api_res$records) && nrow(api_res$records) > 0) {
    archived_file <- archive_new_snapshot(api_res$records, archive_dir = archive_dir)
    cfg$last_success_time <- format(now, "%Y-%m-%d %H:%M:%S")
    cfg$last_status <- "Success"
    cfg$last_error_message <- NULL
    save_data_collection_config(cfg, config_file = config_file)
    
    return(list(
      success = TRUE,
      status_code = 200,
      error_type = NULL,
      message = paste0("Successfully collected and archived ", nrow(api_res$records), " genuine observations into ", basename(archived_file)),
      collection_time = now,
      is_live = TRUE,
      new_records = nrow(api_res$records),
      archived_file = archived_file
    ))
  } else {
    cfg$last_status <- if (!is.null(api_res$error_type)) api_res$error_type else "Error"
    cfg$last_error_message <- api_res$message
    save_data_collection_config(cfg, config_file = config_file)
    
    return(list(
      success = FALSE,
      status_code = api_res$status_code,
      error_type = if (!is.null(api_res$error_type)) api_res$error_type else "Collection Failed",
      message = api_res$message,
      collection_time = now,
      is_live = FALSE,
      new_records = 0,
      archived_file = NULL
    ))
  }
}

#' Detect the verified installed Rscript executable path
get_system_rscript_path <- function() {
  # 1. R.home("bin")
  r_bin <- R.home("bin")
  cand1 <- file.path(r_bin, "Rscript.exe")
  if (file.exists(cand1)) return(normalizePath(cand1, winslash = "\\"))
  
  cand2 <- file.path(r_bin, "x64", "Rscript.exe")
  if (file.exists(cand2)) return(normalizePath(cand2, winslash = "\\"))
  
  # 2. Sys.which
  sw <- Sys.which("Rscript")
  if (nzchar(sw) && file.exists(sw)) return(normalizePath(sw, winslash = "\\"))
  
  # 3. Known system installation paths
  std_paths <- c(
    "C:\\Program Files\\R\\R-4.4.2\\bin\\Rscript.exe",
    "C:\\Program Files\\R\\R-4.4.3\\bin\\Rscript.exe",
    "C:\\Program Files\\R\\R-4.4.1\\bin\\Rscript.exe",
    "C:\\Program Files\\R\\R-4.4.0\\bin\\Rscript.exe"
  )
  for (sp in std_paths) {
    if (file.exists(sp)) return(sp)
  }
  
  "Rscript.exe"
}

#' Inspect and summarize background sync logs from logs/background_sync.log
get_background_sync_log_summary <- function(log_file = "logs/background_sync.log") {
  if (!file.exists(log_file)) {
    return(list(
      is_configured = FALSE,
      last_run_time = NULL,
      last_status = "Not Executed / No Logs Found",
      summary_text = "Background scheduler has not yet logged an execution.",
      log_lines = character(0)
    ))
  }
  
  lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character(0))
  if (length(lines) == 0) {
    return(list(
      is_configured = TRUE,
      last_run_time = format(file.info(log_file)$mtime, "%Y-%m-%d %H:%M:%S IST"),
      last_status = "Log Initialized",
      summary_text = "Log file exists but contains no entries yet.",
      log_lines = character(0)
    ))
  }
  
  # Extract timestamped lines
  ts_lines <- grep("^\\[[0-9]{4}-[0-9]{2}-[0-9]{2}", lines, value = TRUE)
  last_ts_str <- if (length(ts_lines) > 0) {
    sub("^\\[([^]]+)\\].*", "\\1", tail(ts_lines, 1))
  } else {
    format(file.info(log_file)$mtime, "%Y-%m-%d %H:%M:%S IST")
  }
  
  recent_20 <- tail(lines, 20)
  has_success <- any(grepl("\\[SUCCESS\\]", recent_20))
  has_rate_limit <- any(grepl("Rate Limited|HTTP 429", recent_20))
  has_network_err <- any(grepl("Network/DNS Failure|Timeout", recent_20))
  
  status_badge <- if (has_success) {
    "Operational (Last collection succeeded)"
  } else if (has_rate_limit) {
    "Rate Limited (OGD quota reached)"
  } else if (has_network_err) {
    "Network / Upstream Error"
  } else {
    "Active Worker"
  }
  
  list(
    is_configured = TRUE,
    last_run_time = last_ts_str,
    last_status = status_badge,
    summary_text = sprintf("Last execution logged at %s: %s", last_ts_str, status_badge),
    log_lines = tail(lines, 8)
  )
}
