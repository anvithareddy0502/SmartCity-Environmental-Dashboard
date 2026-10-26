# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/data_loader.R
# Purpose: Robust data loading from genuine CPCB CSV, preserving original data,
#          parsing timestamps, and enforcing explicit column specifications.
# ==============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
})

#' Find and return the valid path to the baseline air quality dataset
find_data_path <- function(custom_path = NULL) {
  if (!is.null(custom_path) && file.exists(custom_path)) {
    return(custom_path)
  }
  
  candidate_paths <- c(
    "data/air_quality.csv",
    "./data/air_quality.csv",
    "../data/air_quality.csv",
    file.path(getwd(), "data", "air_quality.csv")
  )
  
  for (p in candidate_paths) {
    if (file.exists(p)) {
      return(normalizePath(p, mustWork = TRUE))
    }
  }
  
  stop("CRITICAL ERROR: Unable to locate baseline dataset 'data/air_quality.csv'. Ensure the file exists in the data directory.")
}

#' Load and parse genuine CPCB air quality dataset
#' 
#' @param file_path Optional explicit path to the CSV file
#' @return A list containing the parsed dataframe, raw count, metadata, and data freshness info
load_genuine_air_quality <- function(file_path = NULL) {
  resolved_path <- find_data_path(file_path)
  
  # Strict column type definitions matching official CPCB data.gov.in resource
  col_specs <- cols(
    country = col_character(),
    state = col_character(),
    city = col_character(),
    station = col_character(),
    last_update = col_character(),
    latitude = col_double(),
    longitude = col_double(),
    pollutant_id = col_character(),
    pollutant_min = col_double(),
    pollutant_max = col_double(),
    pollutant_avg = col_double()
  )
  
  raw_df <- read_csv(
    resolved_path,
    col_types = col_specs,
    trim_ws = TRUE,
    show_col_types = FALSE
  )
  
  # Timestamp of when the dashboard/script read this file from storage
  collection_time <- Sys.time()
  
  # Clean and parse without modifying original readings or dropping rows
  cleaned_df <- raw_df %>%
    mutate(
      country = str_trim(country),
      state = str_trim(state),
      city = str_trim(city),
      station = str_trim(station),
      pollutant_id = str_to_upper(str_trim(pollutant_id)),
      # Preserve raw string timestamp
      last_update_raw = last_update,
      # Parse Indian date format: DD-MM-YYYY HH:MM
      observation_time = dmy_hm(last_update, tz = "Asia/Kolkata"),
      collection_time = collection_time
    )
  
  # Unique timestamps present in the source observations
  unique_obs_times <- unique(na.omit(cleaned_df$observation_time))
  has_multi_timestamps <- length(unique_obs_times) > 1
  latest_obs_time <- if (length(unique_obs_times) > 0) max(unique_obs_times) else NA
  
  # Compute freshness latency (difference between current time and observation time)
  freshness_latency_hours <- if (!is.na(latest_obs_time)) {
    as.numeric(difftime(Sys.time(), latest_obs_time, units = "hours"))
  } else {
    NA_real_
  }
  
  metadata <- list(
    source = "CPCB / Open Government Data Platform India (data.gov.in)",
    source_url = "https://www.data.gov.in/resource/real-time-air-quality-index-various-locations",
    file_path = resolved_path,
    file_size_bytes = file.info(resolved_path)$size,
    total_records = nrow(cleaned_df),
    stations_represented = n_distinct(cleaned_df$station),
    cities_represented = n_distinct(cleaned_df$city),
    states_represented = n_distinct(cleaned_df$state),
    unique_observation_timestamps = length(unique_obs_times),
    latest_observation_time = latest_obs_time,
    collection_time = collection_time,
    has_multi_timestamps = has_multi_timestamps,
    is_live = FALSE,
    mode_label = "CPCB Baseline Snapshot (Offline Mode)",
    freshness_latency_hours = round(freshness_latency_hours, 1)
  )
  
  return(list(
    data = cleaned_df,
    metadata = metadata
  ))
}
