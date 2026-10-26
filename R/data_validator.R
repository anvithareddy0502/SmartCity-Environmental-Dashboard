# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/data_validator.R
# Purpose: Data validation, missing value audits, outlier detection without
#          destructive zero-imputation, and wide-matrix alignment for analysis.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

#' Validate geographical coordinates against the Indian subcontinental bounding box
validate_coordinates <- function(df) {
  # Bounding box for Indian territory: Lat [6.0, 38.0] N, Lon [68.0, 98.0] E
  invalid_lat <- which(is.na(df$latitude) | df$latitude < 6.0 | df$latitude > 38.0)
  invalid_lon <- which(is.na(df$longitude) | df$longitude < 68.0 | df$longitude > 98.0)
  
  invalid_rows <- unique(c(invalid_lat, invalid_lon))
  
  list(
    is_valid = (length(invalid_rows) == 0),
    invalid_count = length(invalid_rows),
    invalid_indices = invalid_rows,
    bounding_box = list(lat_min = 6.0, lat_max = 38.0, lon_min = 68.0, lon_max = 98.0)
  )
}

#' Audit potential duplicate records based on composite station key
audit_duplicates <- function(df) {
  dup_rows <- df %>%
    group_by(station, pollutant_id, last_update_raw) %>%
    filter(n() > 1) %>%
    ungroup()
  
  list(
    is_clean = (nrow(dup_rows) == 0),
    duplicate_count = nrow(dup_rows),
    duplicates = dup_rows
  )
}

#' Perform comprehensive missing value and reporting completeness audit
audit_missing_values <- function(df) {
  total_rows <- nrow(df)
  
  overall_summary <- df %>%
    summarise(
      total_rows = n(),
      na_min = sum(is.na(pollutant_min)),
      na_max = sum(is.na(pollutant_max)),
      na_avg = sum(is.na(pollutant_avg)),
      pct_complete = round(100 * sum(!is.na(pollutant_avg)) / n(), 2)
    )
  
  pollutant_summary <- df %>%
    group_by(pollutant_id) %>%
    summarise(
      stations_represented = n(),
      valid_readings = sum(!is.na(pollutant_avg)),
      missing_readings = sum(is.na(pollutant_avg)),
      pct_completeness = round(100 * valid_readings / stations_represented, 2),
      min_reported = suppressWarnings(min(pollutant_avg, na.rm = TRUE)),
      median_reported = suppressWarnings(median(pollutant_avg, na.rm = TRUE)),
      mean_reported = suppressWarnings(round(mean(pollutant_avg, na.rm = TRUE), 2)),
      max_reported = suppressWarnings(max(pollutant_avg, na.rm = TRUE)),
      .groups = "drop"
    )
  
  state_completeness <- df %>%
    group_by(state) %>%
    summarise(
      total_records = n(),
      stations_represented = n_distinct(station),
      valid_readings = sum(!is.na(pollutant_avg)),
      missing_readings = sum(is.na(pollutant_avg)),
      pct_completeness = round(100 * valid_readings / total_records, 2),
      .groups = "drop"
    ) %>%
    arrange(pct_completeness)
  
  list(
    overall = overall_summary,
    by_pollutant = pollutant_summary,
    by_state = state_completeness
  )
}

#' Detect statistical anomalies and sensor flatlines without modifying raw values
detect_anomalies <- function(df) {
  # Flag zero flatlines: where min == max == avg == 0 (potential sensor pinned to 0)
  flatlines <- df %>%
    filter(!is.na(pollutant_avg) & pollutant_min == 0 & pollutant_max == 0 & pollutant_avg == 0) %>%
    mutate(anomaly_type = "Zero Flatline (Min=Max=Avg=0)")
  
  # Non-destructive statistical outliers per pollutant using Tukey's 1.5*IQR rule
  outlier_list <- list()
  pollutants <- unique(df$pollutant_id)
  
  for (pol in pollutants) {
    sub_df <- df %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
    if (nrow(sub_df) > 10) {
      q1 <- quantile(sub_df$pollutant_avg, 0.25, na.rm = TRUE)
      q3 <- quantile(sub_df$pollutant_avg, 0.75, na.rm = TRUE)
      iqr_val <- q3 - q1
      upper_fence <- q3 + 1.5 * iqr_val
      lower_fence <- max(0, q1 - 1.5 * iqr_val)
      
      outliers <- sub_df %>%
        filter(pollutant_avg > upper_fence | pollutant_avg < lower_fence) %>%
        mutate(
          anomaly_type = if_else(pollutant_avg > upper_fence, "Statistical Upper Outlier (> Q3+1.5*IQR)", "Statistical Lower Outlier (< Q1-1.5*IQR)"),
          threshold_fence = upper_fence
        )
      
      outlier_list[[pol]] <- outliers
    }
  }
  
  all_outliers <- bind_rows(outlier_list)
  
  list(
    flatlines = flatlines,
    outliers = all_outliers,
    total_flatlines = nrow(flatlines),
    total_outliers = nrow(all_outliers)
  )
}

#' Create wide-format aligned data frame of pollutants per station
#' Preserves NAs without imputing zeros for fair correlation and multi-pollutant comparison
create_aligned_wide_data <- function(df) {
  wide_df <- df %>%
    select(state, city, station, latitude, longitude, last_update_raw, observation_time, pollutant_id, pollutant_avg) %>%
    pivot_wider(
      names_from = pollutant_id,
      values_from = pollutant_avg
    )
  
  # Pairwise reporting count matrix (to disclose sample size for correlations)
  pollutant_cols <- intersect(c("CO", "NH3", "NO2", "OZONE", "PM10", "PM2.5", "SO2"), names(wide_df))
  pairwise_matrix <- matrix(NA, nrow = length(pollutant_cols), ncol = length(pollutant_cols),
                            dimnames = list(pollutant_cols, pollutant_cols))
  
  for (p1 in pollutant_cols) {
    for (p2 in pollutant_cols) {
      valid_both <- sum(!is.na(wide_df[[p1]]) & !is.na(wide_df[[p2]]))
      pairwise_matrix[p1, p2] <- valid_both
    }
  }
  
  list(
    wide_data = wide_df,
    pollutant_columns = pollutant_cols,
    pairwise_valid_counts = pairwise_matrix
  )
}
