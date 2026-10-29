# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/forecasting_engine.R
# Purpose: AI & Statistical Air Quality Forecasting Engine
#          Implements chronological train/test splitting, Persistence Naive Baseline,
#          Auto-ARIMA with prediction intervals, MAE/RMSE evaluation,
#          CPCB threshold alerts, and graceful handling of insufficient history.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(forecast)
  library(stats)
})

#' Prepare and validate a chronological time-series for a specific station & pollutant
#' 
#' @param df Dataframe containing CPCB observations (with station, pollutant_id, pollutant_avg, observation_time)
#' @param station_name Target station name (character)
#' @param target_pollutant "PM2.5" or "PM10" (character)
#' @param min_required_obs Minimum observations needed to declare time-series viable for statistical fitting (default: 10)
#' @return A list containing validation status, diagnostic message, and clean chronological data
prepare_station_series <- function(df, station_name, target_pollutant = "PM2.5", min_required_obs = 10) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      status = "NO_DATA",
      message = "Input dataset is empty or NULL.",
      is_viable = FALSE,
      can_forecast = FALSE,
      n_obs = 0,
      data = NULL
    ))
  }
  
  # Ensure column names are standardized
  if (!"pollutant_avg" %in% colnames(df) && "avg_value" %in% colnames(df)) {
    df$pollutant_avg <- df$avg_value
  }
  
  # Parse observation_time if character
  if (!inherits(df$observation_time, "POSIXt") && "last_update" %in% colnames(df)) {
    df$observation_time <- parse_date_time(
      df$last_update, 
      orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM", "d-m-Y H:M:S", "d-m-Y H:M", "Y-m-d H:M:S"), 
      tz = "Asia/Kolkata"
    )
  }
  
  # Filter station, pollutant, and valid values
  station_df <- df %>%
    filter(
      station == station_name,
      pollutant_id == target_pollutant,
      !is.na(pollutant_avg),
      !is.na(observation_time)
    ) %>%
    arrange(observation_time) %>%
    distinct(observation_time, .keep_all = TRUE)
  
  n_obs <- nrow(station_df)
  
  if (n_obs == 0) {
    return(list(
      status = "NO_OBSERVATIONS",
      message = paste0("Zero valid observations found for ", target_pollutant, " at station: '", station_name, "'."),
      is_viable = FALSE,
      can_forecast = FALSE,
      n_obs = 0,
      data = NULL
    ))
  }
  
  if (n_obs == 1) {
    return(list(
      status = "SINGLE_SNAPSHOT",
      message = paste0("Only 1 observation available (", format(station_df$observation_time[1], "%d-%b-%Y %H:%M"), 
                       " IST). Longitudinal time-series forecasting requires sequential chronological observations."),
      is_viable = FALSE,
      can_forecast = FALSE,
      n_obs = 1,
      data = station_df
    ))
  }
  
  if (n_obs < min_required_obs) {
    return(list(
      status = "INSUFFICIENT_HISTORY",
      message = paste0("Found ", n_obs, " observations, but a minimum of ", min_required_obs, 
                       " sequential observations is required to fit Auto-ARIMA and evaluate model performance."),
      is_viable = FALSE,
      can_forecast = FALSE,
      n_obs = n_obs,
      data = station_df
    ))
  }
  
  # Calculate time step intervals in hours
  time_diffs <- as.numeric(diff(station_df$observation_time), units = "hours")
  median_step_hours <- if (length(time_diffs) > 0) round(median(time_diffs, na.rm = TRUE), 2) else 1
  
  # Detect irregular or missing observation gaps (> 2.0x median step)
  missing_intervals_count <- if (length(time_diffs) > 0 && median_step_hours > 0) {
    sum(time_diffs > 2.0 * median_step_hours)
  } else {
    0
  }
  irregularity_pct <- if (length(time_diffs) > 0) round((missing_intervals_count / length(time_diffs)) * 100, 1) else 0.0
  is_regular <- (missing_intervals_count == 0 || irregularity_pct <= 15.0)
  total_span_hours <- round(as.numeric(difftime(max(station_df$observation_time), min(station_df$observation_time), units = "hours")), 1)
  
  list(
    status = "VIABLE",
    message = paste0("Viable time series with ", n_obs, " chronological observations (median interval: ", 
                     median_step_hours, " hrs, span: ", total_span_hours, " hrs)."),
    is_viable = TRUE,
    can_forecast = TRUE,
    n_obs = n_obs,
    median_step_hours = median_step_hours,
    missing_intervals_count = missing_intervals_count,
    irregularity_pct = irregularity_pct,
    is_regular = is_regular,
    total_span_hours = total_span_hours,
    earliest_time = min(station_df$observation_time),
    latest_time = max(station_df$observation_time),
    data = station_df
  )
}

#' Strictly chronological train/test partitioning (No random shuffling)
#' 
#' @param series_df Chronological data frame from prepare_station_series
#' @param train_ratio Proportion of points reserved for training (default: 0.75)
#' @param min_test Minimum test set points required for meaningful evaluation (default: 2)
#' @return List with train and test subsets
split_chronological <- function(series_df, train_ratio = 0.75, min_test = 2) {
  n_total <- nrow(series_df)
  
  # Ensure at least min_test observations in test partition
  n_train <- floor(n_total * train_ratio)
  n_test <- n_total - n_train
  
  if (n_test < min_test) {
    n_test <- min(min_test, n_total - 2)
    n_train <- n_total - n_test
  }
  
  list(
    train = series_df[1:n_train, ],
    test = series_df[(n_train + 1):n_total, ],
    n_train = n_train,
    n_test = n_test,
    train_end_time = max(series_df$observation_time[1:n_train]),
    test_start_time = min(series_df$observation_time[(n_train + 1):n_total])
  )
}

#' Calculate standard error metrics comparing actual vs predicted values
#' 
#' @param actual Numeric vector of observed test measurements
#' @param predicted Numeric vector of model predictions
#' @param pollutant Target pollutant (for unit labeling)
#' @return List of computed evaluation metrics (MAE, RMSE, MAPE, Directional Accuracy, Concordance)
calculate_forecast_metrics <- function(actual, predicted, pollutant = "PM2.5") {
  # Clean paired vectors
  valid_idx <- !is.na(actual) & !is.na(predicted)
  y <- actual[valid_idx]
  y_hat <- predicted[valid_idx]
  n <- length(y)
  
  if (n == 0) {
    return(list(
      n = 0,
      mae = NA_real_,
      rmse = NA_real_,
      mape = NA_real_,
      directional_accuracy_pct = NA_real_,
      category_concordance_pct = NA_real_,
      unit = ifelse(pollutant == "CO", "mg/m³", "µg/m³")
    ))
  }
  
  # 1. Mean Absolute Error
  mae <- mean(abs(y - y_hat))
  
  # 2. Root Mean Squared Error
  rmse <- sqrt(mean((y - y_hat)^2))
  
  # 3. Mean Absolute Percentage Error (strictly validated against near-zero denominators)
  has_near_zero <- any(y < 1.0)
  if (has_near_zero) {
    mape <- NA_real_
    mape_valid <- FALSE
    mape_note <- "MAPE omitted: observed readings near zero (< 1.0 µg/m³) violate percentage ratio denominator."
  } else {
    mape <- round(mean(abs(y - y_hat) / y) * 100, 1)
    mape_valid <- TRUE
    mape_note <- "Valid (all observed test values ≥ 1.0 µg/m³)"
  }
  
  # 4. Directional / Trend Accuracy (for n >= 2)
  dir_acc <- if (n >= 2) {
    delta_act <- diff(y)
    delta_pred <- diff(y_hat)
    mean(sign(delta_act) == sign(delta_pred) | (delta_act == 0 & delta_pred == 0)) * 100
  } else {
    NA_real_
  }
  
  # 5. CPCB Air Quality Category Concordance
  cat_act <- sapply(y, function(v) {
    if (pollutant %in% c("PM2.5", "PM10")) {
      si <- if (exists("calc_single_subindex")) calc_single_subindex(pollutant, v) else v
      if (exists("get_aqi_category")) get_aqi_category(si) else "Unknown"
    } else "Unknown"
  })
  
  cat_pred <- sapply(y_hat, function(v) {
    if (pollutant %in% c("PM2.5", "PM10")) {
      si <- if (exists("calc_single_subindex")) calc_single_subindex(pollutant, v) else v
      if (exists("get_aqi_category")) get_aqi_category(si) else "Unknown"
    } else "Unknown"
  })
  
  concordance <- mean(cat_act == cat_pred) * 100
  
  list(
    n = n,
    mae = round(mae, 2),
    rmse = round(rmse, 2),
    mape = if (!is.na(mape)) round(mape, 1) else NA_real_,
    mape_valid = mape_valid,
    mape_note = mape_note,
    directional_accuracy_pct = if (!is.na(dir_acc)) round(dir_acc, 1) else NA_real_,
    category_concordance_pct = round(concordance, 1),
    unit = ifelse(pollutant == "CO", "mg/m³", "µg/m³")
  )
}

#' Persistence Naive Baseline Model (Benchmark)
#' Predicts future values as the most recently observed value: y_hat(t+h) = y(t)
#' 
#' @param train_values Numeric vector of training observations
#' @param full_values Numeric vector of all observations or test_len
#' @param test_len Length of test set to evaluate
#' @param future_horizon Number of future steps to project beyond available data
#' @return List with test predictions, future projections, and empirical confidence bounds
fit_persistence_baseline <- function(train_values, full_values = NULL, test_len = 2, future_horizon = 6) {
  # Support flexible calling convention: (train, test_len, horizon) or (train, full, test_len, horizon)
  if (is.null(full_values) || (length(full_values) == 1 && missing(test_len))) {
    if (!is.null(full_values)) test_len <- full_values
    full_values <- train_values
  }
  
  last_train_val <- tail(train_values, 1)
  last_full_val <- tail(full_values, 1)
  
  # Test predictions: flat persistence from end of training set
  test_preds <- rep(last_train_val, test_len)
  
  # Future forecasts: flat persistence from end of full observed series
  future_preds <- rep(last_full_val, future_horizon)
  
  # Empirical difference standard deviation for heuristic intervals
  diff_sd <- if (length(train_values) >= 3) sd(diff(train_values), na.rm = TRUE) else 5.0
  if (is.na(diff_sd) || diff_sd == 0) diff_sd <- 5.0
  
  # Widen intervals with sqrt(horizon step)
  steps <- seq_len(future_horizon)
  lo80 <- pmax(0, future_preds - 1.28 * diff_sd * sqrt(steps))
  hi80 <- future_preds + 1.28 * diff_sd * sqrt(steps)
  lo95 <- pmax(0, future_preds - 1.96 * diff_sd * sqrt(steps))
  hi95 <- future_preds + 1.96 * diff_sd * sqrt(steps)
  
  list(
    success = TRUE,
    model_name = "Persistence Naive Baseline",
    test_predictions = test_preds,
    test_point_forecasts = test_preds,
    future_point_forecasts = future_preds,
    future_predictions = future_preds,
    lo80 = lo80,
    hi80 = hi80,
    lo95 = lo95,
    hi95 = hi95
  )
}

#' Statistical Auto-ARIMA Forecasting Model
#' Selects optimal ARIMA(p,d,q) order via AICc minimization and computes calibrated prediction intervals
#' 
#' @param train_values Numeric vector of training observations
#' @param full_values Numeric vector of all available observations (train + test)
#' @param test_len Number of steps in test set
#' @param future_horizon Number of steps to project into future
#' @return List with fitted model, test predictions, future forecasts, and confidence intervals
fit_auto_arima_model <- function(train_values, full_values, test_len, future_horizon = 6) {
  # 1. Fit Auto-ARIMA on Training Set
  fit_train <- tryCatch({
    auto.arima(ts(train_values, frequency = 1), seasonal = FALSE, stepwise = TRUE, approximation = FALSE)
  }, error = function(e) {
    # Fallback to simple random walk with drift or ARIMA(1,0,0)
    tryCatch({
      auto.arima(ts(train_values, frequency = 1), seasonal = FALSE)
    }, error = function(e2) NULL)
  })
  
  if (is.null(fit_train)) {
    return(list(
      success = FALSE,
      message = "Auto-ARIMA optimization failed to converge on the training sequence."
    ))
  }
  
  # Test set forecast
  fc_test <- forecast(fit_train, h = test_len)
  test_preds <- pmax(0, as.numeric(fc_test$mean))
  
  # 2. Retrain on Full Dataset for Future Projections
  fit_full <- tryCatch({
    auto.arima(ts(full_values, frequency = 1), seasonal = FALSE, stepwise = TRUE)
  }, error = function(e) fit_train)
  
  fc_future <- forecast(fit_full, h = future_horizon)
  
  point_forecast <- pmax(0, as.numeric(fc_future$mean))
  lo80 <- pmax(0, as.numeric(fc_future$lower[, "80%"]))
  hi80 <- pmax(0, as.numeric(fc_future$upper[, "80%"]))
  lo95 <- pmax(0, as.numeric(fc_future$lower[, "95%"]))
  hi95 <- pmax(0, as.numeric(fc_future$upper[, "95%"]))
  
  order_str <- paste0("ARIMA(", paste(arimaorder(fit_full)[1:3], collapse = ","), ")")
  
  list(
    success = TRUE,
    model_name = paste("Auto-ARIMA", order_str),
    order = order_str,
    model_fit = fit_full,
    test_predictions = test_preds,
    test_point_forecasts = test_preds,
    future_point_forecasts = point_forecast,
    future_predictions = point_forecast,
    lo80 = lo80,
    hi80 = hi80,
    lo95 = lo95,
    hi95 = hi95
  )
}

# Alias for fit_auto_arima_model
fit_auto_arima <- fit_auto_arima_model

# Alias for calculate_forecast_metrics
calculate_evaluation_metrics <- calculate_forecast_metrics

#' Exponential Smoothing State Space (ETS) Forecasting Model
#' Fits an optimal error/trend state-space model via likelihood maximization
#' and computes calibrated 80% and 95% prediction intervals.
#' 
#' @param train_values Numeric vector of training observations
#' @param full_values Numeric vector of all available observations (train + test)
#' @param test_len Number of steps in test set
#' @param future_horizon Number of steps to project into future
#' @return List with fitted model, test predictions, future forecasts, and confidence intervals
fit_ets_model <- function(train_values, full_values, test_len, future_horizon = 6) {
  # 1. Fit ETS on Training Set (non-seasonal ZZN for shorter continuous air quality series)
  fit_train <- tryCatch({
    ets(ts(train_values, frequency = 1), model = "ZZN", damped = NULL)
  }, error = function(e) {
    tryCatch({
      ets(ts(train_values, frequency = 1), model = "ANN")
    }, error = function(e2) NULL)
  })
  
  if (is.null(fit_train)) {
    return(list(
      success = FALSE,
      message = "Exponential Smoothing (ETS) failed to converge on the training sequence."
    ))
  }
  
  # Test set forecast
  fc_test <- forecast(fit_train, h = test_len)
  test_preds <- pmax(0, as.numeric(fc_test$mean))
  
  # 2. Retrain on Full Dataset for Future Projections
  fit_full <- tryCatch({
    ets(ts(full_values, frequency = 1), model = "ZZN", damped = NULL)
  }, error = function(e) fit_train)
  
  fc_future <- forecast(fit_full, h = future_horizon)
  
  point_forecast <- pmax(0, as.numeric(fc_future$mean))
  lo80 <- pmax(0, as.numeric(fc_future$lower[, "80%"]))
  hi80 <- pmax(0, as.numeric(fc_future$upper[, "80%"]))
  lo95 <- pmax(0, as.numeric(fc_future$lower[, "95%"]))
  hi95 <- pmax(0, as.numeric(fc_future$upper[, "95%"]))
  
  model_type_str <- paste0("ETS(", paste(fit_full$components[1:3], collapse = ","), ")")
  
  list(
    success = TRUE,
    model_name = paste("Exponential Smoothing", model_type_str),
    order = model_type_str,
    model_fit = fit_full,
    test_predictions = test_preds,
    test_point_forecasts = test_preds,
    future_point_forecasts = point_forecast,
    future_predictions = point_forecast,
    lo80 = lo80,
    hi80 = hi80,
    lo95 = lo95,
    hi95 = hi95
  )
}

#' Comprehensive Pollution Risk Classification
#' Evaluates current vs projected concentrations, transitions across CPCB NAQI breakpoints,
#' potential deterioration/improvement, and prediction uncertainty.
#' 
#' @param current_val Most recent observed concentration
#' @param predicted_val Point forecast concentration at horizon h
#' @param lo95 Lower 95% confidence bound
#' @param hi95 Upper 95% confidence bound
#' @param pollutant "PM2.5" or "PM10"
#' @param horizon Forecast horizon step
#' @param is_viable Logical indicating whether station has sufficient history
#' @return Structured list containing detailed risk attributes, transition state, and plain-language explanation
classify_pollution_risk <- function(current_val, predicted_val, lo95 = NA, hi95 = NA, 
                                    pollutant = "PM2.5", horizon = 6, is_viable = TRUE) {
  # CPCB NAQI Breakpoints
  thresh <- if (pollutant == "PM2.5") {
    list(good = 30, satisfactory = 60, moderate = 90, poor = 120, very_poor = 250, severe = Inf)
  } else {
    list(good = 50, satisfactory = 100, moderate = 250, poor = 350, very_poor = 430, severe = Inf)
  }
  
  get_cat <- function(val) {
    if (is.na(val)) return("Unknown")
    if (pollutant == "PM2.5") {
      if (val <= 30) "Good"
      else if (val <= 60) "Satisfactory"
      else if (val <= 90) "Moderate"
      else if (val <= 120) "Poor"
      else if (val <= 250) "Very Poor"
      else "Severe"
    } else {
      if (val <= 50) "Good"
      else if (val <= 100) "Satisfactory"
      else if (val <= 250) "Moderate"
      else if (val <= 350) "Poor"
      else if (val <= 430) "Very Poor"
      else "Severe"
    }
  }
  
  cat_rank <- c("Good" = 1, "Satisfactory" = 2, "Moderate" = 3, "Poor" = 4, "Very Poor" = 5, "Severe" = 6, "Unknown" = 0)
  unit <- ifelse(pollutant == "CO", "mg/m³", "µg/m³")
  
  if (!is_viable || is.na(predicted_val)) {
    return(list(
      status = "INSUFFICIENT_HISTORY",
      risk_state = "INSUFFICIENT_HISTORY",
      risk_label = "Insufficient History - Gated",
      risk_level = "INSUFFICIENT_DATA",
      badge_class = "bg-secondary text-white",
      badge_color = "#64748B",
      current_category = get_cat(current_val),
      predicted_category = "N/A",
      current_val = current_val,
      predicted_val = predicted_val,
      delta_val = NA_real_,
      delta_pct = NA_real_,
      delta = NA_real_,
      pct_change = NA_real_,
      statutory_threshold = if (pollutant == "PM2.5") 60 else 100,
      threshold_val = if (pollutant == "PM2.5") 60 else 100,
      threshold_name = if (pollutant == "PM2.5") "National 24-hr Standard (60 µg/m³)" else "National 24-hr Standard (100 µg/m³)",
      uncertainty_span = NA_real_,
      uncertainty_bounds = "N/A",
      timing = paste0("+", horizon, " Hours"),
      breach_projected = FALSE,
      plain_explanation = "Station possesses insufficient sequential observations to fit statistical models. Prediction gated to prevent fabricating synthetic values.",
      rationale = "Station possesses insufficient sequential observations to fit statistical models. Prediction gated to prevent fabricating synthetic values.",
      actionable_advisory = "Continue automated data accumulation until at least 10 sequential timestamps are available.",
      civic_advisory = "Continue automated data accumulation until at least 10 sequential timestamps are available.",
      disclaimer = "Model-based statistical estimate; does not constitute an official CPCB statutory declaration."
    ))
  }
  
  curr_cat <- get_cat(current_val)
  pred_cat <- get_cat(predicted_val)
  r_curr <- cat_rank[[curr_cat]]
  r_pred <- cat_rank[[pred_cat]]
  
  delta_val <- round(predicted_val - current_val, 1)
  delta_pct <- if (!is.na(current_val) && current_val > 0) round((delta_val / current_val) * 100, 1) else 0.0
  
  uncert_span <- if (!is.na(lo95) && !is.na(hi95)) round(hi95 - lo95, 1) else NA_real_
  is_wide_uncertainty <- (!is.na(uncert_span) && uncert_span > (1.5 * pmax(10, predicted_val)))
  
  # Classify Risk State
  if (r_pred > r_curr || (delta_pct >= 15.0 && delta_val >= 5.0)) {
    risk_state <- "POTENTIAL_DETERIORATION"
    risk_label <- "Potential Deterioration"
    risk_level <- "HIGH"
    badge_class <- if (r_pred >= 5) "bg-danger text-white" else "bg-warning text-dark"
    badge_color <- "#DC2626"
    plain_explanation <- sprintf(
      "Statistical projection indicates air quality is likely to deteriorate over the next +%d hours. Predicted %s concentration rises from %.1f %s (%s) to %.1f %s (%s), an estimated change of %+.1f %s (%+.1f%%).",
      horizon, pollutant, current_val, unit, curr_cat, predicted_val, unit, pred_cat, delta_val, unit, delta_pct
    )
    actionable_advisory <- if (r_pred >= 5) {
      "High probability of entering Severe/Very Poor range. Vulnerable groups, elderly, and individuals with respiratory conditions are advised to minimize outdoor exertion."
    } else {
      "Elevated pollutant trend projected. Recommend monitoring local ventilation conditions and considering indoor air purification."
    }
  } else if (r_pred < r_curr || (delta_pct <= -15.0 && delta_val <= -5.0)) {
    risk_state <- "POTENTIAL_IMPROVEMENT"
    risk_label <- "Potential Improvement"
    risk_level <- "LOW"
    badge_class <- "bg-success text-white"
    badge_color <- "#059669"
    plain_explanation <- sprintf(
      "Statistical projection indicates air quality is expected to improve over the next +%d hours. Predicted %s concentration drops from %.1f %s (%s) to %.1f %s (%s), an estimated reduction of %.1f %s (%.1f%%).",
      horizon, pollutant, current_val, unit, curr_cat, predicted_val, unit, pred_cat, abs(delta_val), unit, abs(delta_pct)
    )
    actionable_advisory <- "Ambient pollutant concentrations are projected to subside. Favorable atmospheric dispersion or reduced emission activity anticipated."
  } else if (is_wide_uncertainty) {
    risk_state <- "HIGH_UNCERTAINTY"
    risk_label <- "High Forecast Uncertainty"
    risk_level <- "UNCERTAIN"
    badge_class <- "bg-info text-dark"
    badge_color <- "#D97706"
    plain_explanation <- sprintf(
      "Predicted %s concentration is %.1f %s (%s), but the 95%% prediction interval is wide (%.1f to %.1f %s, span: %.1f %s), reflecting high volatility in the underlying series.",
      pollutant, predicted_val, unit, pred_cat, lo95, hi95, unit, uncert_span, unit
    )
    actionable_advisory <- "Model projections carry wide confidence intervals. Interpret point forecasts with caution and monitor incoming hourly telemetry."
  } else {
    risk_state <- "STABLE"
    risk_label <- "Stable / Sustained Range"
    risk_level <- "MODERATE"
    badge_class <- "bg-secondary text-white"
    badge_color <- "#475569"
    plain_explanation <- sprintf(
      "Predicted %s concentration (%.1f %s) remains within the same CPCB category (%s) as the observed reading (%.1f %s), indicating stable ambient conditions.",
      pollutant, predicted_val, unit, pred_cat, current_val, unit
    )
    actionable_advisory <- "Current air quality conditions are expected to persist without major category shifts over the forecast horizon."
  }
  
  # Relevant Threshold
  stat_thresh <- if (pollutant == "PM2.5") {
    if (predicted_val >= 250 || (hi95 >= 250 && !is.na(hi95))) list(val = 250, name = "Severe Hazard Threshold (250 µg/m³)")
    else if (predicted_val >= 120 || (hi95 >= 120 && !is.na(hi95))) list(val = 120, name = "Very Poor Threshold (120 µg/m³)")
    else if (predicted_val >= 90) list(val = 90, name = "Poor Air Quality Threshold (90 µg/m³)")
    else list(val = 60, name = "National Ambient 24-hr Standard (60 µg/m³)")
  } else {
    if (predicted_val >= 430 || (hi95 >= 430 && !is.na(hi95))) list(val = 430, name = "Severe Hazard Threshold (430 µg/m³)")
    else if (predicted_val >= 350 || (hi95 >= 350 && !is.na(hi95))) list(val = 350, name = "Very Poor Threshold (350 µg/m³)")
    else if (predicted_val >= 250) list(val = 250, name = "Poor Air Quality Threshold (250 µg/m³)")
    else list(val = 100, name = "National Ambient 24-hr Standard (100 µg/m³)")
  }
  
  breach_projected <- isTRUE(predicted_val >= stat_thresh$val || (!is.na(hi95) && hi95 >= stat_thresh$val))
  uncert_str <- if (!is.na(lo95) && !is.na(hi95)) sprintf("[%.1f - %.1f %s]", lo95, hi95, unit) else "N/A"
  
  list(
    status = risk_state,
    risk_state = risk_state,
    risk_label = risk_label,
    risk_level = risk_level,
    badge_class = badge_class,
    badge_color = badge_color,
    current_category = curr_cat,
    predicted_category = pred_cat,
    current_val = current_val,
    predicted_val = predicted_val,
    delta_val = delta_val,
    delta_pct = delta_pct,
    delta = delta_val,
    pct_change = delta_pct,
    statutory_threshold = stat_thresh$val,
    threshold_val = stat_thresh$val,
    threshold_name = stat_thresh$name,
    breach_projected = breach_projected,
    uncertainty_span = uncert_span,
    uncertainty_bounds = uncert_str,
    timing = paste0("+", horizon, " Hours"),
    plain_explanation = plain_explanation,
    rationale = plain_explanation,
    actionable_advisory = actionable_advisory,
    civic_advisory = actionable_advisory,
    disclaimer = "Model-based statistical estimate; does not constitute an official CPCB statutory declaration."
  )
}

#' Assess early warning alert level based on forecasted concentration and CPCB thresholds
#' 
#' @param forecast_val Point prediction in native units
#' @param lo95_val Lower 95% bound
#' @param hi95_val Upper 95% bound
#' @param pollutant Target pollutant ("PM2.5" or "PM10")
#' @return List with warning status, severity class, badge color, and actionable advisory
assess_early_warning <- function(forecast_val, lo95_val, hi95_val, pollutant = "PM2.5") {
  # CPCB NAQI critical thresholds:
  # PM2.5: Moderate > 60, Poor > 90, Very Poor > 120, Severe > 250
  # PM10:  Moderate > 100, Poor > 250, Very Poor > 350, Severe > 430
  
  thresh <- if (pollutant == "PM2.5") {
    list(moderate = 60, poor = 90, very_poor = 120, severe = 250)
  } else {
    list(moderate = 100, poor = 250, very_poor = 350, severe = 430)
  }
  
  if (is.na(forecast_val)) {
    return(list(
      alert_active = FALSE,
      severity = "Normal",
      level_code = 0,
      badge_class = "bg-secondary",
      message = "No forecast available."
    ))
  }
  
  if (forecast_val >= thresh$severe || hi95_val >= thresh$severe) {
    list(
      alert_active = TRUE,
      severity = "Severe Risk",
      level_code = 4,
      badge_class = "bg-danger",
      message = paste0("CRITICAL ALERT: Forecasted ", pollutant, " (", round(forecast_val, 1), 
                       " µg/m³) is projected to breach the CPCB Severe threshold (>", thresh$severe, " µg/m³).")
    )
  } else if (forecast_val >= thresh$very_poor || hi95_val >= thresh$very_poor) {
    list(
      alert_active = TRUE,
      severity = "Very Poor",
      level_code = 3,
      badge_class = "bg-danger",
      message = paste0("HIGH ALERT: Projected ", pollutant, " enters CPCB Very Poor category (>", thresh$very_poor, 
                       " µg/m³). Respiratory sensitivity warnings advised.")
    )
  } else if (forecast_val >= thresh$poor) {
    list(
      alert_active = TRUE,
      severity = "Poor",
      level_code = 2,
      badge_class = "bg-warning text-dark",
      message = paste0("ELEVATED ALERT: Projected ", pollutant, " concentration breaches the CPCB Poor threshold (>", thresh$poor, " µg/m³).")
    )
  } else if (forecast_val >= thresh$moderate) {
    list(
      alert_active = FALSE,
      severity = "Moderate",
      level_code = 1,
      badge_class = "bg-info text-dark",
      message = paste0("MODERATE: Projected ", pollutant, " levels remain within standard urban range (", thresh$moderate, "-", thresh$poor, " µg/m³).")
    )
  } else {
    list(
      alert_active = FALSE,
      severity = "Satisfactory",
      level_code = 0,
      badge_class = "bg-success",
      message = paste0("HEALTHY: Projected ", pollutant, " levels are within acceptable national ambient standards.")
    )
  }
}

#' Master Air Quality Forecasting Pipeline
#' Orchestrates series validation, chronological split, baseline & Auto-ARIMA model fitting,
#' out-of-sample evaluation, future projection, and early warning detection.
#' 
#' @param data Raw or repository dataset
#' @param station_name Monitoring station name
#' @param target_pollutant "PM2.5" or "PM10" (default: "PM2.5")
#' @param horizon Number of future projection time steps (default: 6)
#' @param model_choice "Auto-ARIMA" or "Persistence Baseline" (default: "Auto-ARIMA")
#' @param train_ratio Chronological train/test split proportion (default: 0.75)
#' @param min_required_obs Minimum observations needed for statistical modeling (default: 10)
#' @return Comprehensive forecast result list
run_air_quality_forecast <- function(data, station_name, target_pollutant = "PM2.5", 
                                     horizon = 6, model_choice = "Auto-ARIMA",
                                     train_ratio = 0.75, min_required_obs = 10) {
  
  # Step 1: Prepare & Validate Series
  # Minimum observations dynamically scaled to forecast horizon to prevent severe overfitting
  req_for_horizon <- if (horizon <= 6) 10 else if (horizon <= 12) 16 else 28
  effective_min_obs <- max(min_required_obs, req_for_horizon)
  
  prep <- prepare_station_series(
    df = data,
    station_name = station_name,
    target_pollutant = target_pollutant,
    min_required_obs = min_required_obs
  )
  
  if (!prep$is_viable) {
    return(list(
      can_forecast = FALSE,
      status = prep$status,
      station = station_name,
      pollutant = target_pollutant,
      n_obs = prep$n_obs,
      min_required = min_required_obs,
      message = prep$message,
      historical_series = prep$data,
      baseline_metrics = NULL,
      arima_metrics = NULL,
      future_forecasts = NULL,
      early_warning = NULL
    ))
  }
  
  # Check horizon sufficiency
  if (prep$n_obs < effective_min_obs) {
    return(list(
      can_forecast = FALSE,
      status = "INSUFFICIENT_FOR_HORIZON",
      station = station_name,
      pollutant = target_pollutant,
      n_obs = prep$n_obs,
      min_required = effective_min_obs,
      message = sprintf("Station '%s' has %d observations. A +%d-hour forecast horizon requires at least %d sequential observations to evaluate out-of-sample error reliably.",
                        station_name, prep$n_obs, horizon, effective_min_obs),
      historical_series = prep$data,
      baseline_metrics = NULL,
      arima_metrics = NULL,
      future_forecasts = NULL,
      early_warning = NULL
    ))
  }
  
  clean_data <- prep$data
  n_total <- nrow(clean_data)
  
  # Step 2: Chronological Train/Test Partition
  split_res <- split_chronological(clean_data, train_ratio = train_ratio, min_test = 2)
  train_df <- split_res$train
  test_df <- split_res$test
  n_train <- nrow(train_df)
  n_test <- nrow(test_df)
  
  y_train <- train_df$pollutant_avg
  y_test <- test_df$pollutant_avg
  y_full <- clean_data$pollutant_avg
  
  # Step 3: Fit Persistence Baseline
  base_res <- fit_persistence_baseline(
    train_values = y_train,
    test_len = n_test,
    future_horizon = horizon
  )
  base_metrics <- calculate_forecast_metrics(y_test, base_res$test_predictions, pollutant = target_pollutant)
  
  # Step 4: Fit Candidate Models (Auto-ARIMA and Exponential Smoothing ETS)
  # 4A. Auto-ARIMA
  arima_res <- fit_auto_arima_model(
    train_values = y_train,
    full_values = y_full,
    test_len = n_test,
    future_horizon = horizon
  )
  
  arima_metrics <- if (arima_res$success) {
    m <- calculate_forecast_metrics(y_test, arima_res$test_predictions, pollutant = target_pollutant)
    lift_pct <- if (!is.na(base_metrics$mae) && base_metrics$mae > 0) {
      round(((base_metrics$mae - m$mae) / base_metrics$mae) * 100, 1)
    } else 0.0
    m$lift_pct <- lift_pct
    m$model_order <- arima_res$order
    m
  } else {
    NULL
  }
  
  # 4B. Exponential Smoothing State Space (ETS)
  ets_res <- fit_ets_model(
    train_values = y_train,
    full_values = y_full,
    test_len = n_test,
    future_horizon = horizon
  )
  
  ets_metrics <- if (ets_res$success) {
    m <- calculate_forecast_metrics(y_test, ets_res$test_predictions, pollutant = target_pollutant)
    lift_pct <- if (!is.na(base_metrics$mae) && base_metrics$mae > 0) {
      round(((base_metrics$mae - m$mae) / base_metrics$mae) * 100, 1)
    } else 0.0
    m$lift_pct <- lift_pct
    m$model_order <- ets_res$order
    m
  } else {
    NULL
  }
  
  # Step 5: Construct Out-of-Sample Test Evaluation Table
  test_comparison <- data.frame(
    observation_time = test_df$observation_time,
    time_label = format(test_df$observation_time, "%d-%b %H:%M"),
    actual = round(y_test, 1),
    baseline_pred = round(base_res$test_predictions, 1),
    arima_pred = if (arima_res$success) round(arima_res$test_predictions, 1) else rep(NA_real_, n_test),
    ets_pred = if (ets_res$success) round(ets_res$test_predictions, 1) else rep(NA_real_, n_test),
    stringsAsFactors = FALSE
  )
  
  # Step 6: Multi-Model Candidate Performance Evaluation & Best Selection
  models_list <- list(
    list(name = "Persistence Naive Baseline", type = "Persistence Baseline", metrics = base_metrics, res = base_res, key = "Persistence Baseline"),
    list(name = if (arima_res$success) arima_res$model_name else "Auto-ARIMA (Failed)", type = "Auto-ARIMA", metrics = arima_metrics, res = arima_res, key = "Auto-ARIMA"),
    list(name = if (ets_res$success) ets_res$model_name else "ETS (Failed)", type = "Exponential Smoothing", metrics = ets_metrics, res = ets_res, key = "Exponential Smoothing (ETS)")
  )
  
  # Extract MAEs for valid fitted models
  valid_models <- list()
  for (m_item in models_list) {
    if (!is.null(m_item$metrics) && !is.na(m_item$metrics$mae)) {
      valid_models[[length(valid_models) + 1]] <- m_item
    }
  }
  
  # Identify superior model based strictly on held-out test MAE
  best_mae_val <- Inf
  best_model_item <- models_list[[1]]
  
  for (vm in valid_models) {
    if (vm$metrics$mae < best_mae_val) {
      best_mae_val <- vm$metrics$mae
      best_model_item <- vm
    }
  }
  
  sup_model <- best_model_item$name
  sup_badge <- paste("Best Performer:", best_model_item$type)
  
  if (best_model_item$key == "Auto-ARIMA") {
    sup_reason <- sprintf("Auto-ARIMA achieved the lowest held-out test MAE (%.2f vs %.2f %s baseline, a %.1f%% error reduction).",
                          best_model_item$metrics$mae, base_metrics$mae, base_metrics$unit, best_model_item$metrics$lift_pct)
  } else if (best_model_item$key == "Exponential Smoothing (ETS)") {
    sup_reason <- sprintf("Exponential Smoothing (ETS) achieved the lowest held-out test MAE (%.2f vs %.2f %s baseline, a %.1f%% error reduction).",
                          best_model_item$metrics$mae, base_metrics$mae, base_metrics$unit, best_model_item$metrics$lift_pct)
  } else {
    sup_reason <- sprintf("Persistence Baseline achieved the lowest held-out test MAE (%.2f %s). Real data exhibits high persistence where naive projection outperformed statistical fits.",
                          base_metrics$mae, base_metrics$unit)
  }
  
  # Candidate models comparison data frame
  candidate_models_table <- data.frame(
    model_name = sapply(models_list, function(x) x$name),
    model_family = sapply(models_list, function(x) x$type),
    test_mae = sapply(models_list, function(x) if (!is.null(x$metrics)) x$metrics$mae else NA_real_),
    test_rmse = sapply(models_list, function(x) if (!is.null(x$metrics)) x$metrics$rmse else NA_real_),
    test_mape = sapply(models_list, function(x) if (!is.null(x$metrics) && isTRUE(x$metrics$mape_valid)) paste0(x$metrics$mape, "%") else "N/A"),
    directional_accuracy = sapply(models_list, function(x) if (!is.null(x$metrics) && !is.na(x$metrics$directional_accuracy_pct)) paste0(x$metrics$directional_accuracy_pct, "%") else "N/A"),
    concordance = sapply(models_list, function(x) if (!is.null(x$metrics) && !is.na(x$metrics$category_concordance_pct)) paste0(x$metrics$category_concordance_pct, "%") else "N/A"),
    lift_vs_baseline = sapply(models_list, function(x) {
      if (identical(x$key, "Persistence Baseline")) "0.0% (Baseline)"
      else if (!is.null(x$metrics) && !is.na(x$metrics$lift_pct)) paste0(if (x$metrics$lift_pct > 0) "+" else "", x$metrics$lift_pct, "%")
      else "N/A"
    }),
    is_best = sapply(models_list, function(x) identical(x$name, sup_model)),
    stringsAsFactors = FALSE
  )
  
  # Step 7: Construct Future Projections Table
  step_hours <- prep$median_step_hours
  last_time <- max(clean_data$observation_time)
  future_times <- last_time + (seq_len(horizon) * as.difftime(step_hours, units = "hours"))
  
  # Select active model according to user choice or automated best selection
  if (model_choice == "Best Model (Auto-Selected)") {
    chosen_item <- best_model_item
  } else if (model_choice == "Auto-ARIMA" && arima_res$success) {
    chosen_item <- models_list[[2]]
  } else if (model_choice == "Exponential Smoothing (ETS)" && ets_res$success) {
    chosen_item <- models_list[[3]]
  } else {
    chosen_item <- models_list[[1]]
  }
  
  active_model_str <- chosen_item$name
  chosen_preds <- chosen_item$res$future_point_forecasts
  chosen_lo80  <- chosen_item$res$lo80
  chosen_hi80  <- chosen_item$res$hi80
  chosen_lo95  <- chosen_item$res$lo95
  chosen_hi95  <- chosen_item$res$hi95
  
  future_df <- data.frame(
    step = seq_len(horizon),
    observation_time = future_times,
    time_label = format(future_times, "%d-%b %H:%M"),
    point_forecast = round(chosen_preds, 1),
    lo80 = round(chosen_lo80, 1),
    hi80 = round(chosen_hi80, 1),
    lo95 = round(chosen_lo95, 1),
    hi95 = round(chosen_hi95, 1),
    stringsAsFactors = FALSE
  )
  
  # Attach CPCB Category to point forecasts
  future_df$cpcb_category <- sapply(future_df$point_forecast, function(v) {
    si <- if (exists("calc_single_subindex")) calc_single_subindex(target_pollutant, v) else v
    if (exists("get_aqi_category")) get_aqi_category(si) else "Unknown"
  })
  
  # Step 8: Early Warning Assessment & Comprehensive Risk Classification
  curr_observed_val <- tail(clean_data$pollutant_avg, 1)
  final_forecast_val <- tail(future_df$point_forecast, 1)
  final_lo95 <- tail(future_df$lo95, 1)
  final_hi95 <- tail(future_df$hi95, 1)
  
  peak_forecast <- max(future_df$point_forecast, na.rm = TRUE)
  peak_lo95 <- future_df$lo95[which.max(future_df$point_forecast)]
  peak_hi95 <- max(future_df$hi95, na.rm = TRUE)
  early_warn <- assess_early_warning(peak_forecast, peak_lo95, peak_hi95, pollutant = target_pollutant)
  
  risk_class <- classify_pollution_risk(
    current_val = curr_observed_val,
    predicted_val = final_forecast_val,
    lo95 = final_lo95,
    hi95 = final_hi95,
    pollutant = target_pollutant,
    horizon = horizon,
    is_viable = TRUE
  )
  
  test_start_t <- min(test_df$observation_time)
  test_end_t <- max(test_df$observation_time)
  eval_hours <- round(as.numeric(difftime(test_end_t, test_start_t, units = "hours")), 1)
  eval_period_str <- sprintf("%s to %s IST (%d test observations across %.1f hrs)",
                             format(test_start_t, "%d-%b %H:%M"),
                             format(test_end_t, "%d-%b %H:%M"),
                             n_test, eval_hours)

  list(
    can_forecast = TRUE,
    status = "SUCCESS",
    station = station_name,
    pollutant = target_pollutant,
    n_obs = n_total,
    n_train = n_train,
    n_test = n_test,
    median_step_hours = step_hours,
    last_observation_time = last_time,
    message = paste0("Model successfully evaluated on ", n_test, " unseen chronological observations."),
    historical_series = clean_data,
    train_data = train_df,
    test_data = test_df,
    test_comparison = test_comparison,
    candidate_models_table = candidate_models_table,
    baseline_metrics = base_metrics,
    arima_metrics = arima_metrics,
    ets_metrics = ets_metrics,
    superior_model = sup_model,
    superior_badge = sup_badge,
    superior_reason = sup_reason,
    evaluation_period = eval_period_str,
    data_leakage_prevented = TRUE,
    active_model_name = active_model_str,
    future_forecasts = future_df,
    early_warning = early_warn,
    risk_classification = risk_class
  )
}

#' Forecast-Based Hotspot Ranking & Insufficient-Data Station Segregation
#' Ranks viable stations (N >= 10) by predicted pollutant levels, while segregating
#' inadequate-data locations (N < 10) into an audit list rather than inventing fictitious values.
#' 
#' @param data Full repository data frame
#' @param target_pollutant "PM2.5" or "PM10"
#' @param horizon Forecast horizon step (default: 6)
#' @return List with ranked_hotspots data frame, inadequate_stations data frame, and summary counts
rank_forecast_hotspots <- function(data, target_pollutant = "PM2.5", horizon = 6) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      ranked_hotspots = data.frame(),
      inadequate_stations = data.frame(),
      total_stations = 0,
      viable_count = 0,
      inadequate_count = 0
    ))
  }
  
  # Standardize columns
  if (!"pollutant_avg" %in% colnames(data) && "avg_value" %in% colnames(data)) {
    data$pollutant_avg <- data$avg_value
  }
  if (!"observation_time" %in% colnames(data) && "last_update" %in% colnames(data)) {
    data$observation_time <- parse_date_time(
      data$last_update, 
      orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM", "d-m-Y H:M:S", "d-m-Y H:M", "Y-m-d H:M:S"), 
      tz = "Asia/Kolkata"
    )
  }
  
  pol_data <- data %>%
    filter(pollutant_id == target_pollutant, !is.na(pollutant_avg), !is.na(observation_time))
  
  if (nrow(pol_data) == 0) {
    return(list(
      ranked_hotspots = data.frame(),
      inadequate_stations = data.frame(),
      total_stations = 0,
      viable_count = 0,
      inadequate_count = 0
    ))
  }
  
  # Summarise stations
  stn_meta <- pol_data %>%
    group_by(station, city, state) %>%
    summarise(
      latitude = first(latitude),
      longitude = first(longitude),
      current_reading = round(tail(pollutant_avg, 1), 1),
      n_obs = n_distinct(observation_time),
      last_update_time = max(observation_time),
      .groups = "drop"
    )
  
  # Split into viable (n_obs >= 10) and inadequate (n_obs < 10)
  viable_meta <- stn_meta %>% filter(n_obs >= 10)
  inadequate_meta <- stn_meta %>% filter(n_obs < 10)
  
  # Process Viable Stations
  ranked_rows <- list()
  if (nrow(viable_meta) > 0) {
    for (i in seq_len(nrow(viable_meta))) {
      s_name <- viable_meta$station[i]
      s_df <- pol_data %>% filter(station == s_name) %>% arrange(observation_time)
      
      fc_out <- tryCatch({
        run_air_quality_forecast(s_df, station_name = s_name, target_pollutant = target_pollutant, 
                                 horizon = horizon, model_choice = "Best Model (Auto-Selected)", min_required_obs = 10)
      }, error = function(e) NULL)
      
      if (!is.null(fc_out) && fc_out$can_forecast) {
        last_fc_row <- tail(fc_out$future_forecasts, 1)
        rk_eval <- fc_out$risk_classification
        
        ranked_rows[[length(ranked_rows) + 1]] <- data.frame(
          station = s_name,
          city = viable_meta$city[i],
          state = viable_meta$state[i],
          current_value = viable_meta$current_reading[i],
          last_timestamp = format(viable_meta$last_update_time[i], "%d-%b %H:%M"),
          predicted_value = last_fc_row$point_forecast,
          forecast_horizon = paste0("+", horizon, "h"),
          model_name = fc_out$active_model_name,
          uncertainty_interval = sprintf("[%.1f - %.1f]", last_fc_row$lo95, last_fc_row$hi95),
          lo95 = last_fc_row$lo95,
          hi95 = last_fc_row$hi95,
          risk_category = rk_eval$risk_label,
          statutory_threshold = rk_eval$threshold_name,
          data_sufficiency = "Forecast Ready",
          n_obs = viable_meta$n_obs[i],
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  ranked_df <- if (length(ranked_rows) > 0) {
    res_df <- bind_rows(ranked_rows) %>% arrange(desc(predicted_value))
    res_df$rank <- seq_len(nrow(res_df))
    res_df[, c("rank", "station", "city", "state", "current_value", "last_timestamp", 
               "predicted_value", "forecast_horizon", "model_name", "uncertainty_interval", 
               "lo95", "hi95", "risk_category", "statutory_threshold", "data_sufficiency", "n_obs")]
  } else {
    data.frame(
      rank = integer(0), station = character(0), city = character(0), state = character(0),
      current_value = numeric(0), last_timestamp = character(0), predicted_value = numeric(0),
      forecast_horizon = character(0), model_name = character(0), uncertainty_interval = character(0),
      lo95 = numeric(0), hi95 = numeric(0), risk_category = character(0),
      statutory_threshold = character(0), data_sufficiency = character(0), n_obs = integer(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Format Inadequate Stations Table (No fake predictions assigned!)
  inadequate_df <- inadequate_meta %>%
    mutate(
      last_timestamp = format(last_update_time, "%d-%b %H:%M"),
      required_observations = 10,
      missing_observations = 10 - n_obs,
      data_sufficiency = paste0("Gated (", n_obs, "/10 timestamps)"),
      status_guidance = "Awaiting longitudinal accumulation in Historical Trends"
    ) %>%
    select(station, city, state, current_value = current_reading, last_timestamp, 
           n_obs, required_observations, missing_observations, data_sufficiency, status_guidance) %>%
    arrange(desc(current_value))
  
  list(
    ranked_hotspots = ranked_df,
    inadequate_stations = inadequate_df,
    total_stations = nrow(stn_meta),
    viable_count = nrow(ranked_df),
    inadequate_count = nrow(inadequate_df)
  )
}

#' Generate Network-Wide Future Hotspot Projections for Leaflet Mapping
#' Iterates through unique stations; for stations with sufficient history, uses Auto-ARIMA;
#' for single-snapshot stations, uses persistence baseline.
#' 
#' @param data Full repository data frame
#' @param target_pollutant "PM2.5" or "PM10"
#' @param horizon Forecast horizon step (default: 6)
#' @return Dataframe with station coordinates, observed value, projected value, and projected category
forecast_network_hotspots <- function(data, target_pollutant = "PM2.5", horizon = 6) {
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  
  # Ensure standard column names
  if (!"pollutant_avg" %in% colnames(data) && "avg_value" %in% colnames(data)) {
    data$pollutant_avg <- data$avg_value
  }
  
  if (!"observation_time" %in% colnames(data) && "last_update" %in% colnames(data)) {
    data$observation_time <- parse_date_time(
      data$last_update, 
      orders = c("dmy HMS", "ymd HMS", "dmy HM", "ymd HM", "d-m-Y H:M:S", "d-m-Y H:M", "Y-m-d H:M:S"), 
      tz = "Asia/Kolkata"
    )
  }
  
  # Filter target pollutant
  pol_data <- data %>%
    filter(pollutant_id == target_pollutant, !is.na(pollutant_avg))
  
  if (nrow(pol_data) == 0) return(data.frame())
  
  # Station summary
  stations_meta <- pol_data %>%
    group_by(station, city, state) %>%
    summarise(
      latitude = first(latitude),
      longitude = first(longitude),
      current_reading = last(pollutant_avg),
      n_obs = n(),
      last_time = if ("observation_time" %in% colnames(pol_data)) max(observation_time, na.rm = TRUE) else Sys.time(),
      .groups = "drop"
    )
  
  projected_vals <- numeric(nrow(stations_meta))
  projection_methods <- character(nrow(stations_meta))
  
  for (i in seq_len(nrow(stations_meta))) {
    stn <- stations_meta$station[i]
    n_o <- stations_meta$n_obs[i]
    
    if (n_o >= 10) {
      # Viable history: attempt Auto-ARIMA point forecast
      stn_sub <- pol_data %>% filter(station == stn) %>% arrange(observation_time)
      arima_fit <- tryCatch({
        auto.arima(ts(stn_sub$pollutant_avg, frequency = 1), seasonal = FALSE)
      }, error = function(e) NULL)
      
      if (!is.null(arima_fit)) {
        fc <- forecast(arima_fit, h = horizon)
        projected_vals[i] <- pmax(0, round(as.numeric(tail(fc$mean, 1)), 1))
        projection_methods[i] <- "Auto-ARIMA Point Forecast"
      } else {
        projected_vals[i] <- round(stations_meta$current_reading[i], 1)
        projection_methods[i] <- "Persistence Baseline"
      }
    } else {
      # Single observation or small history: persistence baseline
      projected_vals[i] <- round(stations_meta$current_reading[i], 1)
      projection_methods[i] <- "Persistence Baseline (Limited History)"
    }
  }
  
  stations_meta$projected_value <- projected_vals
  stations_meta$projection_method <- projection_methods
  stations_meta$delta_proj <- stations_meta$projected_value - stations_meta$current_reading
  
  # Assign projected CPCB Category
  stations_meta$projected_category <- sapply(stations_meta$projected_value, function(v) {
    si <- if (exists("calc_single_subindex")) calc_single_subindex(target_pollutant, v) else v
    if (exists("get_aqi_category")) get_aqi_category(si) else "Unknown"
  })
  
  stations_meta
}
