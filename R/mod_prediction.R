# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/mod_prediction.R
# Purpose: Module 8 — AI Air Quality Forecast & Intelligent Early Warning Studio
#          Integrates multi-model candidate forecasting (Auto-ARIMA, ETS, Persistence Baseline),
#          dynamic held-out performance selection, predictive uncertainty envelopes (80% & 95%),
#          CPCB threshold-based risk classification, hotspot forecast ranking with strict
#          data sufficiency gating, and explainable pedagogical panels.
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(DT)
  library(scales)
  library(lubridate)
  library(stringr)
})

# ------------------------------------------------------------------------------
# 1. UI Definition
# ------------------------------------------------------------------------------
mod_prediction_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid py-3",
      
      # 1. Page Header
      div(
        class = "page-header-container mb-3",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          div(
            h4(class = "page-header-title m-0", 
               icon("wand-magic-sparkles", class = "me-2 text-primary"), 
               "AI Forecast & Early Warning Studio"),
            p(class = "page-header-sub m-0 text-muted small", 
              "Multi-model time-series forecasting, predictive uncertainty envelopes, CPCB threshold risk classification, and hotspot ranking.")
          ),
          span(
            class = "badge bg-light text-primary border py-2 px-3",
            style = "font-size: 0.85rem;",
            icon("microchip", class = "me-1"), "Auto-ARIMA, ETS & Persistence Engine"
          )
        )
      ),
      
      # Collapsible Methodological Transparency Card
      tags$details(
        class = "card p-2 mb-3 bg-light border-0 small text-muted",
        tags$summary(
          class = "fw-semibold text-secondary",
          style = "cursor: pointer;",
          icon("circle-info", class = "me-1 text-primary"),
          "Forecasting Methodology, Model Validation & Data Integrity Standards (Click to expand)"
        ),
        div(
          class = "pt-2 px-2",
          p(class = "mb-1", 
            strong("Multi-Model Candidate Architecture: "), 
            "Evaluates an empirical Persistence Naive Baseline, an automated ARIMA(p,d,q) model selected via corrected AICc, and an Exponential Smoothing State Space (ETS) model. The superior model is dynamically selected based strictly on held-out test performance."),
          p(class = "mb-1", 
            strong("Strict Chronological Evaluation: "), 
            "Evaluation metrics (MAE, RMSE, MAPE) are calculated strictly on held-out future observations (75% train, 25% test partition). Random time-series shuffling and temporal data leakage are strictly prevented. MAPE is safely omitted if actual values approach zero."),
          p(class = "mb-1", 
            strong("CPCB NAQI Statutory Risk Classification: "), 
            "Alerts classify trajectory states into Potential Deterioration (≥ +15%), Potential Improvement (≤ -15%), Stable, High Uncertainty, or Insufficient History, mapped to CPCB NAQI break points (Moderate: >60/100, Poor: >90/250, Very Poor: >120/350, Severe: >250/430 µg/m³). All alerts denote indicative statistical estimates, not official regulatory declarations."),
          p(class = "mb-0", 
            strong("Scientific Data Sufficiency Standard: "), 
            "Reliable time-series modeling requires at least 10–24 sequential timestamps per station. Stations with fewer than 10 timestamps are strictly gated and segregated into an audit table rather than assigning invented predictions.")
        )
      ),
      
      # 2. Control & Filter Panel
      div(
        class = "card p-3 mb-3 shadow-sm border-0 bg-white",
        fluidRow(
          # State Selector
          column(
            width = 3,
            selectInput(
              inputId = ns("pred_filter_state"),
              label = "State / UT:",
              choices = c("All States", sort(unique(BASE_DATA$state))),
              selected = "Delhi",
              width = "100%"
            )
          ),
          # City Selector
          column(
            width = 2,
            selectInput(
              inputId = ns("pred_filter_city"),
              label = "City:",
              choices = c("All Cities"),
              selected = "All Cities",
              width = "100%"
            )
          ),
          # Station Selector
          column(
            width = 3,
            selectInput(
              inputId = ns("pred_filter_station"),
              label = "Monitoring Station:",
              choices = NULL,
              width = "100%"
            )
          ),
          # Pollutant Selector
          column(
            width = 2,
            selectInput(
              inputId = ns("pred_filter_pollutant"),
              label = "Target Pollutant:",
              choices = c("PM2.5" = "PM2.5", "PM10" = "PM10"),
              selected = "PM2.5",
              width = "100%"
            )
          ),
          # Horizon Selector
          column(
            width = 2,
            selectInput(
              inputId = ns("pred_filter_horizon"),
              label = "Forecast Horizon:",
              choices = c("+6 Hours (Short-Term)" = 6, "+12 Hours (Half-Day)" = 12, "+24 Hours (Full-Day)" = 24),
              selected = 6,
              width = "100%"
            )
          )
        ),
        fluidRow(
          # Model Selector
          column(
            width = 4,
            selectInput(
              inputId = ns("pred_filter_model"),
              label = "Forecasting Model:",
              choices = c(
                "Best Model (Auto-Selected)" = "Best Model (Auto-Selected)",
                "Auto-ARIMA (AICc Optimized)" = "Auto-ARIMA",
                "Exponential Smoothing (ETS)" = "Exponential Smoothing (ETS)",
                "Persistence Naive Baseline" = "Persistence Baseline"
              ),
              selected = "Best Model (Auto-Selected)",
              width = "100%"
            )
          ),
          column(
            width = 8,
            div(
              style = "margin-top: 28px;",
              class = "d-flex justify-content-end align-items-center gap-2",
              actionButton(
                inputId = ns("btn_pred_reset"),
                label = "Reset Filters",
                icon = icon("rotate-left"),
                class = "btn btn-outline-secondary btn-sm"
              )
            )
          )
        )
      ),
      
      # 3. Dynamic Readiness Status Banner
      uiOutput(ns("pred_status_banner")),
      
      # 4. Summary KPI Cards
      fluidRow(
        # KPI 1: Current Observed Reading
        column(
          width = 3,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Current Observed Reading"),
            div(class = "kpi-value text-dark", textOutput(ns("kpi_last_reading"))),
            p(class = "kpi-subtitle fw-bold", textOutput(ns("kpi_last_time"))),
            p(class = "small text-muted m-0", textOutput(ns("kpi_station_name")))
          )
        ),
        # KPI 2: Predicted Future Concentration
        column(
          width = 3,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Predicted Concentration"),
            div(class = "kpi-value text-primary", textOutput(ns("kpi_pred_val"))),
            p(class = "kpi-subtitle fw-bold", uiOutput(ns("kpi_pred_cat"))),
            p(class = "small text-muted m-0", textOutput(ns("kpi_pred_horizon")))
          )
        ),
        # KPI 3: Projected Trajectory & Risk
        column(
          width = 3,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Trajectory & Risk Status"),
            div(class = "kpi-value text-dark", uiOutput(ns("kpi_trajectory_badge"))),
            p(class = "kpi-subtitle fw-bold", textOutput(ns("kpi_trajectory_sub"))),
            p(class = "small text-muted m-0", textOutput(ns("kpi_threshold_ref")))
          )
        ),
        # KPI 4: Uncertainty Envelope & Model
        column(
          width = 3,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Uncertainty Envelope & Model"),
            div(class = "kpi-value text-primary", textOutput(ns("kpi_uncertainty_range"))),
            p(class = "kpi-subtitle fw-bold", textOutput(ns("kpi_active_model_name"))),
            p(class = "small text-muted m-0", textOutput(ns("kpi_test_mae_info")))
          )
        )
      ),
      
      br(),
      
      # 5. Explainable Early Warning Alert Panel
      uiOutput(ns("pred_early_warning_ui")),
      
      br(),
      
      # 6. Tabbed Analytical Panels
      navset_card_tab(
        id = ns("pred_tabs"),
        
        # TAB 1: Predictive Trajectory & Uncertainty Envelope
        nav_panel(
          title = "Predictive Trajectory",
          icon = icon("chart-line"),
          div(
            class = "py-2",
            div(
              class = "d-flex justify-content-between align-items-center mb-1",
              h6(class = "card-title m-0 fw-bold", textOutput(ns("trajectory_chart_title"), inline = TRUE)),
              span(class = "badge bg-light text-secondary border", textOutput(ns("trajectory_chart_badge"), inline = TRUE))
            ),
            p(class = "small text-muted mb-2", 
              "Solid line represents verified historical observations; dashed line marks point forecasts, bounded by shaded 80% and 95% uncertainty envelopes."),
            plotlyOutput(ns("forecast_trajectory_plot"), height = "420px"),
            uiOutput(ns("understand_forecast_trajectory"))
          )
        ),
        
        # TAB 2: Future Hotspot Geospatial Projection
        nav_panel(
          title = "Future Hotspots Map",
          icon = icon("map-location-dot"),
          div(
            class = "py-2",
            div(
              class = "d-flex justify-content-between align-items-center mb-1",
              h6(class = "card-title m-0 fw-bold", textOutput(ns("hotspot_map_title"), inline = TRUE)),
              span(class = "badge bg-light text-secondary border", "National Projected Coverage")
            ),
            p(class = "small text-muted mb-2", 
              "Projected station-level air quality index categories at the selected forecast horizon. Click station markers for concentration details and projection methodology."),
            leafletOutput(ns("forecast_hotspot_map"), height = "450px"),
            uiOutput(ns("understand_hotspot_map"))
          )
        ),
        
        # TAB 3: Out-of-Sample Model Evaluation & Benchmark Comparison
        nav_panel(
          title = "Model Evaluation & Transparency",
          icon = icon("scale-balanced"),
          div(
            class = "py-2",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title m-0 fw-bold", "Out-of-Sample Multi-Model Performance Comparison on Held-Out Observations"),
              span(class = "badge bg-light text-primary border", "Zero Data Leakage")
            ),
            p(class = "small text-muted mb-3", 
              "Models (Persistence Baseline, Auto-ARIMA, and ETS) are benchmarked strictly on unseen chronological test observations (75% train, 25% test partition). The superior model is selected dynamically based on minimum held-out test MAE."),
            uiOutput(ns("model_evaluation_ui"))
          )
        ),
        
        # TAB 4: Future Projection Schedule & CSV Export
        nav_panel(
          title = "Forecast Schedule",
          icon = icon("table-list"),
          div(
            class = "py-2",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title m-0 fw-bold", "Chronological Forecast Schedule Table"),
              downloadButton(
                outputId = ns("dl_forecast_csv"),
                label = "Download Forecast Schedule (CSV)",
                class = "btn-primary btn-sm"
              )
            ),
            p(class = "small text-muted mb-2", 
              "Tabulated step-by-step point projections, calibrated uncertainty bounds (Lo80, Hi80, Lo95, Hi95), and projected CPCB categories."),
            DTOutput(ns("forecast_schedule_table"))
          )
        ),
        
        # TAB 5: Forecast Hotspot Rankings & Data Sufficiency Audit
        nav_panel(
          title = "Forecast Hotspot Rankings",
          icon = icon("ranking-star"),
          div(
            class = "py-2",
            div(
              class = "d-flex justify-content-between align-items-center mb-1",
              h6(class = "card-title m-0 fw-bold", "National Forecast Hotspot Rankings & Data Sufficiency Audit"),
              span(class = "badge bg-light text-secondary border", "Scientific Data Integrity Standard")
            ),
            p(class = "small text-muted mb-3", 
              "Stations with sufficient historical depth (N ≥ 10 sequential timestamps) are statistically projected and ranked by anticipated pollutant concentration. Stations with inadequate historical depth (N < 10) are strictly segregated into the data sufficiency audit table below with zero fabricated predictions."),
            
            navset_pill(
              id = ns("hotspot_subtabs"),
              nav_panel(
                title = "Viable Forecast Hotspot Rankings (N ≥ 10)",
                div(
                  class = "pt-3",
                  uiOutput(ns("viable_hotspots_ui"))
                )
              ),
              nav_panel(
                title = "Data Sufficiency Exclusion Audit (N < 10)",
                div(
                  class = "pt-3",
                  uiOutput(ns("inadequate_stations_ui"))
                )
              )
            ),
            br(),
            uiOutput(ns("understand_hotspot_ranking"))
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# 2. Server Logic
# ------------------------------------------------------------------------------
mod_prediction_server <- function(id, shared_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Historical Repository Access (reacts to shared_trigger)
    historical_repo <- reactive({
      if (!is.null(shared_trigger)) {
        shared_trigger()
      }
      load_historical_repository(base_df = BASE_DATA, archive_dir = "data/archive")
    })
    
    # --------------------------------------------------------------------------
    # Cascading State -> City -> Station Dropdowns
    # --------------------------------------------------------------------------
    observeEvent(input$pred_filter_state, {
      req(input$pred_filter_state)
      st <- input$pred_filter_state
      df <- historical_repo()$data
      
      cities <- if (identical(st, "All States")) {
        c("All Cities", sort(unique(df$city)))
      } else {
        c("All Cities", sort(unique(df$city[df$state == st])))
      }
      
      updateSelectInput(session, "pred_filter_city", choices = cities, selected = "All Cities")
    })
    
    observeEvent(list(input$pred_filter_state, input$pred_filter_city), {
      req(input$pred_filter_state, input$pred_filter_city)
      st <- input$pred_filter_state
      ct <- input$pred_filter_city
      df <- historical_repo()$data
      
      sub_df <- df
      if (!identical(st, "All States")) sub_df <- sub_df %>% filter(state == st)
      if (!identical(ct, "All Cities")) sub_df <- sub_df %>% filter(city == ct)
      
      stations <- sort(unique(sub_df$station))
      default_stn <- if ("Anand Vihar, Delhi - DPCC" %in% stations) {
        "Anand Vihar, Delhi - DPCC"
      } else if (length(stations) > 0) {
        stations[1]
      } else {
        NULL
      }
      
      updateSelectInput(session, "pred_filter_station", choices = stations, selected = default_stn)
    })
    
    # Reset Filters Button
    observeEvent(input$btn_pred_reset, {
      updateSelectInput(session, "pred_filter_state", selected = "Delhi")
      updateSelectInput(session, "pred_filter_city", selected = "All Cities")
      updateSelectInput(session, "pred_filter_pollutant", selected = "PM2.5")
      updateSelectInput(session, "pred_filter_horizon", selected = 6)
      updateSelectInput(session, "pred_filter_model", selected = "Best Model (Auto-Selected)")
    })
    
    # Reactive User Selections
    curr_station <- reactive({ req(input$pred_filter_station) })
    curr_pollutant <- reactive({ req(input$pred_filter_pollutant) })
    curr_horizon <- reactive({ as.numeric(req(input$pred_filter_horizon)) })
    curr_model <- reactive({ req(input$pred_filter_model) })
    
    # --------------------------------------------------------------------------
    # Execute Master Forecasting Pipeline
    # --------------------------------------------------------------------------
    forecast_result <- reactive({
      repo <- historical_repo()
      stn <- curr_station()
      pol <- curr_pollutant()
      h <- curr_horizon()
      m <- curr_model()
      
      run_air_quality_forecast(
        data = repo$data,
        station_name = stn,
        target_pollutant = pol,
        horizon = h,
        model_choice = m,
        train_ratio = 0.75,
        min_required_obs = 10
      )
    })
    
    # Reactive Hotspot Rankings
    hotspot_rankings <- reactive({
      repo <- historical_repo()
      pol <- curr_pollutant()
      h <- curr_horizon()
      rank_forecast_hotspots(repo$data, target_pollutant = pol, horizon = h)
    })
    
    # --------------------------------------------------------------------------
    # Readiness Status Banner
    # --------------------------------------------------------------------------
    output$pred_status_banner <- renderUI({
      res <- forecast_result()
      repo <- historical_repo()
      
      if (!res$can_forecast) {
        is_single_snapshot <- identical(res$status, "SINGLE_SNAPSHOT") || res$n_obs <= 1
        is_insufficient_horizon <- identical(res$status, "INSUFFICIENT_FOR_HORIZON")
        
        state_badge <- if (is_single_snapshot) {
          span(class = "badge bg-secondary text-white me-2", "Baseline Snapshot (N=1)")
        } else if (is_insufficient_horizon) {
          span(class = "badge bg-warning text-dark me-2", paste0("Insufficient Depth for +", curr_horizon(), "h"))
        } else {
          span(class = "badge bg-warning text-dark me-2", paste0("Awaiting Data (N=", res$n_obs, "/10)"))
        }
        
        next_action_text <- if (is_single_snapshot) {
          "Accumulate additional snapshots via 'Historical Trends' auto-polling or background sync. The forecasting engine activates automatically once N ≥ 10 observations accumulate for this station."
        } else if (is_insufficient_horizon) {
          sprintf("Select a shorter forecast horizon (+6 Hours) or accumulate %d more observations to reach the %d-observation requirement for the +%d-hour horizon.",
                  res$min_required - res$n_obs, res$min_required, curr_horizon())
        } else {
          sprintf("Station '%s' has %d observations (requires ≥10 for time-series modeling). Continue accumulating snapshots in Historical Trends.",
                  res$station, res$n_obs)
        }
        
        div(
          class = "alert alert-warning mb-3 shadow-sm border",
          style = "border-left: 5px solid #D97706 !important;",
          div(
            class = "d-flex align-items-center mb-1",
            icon("lock", class = "fa-lg me-2 text-warning"),
            div(class = "fw-bold", state_badge, "Historical Data Readiness: Time-Series Models Gated")
          ),
          div(class = "small text-muted mb-2", res$message),
          div(
            class = "small p-2 rounded bg-white border text-dark",
            icon("lightbulb", class = "me-1 text-warning"),
            strong("Next Action: "),
            next_action_text
          )
        )
      } else {
        # Forecasting Operational
        sup_cls <- switch(
          res$superior_model,
          "Auto-ARIMA" = "badge bg-success text-white",
          "Exponential Smoothing" = "badge bg-info text-dark",
          "badge bg-primary text-white"
        )
        
        div(
          class = "alert alert-success mb-3 shadow-sm border",
          style = "border-left: 5px solid #059669 !important;",
          div(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-1",
            div(
              icon("circle-check", class = "fa-lg me-2 text-success"),
              span(class = "fw-bold", paste0("Forecasting Operational: ", res$active_model_name)),
              span(class = paste("ms-2", sup_cls), paste("Superior Model:", res$superior_model))
            ),
            div(
              span(class = "badge bg-light text-secondary border me-1", 
                   icon("shield-halved", class = "me-1 text-success"), "Data Leakage: None"),
              span(class = "badge bg-light text-secondary border", 
                   icon("calendar-check", class = "me-1 text-primary"), paste0("Test N: ", res$n_test))
            )
          ),
          div(class = "small text-muted mb-2", 
              sprintf("Evaluated on %s. Out-of-sample MAE: %.2f %s. %s",
                      res$evaluation_period,
                      if (!is.null(res$candidate_models_table)) {
                        m_row <- res$candidate_models_table %>% filter(model_name == res$active_model_name)
                        if (nrow(m_row) > 0 && !is.na(m_row$test_mae[1])) m_row$test_mae[1] else res$baseline_metrics$mae
                      } else {
                        res$baseline_metrics$mae
                      },
                      res$baseline_metrics$unit,
                      res$superior_reason)),
          div(
            class = "small p-2 rounded bg-white border text-dark",
            icon("lightbulb", class = "me-1 text-success"),
            strong("Next Action: "),
            "Review trajectory projection with 80%/95% confidence intervals, early warning risk advisory, and candidate model evaluation below."
          )
        )
      }
    })
    
    # --------------------------------------------------------------------------
    # KPI Summaries
    # --------------------------------------------------------------------------
    # KPI 1: Current Observed Reading
    output$kpi_last_reading <- renderText({
      res <- forecast_result()
      unit <- "µg/m³"
      if (!is.null(res$historical_series) && nrow(res$historical_series) > 0) {
        val <- tail(res$historical_series$pollutant_avg, 1)
        paste0(round(val, 1), " ", unit)
      } else {
        "N/A"
      }
    })
    
    output$kpi_last_time <- renderText({
      res <- forecast_result()
      if (!is.null(res$historical_series) && nrow(res$historical_series) > 0) {
        t <- max(res$historical_series$observation_time)
        format(t, "%d-%b-%Y %H:%M IST")
      } else {
        "No Observations"
      }
    })
    
    output$kpi_station_name <- renderText({
      curr_station()
    })
    
    # KPI 2: Predicted Future Concentration
    output$kpi_pred_val <- renderText({
      res <- forecast_result()
      unit <- "µg/m³"
      if (res$can_forecast && !is.null(res$future_forecasts)) {
        val <- tail(res$future_forecasts$point_forecast, 1)
        paste0(round(val, 1), " ", unit)
      } else {
        "Gated (N < 10)"
      }
    })
    
    output$kpi_pred_cat <- renderUI({
      res <- forecast_result()
      pol <- curr_pollutant()
      
      cat_name <- if (res$can_forecast && !is.null(res$future_forecasts)) {
        tail(res$future_forecasts$cpcb_category, 1)
      } else if (!is.null(res$historical_series) && nrow(res$historical_series) > 0) {
        val <- tail(res$historical_series$pollutant_avg, 1)
        si <- calc_single_subindex(pol, val)
        get_aqi_category(si)
      } else {
        "Unknown"
      }
      
      badge_bg <- switch(
        cat_name,
        "Good" = "#00B050",
        "Satisfactory" = "#92D050",
        "Moderate" = "#D97706",
        "Poor" = "#EA580C",
        "Very Poor" = "#DC2626",
        "Severe" = "#7030A0",
        "#64748B"
      )
      
      span(
        class = "badge text-white py-1 px-2",
        style = paste0("background-color: ", badge_bg, "; font-size: 0.90rem;"),
        cat_name
      )
    })
    
    output$kpi_pred_horizon <- renderText({
      res <- forecast_result()
      h <- curr_horizon()
      if (res$can_forecast) {
        paste0("Horizon: +", h, " Hours (Point Forecast)")
      } else {
        "Current Observed Status"
      }
    })
    
    # KPI 3: Projected Trajectory & Risk
    output$kpi_trajectory_badge <- renderUI({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$risk_classification)) {
        rc <- res$risk_classification
        badge_cls <- rc$badge_class
        span(class = paste("badge", badge_cls, "py-1 px-2"), style = "font-size: 0.85rem;", rc$risk_label)
      } else {
        span(class = "badge bg-secondary text-white py-1 px-2", style = "font-size: 0.85rem;", "INSUFFICIENT HISTORY")
      }
    })
    
    output$kpi_trajectory_sub <- renderText({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$risk_classification)) {
        rc <- res$risk_classification
        sign_str <- if (rc$delta >= 0) "+" else ""
        sprintf("Δ: %s%.1f µg/m³ (%s%.1f%%)", sign_str, rc$delta, sign_str, rc$pct_change)
      } else {
        "Awaiting sequential timestamps"
      }
    })
    
    output$kpi_threshold_ref <- renderText({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$risk_classification)) {
        res$risk_classification$threshold_name
      } else {
        "CPCB NAQI Standard Grounded"
      }
    })
    
    # KPI 4: Uncertainty Envelope & Model
    output$kpi_uncertainty_range <- renderText({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$future_forecasts)) {
        lo <- tail(res$future_forecasts$lo95, 1)
        hi <- tail(res$future_forecasts$hi95, 1)
        sprintf("95%% CI: [%.1f - %.1f]", lo, hi)
      } else {
        "Requires N ≥ 10 Obs"
      }
    })
    
    output$kpi_active_model_name <- renderText({
      res <- forecast_result()
      if (res$can_forecast) {
        res$active_model_name
      } else {
        "Persistence Baseline"
      }
    })
    
    output$kpi_test_mae_info <- renderText({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$candidate_models_table)) {
        m_row <- res$candidate_models_table %>% filter(model_name == res$active_model_name)
        mae_val <- if (nrow(m_row) > 0 && !is.na(m_row$test_mae[1])) m_row$test_mae[1] else res$baseline_metrics$mae
        sprintf("Held-Out Test MAE: %.2f µg/m³", mae_val)
      } else {
        "Zero synthetic data substitution"
      }
    })
    
    # Legacy KPI Output Bindings (Full Backward Compatibility)
    output$kpi_test_mae <- renderText({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$candidate_models_table)) {
        m_row <- res$candidate_models_table %>% filter(model_name == res$active_model_name)
        mae_val <- if (nrow(m_row) > 0 && !is.na(m_row$test_mae[1])) m_row$test_mae[1] else res$baseline_metrics$mae
        paste0(mae_val, " µg/m³")
      } else {
        "Baseline Snapshot"
      }
    })
    
    output$kpi_test_mae_sub <- renderText({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$baseline_metrics)) {
        paste0("Naive Baseline MAE: ", res$baseline_metrics$mae, " µg/m³")
      } else {
        "Requires ≥10 sequential observations"
      }
    })
    
    output$kpi_test_rmse <- renderText({
      res <- forecast_result()
      if (res$can_forecast && !is.null(res$candidate_models_table)) {
        m_row <- res$candidate_models_table %>% filter(model_name == res$active_model_name)
        rmse_val <- if (nrow(m_row) > 0 && !is.na(m_row$test_rmse[1])) m_row$test_rmse[1] else res$baseline_metrics$rmse
        paste0(rmse_val, " µg/m³")
      } else {
        "Gated"
      }
    })
    
    output$kpi_lift_sub <- renderText({
      res <- forecast_result()
      if (res$can_forecast) {
        paste0("Superior: ", res$superior_model)
      } else {
        "Awaiting multi-timestamp archive"
      }
    })
    
    output$kpi_projected_category <- renderUI({
      res <- forecast_result()
      pol <- curr_pollutant()
      cat_name <- if (res$can_forecast && !is.null(res$future_forecasts)) {
        tail(res$future_forecasts$cpcb_category, 1)
      } else if (!is.null(res$historical_series) && nrow(res$historical_series) > 0) {
        val <- tail(res$historical_series$pollutant_avg, 1)
        si <- calc_single_subindex(pol, val)
        get_aqi_category(si)
      } else {
        "Unknown"
      }
      span(class = "badge bg-secondary text-white py-1 px-2", cat_name)
    })
    
    output$kpi_projected_horizon <- renderText({
      res <- forecast_result()
      h <- curr_horizon()
      if (res$can_forecast) paste0("Projected Horizon: +", h, " Hours") else "Current Snapshot Status"
    })
    
    # --------------------------------------------------------------------------
    # Explainable Early Warning Alert Panel
    # --------------------------------------------------------------------------
    output$pred_early_warning_ui <- renderUI({
      res <- forecast_result()
      pol <- curr_pollutant()
      stn <- curr_station()
      h <- curr_horizon()
      
      if (!res$can_forecast || is.null(res$risk_classification)) {
        return(NULL)
      }
      
      rc <- res$risk_classification
      card_border <- rc$badge_color
      card_bg <- switch(
        rc$status,
        "POTENTIAL_DETERIORATION" = "#FEF2F2",
        "POTENTIAL_IMPROVEMENT" = "#F0FDF4",
        "HIGH_UNCERTAINTY" = "#FFFBEB",
        "#F8FAFC"
      )
      
      div(
        class = "card p-3 shadow-sm border-0 mb-3",
        style = paste0("border-left: 5px solid ", card_border, " !important; background-color: ", card_bg, ";"),
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-2",
          div(
            span(class = paste("badge me-2", rc$badge_class), rc$risk_label),
            strong(style = paste0("color: ", card_border, "; font-size: 1.05rem;"), 
                   paste("Intelligent Early-Warning Assessment:", pol, "at", stn))
          ),
          span(class = "badge bg-white text-dark border", paste("Horizon: +", h, "Hours"))
        ),
        
        fluidRow(
          column(
            width = 4,
            div(
              class = "p-2 bg-white rounded border mb-2",
              div(class = "small text-muted", "Concentration Trajectory:"),
              div(class = "fw-bold", 
                  sprintf("Observed: %.1f µg/m³ → Predicted: %.1f µg/m³", rc$current_val, rc$predicted_val)),
              div(class = "small", 
                  style = paste0("color: ", card_border, "; font-weight: 600;"),
                  sprintf("Projected Change: %s%.1f µg/m³ (%s%.1f%%)", 
                          if (rc$delta >= 0) "+" else "", rc$delta,
                          if (rc$pct_change >= 0) "+" else "", rc$pct_change))
            )
          ),
          column(
            width = 4,
            div(
              class = "p-2 bg-white rounded border mb-2",
              div(class = "small text-muted", "Statutory CPCB NAQI Benchmark:"),
              div(class = "fw-bold", rc$threshold_name),
              div(class = "small text-muted", 
                  sprintf("Threshold: %.0f µg/m³ | Breach: %s", 
                          rc$threshold_val, if (isTRUE(rc$breach_projected)) "YES" else "NO"))
            )
          ),
          column(
            width = 4,
            div(
              class = "p-2 bg-white rounded border mb-2",
              div(class = "small text-muted", "95% Prediction Interval:"),
              div(class = "fw-bold text-primary", rc$uncertainty_bounds),
              div(class = "small text-muted", paste("Active Model:", res$active_model_name))
            )
          )
        ),
        
        div(
          class = "mt-2 p-2 bg-white rounded border",
          div(class = "fw-semibold text-secondary mb-1", icon("circle-info", class = "me-1 text-primary"), "Explainable Analytical Rationale:"),
          p(class = "small text-dark mb-2", rc$rationale),
          div(class = "fw-semibold text-secondary mb-1", icon("shield-halved", class = "me-1 text-warning"), "Civic & Public Health Advisory:"),
          p(class = "small text-dark mb-1", rc$civic_advisory),
          div(class = "small text-muted fst-italic mt-2 pt-1 border-top", rc$disclaimer)
        )
      )
    })
    
    # --------------------------------------------------------------------------
    # TAB 1: Predictive Trajectory Plotly Chart
    # --------------------------------------------------------------------------
    output$trajectory_chart_title <- renderText({
      pol <- curr_pollutant()
      stn <- curr_station()
      paste0("Air Quality Trajectory & Uncertainty Envelope: ", pol, " at ", stn)
    })
    
    output$trajectory_chart_badge <- renderText({
      m <- curr_model()
      h <- curr_horizon()
      paste0(m, " | +", h, "h Horizon")
    })
    
    output$forecast_trajectory_plot <- renderPlotly({
      res <- forecast_result()
      pol <- curr_pollutant()
      unit <- "µg/m³"
      
      # Insufficient history state: render educational fallback
      if (!res$can_forecast) {
        df_hist <- res$historical_series
        
        if (is.null(df_hist) || nrow(df_hist) == 0) {
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = paste("No valid observations found for", pol), color = "#64748B", size = 4) +
            theme_void()
          return(ggplotly(p))
        }
        
        # Single point / limited history visualization with clear gating annotation
        p <- ggplot(df_hist, aes(x = observation_time, y = pollutant_avg)) +
          geom_point(color = "#1E3A8A", size = 4) +
          annotate("rect", xmin = df_hist$observation_time[1] - 3600, xmax = df_hist$observation_time[1] + 3600*12,
                   ymin = 0, ymax = max(df_hist$pollutant_avg * 1.5, 100), alpha = 0.05, fill = "#F59E0B") +
          annotate("text", x = df_hist$observation_time[1] + 3600*6, y = df_hist$pollutant_avg,
                   label = sprintf("Station has %d observation(s).\nTime-series statistical forecasting requires ≥10 sequential observations.\nTrajectory curve gated to prevent fabricating synthetic trends.", nrow(df_hist)),
                   color = "#B45309", size = 3.5, fontface = "bold") +
          scale_x_datetime(date_labels = "%d-%b %H:%M") +
          labs(
            x = "Observation Timestamp (IST)",
            y = paste0("Concentration (", unit, ")")
          ) +
          theme_minimal(base_size = 11) +
          theme(panel.grid.minor = element_blank())
        
        return(ggplotly(p) %>% plotly::config(displayModeBar = FALSE))
      }
      
      # Viable forecasting sequence
      df_hist <- res$historical_series
      df_fc <- res$future_forecasts
      
      p <- ggplot() +
        # 1. Prediction Interval Ribbon (95%)
        geom_ribbon(data = df_fc, aes(x = observation_time, ymin = lo95, ymax = hi95), 
                    fill = "#93C5FD", alpha = 0.35) +
        # 2. Prediction Interval Ribbon (80%)
        geom_ribbon(data = df_fc, aes(x = observation_time, ymin = lo80, ymax = hi80), 
                    fill = "#60A5FA", alpha = 0.45) +
        # 3. Historical Observations (Solid Navy Line & Points)
        geom_line(data = df_hist, aes(x = observation_time, y = pollutant_avg, group = 1), 
                  color = "#1E3A8A", linewidth = 1.1) +
        geom_point(data = df_hist, aes(x = observation_time, y = pollutant_avg, 
                                       text = paste0("Observed Historical\nTime: ", format(observation_time, "%d-%b %H:%M"), " IST\n",
                                                     pol, ": ", pollutant_avg, " ", unit)), 
                   color = "#1E3A8A", size = 2.5) +
        # 4. Vertical Partition Line
        geom_vline(xintercept = as.numeric(max(df_hist$observation_time)), 
                   linetype = "dashed", color = "#64748B", linewidth = 0.8) +
        # 5. Future Point Forecasts (Dashed Red Line & Diamond Points)
        geom_line(data = df_fc, aes(x = observation_time, y = point_forecast, group = 1), 
                  color = "#DC2626", linewidth = 1.1, linetype = "dashed") +
        geom_point(data = df_fc, aes(x = observation_time, y = point_forecast, 
                                     text = paste0("Projected Forecast (", res$active_model_name, ")\nTime: ", time_label, " IST\nForecast: ", 
                                                   point_forecast, " ", unit, "\n80% CI: [", lo80, ", ", hi80, "]\n95% CI: [", 
                                                   lo95, ", ", hi95, "]\nCategory: ", cpcb_category)), 
                   color = "#DC2626", size = 3, shape = 18) +
        scale_x_datetime(date_labels = "%d-%b\n%H:%M", breaks = scales::pretty_breaks(n = 6)) +
        labs(
          x = "Timeline (IST)",
          y = paste0("Concentration (", unit, ")")
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
    
    # Understand Predictive Trajectory Chart
    output$understand_forecast_trajectory <- renderUI({
      res <- forecast_result()
      pol <- curr_pollutant()
      stn <- curr_station()
      unit <- "µg/m³"
      
      findings <- if (!res$can_forecast) {
        paste0("Currently operating with ", res$n_obs, " observation(s) at ", stn, ". ",
               "Predictive time-series trajectories and uncertainty cones remain gated to prevent fabricating synthetic trends. ",
               "To unlock statistical modeling, accumulate sequential observations in the Historical Trends module.")
      } else {
        last_obs <- tail(res$historical_series$pollutant_avg, 1)
        peak_fc <- max(res$future_forecasts$point_forecast)
        cat_final <- tail(res$future_forecasts$cpcb_category, 1)
        paste0("For ", stn, ", observed ", pol, " stands at ", round(last_obs, 1), " ", unit, ". ",
               "The model (", res$active_model_name, ") projects concentrations to reach a peak of ", round(peak_fc, 1), " ", unit, 
               " with a forecasted horizon category of '", cat_final, "'. ",
               "The 95% predictive uncertainty envelope reflects model variance on unseen held-out test partitions.")
      }
      
      build_understand_chart(
        chart_type = "Time-Series Predictive Trajectory & Uncertainty Cone",
        what_shows = "This chart tracks verified historical ambient pollutant concentrations leading into future statistical point forecasts and calibrated 80% and 95% uncertainty envelopes.",
        how_to_read = "The horizontal x-axis marks observation timestamps in IST. The vertical y-axis plots pollutant concentration in µg/m³. The solid navy line represents verified historical observations. The dashed vertical line marks the forecasting boundary, followed by dashed red line point forecasts and shaded blue prediction cones.",
        colors_meaning = "Navy blue indicates verified historical readings. Red diamonds and dashed lines highlight future point projections. Light and medium blue shaded ribbons delineate the 95% and 80% prediction intervals.",
        dynamic_findings = findings,
        important_note = "Forecasts represent statistical and baseline model projections grounded in historical observations. They cannot anticipate sudden local emission spikes or abrupt meteorological changes."
      )
    })
    
    # --------------------------------------------------------------------------
    # TAB 2: Future Hotspot Map
    # --------------------------------------------------------------------------
    output$hotspot_map_title <- renderText({
      pol <- curr_pollutant()
      h <- curr_horizon()
      paste0("Projected Spatial Hotspots Map: ", pol, " (+", h, " Hours Horizon)")
    })
    
    output$forecast_hotspot_map <- renderLeaflet({
      repo <- historical_repo()
      pol <- curr_pollutant()
      h <- curr_horizon()
      
      hotspot_df <- forecast_network_hotspots(repo$data, target_pollutant = pol, horizon = h)
      
      m <- leaflet(options = leafletOptions(minZoom = 3, maxZoom = 14)) %>%
        addTiles(group = "OpenStreetMap (Standard)") %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
        addProviderTiles("CartoDB.Voyager", group = "CartoDB Voyager") %>%
        addProviderTiles("Esri.WorldStreetMap", group = "Esri World Street") %>%
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
        fitBounds(lng1 = 68.5, lat1 = 7.5, lng2 = 97.5, lat2 = 35.5) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap (Standard)", "CartoDB Positron", "CartoDB Voyager", "Esri World Street"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      if (nrow(hotspot_df) == 0) return(m)
      
      # Category color mapping
      cat_colors <- c(
        "Good" = "#00B050",
        "Satisfactory" = "#92D050",
        "Moderate" = "#D97706",
        "Poor" = "#EA580C",
        "Very Poor" = "#DC2626",
        "Severe" = "#7030A0",
        "Unknown" = "#64748B"
      )
      
      marker_colors <- unname(sapply(hotspot_df$projected_category, function(cat) {
        if (cat %in% names(cat_colors)) cat_colors[[cat]] else "#64748B"
      }))
      
      popups <- sapply(seq_len(nrow(hotspot_df)), function(i) {
        paste0(
          "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.4;'>",
          "<div style='font-weight: 700; color: #1E3A8A;'>", hotspot_df$station[i], "</div>",
          "<div style='color: #475569;'>", hotspot_df$city[i], ", ", hotspot_df$state[i], "</div>",
          "<hr style='margin: 4px 0;'/>",
          "<div><strong>Current Observed: </strong>", hotspot_df$current_reading[i], " µg/m³</div>",
          "<div><strong>Projected (+", h, "h): </strong>", hotspot_df$projected_value[i], " µg/m³</div>",
          "<div><strong>Projected Category: </strong>",
          "<span style='color: ", marker_colors[i], "; font-weight: bold;'>", hotspot_df$projected_category[i], "</span></div>",
          "<div style='font-size: 11px; color: #64748B; margin-top: 4px;'>Method: ", hotspot_df$projection_method[i], "</div>",
          "</div>"
        )
      })
      
      m %>% addCircleMarkers(
        data = hotspot_df,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5.5,
        fillColor = marker_colors,
        fillOpacity = 0.85,
        color = "#FFFFFF",
        weight = 1.0,
        popup = popups,
        label = ~paste0(station, ": ", projected_value, " µg/m³ (", projected_category, ")")
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#00B050", "#92D050", "#D97706", "#EA580C", "#DC2626", "#7030A0"),
        labels = c("Good (0-30)", "Satisfactory (31-60)", "Moderate (61-90)", "Poor (91-120)", "Very Poor (121-250)", "Severe (>250)"),
        title = paste("Projected", pol, "Category"),
        opacity = 0.9
      )
    })
    
    # Understand Hotspot Map
    output$understand_hotspot_map <- renderUI({
      pol <- curr_pollutant()
      h <- curr_horizon()
      
      findings <- paste0(
        "Projecting spatial concentration distributions across all active monitoring stations for the +", h,
        " hour horizon. Stations with sufficient longitudinal depth utilize statistical models; ",
        "stations with limited historical depth maintain calibrated persistence projections. ",
        "Color swatches reflect projected CPCB Air Quality Index categories."
      )
      
      build_understand_chart(
        chart_type = "Geospatial Projected Hotspot Leaflet Map",
        what_shows = "This map plots continuous monitoring stations across India color-coded by projected CPCB categories at the selected forecast horizon, identifying geographic regions susceptible to acute exposure.",
        how_to_read = "Circle markers represent physical CAAQM stations. Use the zoom and pan controls to explore regional hotspots. Click any marker to view current observations, projected values, projected categories, and underlying estimation methodology.",
        colors_meaning = "Standard CPCB 6-tier air quality palette: Green (Good), Light Green (Satisfactory), Yellow/Amber (Moderate), Orange (Poor), Red (Very Poor), and Purple (Severe).",
        dynamic_findings = findings,
        important_note = "Hotspot projections denote mathematical model estimates. In baseline mode, stations utilize persistence projections until multi-timestamp archives are ingested."
      )
    })
    
    # --------------------------------------------------------------------------
    # TAB 3: Model Evaluation & Performance Transparency
    # --------------------------------------------------------------------------
    output$model_evaluation_ui <- renderUI({
      res <- forecast_result()
      
      if (!res$can_forecast) {
        div(
          class = "card p-3 border-0 bg-light",
          h6(class = "fw-bold text-secondary mb-2", "Model Evaluation Protocol & Gating Explanation"),
          p(class = "small text-muted mb-2", 
            "In strict accordance with data science best practices, model evaluation metrics (MAE, RMSE, MAPE) are never reported on synthetic or fabricated test data."),
          p(class = "small text-muted mb-2", 
            sprintf("Station '%s' has %d observation(s). Chronological train/test partitioning (e.g. 75%% train, 25%% test) requires at least 10 sequential timestamps.", res$station, res$n_obs)),
          div(
            class = "interpretation-box p-2 small mt-2",
            icon("circle-info", class = "me-1 text-primary"),
            "Status: Evaluation metrics remain withheld until genuine sequential historical observations are ingested. Synthetic test scores are never substituted for real-world accuracy."
          )
        )
      } else {
        tagList(
          # Candidate Models Scorecard Table
          h6(class = "fw-bold text-primary mb-2", icon("ranking-star", class = "me-1"), "Candidate Models Scorecard on Held-Out Test Observations"),
          p(class = "small text-muted mb-2", 
            "All candidate models were trained strictly on the chronological training partition (75%) and evaluated on the held-out test partition (25%). The model with the lowest test MAE is designated the superior performer."),
          DTOutput(ns("candidate_models_dt")),
          br(),
          # Predictions vs Actuals on Held-Out Test Set
          h6(class = "fw-bold text-dark mb-2", icon("chart-simple", class = "me-1"), "Held-Out Test Set: Predicted vs Actual Observations"),
          p(class = "small text-muted mb-2", "Step-by-step out-of-sample prediction validation for the active model."),
          DTOutput(ns("test_comparison_table"))
        )
      }
    })
    
    output$candidate_models_dt <- renderDT({
      res <- forecast_result()
      if (!res$can_forecast || is.null(res$candidate_models_table)) return(NULL)
      
      tbl <- res$candidate_models_table %>%
        select(
          `Model Specification` = model_name,
          `Family` = model_family,
          `Test MAE (µg/m³)` = test_mae,
          `Test RMSE (µg/m³)` = test_rmse,
          `Test MAPE` = test_mape,
          `Directional Accuracy` = directional_accuracy,
          `Category Concordance` = concordance,
          `Lift vs Baseline` = lift_vs_baseline,
          `Superior Performer` = is_best
        ) %>%
        mutate(
          `Superior Performer` = ifelse(`Superior Performer`, "★ Winner", "")
        )
      
      datatable(
        tbl,
        options = list(pageLength = 5, dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "table table-sm table-striped small align-middle"
      ) %>%
      formatStyle(
        "Superior Performer",
        target = "row",
        backgroundColor = styleEqual("★ Winner", "#F0FDF4")
      )
    })
    
    output$test_comparison_table <- renderDT({
      res <- forecast_result()
      if (!res$can_forecast || is.null(res$test_comparison)) return(NULL)
      
      datatable(
        res$test_comparison,
        options = list(pageLength = 6, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "table table-sm table-striped small"
      )
    })
    
    # --------------------------------------------------------------------------
    # TAB 4: Forecast Schedule Directory & CSV Export
    # --------------------------------------------------------------------------
    output$forecast_schedule_table <- renderDT({
      res <- forecast_result()
      
      if (!res$can_forecast || is.null(res$future_forecasts)) {
        empty_df <- data.frame(
          Status = "Forecast schedule unavailable with limited historical observations. Accumulate sequential snapshots to activate."
        )
        return(datatable(empty_df, options = list(dom = "t"), rownames = FALSE))
      }
      
      df <- res$future_forecasts %>%
        select(
          `Step (+h)` = step,
          `Timestamp (IST)` = time_label,
          `Point Forecast (µg/m³)` = point_forecast,
          `Lo 80% Bound` = lo80,
          `Hi 80% Bound` = hi80,
          `Lo 95% Bound` = lo95,
          `Hi 95% Bound` = hi95,
          `CPCB Category` = cpcb_category
        )
      
      datatable(
        df,
        options = list(pageLength = 12, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "table table-sm table-striped small align-middle"
      )
    })
    
    output$dl_forecast_csv <- downloadHandler(
      filename = function() {
        stn_clean <- gsub("[^a-zA-Z0-9]", "_", curr_station())
        paste0("cpcb_forecast_", curr_pollutant(), "_", stn_clean, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
      },
      content = function(file) {
        res <- forecast_result()
        if (res$can_forecast && !is.null(res$future_forecasts)) {
          write.csv(res$future_forecasts, file, row.names = FALSE)
        } else {
          write.csv(data.frame(Notice = "Forecasts unavailable in limited historical observation mode."), file, row.names = FALSE)
        }
      }
    )
    
    # --------------------------------------------------------------------------
    # TAB 5: Forecast Hotspot Rankings & Data Sufficiency Audit
    # --------------------------------------------------------------------------
    output$viable_hotspots_ui <- renderUI({
      rk <- hotspot_rankings()
      
      if (rk$viable_count == 0) {
        div(
          class = "card p-4 text-center bg-light border-0",
          icon("lock", class = "fa-2x text-warning mb-2"),
          h6(class = "fw-bold text-dark", "No Stations Currently Meet the N ≥ 10 Historical Depth Requirement"),
          p(class = "small text-muted mb-2", 
            sprintf("The current repository contains %d total monitoring stations across India. Currently, 0 stations have accumulated ≥10 sequential timestamps required for reliable statistical forecasting.", rk$total_stations)),
          p(class = "small text-muted mb-0", 
            "All stations are cataloged in the 'Data Sufficiency Exclusion Audit' sub-tab to maintain full scientific integrity. No synthetic predictions or false ranks are invented.")
        )
      } else {
        tagList(
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            span(class = "badge bg-success text-white", paste(rk$viable_count, "Viable Stations Ranked")),
            span(class = "small text-muted", "Sorted descending by projected concentration")
          ),
          DTOutput(ns("forecast_hotspots_table"))
        )
      }
    })
    
    output$forecast_hotspots_table <- renderDT({
      rk <- hotspot_rankings()
      if (rk$viable_count == 0) return(NULL)
      
      df <- rk$ranked_hotspots %>%
        select(
          `Rank` = rank,
          `Station` = station,
          `City` = city,
          `State` = state,
          `Current (µg/m³)` = current_value,
          `Observed Time` = last_timestamp,
          `Predicted (+h, µg/m³)` = predicted_value,
          `Horizon` = forecast_horizon,
          `Model` = model_name,
          `95% CI` = uncertainty_interval,
          `Risk Category` = risk_category,
          `Statutory Benchmark` = statutory_threshold,
          `Sufficiency` = data_sufficiency
        )
      
      datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "table table-sm table-striped small align-middle"
      )
    })
    
    output$inadequate_stations_ui <- renderUI({
      rk <- hotspot_rankings()
      
      tagList(
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          span(class = "badge bg-warning text-dark", paste(rk$inadequate_count, "Stations Excluded (N < 10)")),
          span(class = "small text-muted", "Gated to prevent fabricating predictions on inadequate data")
        ),
        DTOutput(ns("inadequate_stations_table"))
      )
    })
    
    output$inadequate_stations_table <- renderDT({
      rk <- hotspot_rankings()
      if (rk$inadequate_count == 0) return(NULL)
      
      df <- rk$inadequate_stations %>%
        select(
          `Station` = station,
          `City` = city,
          `State` = state,
          `Current (µg/m³)` = current_value,
          `Observed Time` = last_timestamp,
          `Available Obs (N)` = n_obs,
          `Required Obs` = required_observations,
          `Deficit` = missing_observations,
          `Status` = data_sufficiency,
          `Guidance` = status_guidance
        )
      
      datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE, dom = "tip"),
        rownames = FALSE,
        class = "table table-sm table-striped small align-middle"
      )
    })
    
    output$understand_hotspot_ranking <- renderUI({
      pol <- curr_pollutant()
      h <- curr_horizon()
      rk <- hotspot_rankings()
      
      findings <- sprintf(
        "Audit across %d nationwide monitoring stations for %s (+%dh horizon). %d stations possess sufficient historical depth (N ≥ 10) for statistical ranking, while %d stations are actively excluded and audited in the data sufficiency register until longitudinal data accumulates.",
        rk$total_stations, pol, h, rk$viable_count, rk$inadequate_count
      )
      
      build_understand_chart(
        chart_type = "Forecast Hotspot Ranking & Data Sufficiency Register",
        what_shows = "This register ranks monitoring stations by anticipated future pollutant concentration and audits data readiness across the entire CPCB network.",
        how_to_read = "The Viable Forecast Rankings tab lists stations sorted from highest to lowest predicted concentration, displaying active model, 95% uncertainty interval, and statutory threshold. The Data Sufficiency Exclusion Audit tab catalogs stations with fewer than 10 timestamps, tracking the exact observation deficit.",
        colors_meaning = "Green badges denote forecast-ready stations with verified historical depth; amber badges identify gated stations awaiting additional observation timestamps.",
        dynamic_findings = findings,
        important_note = "Strict exclusion of data-deficient stations is a foundational data science integrity requirement. Inventing predictions for single-snapshot stations introduces massive error and misleading civic alerts."
      )
    })
  })
}
