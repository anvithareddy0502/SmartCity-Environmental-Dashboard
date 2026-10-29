# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: server.R
# Purpose: Server logic for the Shiny application.
# ==============================================================================

server <- function(input, output, session) {
  
  # Reactive baseline data access
  data_r <- reactive({
    BASE_DATA
  })
  
  meta_r <- reactive({
    METADATA
  })
  
  # Shared repository refresh trigger across modules
  repo_refresh_trigger <- reactiveVal(0)
  
  # Reactive repository access for server-level status
  repo_r <- reactive({
    repo_refresh_trigger()
    load_historical_repository(base_df = BASE_DATA, archive_dir = "data/archive")
  })
  
  # Top Header Data Status Label (Subtle, truthful, dynamic)
  output$header_status_label <- renderText({
    repo <- repo_r()
    if (repo$distinct_timestamps_count <= 1) {
      "Data status: Offline snapshot | Updated: 23 Oct 2026, 18:00 IST"
    } else {
      paste0("Data status: Archive synchronized (", repo$distinct_timestamps_count, 
             " timestamps) | Latest: ", format(repo$latest_time, "%d-%b-%Y %H:%M IST"))
    }
  })
  
  # Initialize Executive Overview Module (Stage 2)
  mod_overview_server("overview")
  
  # Initialize Geographic Hotspot Explorer Module (Stage 3)
  mod_map_server("hotspot_map")
  
  # Initialize Advanced Visualization Studio Module (Stage 4)
  mod_viz_studio_server("viz_studio")
  
  # Initialize Anomaly & Data Quality Monitor Module (Stage 5)
  mod_quality_server("quality_monitor")
  
  # Initialize Historical Trends Module (Stage 4 & Stage 6)
  mod_trends_server("historical_trends", shared_trigger = repo_refresh_trigger)
  
  # Initialize Insights, Policy Evidence and Reports Module (Stage 6)
  mod_reports_server("insights_reports")
  
  # Initialize Environmental Data Assistant Module
  mod_assistant_server("data_assistant")
  
  # Initialize AI Forecast & Early Warning Studio Module (Stage 3 & Stage 4)
  mod_prediction_server("ai_forecast", shared_trigger = repo_refresh_trigger)
}
