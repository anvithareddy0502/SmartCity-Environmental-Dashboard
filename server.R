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
  
  # Top Header Data Status Label
  output$header_status_label <- renderText({
    obs_time_str <- "23-10-2026 18:00 IST"
    paste0("CPCB Snapshot: ", obs_time_str, " | Offline Baseline")
  })
  
  # Initialize Executive Overview Module (Stage 2)
  mod_overview_server("overview")
  
  # Initialize Geographic Hotspot Explorer Module (Stage 3)
  mod_map_server("hotspot_map")
  
  # Initialize Advanced Visualization Studio Module (Stage 4)
  mod_viz_studio_server("viz_studio")
  
  # Initialize Anomaly & Data Quality Monitor Module (Stage 5)
  mod_quality_server("quality_monitor")
  
  # Initialize Historical Trends Module (Stage 6)
  mod_trends_server("historical_trends")
  
  # Initialize Insights, Policy Evidence and Reports Module (Stage 6)
  mod_reports_server("insights_reports")
}
