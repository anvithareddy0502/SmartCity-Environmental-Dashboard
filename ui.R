# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: ui.R
# Purpose: User Interface definition using bslib Bootstrap 5 page_navbar.
# ==============================================================================

ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center",
    span("Smart City Environmental Intelligence", style = "font-weight: 700;"),
    span(" | India", style = "font-weight: 400; opacity: 0.85; margin-left: 5px;")
  ),
  theme = APP_THEME,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$meta(name = "description", content = "VIT CSI3005 Advanced Data Visualization Project")
  ),
  
  # Global Data Status Banner
  nav_spacer(),
  nav_item(
    div(
      class = "d-flex align-items-center py-1",
      div(
        class = "status-badge status-badge-offline me-2",
        span(class = "pulse-dot pulse-dot-amber"),
        textOutput("header_status_label", inline = TRUE)
      )
    )
  ),
  
  # TAB 1: Executive Overview
  nav_panel(
    title = "Executive Overview",
    icon = icon("gauge-high"),
    mod_overview_ui("overview")
  ),
  
  # TAB 2: Geographic Hotspot Explorer
  nav_panel(
    title = "Geographic Hotspots",
    icon = icon("map-location-dot"),
    mod_map_ui("hotspot_map")
  ),
  
  # TAB 3: Advanced Visualization Studio
  nav_panel(
    title = "Visualization Studio",
    icon = icon("chart-line"),
    mod_viz_studio_ui("viz_studio")
  ),
  
  # TAB 4: Anomaly & Data Quality Monitor
  nav_panel(
    title = "Data Quality & Anomalies",
    icon = icon("triangle-exclamation"),
    mod_quality_ui("quality_monitor")
  ),
  
  # TAB 5: Historical Trends
  nav_panel(
    title = "Historical Trends",
    icon = icon("clock-rotate-left"),
    mod_trends_ui("historical_trends")
  ),
  
  # TAB 6: Insights & Reports
  nav_panel(
    title = "Insights & Reports",
    icon = icon("file-lines"),
    mod_reports_ui("insights_reports")
  )
)
