# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: ui.R
# Purpose: User Interface definition using bslib Bootstrap 5 page_navbar.
# ==============================================================================

ui <- page_navbar(
  id = "navbar_nav",
  title = div(
    class = "d-flex align-items-center flex-wrap gap-2",
    span("Smart City Environmental Intelligence", style = "font-weight: 700; font-size: 1.05rem; letter-spacing: -0.01em;"),
    span("| India", style = "font-weight: 400; opacity: 0.8; margin-right: 6px; font-size: 0.95rem;"),
    span(
      class = "status-pill-subtle",
      span(class = "status-dot-subtle"),
      textOutput("header_status_label", inline = TRUE)
    ),
    span(
      class = "assistant-nav-pill",
      icon("robot", class = "me-1 text-info"),
      "Assistant Available"
    )
  ),
  theme = APP_THEME,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$meta(name = "description", content = "VIT CSI3005 Advanced Data Visualization Project"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('eval_js', function(code) {
        try { eval(code); } catch(e) {}
      });

      // Ensure Leaflet maps resize and re-render properly on tab switch
      $(document).on('shown.bs.tab', function(e) {
        window.dispatchEvent(new Event('resize'));
        setTimeout(function() {
          $('.leaflet-container').each(function() {
            var mapObj = window.HTMLWidgets && window.HTMLWidgets.find('#' + this.id);
            if (mapObj && mapObj.getMap) {
              var map = mapObj.getMap();
              if (map) {
                map.invalidateSize({ pan: false });
              }
            }
          });
        }, 150);
      });
    "))
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
  ),
  
  # TAB 7: Environmental Data Assistant
  nav_panel(
    title = "Data Assistant",
    icon = icon("robot"),
    mod_assistant_ui("data_assistant")
  ),
  
  # TAB 8: AI Forecast & Early Warning Studio
  nav_panel(
    title = "AI Forecast & Alerts",
    icon = icon("wand-magic-sparkles"),
    mod_prediction_ui("ai_forecast")
  )
)
