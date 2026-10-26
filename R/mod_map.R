# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/mod_map.R
# Purpose: Shiny module for Geographic Hotspot Explorer (Stage 3)
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(leaflet)
  library(plotly)
  library(DT)
  library(scales)
  library(stringr)
})

# UI Module for Geographic Hotspot Explorer
mod_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid py-3",
      
      # Top Provenance & Freshness Status Banner
      div(
        class = "alert alert-secondary d-flex align-items-center mb-3 py-2 px-3",
        role = "alert",
        icon("circle-info", class = "me-2 fs-5 text-primary"),
        div(
          class = "small",
          strong("Spatial Data Source: "), "Central Pollution Control Board (CPCB) CAAQM Network via data.gov.in | ",
          strong("Observation Time: "), "23-10-2026 18:00 IST | ",
          strong("Mode: "), span(class = "badge bg-warning text-dark", "CPCB Offline Baseline Snapshot"), " | ",
          "All 526 station coordinates validated within Indian subcontinental bounds. Missing readings preserved as NA."
        )
      ),
      
      # Interactive Geographic Filter Bar
      div(
        class = "card p-3 mb-3 shadow-sm border-0",
        style = "background-color: #FFFFFF;",
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("map_filter_state"),
              label = tags$span(icon("map-location", class = "me-1 text-primary"), "State / Union Territory:"),
              choices = c("All States", sort(unique(BASE_DATA$state))),
              selected = "All States",
              width = "100%"
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("map_filter_city"),
              label = tags$span(icon("city", class = "me-1 text-primary"), "City:"),
              choices = c("All Cities", sort(unique(BASE_DATA$city))),
              selected = "All Cities",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              inputId = ns("map_filter_pollutant"),
              label = tags$span(icon("smog", class = "me-1 text-primary"), "Pollutant Layer:"),
              choices = c(
                "PM2.5" = "PM2.5",
                "PM10"  = "PM10",
                "NO2"   = "NO2",
                "SO2"   = "SO2",
                "CO"    = "CO",
                "OZONE" = "OZONE",
                "NH3"   = "NH3"
              ),
              selected = "PM2.5",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              inputId = ns("map_filter_category"),
              label = tags$span(icon("layer-group", class = "me-1 text-primary"), "Indicative Category:"),
              choices = c(
                "All Categories",
                "Good (0-50)" = "Good",
                "Satisfactory (51-100)" = "Satisfactory",
                "Moderate (101-200)" = "Moderate",
                "Poor (201-300)" = "Poor",
                "Very Poor (301-400)" = "Very Poor",
                "Severe (401-500)" = "Severe",
                "Ineligible / Missing" = "Ineligible / Missing"
              ),
              selected = "All Categories",
              width = "100%"
            )
          ),
          column(
            width = 2,
            div(
              style = "margin-top: 28px;",
              actionButton(
                inputId = ns("btn_map_reset"),
                label = " Reset Filters",
                icon = icon("rotate-left"),
                class = "btn btn-outline-secondary w-100",
                title = "Reset geographic filters to National Overview"
              )
            )
          )
        )
      ),
      
      # Visualization Options Bar
      div(
        class = "d-flex justify-content-between align-items-center mb-2 px-1",
        div(
          class = "d-flex align-items-center gap-3",
          span(class = "fw-bold small text-secondary", icon("palette", class = "me-1"), "Marker Color Scheme:"),
          radioButtons(
            inputId = ns("map_color_mode"),
            label = NULL,
            choices = c(
              "Pollutant Concentration Gradient" = "conc",
              "CPCB Indicative Category (Official Breakpoints)" = "cpcb"
            ),
            selected = "conc",
            inline = TRUE
          )
        ),
        div(
          span(class = "badge bg-light text-secondary border", textOutput(ns("active_filter_summary"), inline = TRUE))
        )
      ),
      
      # Main Spatial Section: Large Interactive Leaflet Map + Station Details Panel
      fluidRow(
        # Leaflet Map (Left Column - Width 7)
        column(
          width = 7,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title text-primary m-0", icon("earth-asia", class = "me-2"), "Spatial Monitoring Network Map of India"),
              div(
                span(class = "badge bg-primary text-white me-1", textOutput(ns("valid_stations_badge"), inline = TRUE)),
                span(class = "badge bg-light text-secondary border", textOutput(ns("missing_stations_badge"), inline = TRUE))
              )
            ),
            p(class = "small text-muted mb-2", "Click any station marker or table row to inspect telemetry details. Drag or zoom to explore regional clusters."),
            leafletOutput(ns("hotspot_map"), height = "560px"),
            div(
              class = "d-flex justify-content-between align-items-center small text-muted mt-2",
              span("Scale bar: Bottom-Left | Tiles: Esri Light Gray Canvas, OSM, Esri Streets (No API Key Required)"),
              span(icon("circle-info", class = "me-1 text-primary"), "Grey circles indicate sensor offline or missing measurement for the selected pollutant")
            )
          )
        ),
        
        # Station Details & Ranked Comparison Panel (Right Column - Width 5)
        column(
          width = 5,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title text-primary m-0", icon("satellite-dish", class = "me-2"), "Station Telemetry & Profile"),
              span(class = "badge bg-light text-primary border", "Detailed Inspection")
            ),
            
            # Dynamic Selected Station Detail Card
            uiOutput(ns("station_detail_card")),
            
            hr(class = "my-2"),
            
            # Ranked Comparison Mini Chart
            div(
              class = "d-flex justify-content-between align-items-center mb-1",
              h6(class = "card-title text-secondary m-0 small fw-bold", icon("chart-bar", class = "me-1"), textOutput(ns("ranked_chart_title"), inline = TRUE)),
              span(class = "badge bg-light text-secondary border small", "Selected Area Top Ranked")
            ),
            plotlyOutput(ns("ranked_stations_chart"), height = "230px"),
            div(
              class = "small text-muted mt-1 fst-italic",
              "Clicking any bar or table row focuses the map on that station."
            )
          )
        )
      ),
      
      br(),
      
      # Searchable Full Stations Data Table
      fluidRow(
        column(
          width = 12,
          div(
            class = "card p-3 shadow-sm border-0",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title text-primary m-0", icon("table", class = "me-2"), "Filtered Monitoring Stations Directory"),
              span(class = "badge bg-light text-secondary border", textOutput(ns("table_station_count_badge"), inline = TRUE))
            ),
            p(class = "small text-muted mb-2", "Search, sort, or select a station row to view full telemetry. Missing values are preserved as genuine NAs."),
            DTOutput(ns("stations_directory_table"))
          )
        )
      )
    )
  )
}

# Server Module for Geographic Hotspot Explorer
mod_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Defensive Filter Accessors
    curr_state <- reactive({
      if (is.null(input$map_filter_state) || !nzchar(input$map_filter_state)) "All States" else input$map_filter_state
    })
    
    curr_city <- reactive({
      if (is.null(input$map_filter_city) || !nzchar(input$map_filter_city)) "All Cities" else input$map_filter_city
    })
    
    curr_pollutant <- reactive({
      if (is.null(input$map_filter_pollutant) || !nzchar(input$map_filter_pollutant)) "PM2.5" else input$map_filter_pollutant
    })
    
    curr_category <- reactive({
      if (is.null(input$map_filter_category) || !nzchar(input$map_filter_category)) "All Categories" else input$map_filter_category
    })
    
    curr_color_mode <- reactive({
      if (is.null(input$map_color_mode) || !nzchar(input$map_color_mode)) "conc" else input$map_color_mode
    })
    
    # Selected Station State (ReactiveVal initialized to highest station or first station)
    selected_station_name <- reactiveVal(NULL)
    
    # 2. Cascading City Filter Update
    observeEvent(input$map_filter_state, {
      st <- curr_state()
      ct <- curr_city()
      
      if (st == "All States") {
        cities <- c("All Cities", sort(unique(BASE_DATA$city)))
      } else {
        cities <- c("All Cities", sort(unique(BASE_DATA$city[BASE_DATA$state == st])))
      }
      
      new_sel <- if (ct %in% cities) ct else "All Cities"
      updateSelectInput(session, "map_filter_city", choices = cities, selected = new_sel)
    }, ignoreInit = TRUE)
    
    # Reset Filters Button Handler
    observeEvent(input$btn_map_reset, {
      updateSelectInput(session, "map_filter_state", selected = "All States")
      updateSelectInput(session, "map_filter_city", choices = c("All Cities", sort(unique(BASE_DATA$city))), selected = "All Cities")
      updateSelectInput(session, "map_filter_pollutant", selected = "PM2.5")
      updateSelectInput(session, "map_filter_category", selected = "All Categories")
      updateRadioButtons(session, "map_color_mode", selected = "conc")
    })
    
    # 3. Filtered Wide Dataset (for Map and Directory)
    filtered_map_data <- reactive({
      df <- WIDE_WITH_SUBINDICES
      st <- curr_state()
      ct <- curr_city()
      cat_sel <- curr_category()
      
      if (st != "All States") {
        df <- df %>% filter(state == st)
      }
      if (ct != "All Cities") {
        df <- df %>% filter(city == ct)
      }
      if (cat_sel != "All Categories") {
        if (cat_sel == "Ineligible / Missing") {
          df <- df %>% filter(!cpcb_eligible | is.na(indicative_category))
        } else {
          df <- df %>% filter(cpcb_eligible & indicative_category == cat_sel)
        }
      }
      df
    })
    
    # Set default selected station when filtered data changes
    observe({
      df <- filtered_map_data()
      pol <- curr_pollutant()
      if (nrow(df) > 0) {
        current_sel <- selected_station_name()
        if (is.null(current_sel) || !(current_sel %in% df$station)) {
          # Default to station with highest valid reading of selected pollutant, or first row
          valid_df <- df %>% filter(!is.na(.data[[pol]])) %>% arrange(desc(.data[[pol]]))
          if (nrow(valid_df) > 0) {
            selected_station_name(valid_df$station[1])
          } else {
            selected_station_name(df$station[1])
          }
        }
      } else {
        selected_station_name(NULL)
      }
    })
    
    # 4. Badges & Summary Outputs
    output$active_filter_summary <- renderText({
      st <- curr_state()
      ct <- curr_city()
      pol <- curr_pollutant()
      cat_sel <- curr_category()
      paste0("Viewing: ", st, " > ", ct, " | Layer: ", pol, " | Category: ", cat_sel)
    })
    
    output$valid_stations_badge <- renderText({
      df <- filtered_map_data()
      pol <- curr_pollutant()
      valid_n <- sum(!is.na(df[[pol]]))
      paste0(valid_n, " Reporting")
    })
    
    output$missing_stations_badge <- renderText({
      df <- filtered_map_data()
      pol <- curr_pollutant()
      miss_n <- sum(is.na(df[[pol]]))
      paste0(miss_n, " Offline/NA")
    })
    
    output$ranked_chart_title <- renderText({
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      paste0("Ranked Stations (", pol, " in ", unit, ")")
    })
    
    output$table_station_count_badge <- renderText({
      paste0(nrow(filtered_map_data()), " Stations Listed")
    })
    
    # 5. Base Leaflet Map Initialization
    output$hotspot_map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 3, maxZoom = 15)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light Canvas (Esri)") %>%
        addTiles(group = "OpenStreetMap (Standard)") %>%
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map (Esri)") %>%
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
        setView(lng = 78.9629, lat = 22.5937, zoom = 4) %>%
        addLayersControl(
          baseGroups = c("Light Canvas (Esri)", "OpenStreetMap (Standard)", "Street Map (Esri)"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
    
    # Reactive Leaflet Marker Updates
    observe({
      df <- filtered_map_data()
      pol <- curr_pollutant()
      mode <- curr_color_mode()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      sel_stn <- selected_station_name()
      
      proxy <- leafletProxy("hotspot_map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls()
      
      if (nrow(df) == 0) {
        return()
      }
      
      pol_vals <- df[[pol]]
      valid_vals <- na.omit(pol_vals)
      
      # Accessible CPCB discrete category colors
      cpcb_colors <- c(
        "Good"                 = "#00B050",
        "Satisfactory"         = "#92D050",
        "Moderate"             = "#D4AC0D",
        "Poor"                 = "#E67E22",
        "Very Poor"            = "#C0392B",
        "Severe"               = "#7D3C98",
        "Ineligible / Offline" = "#94A3B8"
      )
      
      # Determine marker fill color
      if (mode == "conc") {
        # Continuous viridis/plasma palette for numeric concentrations
        if (length(valid_vals) > 0) {
          min_v <- min(valid_vals)
          max_v <- max(valid_vals)
          if (min_v == max_v) max_v <- min_v + 1
          pal_num <- colorNumeric(
            palette = "plasma",
            domain = c(min_v, max_v),
            na.color = "#94A3B8"
          )
          marker_colors <- ifelse(is.na(pol_vals), "#94A3B8", pal_num(pol_vals))
          
          # Add continuous numeric legend
          proxy %>% addLegend(
            position = "bottomright",
            pal = pal_num,
            values = c(min_v, max_v),
            title = paste0(pol, " (", unit, ")"),
            opacity = 0.9,
            labFormat = labelFormat(suffix = paste0(" ", unit))
          )
        } else {
          marker_colors <- rep("#94A3B8", nrow(df))
        }
      } else {
        # Official CPCB categories mode
        marker_colors <- sapply(seq_len(nrow(df)), function(i) {
          cat_val <- df$indicative_category[i]
          if (is.na(pol_vals[i]) || !df$cpcb_eligible[i] || is.na(cat_val)) {
            return("#94A3B8")
          }
          if (cat_val %in% names(cpcb_colors)) cpcb_colors[[cat_val]] else "#94A3B8"
        })
        
        proxy %>% addLegend(
          position = "bottomright",
          colors = unname(cpcb_colors),
          labels = names(cpcb_colors),
          title = "Indicative Category",
          opacity = 0.9
        )
      }
      
      # Construct informative HTML popups
      popups <- sapply(seq_len(nrow(df)), function(i) {
        st_name <- df$station[i]
        cty <- df$city[i]
        stt <- df$state[i]
        lat_v <- round(df$latitude[i], 4)
        lon_v <- round(df$longitude[i], 4)
        v <- pol_vals[i]
        ind_aqi <- df$indicative_aqi[i]
        ind_cat <- df$indicative_category[i]
        dom_pol <- df$dominant_pollutant[i]
        
        val_str <- if (is.na(v)) {
          "<span style='color: #DC2626; font-weight: 600;'>Sensor Offline / NA</span>"
        } else {
          paste0("<strong>", v, "</strong> ", unit)
        }
        
        aqi_str <- if (!df$cpcb_eligible[i] || is.na(ind_aqi)) {
          "<span style='color: #64748B;'>Ineligible (&lt;3 pollutants or missing particulate)</span>"
        } else {
          paste0("<strong>", ind_aqi, "</strong> (", ind_cat, ", Dom: ", dom_pol, ")")
        }
        
        paste0(
          "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.45; min-width: 210px;'>",
          "<div style='font-weight: 700; color: #1E3A8A; font-size: 14px;'>", st_name, "</div>",
          "<div style='color: #475569;'>", cty, ", ", stt, "</div>",
          "<div style='font-size: 11px; color: #64748B; margin-bottom: 4px;'>Coords: ", lat_v, "°N, ", lon_v, "°E</div>",
          "<hr style='margin: 4px 0;'/>",
          "<div><strong>", pol, " Reading: </strong>", val_str, "</div>",
          "<div><strong>Indicative Sub-Index: </strong>", aqi_str, "</div>",
          "<div style='font-size: 10.5px; color: #94A3B8; margin-top: 4px;'>Timestamp: 23-10-2026 18:00 IST</div>",
          "</div>"
        )
      })
      
      # Add regular circle markers for all stations
      proxy %>% addCircleMarkers(
        data = df,
        layerId = ~station,
        lng = ~longitude,
        lat = ~latitude,
        radius = 7,
        fillColor = marker_colors,
        fillOpacity = 0.85,
        color = "#FFFFFF",
        weight = 1.3,
        popup = popups,
        label = ~paste0(station, " (", city, ")")
      )
      
      # Highlight currently selected station with a distinct pulsing ring
      if (!is.null(sel_stn) && sel_stn %in% df$station) {
        stn_row <- df %>% filter(station == sel_stn) %>% head(1)
        if (nrow(stn_row) > 0) {
          proxy %>% addCircleMarkers(
            lng = stn_row$longitude,
            lat = stn_row$latitude,
            radius = 13,
            fillColor = "transparent",
            color = "#1E3A8A",
            weight = 3,
            dashArray = "4, 4",
            layerId = "highlight_ring"
          )
        }
      }
      
      # Auto-fit bounds when filtering
      st <- curr_state()
      ct <- curr_city()
      if (st != "All States" || ct != "All Cities") {
        valid_coords <- df %>% filter(!is.na(latitude) & !is.na(longitude))
        if (nrow(valid_coords) > 0) {
          proxy %>% fitBounds(
            lng1 = min(valid_coords$longitude) - 0.25,
            lat1 = min(valid_coords$latitude) - 0.25,
            lng2 = max(valid_coords$longitude) + 0.25,
            lat2 = max(valid_coords$latitude) + 0.25
          )
        }
      }
    })
    
    # 6. Marker Click Event: Update Selected Station
    observeEvent(input$hotspot_map_marker_click, {
      click <- input$hotspot_map_marker_click
      if (!is.null(click$id) && click$id != "highlight_ring") {
        selected_station_name(click$id)
      }
    })
    
    # 7. Station Details Card Rendering
    output$station_detail_card <- renderUI({
      stn_name <- selected_station_name()
      df <- filtered_map_data()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      if (is.null(stn_name) || !(stn_name %in% df$station)) {
        return(div(
          class = "text-center py-4 text-muted",
          icon("hand-pointer", class = "fs-3 mb-2"),
          p("Click any station on the map or table to inspect telemetry details.")
        ))
      }
      
      # Pull wide station record
      stn_wide <- df %>% filter(station == stn_name) %>% head(1)
      # Pull all 7 pollutants from baseline raw records for this station
      stn_long <- BASE_DATA %>% filter(station == stn_name)
      
      pol_record <- stn_long %>% filter(pollutant_id == pol) %>% head(1)
      has_pol <- (nrow(pol_record) > 0 && !is.na(pol_record$pollutant_avg))
      
      # CPCB category color badge
      cat_badge <- if (!stn_wide$cpcb_eligible || is.na(stn_wide$indicative_category)) {
        tags$span(class = "badge bg-secondary", "Ineligible per CPCB (<3 pollutants)")
      } else {
        cat_nm <- stn_wide$indicative_category
        bg_col <- case_when(
          cat_nm == "Good" ~ "#00B050",
          cat_nm == "Satisfactory" ~ "#92D050",
          cat_nm == "Moderate" ~ "#D4AC0D",
          cat_nm == "Poor" ~ "#E67E22",
          cat_nm == "Very Poor" ~ "#C0392B",
          TRUE ~ "#7D3C98"
        )
        tags$span(
          class = "badge text-white",
          style = paste0("background-color:", bg_col, ";"),
          paste(cat_nm, "(Index:", stn_wide$indicative_aqi, ")")
        )
      }
      
      tags$div(
        class = "p-2",
        # Station Header
        div(
          class = "d-flex justify-content-between align-items-start mb-2",
          div(
            h6(class = "fw-bold text-primary m-0", stn_wide$station),
            p(class = "text-muted small m-0", paste0(stn_wide$city, ", ", stn_wide$state, " | Coords: ", round(stn_wide$latitude, 3), "°N, ", round(stn_wide$longitude, 3), "°E"))
          ),
          cat_badge
        ),
        
        # Selected Pollutant Metrics
        div(
          class = "p-2 mb-2 rounded border bg-light",
          div(class = "d-flex justify-content-between align-items-center",
              span(class = "fw-bold small", paste("Selected Layer:", pol)),
              span(class = "small text-muted", paste("Unit:", unit))
          ),
          if (has_pol) {
            div(
              class = "row text-center mt-1 g-1",
              div(class = "col-4", div(class = "small text-muted", "Minimum"), div(class = "fw-bold", pol_record$pollutant_min)),
              div(class = "col-4", div(class = "small text-primary", "Average"), div(class = "fw-bold fs-5 text-primary", pol_record$pollutant_avg)),
              div(class = "col-4", div(class = "small text-muted", "Maximum"), div(class = "fw-bold", pol_record$pollutant_max))
            )
          } else {
            div(class = "text-danger small mt-1", icon("triangle-exclamation", class = "me-1"), "Measurement Unavailable (Sensor Offline or Unreported)")
          }
        ),
        
        # All 7 Criteria Pollutants Mini-Profile at this Station
        div(
          class = "small",
          div(class = "fw-bold text-secondary mb-1", "Complete Station Telemetry (7 Criteria Pollutants):"),
          tags$table(
            class = "table table-sm table-bordered mb-1 text-center small",
            tags$thead(
              class = "table-light",
              tags$tr(
                tags$th("Pollutant"),
                tags$th("Min"),
                tags$th("Avg"),
                tags$th("Max"),
                tags$th("Status")
              )
            ),
            tags$tbody(
              lapply(c("PM2.5", "PM10", "NO2", "SO2", "CO", "OZONE", "NH3"), function(p) {
                row_p <- stn_long %>% filter(pollutant_id == p) %>% head(1)
                is_valid <- (nrow(row_p) > 0 && !is.na(row_p$pollutant_avg))
                tags$tr(
                  tags$td(class = "fw-bold text-start", p),
                  tags$td(if (is_valid) row_p$pollutant_min else "-"),
                  tags$td(class = if (p == pol) "fw-bold text-primary" else "", if (is_valid) row_p$pollutant_avg else "-"),
                  tags$td(if (is_valid) row_p$pollutant_max else "-"),
                  tags$td(
                    if (is_valid) {
                      tags$span(class = "badge bg-success-subtle text-success", "Valid")
                    } else {
                      tags$span(class = "badge bg-secondary-subtle text-secondary", "NA")
                    }
                  )
                )
              })
            )
          ),
          div(class = "text-end text-muted", style = "font-size: 10px;", "Source Observation: 23-10-2026 18:00 IST | Offline Baseline")
        )
      )
    })
    
    # 8. Ranked Station Comparison Mini Chart
    output$ranked_stations_chart <- renderPlotly({
      df <- filtered_map_data()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      # Filter stations with valid readings for this pollutant
      rank_df <- df %>%
        filter(!is.na(.data[[pol]])) %>%
        arrange(desc(.data[[pol]])) %>%
        head(10)
      
      if (nrow(rank_df) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("No valid measurements for", pol, "in selected area."), color = "#64748B", size = 3.5) +
          theme_void()
        return(ggplotly(p))
      }
      
      rank_df <- rank_df %>%
        mutate(
          label = paste0(str_trunc(station, 20), " (", city, ")"),
          label = factor(label, levels = rev(label))
        )
      
      p <- ggplot(rank_df, aes(x = .data[[pol]], y = label, text = paste0(
        "Station: ", station, "\nCity: ", city, ", ", state, "\n",
        pol, ": ", .data[[pol]], " ", unit
      ))) +
        geom_col(aes(fill = .data[[pol]]), show.legend = FALSE, width = 0.65) +
        scale_fill_viridis_c(option = "plasma", direction = -1) +
        labs(x = paste0("Reported Concentration (", unit, ")"), y = NULL) +
        theme_minimal(base_size = 9) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 8.5, color = "#1E293B"),
          axis.title.x = element_text(size = 9, color = "#475569")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          margin = list(l = 5, r = 5, t = 5, b = 20),
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # 9. Searchable DT Stations Directory Table
    output$stations_directory_table <- renderDT({
      df <- filtered_map_data()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      table_df <- df %>%
        select(
          Station = station,
          City = city,
          State = state,
          Latitude = latitude,
          Longitude = longitude,
          Reading = all_of(pol),
          `Indicative Sub-Index` = indicative_aqi,
          `Dominant Pollutant` = dominant_pollutant,
          Category = indicative_category
        ) %>%
        mutate(
          Latitude = round(Latitude, 4),
          Longitude = round(Longitude, 4),
          Reading = ifelse(is.na(Reading), NA, round(Reading, 1)),
          Category = ifelse(is.na(Category), "Ineligible / NA", Category)
        )
      
      # Column name with unit for reading
      names(table_df)[names(table_df) == "Reading"] <- paste0(pol, " (", unit, ")")
      
      datatable(
        table_df,
        selection = "single",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          ordering = TRUE,
          scrollX = TRUE,
          dom = "lfrtip"
        ),
        rownames = FALSE,
        class = "table table-sm table-striped table-hover align-middle small"
      ) %>%
        formatStyle(
          "Category",
          backgroundColor = styleEqual(
            c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe", "Ineligible / NA"),
            c("#D1FAE5", "#ECFDF5", "#FEF9C3", "#FFEDD5", "#FEE2E2", "#F3E8FF", "#F1F5F9")
          ),
          color = styleEqual(
            c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe", "Ineligible / NA"),
            c("#065F46", "#047857", "#854D0E", "#9A3412", "#991B1B", "#6B21A8", "#475569")
          ),
          fontWeight = "600"
        )
    })
    
    # Table Row Selection Event: Focus on clicked station
    observeEvent(input$stations_directory_table_rows_selected, {
      selected_idx <- input$stations_directory_table_rows_selected
      df <- filtered_map_data()
      if (length(selected_idx) > 0 && selected_idx <= nrow(df)) {
        stn <- df$station[selected_idx]
        selected_station_name(stn)
        
        # Center map on selected station
        stn_row <- df[selected_idx, ]
        if (!is.na(stn_row$latitude) && !is.na(stn_row$longitude)) {
          leafletProxy("hotspot_map") %>%
            setView(lng = stn_row$longitude, lat = stn_row$latitude, zoom = 11)
        }
      }
    })
    
  })
}
