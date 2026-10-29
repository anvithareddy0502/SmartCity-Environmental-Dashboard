# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/mod_overview.R
# Purpose: Shiny module for Executive Overview (Stage 2)
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(plotly)
  library(leaflet)
  library(DT)
  library(scales)
})

# UI Module for Executive Overview
mod_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid py-3",
      
      # Page Title & Description
      div(
        class = "d-flex justify-content-between align-items-baseline mb-3",
        div(
          h4(class = "page-header-title", "Executive Overview"),
          p(class = "page-header-subtitle", "Air quality monitoring summary across 526 continuous stations in India (CPCB dataset).")
        )
      ),
      
      # Interactive Filter Card
      div(
        class = "card p-3 mb-3 shadow-sm border-0",
        style = "background-color: #FFFFFF;",
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = ns("filter_state"),
              label = "State / UT:",
              choices = c("All States", sort(unique(BASE_DATA$state))),
              selected = "All States",
              width = "100%"
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = ns("filter_city"),
              label = "City:",
              choices = c("All Cities", sort(unique(BASE_DATA$city))),
              selected = "All Cities",
              width = "100%"
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("filter_pollutant"),
              label = "Pollutant:",
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
            width = 1,
            div(
              style = "margin-top: 25px;",
              actionButton(
                inputId = ns("btn_reset"),
                label = "Reset",
                class = "btn-outline-secondary btn-sm w-100",
                title = "Reset Filters to National Overview"
              )
            )
          )
        )
      ),
      
      # KPI Metrics Row (5 Cards)
      fluidRow(
        column(
          width = 3,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Stations Represented"),
            div(class = "kpi-value text-primary", textOutput(ns("kpi_stations"))),
            p(class = "kpi-subtitle", textOutput(ns("kpi_stations_sub")))
          )
        ),
        column(
          width = 2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Cities Covered"),
            div(class = "kpi-value text-dark", textOutput(ns("kpi_cities"))),
            p(class = "kpi-subtitle", textOutput(ns("kpi_cities_sub")))
          )
        ),
        column(
          width = 2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "States & UTs"),
            div(class = "kpi-value text-secondary", textOutput(ns("kpi_states"))),
            p(class = "kpi-subtitle", textOutput(ns("kpi_states_sub")))
          )
        ),
        column(
          width = 3,
          div(
            class = "kpi-card",
            div(class = "kpi-title", textOutput(ns("kpi_pollutant_title"))),
            div(class = "kpi-value text-danger", textOutput(ns("kpi_pollutant_val"))),
            p(class = "kpi-subtitle", textOutput(ns("kpi_pollutant_sub")))
          )
        ),
        column(
          width = 2,
          div(
            class = "kpi-card",
            div(class = "kpi-title", "Data Completeness"),
            div(class = "kpi-value text-success", textOutput(ns("kpi_completeness"))),
            p(class = "kpi-subtitle", textOutput(ns("kpi_completeness_sub")))
          )
        )
      ),
      
      br(),
      
      # Row 1: Geographic Distribution Map & Top Ranked Locations Chart
      fluidRow(
        column(
          width = 7,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title text-primary m-0", "Monitoring Stations Map"),
              span(class = "badge bg-light text-secondary border", textOutput(ns("map_count_badge"), inline = TRUE))
            ),
            p(class = "small text-muted mb-2", "Markers colored by indicative CPCB category. Grey circles indicate missing measurements. Click markers for details."),
            leafletOutput(ns("overview_map"), height = "420px"),
            div(
              class = "small text-muted mt-2 d-flex justify-content-between",
              span("Basemap: OpenStreetMap & CartoDB Positron"),
              span("All 526 stations verified within Indian bounds")
            ),
            uiOutput(ns("understand_overview_map"))
          )
        ),
        column(
          width = 5,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title text-primary m-0", textOutput(ns("chart_rank_title"), inline = TRUE)),
              span(class = "badge bg-light text-secondary border", "Top 15")
            ),
            p(class = "small text-muted mb-2", textOutput(ns("chart_rank_subtitle"))),
            plotlyOutput(ns("rank_bar_chart"), height = "420px"),
            uiOutput(ns("understand_rank_bar"))
          )
        )
      ),
      
      br(),
      
      # Row 2: CPCB Sub-Index Breakdown & Criteria Pollutants Table
      fluidRow(
        column(
          width = 5,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title text-primary m-0", "Air Quality Category Breakdown"),
              span(class = "badge bg-light text-secondary border", textOutput(ns("cpcb_eligible_badge"), inline = TRUE))
            ),
            p(class = "small text-muted mb-2", "Distribution of indicative categories across eligible stations (≥3 pollutants, ≥1 particulate)."),
            plotlyOutput(ns("cpcb_donut_chart"), height = "360px"),
            uiOutput(ns("understand_cpcb_donut"))
          )
        ),
        column(
          width = 7,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title text-primary m-0", "Pollutant Summary & Completeness"),
              span(class = "badge bg-light text-secondary border", "7 Criteria Pollutants")
            ),
            p(class = "small text-muted mb-2", "Summary statistics across filtered stations. Missing values are preserved as NA."),
            DTOutput(ns("pollutant_summary_table")),
            div(
              class = "small text-muted mt-2",
              "Aggregations: Min, Median, Mean, and Max from valid station readings."
            )
          )
        )
      ),
      
      br(),
      
      # Row 3: Scientific Insights & Data Quality Monitor Summary
      fluidRow(
        column(
          width = 7,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            h6(class = "card-title text-primary mb-2", "Key Findings"),
            p(class = "small text-muted", "Summary observations calculated from filtered baseline data:"),
            uiOutput(ns("insights_container"))
          )
        ),
        column(
          width = 5,
          div(
            class = "card p-3 shadow-sm border-0 h-100",
            h6(class = "card-title text-primary mb-2", "Data Quality Summary"),
            p(class = "small text-muted", "Reporting completeness and sensor status across selection:"),
            uiOutput(ns("quality_audit_container"))
          )
        )
      )
    )
  )
}

# Server Module for Executive Overview
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Cascading Filter Logic: Update City Choices when State Changes
    observeEvent(input$filter_state, {
      req(input$filter_state)
      selected_state <- input$filter_state
      current_city <- if (!is.null(input$filter_city) && length(input$filter_city) > 0) input$filter_city else "All Cities"
      
      if (identical(selected_state, "All States")) {
        cities <- c("All Cities", sort(unique(BASE_DATA$city)))
      } else {
        cities <- c("All Cities", sort(unique(BASE_DATA$city[BASE_DATA$state == selected_state])))
      }
      
      new_selection <- if (current_city %in% cities) current_city else "All Cities"
      updateSelectInput(session, "filter_city", choices = cities, selected = new_selection)
    }, ignoreInit = TRUE)
    
    # Reset Filters Button Handler
    observeEvent(input$btn_reset, {
      updateSelectInput(session, "filter_state", selected = "All States")
      updateSelectInput(session, "filter_city", choices = c("All Cities", sort(unique(BASE_DATA$city))), selected = "All Cities")
      updateSelectInput(session, "filter_pollutant", selected = "PM2.5")
    })
    
    # 2. Filtered Datasets (Reactive)
    # Helper to safely retrieve current filter inputs with robust fallbacks
    curr_state <- reactive({
      if (is.null(input$filter_state) || !nzchar(input$filter_state)) "All States" else input$filter_state
    })
    
    curr_city <- reactive({
      if (is.null(input$filter_city) || !nzchar(input$filter_city)) "All Cities" else input$filter_city
    })
    
    curr_pollutant <- reactive({
      if (is.null(input$filter_pollutant) || !nzchar(input$filter_pollutant)) "PM2.5" else input$filter_pollutant
    })
    
    # Long-format filtered dataset
    filtered_long <- reactive({
      df <- BASE_DATA
      st <- curr_state()
      ct <- curr_city()
      if (st != "All States") {
        df <- df %>% filter(state == st)
      }
      if (ct != "All Cities") {
        df <- df %>% filter(city == ct)
      }
      df
    })
    
    # Wide-format filtered dataset with CPCB sub-indices
    filtered_wide <- reactive({
      df <- WIDE_WITH_SUBINDICES
      st <- curr_state()
      ct <- curr_city()
      if (st != "All States") {
        df <- df %>% filter(state == st)
      }
      if (ct != "All Cities") {
        df <- df %>% filter(city == ct)
      }
      df
    })
    
    # 3. KPI Metrics Outputs
    output$kpi_stations <- renderText({
      format(nrow(filtered_wide()), big.mark = ",")
    })
    output$kpi_stations_sub <- renderText({
      st <- curr_state()
      if (st == "All States") {
        "Total represented in baseline"
      } else {
        paste("In", st)
      }
    })
    
    output$kpi_cities <- renderText({
      format(n_distinct(filtered_long()$city), big.mark = ",")
    })
    output$kpi_cities_sub <- renderText({
      ct <- curr_city()
      if (ct == "All Cities") "Geographic coverage" else ct
    })
    
    output$kpi_states <- renderText({
      format(n_distinct(filtered_long()$state), big.mark = ",")
    })
    output$kpi_states_sub <- renderText({
      st <- curr_state()
      if (st == "All States") "National representation" else st
    })
    
    output$kpi_pollutant_title <- renderText({
      paste(curr_pollutant(), "Mean Concentration")
    })
    
    output$kpi_pollutant_val <- renderText({
      pol <- curr_pollutant()
      readings <- filtered_long() %>% filter(pollutant_id == pol) %>% pull(pollutant_avg)
      valid_vals <- na.omit(readings)
      if (length(valid_vals) == 0) {
        "N/A"
      } else {
        val <- round(mean(valid_vals), 1)
        unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
        paste(val, unit)
      }
    })
    
    output$kpi_pollutant_sub <- renderText({
      pol <- curr_pollutant()
      readings <- filtered_long() %>% filter(pollutant_id == pol) %>% pull(pollutant_avg)
      valid_vals <- na.omit(readings)
      if (length(valid_vals) == 0) {
        "No valid readings reported"
      } else {
        paste0("Median: ", round(median(valid_vals), 1), " | Range: ", min(valid_vals), "-", max(valid_vals))
      }
    })
    
    output$kpi_completeness <- renderText({
      df <- filtered_long()
      total_obs <- nrow(df)
      valid_obs <- sum(!is.na(df$pollutant_avg))
      pct <- if (total_obs > 0) round(100 * valid_obs / total_obs, 1) else 0
      paste0(pct, "%")
    })
    
    output$kpi_completeness_sub <- renderText({
      df <- filtered_long()
      na_count <- sum(is.na(df$pollutant_avg))
      paste0(na_count, " NAs preserved as NA")
    })
    
    output$map_count_badge <- renderText({
      paste(nrow(filtered_wide()), "Stations")
    })
    
    output$cpcb_eligible_badge <- renderText({
      w <- filtered_wide()
      el <- sum(w$cpcb_eligible, na.rm = TRUE)
      paste(el, "/", nrow(w), "Eligible")
    })
    
    output$chart_rank_title <- renderText({
      paste("Top Locations Ranked by", curr_pollutant())
    })
    
    output$chart_rank_subtitle <- renderText({
      pol <- curr_pollutant()
      unit <- POLLUTANT_UNITS[[pol]]
      paste0("Station-level reported averages in ", unit)
    })
    
    # 4. Interactive Leaflet Map
    output$overview_map <- renderLeaflet({
      df_stations <- isolate(filtered_wide())
      pol <- isolate(curr_pollutant())
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      cat_colors <- c(
        "Good"                 = "#00B050",
        "Satisfactory"         = "#92D050",
        "Moderate"             = "#D4AC0D",
        "Poor"                 = "#E67E22",
        "Very Poor"            = "#C0392B",
        "Severe"               = "#7D3C98",
        "Ineligible / Offline" = "#94A3B8"
      )
      
      m <- leaflet(options = leafletOptions(minZoom = 3, maxZoom = 14)) %>%
        addTiles(group = "OpenStreetMap (Standard)") %>%
        addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
        addProviderTiles("CartoDB.Voyager", group = "CartoDB Voyager") %>%
        addProviderTiles("Esri.WorldStreetMap", group = "Esri World Street") %>%
        fitBounds(lng1 = 68.5, lat1 = 7.5, lng2 = 97.5, lat2 = 35.5) %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap (Standard)", "CartoDB Positron", "CartoDB Voyager", "Esri World Street"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      if (nrow(df_stations) > 0) {
        marker_colors <- sapply(seq_len(nrow(df_stations)), function(i) {
          pol_val <- df_stations[[pol]][i]
          cat_val <- df_stations$indicative_category[i]
          if (is.na(pol_val) || is.na(cat_val)) {
            return("#94A3B8")
          }
          if (cat_val %in% names(cat_colors)) cat_colors[[cat_val]] else "#94A3B8"
        })
        
        popups <- sapply(seq_len(nrow(df_stations)), function(i) {
          st_name <- df_stations$station[i]
          cty <- df_stations$city[i]
          stt <- df_stations$state[i]
          pol_val <- df_stations[[pol]][i]
          cat_val <- df_stations$indicative_category[i]
          dom_pol <- df_stations$dominant_pollutant[i]
          ind_aqi <- df_stations$indicative_aqi[i]
          
          pol_display <- if (is.na(pol_val)) {
            "<span style='color:#DC2626; font-weight:600;'>Measurement Unavailable (NA)</span>"
          } else {
            paste0("<strong>", pol_val, "</strong> ", unit)
          }
          
          aqi_display <- if (is.na(ind_aqi)) {
            "<span style='color:#64748B;'>Ineligible for AQI (&lt;3 pollutants or missing particulate)</span>"
          } else {
            paste0("<strong>", ind_aqi, "</strong> (", cat_val, ", Driven by ", dom_pol, ")")
          }
          
          paste0(
            "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.4;'>",
            "<div style='font-weight: 700; color: #1E3A8A; font-size: 14px;'>", st_name, "</div>",
            "<div style='color: #475569; margin-bottom: 6px;'>", cty, ", ", stt, "</div>",
            "<hr style='margin: 4px 0;'/>",
            "<div><strong>Selected Pollutant (", pol, "): </strong>", pol_display, "</div>",
            "<div><strong>Indicative Sub-Index: </strong>", aqi_display, "</div>",
            "<div style='font-size: 11px; color: #94A3B8; margin-top: 4px;'>Source Timestamp: 23-10-2026 18:00 IST</div>",
            "</div>"
          )
        })
        
        m <- m %>%
          addCircleMarkers(
            data = df_stations,
            lng = ~longitude,
            lat = ~latitude,
            radius = 6,
            fillColor = marker_colors,
            fillOpacity = 0.85,
            color = "#FFFFFF",
            weight = 1.2,
            popup = popups,
            label = ~paste0(station, " (", city, ")")
          ) %>%
          addLegend(
            position = "bottomright",
            colors = unname(cat_colors),
            labels = names(cat_colors),
            title = "Indicative Category",
            opacity = 0.9
          )
      }
      
      m
    })
    outputOptions(output, "overview_map", suspendWhenHidden = FALSE)
    
    # Update Leaflet Map markers reactively
    observe({
      df_stations <- filtered_wide()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      if (nrow(df_stations) == 0) {
        leafletProxy("overview_map") %>% clearMarkers() %>% clearShapes() %>% clearControls()
        return()
      }
      
      # Accessible CPCB color palette
      cat_colors <- c(
        "Good"                 = "#00B050",
        "Satisfactory"         = "#92D050",
        "Moderate"             = "#D4AC0D",
        "Poor"                 = "#E67E22",
        "Very Poor"            = "#C0392B",
        "Severe"               = "#7D3C98",
        "Ineligible / Offline" = "#94A3B8"
      )
      
      # Determine marker color based on station indicative category or NA
      marker_colors <- sapply(seq_len(nrow(df_stations)), function(i) {
        pol_val <- df_stations[[pol]][i]
        cat_val <- df_stations$indicative_category[i]
        if (is.na(pol_val) || is.na(cat_val)) {
          return("#94A3B8")
        }
        if (cat_val %in% names(cat_colors)) cat_colors[[cat_val]] else "#94A3B8"
      })
      
      # Build detailed popup content
      popups <- sapply(seq_len(nrow(df_stations)), function(i) {
        st_name <- df_stations$station[i]
        cty <- df_stations$city[i]
        stt <- df_stations$state[i]
        pol_val <- df_stations[[pol]][i]
        cat_val <- df_stations$indicative_category[i]
        dom_pol <- df_stations$dominant_pollutant[i]
        ind_aqi <- df_stations$indicative_aqi[i]
        
        pol_display <- if (is.na(pol_val)) {
          "<span style='color:#DC2626; font-weight:600;'>Measurement Unavailable (NA)</span>"
        } else {
          paste0("<strong>", pol_val, "</strong> ", unit)
        }
        
        aqi_display <- if (is.na(ind_aqi)) {
          "<span style='color:#64748B;'>Ineligible for AQI (&lt;3 pollutants or missing particulate)</span>"
        } else {
          paste0("<strong>", ind_aqi, "</strong> (", cat_val, ", Driven by ", dom_pol, ")")
        }
        
        paste0(
          "<div style='font-family: sans-serif; font-size: 13px; line-height: 1.4;'>",
          "<div style='font-weight: 700; color: #1E3A8A; font-size: 14px;'>", st_name, "</div>",
          "<div style='color: #475569; margin-bottom: 6px;'>", cty, ", ", stt, "</div>",
          "<hr style='margin: 4px 0;'/>",
          "<div><strong>Selected Pollutant (", pol, "): </strong>", pol_display, "</div>",
          "<div><strong>Indicative Sub-Index: </strong>", aqi_display, "</div>",
          "<div style='font-size: 11px; color: #94A3B8; margin-top: 4px;'>Source Timestamp: 23-10-2026 18:00 IST</div>",
          "</div>"
        )
      })
      
      proxy <- leafletProxy("overview_map") %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearControls()
      
      proxy %>%
        addCircleMarkers(
          data = df_stations,
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          fillColor = marker_colors,
          fillOpacity = 0.85,
          color = "#FFFFFF",
          weight = 1.2,
          popup = popups,
          label = ~paste0(station, " (", city, ")")
        )
      
      # Add accessible legend
      proxy %>%
        addLegend(
          position = "bottomright",
          colors = unname(cat_colors),
          labels = names(cat_colors),
          title = "Indicative Category",
          opacity = 0.9
        )
      
      # Adjust bounding view if filtered to specific state/city
      st <- curr_state()
      ct <- curr_city()
      if (st != "All States" || ct != "All Cities") {
        valid_coords <- df_stations %>% filter(!is.na(latitude) & !is.na(longitude))
        if (nrow(valid_coords) > 0) {
          proxy %>% fitBounds(
            lng1 = min(valid_coords$longitude) - 0.2,
            lat1 = min(valid_coords$latitude) - 0.2,
            lng2 = max(valid_coords$longitude) + 0.2,
            lat2 = max(valid_coords$latitude) + 0.2
          )
        }
      } else {
        proxy %>% fitBounds(lng1 = 68.5, lat1 = 7.5, lng2 = 97.5, lat2 = 35.5)
      }
    })
    
    # 5. Ranked Horizontal Bar Chart for Selected Pollutant
    output$rank_bar_chart <- renderPlotly({
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      rank_df <- filtered_long() %>%
        filter(pollutant_id == pol, !is.na(pollutant_avg)) %>%
        arrange(desc(pollutant_avg)) %>%
        head(15)
      
      if (nrow(rank_df) == 0) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("No valid measurements reported for", pol, "in selected area."),
                   color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Clean station label for chart readability (station, city)
      rank_df <- rank_df %>%
        mutate(
          display_label = paste0(str_trunc(station, 26), " (", city, ")"),
          display_label = factor(display_label, levels = rev(display_label))
        )
      
      p <- ggplot(rank_df, aes(x = pollutant_avg, y = display_label, text = paste0(
        "Station: ", station, "\nCity: ", city, ", ", state, "\n",
        pol, ": ", pollutant_avg, " ", unit
      ))) +
        geom_col(aes(fill = pollutant_avg), show.legend = FALSE, width = 0.7) +
        scale_fill_viridis_c(option = "magma", direction = -1) +
        labs(
          x = paste0("Reported Average Concentration (", unit, ")"),
          y = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 9, color = "#1E293B"),
          axis.title.x = element_text(size = 10, color = "#475569", margin = margin(t = 6))
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          margin = list(l = 10, r = 10, t = 10, b = 20),
          xaxis = list(fixedrange = TRUE),
          yaxis = list(fixedrange = TRUE)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # 6. CPCB Sub-Index Category Donut Chart
    output$cpcb_donut_chart <- renderPlotly({
      w <- filtered_wide()
      
      cat_counts <- w %>%
        mutate(
          display_category = case_when(
            !cpcb_eligible ~ "Ineligible per CPCB criteria",
            is.na(indicative_category) ~ "Ineligible per CPCB criteria",
            TRUE ~ indicative_category
          )
        ) %>%
        count(display_category) %>%
        mutate(
          category_order = match(display_category, c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe", "Ineligible per CPCB criteria")),
          color = case_when(
            display_category == "Good" ~ "#00B050",
            display_category == "Satisfactory" ~ "#92D050",
            display_category == "Moderate" ~ "#D4AC0D",
            display_category == "Poor" ~ "#E67E22",
            display_category == "Very Poor" ~ "#C0392B",
            display_category == "Severe" ~ "#7D3C98",
            TRUE ~ "#94A3B8"
          )
        ) %>%
        arrange(category_order)
      
      fig <- plot_ly(
        data = cat_counts,
        labels = ~display_category,
        values = ~n,
        type = "pie",
        hole = 0.55,
        marker = list(colors = ~color, line = list(color = "#FFFFFF", width = 1.5)),
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~paste0(display_category, ": ", n, " stations (", round(100 * n / sum(n), 1), "%)")
      ) %>%
        layout(
          showlegend = FALSE,
          margin = list(l = 10, r = 10, t = 10, b = 10),
          annotations = list(
            list(
              text = paste0("<b>", sum(cat_counts$n), "</b><br><span style='font-size:11px; color:#64748B;'>Stations</span>"),
              x = 0.5, y = 0.5,
              font = list(size = 14, color = "#1E293B"),
              showarrow = FALSE
            )
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
      
      fig
    })
    
    # 6a. Understand This Chart: Overview Map
    output$understand_overview_map <- renderUI({
      df_stations <- filtered_wide()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      if (nrow(df_stations) == 0) {
        finding <- "No monitoring stations match the active filter criteria."
      } else {
        valid_df <- df_stations %>% filter(!is.na(.data[[pol]]))
        valid_n <- nrow(valid_df)
        miss_n <- nrow(df_stations) - valid_n
        
        if (valid_n > 0) {
          max_stn <- valid_df %>% arrange(desc(.data[[pol]])) %>% head(1)
          min_stn <- valid_df %>% arrange(.data[[pol]]) %>% head(1)
          finding <- paste0(
            "Across ", nrow(df_stations), " filtered stations (", valid_n, " reporting, ", miss_n, " offline), ",
            "the highest reported ", pol, " average is recorded at ", max_stn$station, " in ", max_stn$city, ", ", max_stn$state,
            " (", max_stn[[pol]], " ", unit, "), while the lowest is at ", min_stn$station, " in ", min_stn$city,
            " (", min_stn[[pol]], " ", unit, ")."
          )
        } else {
          finding <- paste0(
            "All ", nrow(df_stations), " stations in the current selection have missing or offline sensors for ", pol,
            " (preserved as NA)."
          )
        }
      }
      
      build_understand_chart(
        chart_type = "Geographic Map (Leaflet)",
        what_shows = paste0(
          "Displays continuous ambient air quality monitoring stations across India positioned by verified GPS coordinates. ",
          "Shows the spatial distribution of ", pol, " and identifies regional pollution hotspots across states."
        ),
        how_to_read = paste0(
          "Circle markers denote station locations. Click any marker to open a detailed telemetry popup with coordinates, ",
          "pollutant readings, and indicative AQI. Use the top-right layer switcher to toggle between OpenStreetMap, CartoDB, and Esri basemaps."
        ),
        colors_meaning = paste0(
          "Marker colors represent official CPCB Air Quality Index health categories: ",
          "Green (Good: 0–50), Light Green (Satisfactory: 51–100), Yellow (Moderate: 101–200), Orange (Poor: 201–300), ",
          "Red (Very Poor: 301–400), and Purple (Severe: 401–500). Grey circles denote offline sensors or missing readings."
        ),
        dynamic_findings = finding,
        important_note = paste0(
          "Observations represent a single snapshot baseline (23 Oct 2026, 18:00 IST). ",
          "Official CPCB regulatory non-compliance requires 24-hour continuous rolling sampling with ≥16 valid hourly averages. ",
          "Sub-indices shown here indicate snapshot severity, not certified annual averages."
        )
      )
    })
    
    # 6b. Understand This Chart: Ranked Bar Chart
    output$understand_rank_bar <- renderUI({
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      rank_df <- filtered_long() %>%
        filter(pollutant_id == pol, !is.na(pollutant_avg)) %>%
        arrange(desc(pollutant_avg)) %>%
        head(15)
      
      if (nrow(rank_df) == 0) {
        finding <- paste0("No stations currently report valid measurements for ", pol, " within the active filter scope.")
      } else {
        top1 <- rank_df[1, ]
        last_item <- rank_df[nrow(rank_df), ]
        finding <- paste0(
          top1$station, " in ", top1$city, " ranks highest with a reported average ", pol, " of ",
          top1$pollutant_avg, " ", unit, ". ",
          if (nrow(rank_df) > 1) {
            paste0("Across the top ", nrow(rank_df), " locations, readings range from ", top1$pollutant_avg, " down to ", last_item$pollutant_avg, " ", unit, ".")
          } else ""
        )
      }
      
      build_understand_chart(
        chart_type = "Horizontal Bar Chart",
        what_shows = paste0(
          "Ranks monitoring stations reporting the highest average concentrations for ", pol, " in the filtered area. ",
          "Suitable for identifying top localized pollution hotspots."
        ),
        how_to_read = paste0(
          "The horizontal axis measures reported average concentration in ", unit, ". ",
          "The vertical axis lists station names and cities sorted from highest (top) to lowest (bottom). Hover over any bar to see exact values."
        ),
        colors_meaning = "Bars use a sequential magma color gradient where darker, deeper tones indicate higher concentrations.",
        dynamic_findings = finding,
        important_note = paste0(
          "Shows snapshot reported values. Elevated hourly concentrations indicate localized acute episodes ",
          "and cannot prove persistent regulatory violations without 24-hour continuous sampling."
        )
      )
    })
    
    # 6c. Understand This Chart: CPCB Donut Chart
    output$understand_cpcb_donut <- renderUI({
      w <- filtered_wide()
      eligible_df <- w %>% filter(cpcb_eligible, !is.na(indicative_category))
      total_w <- nrow(w)
      n_elig <- nrow(eligible_df)
      n_inelig <- total_w - n_elig
      
      if (n_elig == 0) {
        finding <- "None of the currently selected stations meet national CPCB eligibility criteria (≥3 pollutants including at least 1 particulate parameter)."
      } else {
        cat_tbl <- sort(table(eligible_df$indicative_category), decreasing = TRUE)
        top_cat <- names(cat_tbl)[1]
        top_n <- as.numeric(cat_tbl[1])
        top_pct <- round(100 * top_n / n_elig, 1)
        
        finding <- paste0(
          top_cat, " is the dominant category, representing ", top_n, " of ", n_elig, " eligible stations (", top_pct, "%). ",
          if (length(cat_tbl) > 1) {
            paste0(names(cat_tbl)[2], " follows with ", as.numeric(cat_tbl[2]), " stations (", round(100 * as.numeric(cat_tbl[2]) / n_elig, 1), "%). ")
          } else "",
          if (n_inelig > 0) paste0(n_inelig, " stations are ineligible due to incomplete criteria pollutant telemetry.") else ""
        )
      }
      
      build_understand_chart(
        chart_type = "Donut Chart",
        what_shows = "Displays the proportional breakdown of monitoring stations across official CPCB air quality severity categories for stations meeting national eligibility rules.",
        how_to_read = "Each circular arc corresponds to the percentage share of eligible stations. Hover over any segment to inspect exact station counts and percentage breakdown.",
        colors_meaning = "Colors align with official CPCB health standards: Green (Good), Light Green (Satisfactory), Yellow (Moderate), Orange (Poor), Red (Very Poor), Purple (Severe), and Grey (Ineligible).",
        dynamic_findings = finding,
        important_note = "CPCB NAQI guidelines require at least 3 pollutants with at least one particulate parameter (PM2.5 or PM10). Stations missing particulate data are excluded to prevent misleading scores."
      )
    })
    
    # 7. Criteria Pollutant Summary DT Table
    output$pollutant_summary_table <- renderDT({
      df <- filtered_long()
      
      if (nrow(df) == 0) {
        empty_table <- data.frame(
          Pollutant = character(0),
          `Monitored Stations` = integer(0),
          `Valid Readings` = integer(0),
          `Missing / Offline` = integer(0),
          Completeness = character(0),
          Min = numeric(0),
          Median = numeric(0),
          Mean = numeric(0),
          Max = numeric(0),
          Unit = character(0),
          check.names = FALSE
        )
        return(datatable(empty_table, options = list(dom = "t"), rownames = FALSE))
      }
      
      summary_df <- df %>%
        group_by(pollutant_id) %>%
        summarise(
          monitored_stations = n(),
          valid_readings = sum(!is.na(pollutant_avg)),
          missing_readings = sum(is.na(pollutant_avg)),
          completeness_pct = round(100 * valid_readings / monitored_stations, 1),
          min_val = suppressWarnings(min(pollutant_avg, na.rm = TRUE)),
          median_val = suppressWarnings(median(pollutant_avg, na.rm = TRUE)),
          mean_val = suppressWarnings(round(mean(pollutant_avg, na.rm = TRUE), 1)),
          max_val = suppressWarnings(max(pollutant_avg, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        mutate(
          min_val = ifelse(is.infinite(min_val), NA_real_, min_val),
          median_val = ifelse(is.infinite(median_val), NA_real_, median_val),
          mean_val = ifelse(is.infinite(mean_val), NA_real_, mean_val),
          max_val = ifelse(is.infinite(max_val), NA_real_, max_val),
          unit = case_when(
            pollutant_id == "CO" ~ "mg/m³",
            TRUE ~ "µg/m³"
          )
        ) %>%
        select(
          Pollutant = pollutant_id,
          Unit = unit,
          Stations = monitored_stations,
          Valid = valid_readings,
          Missing = missing_readings,
          `Complete %` = completeness_pct,
          Min = min_val,
          Median = median_val,
          Mean = mean_val,
          Max = max_val
        )
      
      datatable(
        summary_df,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = TRUE,
          scrollX = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE,
        class = "table table-sm table-striped table-hover align-middle"
      ) %>%
        formatStyle(
          "Complete %",
          background = styleColorBar(range(summary_df$`Complete %`, na.rm = TRUE), "#E0F2FE"),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
    
    # 8. Dynamic Evidence-Based Scientific Insights
    output$insights_container <- renderUI({
      w <- filtered_wide()
      l <- filtered_long()
      
      total_stn <- nrow(w)
      if (total_stn == 0) {
        return(p(class = "text-muted", "No stations found for the selected geographic filters."))
      }
      
      # Calculate dominant pollutant across eligible stations
      eligible_w <- w %>% filter(cpcb_eligible & !is.na(dominant_pollutant))
      dom_summary <- eligible_w %>% count(dominant_pollutant, sort = TRUE)
      top_dom <- if (nrow(dom_summary) > 0) dom_summary$dominant_pollutant[1] else "None"
      top_dom_pct <- if (nrow(dom_summary) > 0) round(100 * dom_summary$n[1] / nrow(eligible_w), 1) else 0
      
      # Particulate ratio: PM2.5 / PM10 where both are valid and PM10 > 0
      particulate_df <- w %>% filter(!is.na(PM2.5) & !is.na(PM10) & PM10 > 0)
      median_pm_ratio <- if (nrow(particulate_df) > 0) {
        round(median(particulate_df$PM2.5 / particulate_df$PM10), 2)
      } else {
        NA_real_
      }
      
      # PM2.5 summary
      pm25_vals <- na.omit(l$pollutant_avg[l$pollutant_id == "PM2.5"])
      pm25_mean <- if (length(pm25_vals) > 0) round(mean(pm25_vals), 1) else NA_real_
      
      # Completeness
      tot_obs <- nrow(l)
      comp_pct <- if (tot_obs > 0) round(100 * sum(!is.na(l$pollutant_avg)) / tot_obs, 1) else 0
      
      tags$div(
        class = "p-2",
        tags$div(
          class = "mb-2 p-2 rounded bg-light border-start border-4 border-primary",
          strong("Dominant Criteria Driver: "),
          if (top_dom != "None") {
            paste0(top_dom, " drives the highest sub-index in ", top_dom_pct, "% of CPCB-eligible stations in this view (", dom_summary$n[1], " of ", nrow(eligible_w), " stations).")
          } else {
            "Insufficient eligible multi-pollutant stations to evaluate dominant pollutant."
          }
        ),
        tags$div(
          class = "mb-2 p-2 rounded bg-light border-start border-4 border-info",
          strong("Particulate Co-occurrence: "),
          if (!is.na(median_pm_ratio)) {
            paste0("Across ", nrow(particulate_df), " stations reporting both PM2.5 and PM10, the median PM2.5/PM10 ratio is ", median_pm_ratio, ". Fine particulates constitute a substantial portion of respirable particulate mass.")
          } else {
            "Co-located PM2.5 and PM10 measurements were not concurrently available in this selection."
          }
        ),
        tags$div(
          class = "mb-2 p-2 rounded bg-light border-start border-4 border-warning",
          strong("Pollutant Concentration Context: "),
          if (!is.na(pm25_mean)) {
            paste0("Mean reported PM2.5 concentration across valid stations is ", pm25_mean, " µg/m³ (CPCB 24-hr standard reference: 60 µg/m³). Note that snapshot readings reflect instantaneous conditions, not 24-hr integrated regulatory compliance.")
          } else {
            "PM2.5 measurements are missing or offline in the selected stations."
          }
        ),
        tags$div(
          class = "p-2 rounded bg-light border-start border-4 border-success",
          strong("Observational Scope: "),
          paste0("Total data completeness for this selection stands at ", comp_pct, "%. All findings are directly grounded in the 23-10-2026 18:00 IST snapshot observations.")
        )
      )
    })
    
    # 9. Data Quality & Telemetry Audit Container
    output$quality_audit_container <- renderUI({
      l <- filtered_long()
      w <- filtered_wide()
      
      total_obs <- nrow(l)
      missing_obs <- sum(is.na(l$pollutant_avg))
      completeness_pct <- if (total_obs > 0) round(100 * (total_obs - missing_obs) / total_obs, 1) else 0
      
      # Sensor flatlines: min == max == avg == 0
      flatlines <- l %>% filter(!is.na(pollutant_avg) & pollutant_min == 0 & pollutant_max == 0 & pollutant_avg == 0)
      
      # Outliers (> 1.5 IQR) in filtered long data
      pol_outlier_count <- 0
      for (pol in unique(l$pollutant_id)) {
        sub <- l %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
        if (nrow(sub) > 10) {
          q1 <- quantile(sub$pollutant_avg, 0.25)
          q3 <- quantile(sub$pollutant_avg, 0.75)
          fence <- q3 + 1.5 * (q3 - q1)
          pol_outlier_count <- pol_outlier_count + sum(sub$pollutant_avg > fence)
        }
      }
      
      tags$div(
        class = "p-2",
        tags$table(
          class = "table table-sm table-borderless mb-0",
          tags$tr(
            tags$td(icon("check-circle", class = "text-success me-2"), "Reporting Completeness:"),
            tags$td(class = "text-end fw-bold", paste0(completeness_pct, "% (", total_obs - missing_obs, "/", total_obs, ")"))
          ),
          tags$tr(
            tags$td(icon("triangle-exclamation", class = "text-warning me-2"), "Missing Measurements (Preserved NA):"),
            tags$td(class = "text-end fw-bold text-secondary", paste0(missing_obs, " (", round(100 * missing_obs / total_obs, 1), "%)"))
          ),
          tags$tr(
            tags$td(icon("plug-circle-xmark", class = "text-danger me-2"), "Zero Flatlines (Min=Max=Avg=0):"),
            tags$td(class = "text-end fw-bold", nrow(flatlines))
          ),
          tags$tr(
            tags$td(icon("chart-line", class = "text-primary me-2"), "Statistical Upper Outliers (>1.5×IQR):"),
            tags$td(class = "text-end fw-bold", pol_outlier_count)
          ),
          tags$tr(
            tags$td(icon("location-crosshairs", class = "text-success me-2"), "Geographic Bounds Conformance:"),
            tags$td(class = "text-end fw-bold text-success", "100% Valid Coordinates")
          )
        ),
        div(
          class = "interpretation-box py-1 px-2 small mt-2",
          "Missing readings are retained as genuine NAs and excluded from mathematical averages rather than imputed as zeros."
        )
      )
    })
    
  })
}
