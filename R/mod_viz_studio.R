# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/mod_viz_studio.R
# Purpose: Shiny module for Advanced Visualization Studio (Stage 4)
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(viridis)
  library(stringr)
  library(DT)
})

# UI Module for Advanced Visualization Studio
mod_viz_studio_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid py-3",
      
      # Top Provenance & Caveat Banner
      div(
        class = "alert alert-secondary d-flex align-items-center mb-3 py-2 px-3",
        role = "alert",
        icon("circle-info", class = "me-2 fs-5 text-primary"),
        div(
          class = "small",
          strong("Analytical Dataset: "), "Central Pollution Control Board (CPCB) Baseline Snapshot (23-10-2026 18:00 IST) | ",
          strong("Visualization Standard: "), "Cross-pollutant raw concentrations are never combined on shared numerical scales. ",
          "Distributions preserve genuine NAs without zero-substitution. No synthetic historical or seasonal trends are reported."
        )
      ),
      
      # Interactive Controls Card
      div(
        class = "card p-3 mb-3 shadow-sm border-0",
        style = "background-color: #FFFFFF;",
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("studio_filter_state"),
              label = tags$span(icon("map-location", class = "me-1 text-primary"), "Geographic Scope (State):"),
              choices = c("All States", sort(unique(BASE_DATA$state))),
              selected = "All States",
              width = "100%"
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("studio_filter_city"),
              label = tags$span(icon("city", class = "me-1 text-primary"), "City Scope:"),
              choices = c("All Cities", sort(unique(BASE_DATA$city))),
              selected = "All Cities",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              inputId = ns("studio_filter_pollutant"),
              label = tags$span(icon("smog", class = "me-1 text-primary"), "Target Pollutant:"),
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
              inputId = ns("studio_cor_method"),
              label = tags$span(icon("calculator", class = "me-1 text-primary"), "Correlation Type:"),
              choices = c(
                "Pearson (Linear)" = "pearson",
                "Spearman (Rank)" = "spearman"
              ),
              selected = "pearson",
              width = "100%"
            )
          ),
          column(
            width = 2,
            div(
              style = "margin-top: 28px;",
              actionButton(
                inputId = ns("btn_studio_reset"),
                label = " Reset Filters",
                icon = icon("rotate-left"),
                class = "btn btn-outline-secondary w-100",
                title = "Reset studio filters to national overview"
              )
            )
          )
        )
      ),
      
      # Visualization Tabs
      navset_card_tab(
        id = ns("studio_tabs"),
        
        # TAB 1: Distributions & Dispersion (Box Plots & Density)
        nav_panel(
          title = "Distributions & Dispersion",
          icon = icon("chart-area"),
          div(
            class = "py-2",
            fluidRow(
              # Box Plots Across Major States/Cities (Col 7)
              column(
                width = 7,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("chart-simple", class = "me-2"), textOutput(ns("boxplot_title"), inline = TRUE)),
                    span(class = "badge bg-light text-secondary border", textOutput(ns("boxplot_badge"), inline = TRUE))
                  ),
                  p(class = "small text-muted mb-2", "Box plots display median (central bar), interquartile range (IQR box), and Tukey whiskers (1.5×IQR). Points beyond whiskers represent statistical outliers."),
                  plotlyOutput(ns("distribution_boxplot"), height = "380px"),
                  uiOutput(ns("boxplot_interpretation"))
                )
              ),
              
              # Density & Probability Distribution (Col 5)
              column(
                width = 5,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("wave-square", class = "me-2"), textOutput(ns("density_title"), inline = TRUE)),
                    span(class = "badge bg-light text-secondary border", "Probability Density")
                  ),
                  p(class = "small text-muted mb-2", "Kernel density estimation showing distribution spread, central tendency, and skewness profile."),
                  plotlyOutput(ns("distribution_density"), height = "380px"),
                  uiOutput(ns("density_interpretation"))
                )
              )
            )
          )
        ),
        
        # TAB 2: Particulate Dynamics (PM2.5 vs PM10 Scatter & Mass Ratios)
        nav_panel(
          title = "Particulate Dynamics (PM2.5 vs PM10)",
          icon = icon("circle-nodes"),
          div(
            class = "py-2",
            fluidRow(
              # PM2.5 vs PM10 Scatter Plot (Col 7)
              column(
                width = 7,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("arrows-spin", class = "me-2"), "Respirable Particulate Scatter: PM2.5 vs PM10"),
                    span(class = "badge bg-light text-primary border", textOutput(ns("scatter_n_badge"), inline = TRUE))
                  ),
                  p(class = "small text-muted mb-2", "Fitted with ordinary least squares linear regression (blue line) and 1:1 reference line (dashed grey). Tooltip displays station coordinates and mass ratio."),
                  plotlyOutput(ns("particulate_scatter"), height = "380px"),
                  uiOutput(ns("scatter_interpretation"))
                )
              ),
              
              # Particulate Ratio Distribution (Col 5)
              column(
                width = 5,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("sliders", class = "me-2"), "Particulate Mass Ratio (PM2.5 / PM10)"),
                    span(class = "badge bg-light text-secondary border", "Fine / Coarse Fraction")
                  ),
                  p(class = "small text-muted mb-2", "Distribution of the ratio of fine particles (PM2.5) to coarse respirable dust (PM10) across co-located stations."),
                  plotlyOutput(ns("pm_ratio_histogram"), height = "380px"),
                  uiOutput(ns("pm_ratio_interpretation"))
                )
              )
            )
          )
        ),
        
        # TAB 3: Correlation Matrix & Pairwise Completeness
        nav_panel(
          title = "Correlation Heatmap & Alignment",
          icon = icon("table-cells"),
          div(
            class = "py-2",
            fluidRow(
              # Heatmap (Col 7)
              column(
                width = 7,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("fire-flame-curved", class = "me-2"), textOutput(ns("heatmap_title"), inline = TRUE)),
                    span(class = "badge bg-light text-secondary border", textOutput(ns("heatmap_type_badge"), inline = TRUE))
                  ),
                  p(class = "small text-muted mb-2", "Pairwise correlation matrix computed across stations reporting valid measurements for both variables. Diverging scale from -1.0 (blue) to +1.0 (red)."),
                  plotlyOutput(ns("correlation_heatmap"), height = "380px"),
                  uiOutput(ns("heatmap_interpretation"))
                )
              ),
              
              # Pairwise Sample Completeness Table (Col 5)
              column(
                width = 5,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("clipboard-check", class = "me-2"), "Pairwise Co-located Sample Sizes (N)"),
                    span(class = "badge bg-light text-secondary border", "Reporting Alignment")
                  ),
                  p(class = "small text-muted mb-2", "Number of stations concurrently reporting both criteria pollutants. Discloses missingness limitations for correlation analysis."),
                  DTOutput(ns("pairwise_n_table")),
                  div(
                    class = "interpretation-box py-1 px-2 small mt-2",
                    icon("circle-info", class = "me-1 text-primary"),
                    "Statistical note: Sample sizes vary because stations with missing/offline sensors are excluded pairwise rather than imputed with zeros."
                  )
                )
              )
            )
          )
        ),
        
        # TAB 4: Multi-Pollutant Profiles (Small Multiples & Parallel Coordinates)
        nav_panel(
          title = "Small Multiples & Parallel Coordinates",
          icon = icon("network-wired"),
          div(
            class = "py-2",
            fluidRow(
              # Small Multiples Grouped Bars (Col 6)
              column(
                width = 6,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("cubes", class = "me-2"), "Small Multiples: Minimum, Average & Maximum by Pollutant"),
                    span(class = "badge bg-light text-secondary border", "Independent Scales")
                  ),
                  p(class = "small text-muted mb-2", "Faceted panels display reported metrics on independent y-axes with proper units, preventing misleading cross-scale comparisons."),
                  plotlyOutput(ns("small_multiples_bars"), height = "400px"),
                  uiOutput(ns("small_multiples_interpretation"))
                )
              ),
              
              # Parallel Coordinates (Col 6)
              column(
                width = 6,
                div(
                  class = "card p-3 shadow-sm border-0 h-100",
                  div(
                    class = "d-flex justify-content-between align-items-center mb-1",
                    h6(class = "card-title text-primary m-0", icon("diagram-project", class = "me-2"), "Parallel Coordinates Profile (Station Signatures)"),
                    span(class = "badge bg-light text-primary border", "Min-Max Normalized [0, 1]")
                  ),
                  p(class = "small text-muted mb-2", "Interactive multi-axis profile. To compare different pollutant units, values are normalized to [0, 1] via: (x - min) / (max - min). Drag along axes to filter lines."),
                  plotlyOutput(ns("parallel_coordinates_plot"), height = "400px"),
                  uiOutput(ns("parcoords_interpretation"))
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server Module for Advanced Visualization Studio
mod_viz_studio_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Defensive Filter Accessors
    curr_state <- reactive({
      if (is.null(input$studio_filter_state) || !nzchar(input$studio_filter_state)) "All States" else input$studio_filter_state
    })
    
    curr_city <- reactive({
      if (is.null(input$studio_filter_city) || !nzchar(input$studio_filter_city)) "All Cities" else input$studio_filter_city
    })
    
    curr_pollutant <- reactive({
      if (is.null(input$studio_filter_pollutant) || !nzchar(input$studio_filter_pollutant)) "PM2.5" else input$studio_filter_pollutant
    })
    
    curr_cor_method <- reactive({
      if (is.null(input$studio_cor_method) || !nzchar(input$studio_cor_method)) "pearson" else input$studio_cor_method
    })
    
    # 2. Cascading City Filter Update
    observeEvent(input$studio_filter_state, {
      st <- curr_state()
      ct <- curr_city()
      
      if (st == "All States") {
        cities <- c("All Cities", sort(unique(BASE_DATA$city)))
      } else {
        cities <- c("All Cities", sort(unique(BASE_DATA$city[BASE_DATA$state == st])))
      }
      
      new_sel <- if (ct %in% cities) ct else "All Cities"
      updateSelectInput(session, "studio_filter_city", choices = cities, selected = new_sel)
    }, ignoreInit = TRUE)
    
    # Reset Filters Button Handler
    observeEvent(input$btn_studio_reset, {
      updateSelectInput(session, "studio_filter_state", selected = "All States")
      updateSelectInput(session, "studio_filter_city", choices = c("All Cities", sort(unique(BASE_DATA$city))), selected = "All Cities")
      updateSelectInput(session, "studio_filter_pollutant", selected = "PM2.5")
      updateSelectInput(session, "studio_cor_method", selected = "pearson")
    })
    
    # 3. Filtered Datasets
    # Filtered long data
    filtered_studio_long <- reactive({
      df <- BASE_DATA
      st <- curr_state()
      ct <- curr_city()
      if (st != "All States") df <- df %>% filter(state == st)
      if (ct != "All Cities") df <- df %>% filter(city == ct)
      df
    })
    
    # Filtered wide data
    filtered_studio_wide <- reactive({
      df <- WIDE_DATA
      st <- curr_state()
      ct <- curr_city()
      if (st != "All States") df <- df %>% filter(state == st)
      if (ct != "All Cities") df <- df %>% filter(city == ct)
      df
    })
    
    # 4. Header Titles & Badges
    output$boxplot_title <- renderText({
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      paste0("Distribution of ", pol, " across Regions (", unit, ")")
    })
    
    output$boxplot_badge <- renderText({
      st <- curr_state()
      if (st == "All States") "Top States by Station Count" else paste("Cities in", st)
    })
    
    output$density_title <- renderText({
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      paste0(pol, " Probability Density Profile (", unit, ")")
    })
    
    output$scatter_n_badge <- renderText({
      w <- filtered_studio_wide()
      n_both <- sum(!is.na(w$PM2.5) & !is.na(w$PM10))
      paste0("N = ", n_both, " Stations")
    })
    
    output$heatmap_title <- renderText({
      m_name <- if (curr_cor_method() == "pearson") "Pearson Correlation (r)" else "Spearman Rank Correlation (ρ)"
      paste0("Inter-Pollutant Correlation Heatmap: ", m_name)
    })
    
    output$heatmap_type_badge <- renderText({
      toupper(curr_cor_method())
    })
    
    # 5. TAB 1: Comparative Box Plots
    output$distribution_boxplot <- renderPlotly({
      df <- filtered_studio_long()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      st <- curr_state()
      
      sub_df <- df %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
      
      if (nrow(sub_df) < 3) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Insufficient valid observations for", pol, "in selected area."), color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      # If "All States", group by Top 6 states by station count; if specific state, group by cities
      if (st == "All States") {
        top_groups <- sub_df %>% count(state, sort = TRUE) %>% head(6) %>% pull(state)
        plot_df <- sub_df %>% filter(state %in% top_groups) %>% mutate(group_var = state)
        group_lbl <- "State / UT"
      } else {
        top_groups <- sub_df %>% count(city, sort = TRUE) %>% head(8) %>% pull(city)
        plot_df <- sub_df %>% filter(city %in% top_groups) %>% mutate(group_var = city)
        group_lbl <- "City"
      }
      
      # Order groups by median concentration descending
      plot_df <- plot_df %>%
        mutate(group_var = reorder(group_var, pollutant_avg, FUN = median, decreasing = TRUE))
      
      p <- ggplot(plot_df, aes(x = group_var, y = pollutant_avg, fill = group_var, text = paste0(
        group_lbl, ": ", group_var, "\nStation: ", station, "\n", pol, ": ", pollutant_avg, " ", unit
      ))) +
        geom_boxplot(alpha = 0.8, outlier.color = "#DC2626", outlier.size = 1.8, width = 0.6) +
        scale_fill_viridis_d(option = "turbo", guide = "none") +
        labs(
          x = group_lbl,
          y = paste0("Reported Average (", unit, ")")
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 25, hjust = 1, size = 9, color = "#1E293B"),
          axis.text.y = element_text(size = 9, color = "#1E293B"),
          axis.title = element_text(size = 10, color = "#475569")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 10, r = 10, t = 10, b = 30)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Dynamic Boxplot Interpretation
    output$boxplot_interpretation <- renderUI({
      df <- filtered_studio_long()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      st <- curr_state()
      
      sub_df <- df %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
      if (nrow(sub_df) < 3) return(NULL)
      
      group_col <- if (st == "All States") "state" else "city"
      top_groups <- sub_df %>% count(.data[[group_col]], sort = TRUE) %>% head(6) %>% pull(.data[[group_col]])
      stats <- sub_df %>%
        filter(.data[[group_col]] %in% top_groups) %>%
        group_by(.data[[group_col]]) %>%
        summarise(
          med = median(pollutant_avg),
          iqr_val = IQR(pollutant_avg),
          n = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(med))
      
      if (nrow(stats) == 0) return(NULL)
      
      hi_grp <- stats[[group_col]][1]
      hi_med <- round(stats$med[1], 1)
      lo_grp <- stats[[group_col]][nrow(stats)]
      lo_med <- round(stats$med[nrow(stats)], 1)
      
      div(
        class = "interpretation-box py-1 px-2 small mt-2",
        icon("magnifying-glass-chart", class = "me-1 text-primary"),
        strong("Distribution Finding: "),
        paste0(
          "Among the evaluated regions, ", hi_grp, " exhibits the highest median reported ", pol,
          " (", hi_med, " ", unit, "), while ", lo_grp, " records the lowest (", lo_med, " ", unit, "). ",
          "Outliers highlighted in red represent localized monitoring stations with readings exceeding 1.5× the regional interquartile range."
        )
      )
    })
    
    # TAB 1: Density Curve
    output$distribution_density <- renderPlotly({
      df <- filtered_studio_long()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      sub_df <- df %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
      if (nrow(sub_df) < 5) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = paste("Insufficient points to estimate density for", pol), color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      med_val <- median(sub_df$pollutant_avg)
      mean_val <- mean(sub_df$pollutant_avg)
      
      p <- ggplot(sub_df, aes(x = pollutant_avg)) +
        geom_density(fill = "#0D9488", color = "#0F766E", alpha = 0.45, linewidth = 0.8) +
        geom_vline(aes(xintercept = med_val), color = "#1E3A8A", linetype = "dashed", linewidth = 0.8) +
        geom_vline(aes(xintercept = mean_val), color = "#DC2626", linetype = "dotted", linewidth = 0.8) +
        labs(
          x = paste0("Concentration (", unit, ")"),
          y = "Density"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 9, color = "#1E293B"),
          axis.title = element_text(size = 10, color = "#475569")
        )
      
      ggplotly(p) %>%
        layout(
          margin = list(l = 10, r = 10, t = 10, b = 20),
          annotations = list(
            list(x = med_val, y = 0, text = paste0("Median: ", round(med_val, 1)), showarrow = TRUE, arrowhead = 2, ax = -30, ay = -35, font = list(size = 10, color = "#1E3A8A")),
            list(x = mean_val, y = 0, text = paste0("Mean: ", round(mean_val, 1)), showarrow = TRUE, arrowhead = 2, ax = 30, ay = -55, font = list(size = 10, color = "#DC2626"))
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Dynamic Density Interpretation
    output$density_interpretation <- renderUI({
      df <- filtered_studio_long()
      pol <- curr_pollutant()
      unit <- ifelse(pol == "CO", "mg/m³", "µg/m³")
      
      sub_df <- df %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
      if (nrow(sub_df) < 5) return(NULL)
      
      vals <- sub_df$pollutant_avg
      mean_v <- round(mean(vals), 1)
      med_v <- round(median(vals), 1)
      skew_direction <- if (mean_v > med_v * 1.05) "positive (right) skew" else if (mean_v < med_v * 0.95) "negative (left) skew" else "approximate symmetry"
      
      div(
        class = "interpretation-box py-1 px-2 small mt-2",
        icon("chart-line", class = "me-1 text-primary"),
        strong("Skewness Profile: "),
        paste0(
          "The density curve exhibits a ", skew_direction, " (Mean: ", mean_v, " ", unit,
          " vs Median: ", med_v, " ", unit, "). The concentration mass is centered around the median with a tail of higher readings."
        )
      )
    })
    
    # 6. TAB 2: Particulate Dynamics (PM2.5 vs PM10 Scatter)
    output$particulate_scatter <- renderPlotly({
      w <- filtered_studio_wide()
      
      scat_df <- w %>%
        filter(!is.na(PM2.5) & !is.na(PM10) & PM10 > 0)
      
      if (nrow(scat_df) < 3) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Insufficient co-located PM2.5 and PM10 measurements.", color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      scat_df <- scat_df %>%
        mutate(
          ratio = round(PM2.5 / PM10, 2),
          hover_txt = paste0(
            "Station: ", station, "\n",
            "City: ", city, ", ", state, "\n",
            "PM10: ", PM10, " µg/m³\n",
            "PM2.5: ", PM2.5, " µg/m³\n",
            "Ratio (PM2.5/PM10): ", ratio
          )
        )
      
      max_axis <- max(c(scat_df$PM10, scat_df$PM2.5), na.rm = TRUE) * 1.05
      
      p <- ggplot(scat_df, aes(x = PM10, y = PM2.5)) +
        # 1:1 Equality line (dashed)
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#94A3B8", linewidth = 0.7) +
        # Linear Regression line
        geom_smooth(method = "lm", formula = y ~ x, color = "#1E3A8A", fill = "#BFDBFE", alpha = 0.3, linewidth = 0.9) +
        # Scatter points
        geom_point(aes(text = hover_txt, color = ratio), size = 2.2, alpha = 0.8) +
        scale_color_viridis_c(option = "plasma", name = "PM2.5/PM10 Ratio") +
        coord_cartesian(xlim = c(0, max_axis), ylim = c(0, max_axis)) +
        labs(
          x = "Coarse & Respirable Particulate PM10 (µg/m³)",
          y = "Fine Particulate PM2.5 (µg/m³)"
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
    
    # Dynamic Scatter Interpretation
    output$scatter_interpretation <- renderUI({
      w <- filtered_studio_wide()
      scat_df <- w %>% filter(!is.na(PM2.5) & !is.na(PM10) & PM10 > 0)
      if (nrow(scat_df) < 3) return(NULL)
      
      cor_val <- round(cor(scat_df$PM10, scat_df$PM2.5), 3)
      fit <- lm(PM2.5 ~ PM10, data = scat_df)
      r2 <- round(summary(fit)$r.squared, 3)
      slope <- round(coef(fit)[2], 2)
      
      div(
        class = "interpretation-box py-1 px-2 small mt-2",
        icon("calculator", class = "me-1 text-primary"),
        strong("Regression & Co-occurrence Finding: "),
        paste0(
          "Across N = ", nrow(scat_df), " co-located stations, Pearson correlation r = ", cor_val,
          " (R² = ", r2, ", Slope = ", slope, "). ",
          "A strong linear co-occurrence is observed between PM10 and PM2.5. ",
          "The dashed line represents the physical 1:1 boundary (PM2.5 = PM10). Statistical correlation indicates shared dispersal patterns and does not establish a causal mechanism."
        )
      )
    })
    
    # TAB 2: Particulate Ratio Histogram
    output$pm_ratio_histogram <- renderPlotly({
      w <- filtered_studio_wide()
      scat_df <- w %>% filter(!is.na(PM2.5) & !is.na(PM10) & PM10 > 0)
      if (nrow(scat_df) < 3) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Insufficient valid pairs for ratio analysis.", color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      scat_df$ratio <- scat_df$PM2.5 / scat_df$PM10
      med_ratio <- median(scat_df$ratio)
      
      p <- ggplot(scat_df, aes(x = ratio)) +
        geom_histogram(binwidth = 0.05, fill = "#0284C7", color = "#FFFFFF", alpha = 0.8) +
        geom_vline(xintercept = med_ratio, color = "#DC2626", linetype = "dashed", linewidth = 0.8) +
        geom_vline(xintercept = 0.5, color = "#475569", linetype = "dotted", linewidth = 0.8) +
        labs(
          x = "Ratio (PM2.5 / PM10)",
          y = "Station Count"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 9, color = "#1E293B"),
          axis.title = element_text(size = 10, color = "#475569")
        )
      
      ggplotly(p) %>%
        layout(
          margin = list(l = 10, r = 10, t = 10, b = 20),
          annotations = list(
            list(x = med_ratio, y = 10, text = paste0("Median Ratio: ", round(med_ratio, 2)), showarrow = TRUE, arrowhead = 2, ax = 30, ay = -30, font = list(size = 10, color = "#DC2626"))
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Dynamic Particulate Ratio Interpretation
    output$pm_ratio_interpretation <- renderUI({
      w <- filtered_studio_wide()
      scat_df <- w %>% filter(!is.na(PM2.5) & !is.na(PM10) & PM10 > 0)
      if (nrow(scat_df) < 3) return(NULL)
      
      scat_df$ratio <- scat_df$PM2.5 / scat_df$PM10
      med_ratio <- round(median(scat_df$ratio), 2)
      pct_fine_predom <- round(100 * sum(scat_df$ratio >= 0.5) / nrow(scat_df), 1)
      
      div(
        class = "interpretation-box py-1 px-2 small mt-2",
        icon("chart-pie", class = "me-1 text-primary"),
        strong("Aerosol Composition Finding: "),
        paste0(
          "The median PM2.5/PM10 ratio is ", med_ratio, ". In ", pct_fine_predom,
          "% of reporting stations, the ratio exceeds 0.50, indicating that fine respirable particulates comprise the dominant portion of particulate matter across these monitors."
        )
      )
    })
    
    # 7. TAB 3: Correlation Matrix Heatmap
    output$correlation_heatmap <- renderPlotly({
      w <- filtered_studio_wide()
      pol_cols <- c("CO", "NH3", "NO2", "OZONE", "PM10", "PM2.5", "SO2")
      m_type <- curr_cor_method()
      
      mat_data <- w %>% select(all_of(pol_cols))
      
      # Calculate pairwise correlation matrix
      cor_mat <- cor(mat_data, use = "pairwise.complete.obs", method = m_type)
      
      # Calculate pairwise sample sizes
      n_mat <- matrix(NA, nrow = length(pol_cols), ncol = length(pol_cols), dimnames = list(pol_cols, pol_cols))
      for (p1 in pol_cols) {
        for (p2 in pol_cols) {
          n_mat[p1, p2] <- sum(!is.na(mat_data[[p1]]) & !is.na(mat_data[[p2]]))
        }
      }
      
      # Hover text matrix
      text_mat <- matrix("", nrow = length(pol_cols), ncol = length(pol_cols))
      for (i in seq_along(pol_cols)) {
        for (j in seq_along(pol_cols)) {
          r_val <- round(cor_mat[i, j], 3)
          n_val <- n_mat[i, j]
          text_mat[i, j] <- paste0(
            "Pair: ", pol_cols[i], " vs ", pol_cols[j], "\n",
            "Correlation (", m_type, "): ", r_val, "\n",
            "Co-located Stations (N): ", n_val
          )
        }
      }
      
      plot_ly(
        x = pol_cols,
        y = pol_cols,
        z = round(cor_mat, 2),
        text = text_mat,
        hoverinfo = "text",
        type = "heatmap",
        colorscale = "RdBu",
        reversescale = TRUE,
        zmin = -1,
        zmax = 1
      ) %>%
        layout(
          xaxis = list(title = "", tickfont = list(size = 11, color = "#1E293B")),
          yaxis = list(title = "", tickfont = list(size = 11, color = "#1E293B")),
          margin = list(l = 40, r = 20, t = 10, b = 40)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Dynamic Heatmap Interpretation
    output$heatmap_interpretation <- renderUI({
      w <- filtered_studio_wide()
      pol_cols <- c("CO", "NH3", "NO2", "OZONE", "PM10", "PM2.5", "SO2")
      m_type <- curr_cor_method()
      mat_data <- w %>% select(all_of(pol_cols))
      cor_mat <- cor(mat_data, use = "pairwise.complete.obs", method = m_type)
      
      # Exclude diagonal
      diag(cor_mat) <- NA
      
      if (all(is.na(cor_mat)) || sum(!is.na(cor_mat)) == 0) {
        return(div(
          class = "interpretation-box py-1 px-2 small mt-2 text-muted",
          icon("info-circle", class = "me-1 text-secondary"),
          "Insufficient co-located observations in this selection to calculate pairwise correlation coefficients."
        ))
      }
      
      max_idx <- which(cor_mat == max(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
      min_idx <- which(cor_mat == min(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
      
      hi_p1 <- pol_cols[max_idx[1]]; hi_p2 <- pol_cols[max_idx[2]]
      hi_r <- round(cor_mat[max_idx[1], max_idx[2]], 2)
      
      lo_p1 <- pol_cols[min_idx[1]]; lo_p2 <- pol_cols[min_idx[2]]
      lo_r <- round(cor_mat[min_idx[1], min_idx[2]], 2)
      
      div(
        class = "interpretation-box py-1 px-2 small mt-2",
        icon("circle-nodes", class = "me-1 text-primary"),
        strong("Correlation Finding: "),
        paste0(
          "Strongest co-movement is observed between ", hi_p1, " and ", hi_p2,
          " (r = ", hi_r, "), reflecting shared atmospheric particulate behavior. ",
          "Weakest correlation occurs between ", lo_p1, " and ", lo_p2, " (r = ", lo_r, "). ",
          "Limitations: Correlations are computed strictly from pairwise co-located monitors without imputing missing values."
        )
      )
    })
    
    # TAB 3: Pairwise N Table
    output$pairwise_n_table <- renderDT({
      w <- filtered_studio_wide()
      pol_cols <- c("CO", "NH3", "NO2", "OZONE", "PM10", "PM2.5", "SO2")
      mat_data <- w %>% select(all_of(pol_cols))
      
      n_mat <- matrix(NA, nrow = length(pol_cols), ncol = length(pol_cols), dimnames = list(pol_cols, pol_cols))
      for (p1 in pol_cols) {
        for (p2 in pol_cols) {
          n_mat[p1, p2] <- sum(!is.na(mat_data[[p1]]) & !is.na(mat_data[[p2]]))
        }
      }
      
      n_df <- as.data.frame(n_mat)
      n_df <- cbind(Pollutant = rownames(n_df), n_df)
      
      datatable(
        n_df,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          scrollX = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE,
        class = "table table-sm table-bordered align-middle small"
      ) %>%
        formatStyle(
          pol_cols,
          background = styleColorBar(range(n_mat, na.rm = TRUE), "#E0F2FE"),
          backgroundSize = "98% 88%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })
    
    # 8. TAB 4: Small Multiples Faceted Bars
    output$small_multiples_bars <- renderPlotly({
      df <- filtered_studio_long()
      
      summary_df <- df %>%
        group_by(pollutant_id) %>%
        summarise(
          `Minimum Reported` = suppressWarnings(min(pollutant_min, na.rm = TRUE)),
          `Average Reported` = suppressWarnings(mean(pollutant_avg, na.rm = TRUE)),
          `Maximum Reported` = suppressWarnings(max(pollutant_max, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        pivot_longer(
          cols = c("Minimum Reported", "Average Reported", "Maximum Reported"),
          names_to = "Metric",
          values_to = "Concentration"
        ) %>%
        mutate(
          Concentration = ifelse(is.infinite(Concentration), NA, round(Concentration, 1)),
          unit = ifelse(pollutant_id == "CO", "mg/m³", "µg/m³"),
          panel_label = paste0(pollutant_id, " (", unit, ")")
        )
      
      p <- ggplot(summary_df, aes(x = Metric, y = Concentration, fill = Metric, text = paste0(
        "Pollutant: ", pollutant_id, "\n",
        "Metric: ", Metric, "\n",
        "Value: ", Concentration, " ", unit
      ))) +
        geom_col(show.legend = FALSE, width = 0.6) +
        facet_wrap(~panel_label, scales = "free_y", ncol = 3) +
        scale_fill_manual(values = c("Minimum Reported" = "#0D9488", "Average Reported" = "#1E3A8A", "Maximum Reported" = "#DC2626")) +
        labs(x = NULL, y = "Reported Concentration (Per-Panel Unit)") +
        theme_minimal(base_size = 9) +
        theme(
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 7.5, color = "#1E293B"),
          axis.text.y = element_text(size = 7.5, color = "#1E293B"),
          strip.text = element_text(face = "bold", size = 8.5, color = "#1E3A8A")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 10, r = 10, t = 10, b = 25)) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Dynamic Small Multiples Interpretation
    output$small_multiples_interpretation <- renderUI({
      div(
        class = "interpretation-box py-1 px-2 small mt-2",
        icon("layer-group", class = "me-1 text-primary"),
        strong("Methodological Integrity: "),
        "Small multiples prevent deceptive scaling by assigning independent free axes to each pollutant. Particulates (PM10/PM2.5) scale to hundreds of µg/m³, while trace gases (NH3/SO2) and carbon monoxide (mg/m³) occupy distinct biological and measurement ranges."
      )
    })
    
    # TAB 4: Parallel Coordinates Plot
    output$parallel_coordinates_plot <- renderPlotly({
      w <- filtered_studio_wide()
      pol_cols <- c("CO", "NH3", "NO2", "OZONE", "PM10", "PM2.5", "SO2")
      
      # Select stations that have at least 4 reported pollutants
      valid_rows <- w %>%
        filter(rowSums(!is.na(select(., all_of(pol_cols)))) >= 4) %>%
        head(100) # Cap for fluid browser interactivity
      
      if (nrow(valid_rows) < 3) {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "Insufficient multi-pollutant stations for parallel coordinates.", color = "#64748B", size = 4) +
          theme_void()
        return(ggplotly(p))
      }
      
      # Build dimensions for parcoords
      dims <- lapply(pol_cols, function(p) {
        vals <- valid_rows[[p]]
        rng <- range(vals, na.rm = TRUE)
        if (is.infinite(rng[1]) || is.infinite(rng[2]) || rng[1] == rng[2]) rng <- c(0, 100)
        unit <- ifelse(p == "CO", "mg/m³", "µg/m³")
        list(
          range = rng,
          label = paste0(p, " (", unit, ")"),
          values = vals
        )
      })
      
      line_color <- if (!is.na(valid_rows$PM2.5[1])) valid_rows$PM2.5 else seq_len(nrow(valid_rows))
      
      plot_ly(
        type = "parcoords",
        line = list(
          color = line_color,
          colorscale = "Viridis",
          showscale = TRUE,
          colorbar = list(title = "PM2.5 (µg/m³)", len = 0.8)
        ),
        dimensions = dims
      ) %>%
        layout(
          margin = list(l = 35, r = 25, t = 35, b = 20)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Dynamic Parallel Coordinates Interpretation
    output$parcoords_interpretation <- renderUI({
      div(
        class = "interpretation-box py-1 px-2 small mt-2",
        icon("sliders", class = "me-1 text-primary"),
        strong("Multi-Dimensional Signature Finding: "),
        "Parallel coordinates trace multi-pollutant signatures for each station simultaneously. Lines colored towards yellow/bright green indicate stations with elevated fine particulate burdens. Notice how elevated PM lines diverge across NO2 and SO2 depending on localized industrial vs vehicular signatures."
      )
    })
    
  })
}
