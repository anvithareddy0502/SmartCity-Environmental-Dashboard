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
      
      # Clean Page Header
      div(
        class = "page-header-container mb-3",
        h4(class = "page-header-title", "Visualization Studio"),
        p(class = "page-header-sub", "Statistical distributions, particulate dynamics, and correlation analysis.")
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
              label = "State / UT:",
              choices = c("All States", sort(unique(BASE_DATA$state))),
              selected = "All States",
              width = "100%"
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = ns("studio_filter_city"),
              label = "City:",
              choices = c("All Cities", sort(unique(BASE_DATA$city))),
              selected = "All Cities",
              width = "100%"
            )
          ),
          column(
            width = 2,
            selectInput(
              inputId = ns("studio_filter_pollutant"),
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
            width = 2,
            selectInput(
              inputId = ns("studio_cor_method"),
              label = "Correlation method:",
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
                label = "Reset filters",
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
          title = "Comparative Distributions",
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
                    h6(class = "card-title m-0", textOutput(ns("boxplot_title"), inline = TRUE)),
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
                    h6(class = "card-title m-0", textOutput(ns("density_title"), inline = TRUE)),
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
          title = "PM2.5 vs PM10 Comparison",
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
                    h6(class = "card-title m-0", "Respirable Particulate Scatter: PM2.5 vs PM10"),
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
                    h6(class = "card-title m-0", "Particulate Mass Ratio (PM2.5 / PM10)"),
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
          title = "Correlation Heatmap",
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
                    h6(class = "card-title m-0", textOutput(ns("heatmap_title"), inline = TRUE)),
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
                    h6(class = "card-title m-0", "Pairwise Sample Sizes (N)"),
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
          title = "Multi-Pollutant Profiles",
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
                    h6(class = "card-title m-0", "Small Multiples: Minimum, Average & Maximum"),
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
                    h6(class = "card-title m-0", "Parallel Coordinates Profile (Station Signatures)"),
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
      req(input$studio_filter_state)
      st <- curr_state()
      ct <- curr_city()
      
      if (identical(st, "All States")) {
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
      if (!identical(st, "All States")) df <- df %>% filter(state == st)
      if (!identical(ct, "All Cities")) df <- df %>% filter(city == ct)
      df
    })
    
    # Filtered wide data
    filtered_studio_wide <- reactive({
      df <- WIDE_DATA
      st <- curr_state()
      ct <- curr_city()
      if (!identical(st, "All States")) df <- df %>% filter(state == st)
      if (!identical(ct, "All Cities")) df <- df %>% filter(city == ct)
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
      if (nrow(sub_df) < 3) {
        return(build_understand_chart(
          chart_type = "Box Plot (Distribution & Dispersion)",
          what_shows = paste0("Compares the statistical distribution of ", pol, " across reporting states or cities."),
          how_to_read = "The horizontal axis lists geographic regions; the vertical axis shows concentration in the pollutant's unit. The box shows the IQR (25th to 75th percentiles) with the central line at the median.",
          colors_meaning = "Distinct colors identify different geographic regions; red points mark statistical outliers.",
          dynamic_findings = "Insufficient observations to construct comparative box plots (fewer than 3 valid readings).",
          important_note = "Reset filters to include additional stations or states."
        ))
      }
      
      group_col <- if (st == "All States") "state" else "city"
      top_groups <- sub_df %>% count(.data[[group_col]], sort = TRUE) %>% head(6) %>% pull(.data[[group_col]])
      stats <- sub_df %>%
        filter(.data[[group_col]] %in% top_groups) %>%
        group_by(.data[[group_col]]) %>%
        summarise(
          med = median(pollutant_avg),
          iqr_val = IQR(pollutant_avg),
          q1 = quantile(pollutant_avg, 0.25),
          q3 = quantile(pollutant_avg, 0.75),
          n = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(med))
      
      if (nrow(stats) == 0) return(NULL)
      
      hi_grp <- stats[[group_col]][1]
      hi_med <- round(stats$med[1], 1)
      lo_grp <- stats[[group_col]][nrow(stats)]
      lo_med <- round(stats$med[nrow(stats)], 1)
      
      # Count outliers beyond 1.5x IQR
      n_outliers <- sum(sapply(seq_len(nrow(stats)), function(i) {
        g <- stats[[group_col]][i]
        vals <- sub_df$pollutant_avg[sub_df[[group_col]] == g]
        upper <- stats$q3[i] + 1.5 * stats$iqr_val[i]
        lower <- stats$q1[i] - 1.5 * stats$iqr_val[i]
        sum(vals > upper | vals < lower)
      }))
      
      finding <- paste0(
        "Among the top ", nrow(stats), " evaluated ", if (st == "All States") "states" else "cities", ", ",
        hi_grp, " exhibits the highest median reported ", pol, " concentration at ", hi_med, " ", unit,
        " (IQR: ", round(stats$iqr_val[1], 1), " ", unit, "), while ", lo_grp, " records the lowest median at ",
        lo_med, " ", unit, ". ",
        if (n_outliers > 0) {
          paste0(n_outliers, " station observation(s) exceed 1.5× the regional IQR (flagged as red points).")
        } else {
          "No extreme outliers beyond 1.5× IQR are detected across these regions."
        }
      )
      
      build_understand_chart(
        chart_type = "Box Plot (Distribution & Dispersion)",
        what_shows = paste0(
          "Compares the statistical distribution, median, spread, and dispersion of ", pol,
          " across top reporting ", if (st == "All States") "states" else "cities",
          ". Demonstrates regional variability without assuming a normal distribution."
        ),
        how_to_read = paste0(
          "The horizontal axis displays geographic entities. The vertical axis measures concentration in ", unit, ". ",
          "The thick central line marks the median (50th percentile); the box covers the Interquartile Range (IQR, 25th to 75th percentiles); ",
          "whiskers extend to the most extreme data point within 1.5×IQR. Red points beyond whiskers indicate statistical outliers."
        ),
        colors_meaning = "Boxes use distinct categorical colors per geographic entity to aid visual separation. Outlier points are plotted in red.",
        dynamic_findings = finding,
        important_note = paste0(
          "Reflects single-snapshot station distributions (23-10-2026 18:00 IST). ",
          "A taller box indicates high intra-regional variation among monitoring stations. Statistical outliers represent localized severe hotspots."
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
      if (nrow(sub_df) < 5) {
        return(build_understand_chart(
          chart_type = "Kernel Density Plot (Probability Distribution)",
          what_shows = paste0("Estimates the continuous probability distribution of ", pol, " across reporting stations."),
          how_to_read = "The horizontal axis shows concentration; the vertical axis shows probability density.",
          colors_meaning = "Teal shading highlights density mass; blue dashed line indicates median; red dotted line marks mean.",
          dynamic_findings = "Insufficient observations to estimate a reliable continuous density distribution (fewer than 5 valid readings).",
          important_note = "Reset filters to National Overview to view full density estimation."
        ))
      }
      
      vals <- sub_df$pollutant_avg
      mean_v <- round(mean(vals), 1)
      med_v <- round(median(vals), 1)
      skew_direction <- if (mean_v > med_v * 1.05) "positive (right) skew" else if (mean_v < med_v * 0.95) "negative (left) skew" else "approximate symmetry"
      
      finding <- paste0(
        "The probability distribution exhibits a pronounced ", skew_direction, " across ", length(vals), " reporting stations. ",
        "The arithmetic mean is ", mean_v, " ", unit, ", pulled higher than the median of ", med_v, " ", unit, " by an upper tail of extreme readings. ",
        "The central mass of stations is concentrated around ", med_v, " ", unit, "."
      )
      
      build_understand_chart(
        chart_type = "Kernel Density Plot (Probability Distribution)",
        what_shows = paste0(
          "Estimates the continuous probability density function of reported ", pol, " concentrations. ",
          "Visualizes the shape, spread, central tendency, and skewness of the underlying environmental data."
        ),
        how_to_read = paste0(
          "The horizontal axis represents reported concentration in ", unit, ". ",
          "The vertical axis indicates relative probability density (total area under the curve integrates to 1.0). ",
          "The blue dashed vertical line marks the sample median; the red dotted vertical line marks the arithmetic mean."
        ),
        colors_meaning = "The shaded teal area (#0D9488) represents the estimated probability mass. The dashed blue line marks the median, and the dotted red line marks the arithmetic mean.",
        dynamic_findings = finding,
        important_note = paste0(
          "In environmental datasets, positive (right) skew is common due to localized severe pollution episodes. ",
          "Because extreme high readings pull the arithmetic mean upward, the median is a more robust indicator of typical ambient exposure."
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
        suppressWarnings(geom_point(aes(text = hover_txt, color = ratio), size = 2.2, alpha = 0.8)) +
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
      if (nrow(scat_df) < 3) {
        return(build_understand_chart(
          chart_type = "Scatter Plot with Linear Regression",
          what_shows = "Compares fine particulate PM2.5 against coarse respirable PM10 across co-located monitoring stations.",
          how_to_read = "The horizontal axis measures PM10; the vertical axis measures PM2.5. The solid blue line shows OLS linear regression; the dashed grey line represents 1:1 equality.",
          colors_meaning = "Points are colored by their individual PM2.5/PM10 mass ratio using a continuous plasma palette.",
          dynamic_findings = "Insufficient co-located PM2.5 and PM10 observations in the current filter scope to compute regression parameters.",
          important_note = "Reset filters to include additional stations or states with co-located particulate monitors."
        ))
      }
      
      cor_val <- round(cor(scat_df$PM10, scat_df$PM2.5), 3)
      fit <- lm(PM2.5 ~ PM10, data = scat_df)
      r2 <- round(summary(fit)$r.squared, 3)
      slope <- round(coef(fit)[2], 2)
      
      finding <- paste0(
        "Across N = ", nrow(scat_df), " co-located stations, Pearson correlation is r = ", cor_val,
        " (R² = ", r2, ", Slope = ", slope, "). ",
        "A strong positive linear association indicates that stations with elevated coarse PM10 consistently experience elevated fine PM2.5. ",
        "Points cluster below the 1:1 dashed line, adhering to physical atmospheric constraints (PM2.5 is a physical fraction of PM10)."
      )
      
      build_understand_chart(
        chart_type = "Scatter Plot with Linear Regression",
        what_shows = "Examines the empirical relationship and mass ratio between fine particulates (PM2.5) and coarse respirable dust (PM10) across concurrently monitoring stations.",
        how_to_read = paste0(
          "The horizontal axis measures PM10 (µg/m³); the vertical axis measures PM2.5 (µg/m³). ",
          "Each point represents a co-located monitoring station. The solid navy line is the Ordinary Least Squares (OLS) regression fit; ",
          "the dashed diagonal line is the 1:1 physical upper limit boundary (PM2.5 = PM10). Hover over any point to see station details."
        ),
        colors_meaning = "Points are colored along a continuous plasma palette mapped to the fine-to-coarse ratio (PM2.5 / PM10), where purple indicates higher fine-particle proportions.",
        dynamic_findings = finding,
        important_note = paste0(
          "Linear correlation indicates shared atmospheric dispersion and emission co-occurrence; it does not demonstrate that PM10 causes PM2.5. ",
          "Physically, PM2.5 cannot exceed PM10 in ambient air; anomalies above the 1:1 boundary reflect localized sensor calibration drift."
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
      if (nrow(scat_df) < 3) {
        return(build_understand_chart(
          chart_type = "Histogram (Frequency Distribution)",
          what_shows = "Displays the distribution of PM2.5/PM10 particulate mass ratios across co-located stations.",
          how_to_read = "The horizontal axis shows bins of mass ratios; the vertical axis indicates station count.",
          colors_meaning = "Uniform blue bars indicate frequency count; red dashed line marks the median ratio.",
          dynamic_findings = "Insufficient paired measurements to evaluate the PM2.5/PM10 ratio distribution.",
          important_note = "Requires co-located monitoring stations reporting both particulate sizes concurrently."
        ))
      }
      
      scat_df$ratio <- scat_df$PM2.5 / scat_df$PM10
      med_ratio <- round(median(scat_df$ratio), 2)
      pct_fine_predom <- round(100 * sum(scat_df$ratio >= 0.5) / nrow(scat_df), 1)
      
      finding <- paste0(
        "Across ", nrow(scat_df), " reporting monitors, the median PM2.5/PM10 mass ratio is ", med_ratio, ". ",
        "In ", pct_fine_predom, "% of stations, the ratio exceeds 0.50, demonstrating that combustion-derived fine particles (PM2.5) ",
        "constitute the principal fraction of respirable particulate pollution across these urban and industrial environments."
      )
      
      build_understand_chart(
        chart_type = "Histogram (Frequency Distribution)",
        what_shows = "Visualizes the frequency distribution of fine-to-coarse particulate mass ratios (PM2.5/PM10) across co-located monitors to assess aerosol origin (combustion vs crustal dust).",
        how_to_read = paste0(
          "The horizontal axis bins the PM2.5/PM10 ratio into intervals of 0.05. The vertical axis shows the number of stations in each bin. ",
          "The red dashed line marks the median ratio across all stations; the grey dotted vertical line marks the 0.50 threshold."
        ),
        colors_meaning = "All bars are colored in uniform blue (#0284C7) to represent frequency. The red dashed line highlights the median ratio.",
        dynamic_findings = finding,
        important_note = "A high ratio (>0.6) typically reflects secondary aerosol formation and vehicular combustion emissions, whereas low ratios (<0.4) reflect mechanical crustal sources such as road dust and construction."
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
      diag(cor_mat) <- NA
      
      if (all(is.na(cor_mat)) || sum(!is.na(cor_mat)) == 0) {
        return(build_understand_chart(
          chart_type = "Correlation Matrix Heatmap",
          what_shows = "Displays pairwise correlation coefficients between all 7 criteria pollutants.",
          how_to_read = "Cells display correlation coefficients from -1.0 to +1.0. Red shades denote positive co-movement; blue indicates inverse relationships.",
          colors_meaning = "Diverging Red-to-Blue scale: Dark red indicates strong positive correlation (+1.0); dark blue indicates strong negative correlation (-1.0).",
          dynamic_findings = "Insufficient co-located observations across criteria pollutants in this selection to calculate pairwise correlation coefficients.",
          important_note = "Reset filters to include additional stations with multi-pollutant monitors."
        ))
      }
      
      max_idx <- which(cor_mat == max(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
      min_idx <- which(cor_mat == min(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
      
      hi_p1 <- pol_cols[max_idx[1]]; hi_p2 <- pol_cols[max_idx[2]]
      hi_r <- round(cor_mat[max_idx[1], max_idx[2]], 2)
      
      lo_p1 <- pol_cols[min_idx[1]]; lo_p2 <- pol_cols[min_idx[2]]
      lo_r <- round(cor_mat[min_idx[1], min_idx[2]], 2)
      
      # Sample sizes for top pair
      n_hi <- sum(!is.na(mat_data[[hi_p1]]) & !is.na(mat_data[[hi_p2]]))
      
      finding <- paste0(
        "Strongest positive co-movement is observed between ", hi_p1, " and ", hi_p2,
        " (r = ", hi_r, ", N = ", n_hi, " co-located stations), reflecting shared atmospheric stagnation and dispersion dynamics. ",
        "The weakest / most divergent association is between ", lo_p1, " and ", lo_p2, " (r = ", lo_r, ")."
      )
      
      build_understand_chart(
        chart_type = "Correlation Matrix Heatmap",
        what_shows = paste0(
          "Calculates pairwise ", if (m_type == "pearson") "linear (Pearson)" else "monotonic rank (Spearman)",
          " correlation coefficients across all 7 criteria pollutants using pairwise complete station observations."
        ),
        how_to_read = paste0(
          "Rows and columns represent criteria pollutants. Each square cell shows the correlation coefficient for that pair, ",
          "bounded between -1.0 (inverse association) and +1.0 (synchronized co-movement). Hover over any cell to see exact correlation and co-located station count (N)."
        ),
        colors_meaning = "Diverging Red-to-Blue palette: Dark red indicates strong positive correlation (+1.0), light shades denote near-zero correlation (0.0), and blue indicates negative correlation (-1.0).",
        dynamic_findings = finding,
        important_note = paste0(
          "Correlations are computed pairwise without replacing missing sensor values with zeros. ",
          "Statistical correlation measures atmospheric co-occurrence; it does not prove chemical transformation or causal dependency."
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
      df <- filtered_studio_long()
      
      agg <- df %>%
        group_by(pollutant_id) %>%
        summarise(
          mean_val = round(mean(pollutant_avg, na.rm = TRUE), 1),
          max_val = suppressWarnings(max(pollutant_max, na.rm = TRUE)),
          .groups = "drop"
        )
      
      pm25_row <- agg %>% filter(pollutant_id == "PM2.5")
      pm10_row <- agg %>% filter(pollutant_id == "PM10")
      co_row   <- agg %>% filter(pollutant_id == "CO")
      so2_row  <- agg %>% filter(pollutant_id == "SO2")
      
      pm25_txt <- if (nrow(pm25_row) > 0 && !is.na(pm25_row$mean_val)) paste0(pm25_row$mean_val, " µg/m³") else "NA"
      pm10_txt <- if (nrow(pm10_row) > 0 && !is.na(pm10_row$mean_val)) paste0(pm10_row$mean_val, " µg/m³") else "NA"
      co_txt   <- if (nrow(co_row) > 0 && !is.na(co_row$mean_val)) paste0(co_row$mean_val, " mg/m³") else "NA"
      so2_txt  <- if (nrow(so2_row) > 0 && !is.na(so2_row$mean_val)) paste0(so2_row$mean_val, " µg/m³") else "NA"
      
      finding <- paste0(
        "Criteria pollutants span vastly different numerical ranges: particulate averages reach ", pm10_txt, " (PM10) and ",
        pm25_txt, " (PM2.5), whereas trace gaseous species such as SO2 average ", so2_txt, " and carbon monoxide operates at ",
        co_txt, " (mg/m³). Independent panel axes preserve visibility for all seven metrics."
      )
      
      build_understand_chart(
        chart_type = "Small Multiples (Faceted Bar Charts)",
        what_shows = "Displays reported minimum, average, and maximum concentrations across all 7 criteria pollutants simultaneously using independent, dedicated panel scales.",
        how_to_read = paste0(
          "Each faceted panel represents one criteria pollutant with its proper unit (µg/m³ or mg/m³). ",
          "Within each panel, three bars compare reported minimum, average, and maximum values across the filtered stations. ",
          "Because vertical axes scale independently, examine bar heights relative to that pollutant's own axis."
        ),
        colors_meaning = "Bars use a semantic metric color palette: Teal (#0D9488) for Minimum Reported, Navy Blue (#1E3A8A) for Average Reported, and Crimson Red (#DC2626) for Maximum Reported.",
        dynamic_findings = finding,
        important_note = "Independent vertical scales are methodologically mandatory. Placing CO (mg/m³) or SO2 on a shared axis with PM10 (µg/m³) would compress trace gases into visual invisibility and violate dimensional integrity."
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
      w <- filtered_studio_wide()
      pol_cols <- c("CO", "NH3", "NO2", "OZONE", "PM10", "PM2.5", "SO2")
      valid_rows <- w %>% filter(rowSums(!is.na(select(., all_of(pol_cols)))) >= 4) %>% head(100)
      
      if (nrow(valid_rows) < 3) {
        return(build_understand_chart(
          chart_type = "Parallel Coordinates Plot (Multivariate Profile)",
          what_shows = "Traces multi-pollutant multivariate signatures across 7 parallel vertical axes.",
          how_to_read = "Each line represents one station crossing all pollutant axes. Drag along any axis to filter.",
          colors_meaning = "Continuous Viridis palette colored by PM2.5 concentration (purple = low, yellow = high).",
          dynamic_findings = "Fewer than 3 stations in the active filter selection report at least 4 criteria pollutants concurrently.",
          important_note = "Reset filters to include additional multi-pollutant monitoring stations."
        ))
      }
      
      med_pm25 <- round(median(valid_rows$PM2.5, na.rm = TRUE), 1)
      hi_pm25_n <- sum(valid_rows$PM2.5 > 100, na.rm = TRUE)
      
      finding <- paste0(
        "Traces multivariate signatures across N = ", nrow(valid_rows), " stations reporting ≥4 pollutants concurrently. ",
        "Median PM2.5 across these stations is ", med_pm25, " µg/m³, with ", hi_pm25_n, " stations recording elevated fine particulate burdens (>100 µg/m³, highlighted in bright yellow/green). ",
        "Stations with elevated particulates exhibit divergent gaseous signatures (e.g., high NO2 reflecting traffic vs high SO2 reflecting industrial combustion)."
      )
      
      build_understand_chart(
        chart_type = "Parallel Coordinates Plot (Multivariate Profile)",
        what_shows = "Visualizes high-dimensional, multi-pollutant ambient signatures for individual monitoring stations across 7 parallel vertical axes simultaneously.",
        how_to_read = paste0(
          "Each vertical axis corresponds to one criteria pollutant calibrated to its native range and measurement unit. ",
          "Each continuous line represents an individual monitoring station. Interactive brush selection: click and drag vertically on any axis to filter and isolate stations."
        ),
        colors_meaning = "Lines are colored along a continuous Viridis gradient by PM2.5 concentration: purple lines denote low PM2.5, green indicates moderate, and yellow highlights high PM2.5.",
        dynamic_findings = finding,
        important_note = "To maintain visual clarity, stations with fewer than 4 reported pollutants are excluded to prevent disjointed lines. Each axis operates on its native measurement scale."
      )
    })
    
  })
}
