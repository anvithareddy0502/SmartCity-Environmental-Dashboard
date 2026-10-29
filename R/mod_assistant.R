# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/mod_assistant.R
# Purpose: Shiny UI & Server Module for the Environmental Data Assistant
#          - Clean modern chat layout with multiline input (Enter / Shift+Enter)
#          - Subtle analysis loading indicator
#          - Expandable calculation evidence & methodology details
#          - Multi-part compound visualization tabs & records
#          - Ambiguity clarification buttons & conversational context inspector
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(plotly)
  library(DT)
  library(stringr)
})

# ------------------------------------------------------------------------------
# 1. Assistant UI
# ------------------------------------------------------------------------------

mod_assistant_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Client-side JavaScript for Enter to send and Shift+Enter for new line
    tags$script(HTML(sprintf("
      $(document).on('keydown', '#%s', function(e) {
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault();
          $('#%s').click();
        }
      });
    ", ns("user_query_input"), ns("btn_submit_query")))),
    
    div(
      class = "container-fluid py-3",
      
      # Clean Page Header
      div(
        class = "page-header-container mb-3",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          div(
            h4(class = "page-header-title", "Environmental Data Assistant"),
            p(class = "page-header-sub", "Natural language queries grounded strictly in CPCB CAAQM baseline observations.")
          ),
          div(
            class = "d-flex align-items-center gap-2",
            span(
              class = "status-pill-subtle",
              span(class = "status-dot-subtle"),
              "Offline Snapshot: 23-Oct-2026 18:00 IST"
            ),
            actionButton(
              inputId = ns("btn_show_engine_info"),
              label = "Engine Status",
              icon = icon("circle-info"),
              class = "btn btn-sm btn-outline-secondary"
            )
          )
        )
      ),
      
      fluidRow(
        # Main Chat Column (Width 8)
        column(
          width = 8,
          div(
            class = "card p-3 shadow-sm border-0 mb-3 bg-white",
            
            # Optional Example Prompts (Clean single row of pill buttons)
            div(
              class = "mb-3",
              div(class = "small text-secondary fw-semibold mb-2", "Suggested Inquiries (Click to explore):"),
              div(
                class = "d-flex flex-wrap gap-2",
                actionButton(
                  inputId = ns("chip_pm25_hotspots"),
                  label = "Top 5 PM2.5 in Delhi",
                  class = "btn assistant-pill-btn"
                ),
                actionButton(
                  inputId = ns("chip_compare_delhi_mh"),
                  label = "Compare Delhi vs Maharashtra (PM10)",
                  class = "btn assistant-pill-btn"
                ),
                actionButton(
                  inputId = ns("chip_completeness"),
                  label = "Missing Readings & Completeness",
                  class = "btn assistant-pill-btn"
                ),
                actionButton(
                  inputId = ns("chip_compound_query"),
                  label = "Multi-part: Top PM2.5 State & Missing Data",
                  class = "btn assistant-pill-btn"
                )
              )
            ),
            
            hr(class = "my-2 text-muted"),
            
            # Chat Thread Container
            div(
              id = ns("chat_history_container"),
              style = "max-height: 480px; min-height: 280px; overflow-y: auto; padding: 10px 5px;",
              uiOutput(ns("chat_messages_ui"))
            ),
            
            # Subtle Loading / Analyzing Indicator
            uiOutput(ns("loading_indicator_ui")),
            
            hr(class = "my-2 text-muted"),
            
            # Multiline Natural Language Input Group
            div(
              class = "input-group mt-2",
              tags$textarea(
                id = ns("user_query_input"),
                class = "form-control assistant-textarea",
                placeholder = "Ask any question about pollutants, stations, cities, comparisons, or data quality (Enter to send, Shift+Enter for new line)...",
                rows = 2
              ),
              actionButton(
                inputId = ns("btn_submit_query"),
                label = "Send",
                icon = icon("paper-plane"),
                class = "btn btn-primary assistant-send-btn"
              )
            ),
            
            # Understated footer controls
            div(
              class = "d-flex justify-content-between align-items-center mt-2 small text-muted",
              span("Tip: Enter to send • Shift+Enter for new line • Follow-ups inherit active context."),
              actionLink(ns("btn_clear_chat"), "Clear chat history", icon = icon("trash-can"), class = "text-muted small")
            )
          ),
          
          # Dynamic Accompanying Visualizations & Records Card
          uiOutput(ns("assistant_visual_card"))
        ),
        
        # Sidebar: Conversational Context & Capability Boundaries (Width 4)
        column(
          width = 4,
          div(
            class = "card p-3 shadow-sm border-0 mb-3 bg-white",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h6(class = "card-title m-0", icon("sliders", class = "me-2 text-primary"), "Conversational Context"),
              span(class = "badge bg-light text-secondary border small", "Multi-turn State")
            ),
            p(class = "small text-muted mb-2", "The assistant retains filters and entities to resolve follow-up questions."),
            uiOutput(ns("context_inspector_ui")),
            hr(class = "my-2 text-muted"),
            actionButton(
              inputId = ns("btn_reset_context"),
              label = "Reset Context",
              icon = icon("rotate-left"),
              class = "btn btn-sm btn-outline-secondary w-100"
            )
          ),
          
          div(
            class = "card p-3 shadow-sm border-0 mb-3 bg-white",
            h6(class = "card-title m-0 mb-2", icon("shield-halved", class = "me-2 text-primary"), "Reliability & Safety Guarantees"),
            tags$ul(
              class = "small text-muted ps-3 mb-0",
              tags$li(strong("Zero Arbitrary Code: "), "Natural language is translated into verified immutable dplyr execution plans. Code injection is impossible."),
              tags$li(strong("Grounding: "), "Every number is calculated directly from 3,682 genuine CAAQM records. Nothing is hallucinated."),
              tags$li(strong("Data Fidelity: "), "Missing values (328 NAs) are preserved and never silently replaced with zero."),
              tags$li(strong("Ambiguity Clarification: "), "When queries have multiple plausible interpretations, the assistant asks for clarification rather than guessing."),
              tags$li(strong("Domain Boundaries: "), "Predictive weather forecasting and causal liability blaming are explicitly disclosed as unsupported.")
            )
          ),
          
          div(
            class = "card p-3 shadow-sm border-0 bg-white",
            h6(class = "card-title m-0 mb-2", icon("circle-question", class = "me-2 text-primary"), "Supported Question Patterns"),
            tags$div(
              class = "small text-muted",
              tags$div(class = "mb-1", strong("• Multi-part Questions: "), "Combines multiple analytical requirements in one prompt"),
              tags$div(class = "mb-1", strong("• Rankings: "), "Top or lowest stations, cities, or states for any pollutant"),
              tags$div(class = "mb-1", strong("• Comparisons: "), "Compare 2+ states, cities, or co-located PM2.5/PM10 ratio"),
              tags$div(class = "mb-1", strong("• Data Quality: "), "Reporting completeness % and missing measurement audits"),
              tags$div(class = "mb-1", strong("• Anomalies: "), "Tukey 1.5×IQR statistical outliers and zero flatlines"),
              tags$div(class = "mb-0", strong("• CPCB AQI: "), "Sub-index category distribution and station eligibility")
            )
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# 2. Assistant Server
# ------------------------------------------------------------------------------

mod_assistant_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Session state
    rv <- reactiveValues(
      messages = list(),
      context = list(pollutant = "PM2.5", state = NULL, city = NULL, last_operation = NULL),
      latest_result = NULL,
      is_analyzing = FALSE
    )
    
    # Initialize welcome message
    observe({
      msgs <- isolate(rv$messages)
      if (length(msgs) == 0) {
        welcome_res <- exec_op_guide(list())
        rv$messages <- list(
          list(
            role = "assistant",
            time = format(Sys.time(), "%H:%M"),
            text = welcome_res$text,
            evidence = list(
              status = "Operational",
              dataset = "CPCB CAAQM Baseline Snapshot (23-Oct-2026 18:00 IST)",
              monitored_stations = 526,
              cities_represented = 273,
              states_represented = 31
            ),
            options = NULL,
            has_visual = FALSE
          )
        )
      }
    })
    
    # Core Function: Process User Natural Language Query
    process_user_query <- function(user_text) {
      clean_input <- trimws(user_text)
      if (!nzchar(clean_input)) return()
      
      # 1. Record User Message
      user_msg <- list(
        role = "user",
        time = format(Sys.time(), "%H:%M"),
        text = clean_input,
        evidence = NULL,
        options = NULL,
        has_visual = FALSE
      )
      rv$messages <- c(rv$messages, list(user_msg))
      rv$is_analyzing <- TRUE
      
      # 2. Execute Master Assistant Pipeline
      result <- execute_assistant_pipeline(
        query_text = clean_input,
        context = rv$context,
        base_data = BASE_DATA,
        wide_data = WIDE_WITH_SUBINDICES,
        metadata = METADATA
      )
      
      rv$is_analyzing <- FALSE
      if (is.null(result)) return()
      
      # 3. Update Conversational Context
      if (!is.null(result$context)) {
        rv$context <- result$context
      }
      rv$latest_result <- result
      
      # 4. Record Assistant Response
      has_vis <- (length(result$plots) > 0 || length(result$tables) > 0)
      asst_msg <- list(
        role = "assistant",
        time = format(Sys.time(), "%H:%M"),
        text = result$text,
        evidence = result$evidence,
        options = result$options,
        has_visual = has_vis
      )
      rv$messages <- c(rv$messages, list(asst_msg))
      
      # Auto-scroll chat to bottom
      scroll_js <- sprintf("
        var elem = document.getElementById('%s');
        if (elem) elem.scrollTop = elem.scrollHeight;
      ", ns("chat_history_container"))
      shiny::isolate(session$sendCustomMessage("eval_js", scroll_js))
    }
    
    # Submit Query Event
    observeEvent(input$btn_submit_query, {
      req(input$user_query_input)
      process_user_query(input$user_query_input)
      updateTextAreaInput(session, "user_query_input", value = "")
    })
    
    # Prompt Chip Click Handlers
    observeEvent(input$chip_pm25_hotspots, {
      process_user_query("Which stations have the highest PM2.5 in Delhi?")
    })
    
    observeEvent(input$chip_compare_delhi_mh, {
      process_user_query("Compare Delhi and Maharashtra for PM10 and explain the difference.")
    })
    
    observeEvent(input$chip_completeness, {
      process_user_query("How many readings are missing and what is the reporting completeness?")
    })
    
    observeEvent(input$chip_compound_query, {
      process_user_query("Which state has the highest PM2.5 average, which city has the highest PM10 reading, and how many readings are missing?")
    })
    
    # Reset Chat History
    observeEvent(input$btn_clear_chat, {
      rv$messages <- list()
      rv$latest_result <- NULL
      rv$context <- list(pollutant = "PM2.5", state = NULL, city = NULL, last_operation = NULL)
    })
    
    # Reset Context Only
    observeEvent(input$btn_reset_context, {
      rv$context <- list(pollutant = "PM2.5", state = NULL, city = NULL, last_operation = NULL)
      showNotification("Conversational context reset to national baseline.", type = "message", duration = 3)
    })
    
    # Clarification Option Click Handler
    observeEvent(input$clarification_selected, {
      req(input$clarification_selected)
      process_user_query(input$clarification_selected)
    })
    
    # Engine Status Modal
    observeEvent(input$btn_show_engine_info, {
      has_gemini <- nzchar(Sys.getenv("GEMINI_API_KEY"))
      has_openai <- nzchar(Sys.getenv("OPENAI_API_KEY"))
      
      showModal(modalDialog(
        title = div(icon("microchip", class = "me-2 text-primary"), "Environmental Data Assistant Engine Configuration"),
        div(
          class = "small",
          p("The assistant employs a dual-engine architecture prioritizing zero-cost reliability, offline privacy, and mathematical accuracy:"),
          tags$table(
            class = "table table-sm table-bordered",
            tags$tr(
              tags$td(class = "fw-semibold", "Active Primary Engine"),
              tags$td(
                if (has_gemini) span(class = "badge bg-success", "Gemini API Plan Translation Active")
                else if (has_openai) span(class = "badge bg-success", "OpenAI API Plan Translation Active")
                else span(class = "badge bg-primary", "Deterministic Semantic Parser Active (Offline / Zero-API)")
              )
            ),
            tags$tr(
              tags$td(class = "fw-semibold", "Execution Pipeline"),
              tags$td("Pure R & dplyr analytical execution over genuine CAAQM baseline")
            ),
            tags$tr(
              tags$td(class = "fw-semibold", "Arbitrary Code Injection"),
              tags$td(span(class = "badge bg-danger", "Strictly Blocked / Disallowed"))
            ),
            tags$tr(
              tags$td(class = "fw-semibold", "Dataset Provenance"),
              tags$td("CPCB data.gov.in snapshot (23-Oct-2026 18:00 IST)")
            )
          ),
          p(class = "text-muted mb-0", "To enable cloud AI plan translation, set the environment variable GEMINI_API_KEY or OPENAI_API_KEY in your R session. The engine automatically runs in offline deterministic mode when no key is set.")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # --------------------------------------------------------------------------
    # Renderers
    # --------------------------------------------------------------------------
    
    # Render Active Conversational Context Inspector
    output$context_inspector_ui <- renderUI({
      ctx <- rv$context
      tags$div(
        class = "small",
        tags$table(
          class = "table table-sm table-borderless m-0",
          tags$tr(
            tags$td(class = "text-muted", "Active Pollutant:"),
            tags$td(class = "text-end fw-bold", if (!is.null(ctx$pollutant)) span(class = "badge bg-primary text-white", ctx$pollutant) else "PM2.5 (Default)")
          ),
          tags$tr(
            tags$td(class = "text-muted", "Active State / UT:"),
            tags$td(class = "text-end fw-bold", if (!is.null(ctx$state)) span(class = "badge bg-light text-dark border", ctx$state) else "National (All)")
          ),
          tags$tr(
            tags$td(class = "text-muted", "Active City:"),
            tags$td(class = "text-end fw-bold", if (!is.null(ctx$city)) span(class = "badge bg-light text-dark border", ctx$city) else "All Cities")
          ),
          tags$tr(
            tags$td(class = "text-muted", "Last Operation:"),
            tags$td(class = "text-end font-monospace small", if (!is.null(ctx$last_operation)) ctx$last_operation else "Initial Guide")
          )
        )
      )
    })
    
    # Render Typing / Loading Indicator
    output$loading_indicator_ui <- renderUI({
      if (!isTRUE(rv$is_analyzing)) return(NULL)
      div(
        class = "assistant-typing-indicator",
        span(class = "dot"), span(class = "dot"), span(class = "dot"),
        span(class = "ms-2 small text-muted", "Evaluating CAAQM observations across analytical pipelines...")
      )
    })
    
    # Render Chat Thread
    output$chat_messages_ui <- renderUI({
      msgs <- rv$messages
      if (length(msgs) == 0) return(NULL)
      
      tagList(
        lapply(seq_along(msgs), function(idx) {
          m <- msgs[[idx]]
          is_user <- identical(m$role, "user")
          
          if (is_user) {
            # User Speech Bubble (Clean dark blue, right-aligned)
            div(
              class = "d-flex justify-content-end mb-3",
              div(
                style = "max-width: 82%;",
                div(
                  class = "p-2 px-3 rounded text-white bg-primary shadow-sm",
                  style = "border-radius: 12px 12px 2px 12px !important; font-size: 0.9rem; line-height: 1.45;",
                  m$text
                ),
                div(class = "text-end text-muted mt-1", style = "font-size: 0.72rem;", m$time)
              )
            )
          } else {
            # Assistant Response Card (Light card with left accent border)
            div(
              class = "d-flex justify-content-start mb-3",
              div(
                style = "max-width: 92%; width: 100%;",
                div(
                  class = "card p-3 shadow-sm border bg-light",
                  style = "border-radius: 12px 12px 12px 2px !important; border-left: 3px solid #1E3A8A !important;",
                  
                  # Header row
                  div(
                    class = "d-flex justify-content-between align-items-center mb-2",
                    span(class = "fw-bold small text-primary", icon("robot", class = "me-1"), "Environmental Data Assistant"),
                    span(class = "text-muted", style = "font-size: 0.72rem;", m$time)
                  ),
                  
                  # Concise Answer Text
                  div(
                    style = "font-size: 0.88rem; line-height: 1.5; color: #1E293B;",
                    HTML(m$text)
                  ),
                  
                  # Clarification Options (if applicable)
                  if (!is.null(m$options) && length(m$options) > 0) {
                    div(
                      class = "mt-3 p-2 bg-white rounded border",
                      div(class = "small fw-semibold text-secondary mb-2", icon("hand-pointer", class = "me-1 text-primary"), "Select a clarifying option to proceed:"),
                      div(
                        class = "d-flex flex-wrap gap-2",
                        lapply(m$options, function(opt) {
                          actionButton(
                            inputId = ns(paste0("btn_opt_", abs(digest::digest2int(paste0(idx, opt))))),
                            label = opt,
                            class = "btn btn-sm btn-outline-primary",
                            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("clarification_selected"), opt)
                          )
                        })
                      )
                    )
                  },
                  
                  # Expandable Calculation Evidence & Provenance Details
                  if (!is.null(m$evidence)) {
                    tags$details(
                      class = "assistant-evidence-details mt-2 pt-2 border-top",
                      tags$summary(
                        class = "text-secondary small fw-semibold",
                        style = "cursor: pointer; font-size: 0.78rem;",
                        icon("magnifying-glass-chart", class = "me-1 text-primary"),
                        "View Calculation Evidence & Methodology Details"
                      ),
                      div(
                        class = "mt-2 p-2 bg-white rounded border small",
                        style = "font-size: 0.75rem; color: #475569;",
                        tags$div(
                          class = "d-flex flex-wrap gap-1 mb-1",
                          lapply(names(m$evidence), function(k) {
                            val <- m$evidence[[k]]
                            if (length(val) == 1 && nzchar(as.character(val))) {
                              span(
                                class = "badge bg-light text-secondary border",
                                paste0(str_replace_all(k, "_", " "), ": ", val)
                              )
                            }
                          })
                        )
                      )
                    )
                  },
                  
                  # Visual Loaded Pill
                  if (m$has_visual) {
                    div(
                      class = "mt-2 text-primary small fw-semibold",
                      icon("chart-column", class = "me-1"), "Interactive visual and record table loaded below ↓"
                    )
                  }
                )
              )
            )
          }
        })
      )
    })
    
    # Render Accompanying Visualizations & Records Card
    output$assistant_visual_card <- renderUI({
      res <- rv$latest_result
      if (is.null(res)) return(NULL)
      
      plots_list <- res$plots
      tables_list <- res$tables
      
      if (length(plots_list) == 0 && length(tables_list) == 0) return(NULL)
      
      div(
        class = "card p-3 shadow-sm border-0 bg-white",
        div(
          class = "d-flex justify-content-between align-items-center mb-2",
          h6(class = "card-title m-0", icon("chart-simple", class = "me-2 text-primary"), "Accompanying Visualizations & Data Records"),
          span(class = "badge bg-light text-secondary border", paste(length(plots_list), "Chart(s) |", length(tables_list), "Table(s)"))
        ),
        
        # Build Navigation Tabs for Visuals
        navset_card_tab(
          if (length(plots_list) > 0) {
            nav_panel(
              title = "Interactive Visualization",
              icon = icon("chart-bar"),
              plotlyOutput(ns("assistant_plotly_chart"), height = "360px")
            )
          },
          if (length(tables_list) > 0) {
            nav_panel(
              title = "Data Records Table",
              icon = icon("table"),
              div(class = "py-1", DTOutput(ns("assistant_dt_table")))
            )
          }
        )
      )
    })
    
    # Render Active Plotly Chart
    output$assistant_plotly_chart <- renderPlotly({
      res <- rv$latest_result
      req(res, length(res$plots) > 0)
      res$plots[[1]]
    })
    
    # Render Active DT Table
    output$assistant_dt_table <- renderDT({
      res <- rv$latest_result
      req(res, length(res$tables) > 0)
      datatable(
        res$tables[[1]],
        options = list(
          pageLength = 8,
          dom = "tip",
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = "table table-sm table-striped table-hover small"
      )
    })
    
  })
}
