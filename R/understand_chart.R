# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/understand_chart.R
# Purpose: Reusable 'Understand This Chart' component providing standardized,
#          dynamically computed pedagogical explanations underneath every
#          visualization across all six dashboard pages.
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(htmltools)
})

#' Generate a standardized 'Understand This Chart' explanation card
#'
#' @param chart_type Character string identifying the visualization type (e.g., 'Horizontal Bar Chart', 'Geographic Leaflet Map', 'Box Plot', etc.)
#' @param what_shows Plain English explanation of what information is presented and why this visualization type is appropriate.
#' @param how_to_read Clear guide to axes, bars, lines, points, shapes, legends, and hover interactions.
#' @param colors_meaning Accurate explanation of real colour encoding (gradients, CPCB categories, or neutral uniform styling).
#' @param dynamic_findings Dynamically calculated findings grounded strictly in currently displayed records, aggregations, and units.
#' @param important_note Disclosed scientific limitations (e.g., single snapshot vs 24-hr CPCB regulatory standard, units, missing data).
#' @param open_by_default Logical indicating whether the detailed guide is expanded by default (default FALSE).
#'
#' @return A Shiny tagList / div containing the styled explanation component.
build_understand_chart <- function(
  chart_type,
  what_shows,
  how_to_read,
  colors_meaning,
  dynamic_findings,
  important_note,
  open_by_default = FALSE
) {
  tags$div(
    class = "understand-chart-card mt-2",
    # Card Header
    tags$div(
      class = "d-flex justify-content-between align-items-center mb-1 pb-1 border-bottom",
      tags$div(
        class = "understand-chart-title d-flex align-items-center",
        tags$i(class = "fa-solid fa-circle-info text-primary me-2"),
        tags$span(style = "font-weight: 700; color: #1E3A8A; font-size: 0.82rem; letter-spacing: -0.01em;", "Understand This Chart"),
        tags$span(
          class = "badge bg-white text-secondary border ms-2",
          style = "font-weight: 500; font-size: 0.72rem;",
          chart_type
        )
      )
    ),
    
    # Primary Finding: What the results tell us (Immediate visible feedback)
    tags$div(
      class = "understand-chart-summary my-1",
      tags$span(style = "font-weight: 700; color: #0F172A; font-size: 0.81rem;", "What the results tell us: "),
      tags$span(style = "color: #334155; font-size: 0.81rem; line-height: 1.45;", dynamic_findings)
    ),
    
    # Expandable Deep-Dive: Chart Guide, How to Read, Colours & Limitations
    tags$details(
      class = "understand-chart-details mt-1",
      open = if (open_by_default) "open" else NULL,
      tags$summary(
        class = "text-primary small",
        style = "cursor: pointer; user-select: none; font-weight: 500; font-size: 0.77rem;",
        tags$span("How to read, colour meanings & methodology details")
      ),
      tags$div(
        class = "understand-chart-body pt-2 mt-1 border-top",
        style = "font-size: 0.78rem; line-height: 1.45; color: #475569;",
        
        # Section 1: What this chart shows
        tags$div(
          class = "mb-1",
          tags$strong(style = "color: #1E293B;", "What this chart shows: "),
          tags$span(what_shows)
        ),
        
        # Section 2: How to read it
        tags$div(
          class = "mb-1",
          tags$strong(style = "color: #1E293B;", "How to read it: "),
          tags$span(how_to_read)
        ),
        
        # Section 3: What the colours mean
        tags$div(
          class = "mb-1",
          tags$strong(style = "color: #1E293B;", "What the colours mean: "),
          tags$span(colors_meaning)
        ),
        
        # Section 4: Important note / limitations
        tags$div(
          class = "mb-0 text-secondary",
          style = "background: rgba(241, 245, 249, 0.7); padding: 4px 8px; border-radius: 4px; border-left: 3px solid #94A3B8; margin-top: 5px;",
          tags$strong(style = "color: #475569;", "Important note: "),
          tags$span(important_note)
        )
      )
    )
  )
}
