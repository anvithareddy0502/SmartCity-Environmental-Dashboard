# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: app.R
# Purpose: Main application entry point for RStudio and R command line.
# ==============================================================================

# Source global configuration and data structures
source("global.R")

# Source user interface
source("ui.R")

# Source server logic
source("server.R")

# Launch Shiny application
shinyApp(ui = ui, server = server)
