# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/package_check.R
# Purpose: Verifies required R packages and returns a clean diagnostic summary.
# ==============================================================================

check_required_packages <- function() {
  required_pkgs <- c(
    "shiny",
    "bslib",
    "shinyWidgets",
    "DT",
    "ggplot2",
    "plotly",
    "leaflet",
    "sf",
    "dplyr",
    "tidyr",
    "readr",
    "lubridate",
    "stringr",
    "scales",
    "viridis",
    "corrplot",
    "httr",
    "jsonlite",
    "curl"
  )
  
  installed <- rownames(installed.packages())
  missing_pkgs <- setdiff(required_pkgs, installed)
  
  status_df <- data.frame(
    package = required_pkgs,
    installed = required_pkgs %in% installed,
    stringsAsFactors = FALSE
  )
  
  if (length(missing_pkgs) > 0) {
    warning(paste(
      "The following required packages are missing:",
      paste(missing_pkgs, collapse = ", "),
      "\nPlease install them using: install.packages(c(" ,
      paste0("'", missing_pkgs, "'", collapse = ", "),
      "))"
    ))
  }
  
  return(list(
    all_installed = (length(missing_pkgs) == 0),
    missing = missing_pkgs,
    status = status_df
  ))
}
