# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: global.R
# Purpose: Global configurations, package verification, data loading,
#          and helper initialization for the Shiny application.
# ==============================================================================

# 1. Package Verification
source("R/package_check.R")
pkg_check <- check_required_packages()

# 2. Load Core Libraries
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyWidgets)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(stringr)
  library(scales)
  library(viridis)
  library(corrplot)
  library(httr)
  library(jsonlite)
})

# Explicitly ensure plotly::config is not masked by httr::config (which returns S3 'request')
config <- plotly::config

# 3. Source Core Analytical & Loading Scripts
source("R/data_loader.R")
source("R/data_validator.R")
source("R/cpcb_subindex.R")
source("R/data_collector.R")

# 4. Global Variables & Baseline Data Ingestion
# Load genuine CPCB dataset from data/air_quality.csv
LOADED_DATASET <- load_genuine_air_quality()
BASE_DATA <- LOADED_DATASET$data
METADATA <- LOADED_DATASET$metadata

# 5. Preprocessing & Validation Audits
COORD_AUDIT <- validate_coordinates(BASE_DATA)
NA_AUDIT <- audit_missing_values(BASE_DATA)
ANOMALY_AUDIT <- detect_anomalies(BASE_DATA)
WIDE_DATA_OBJ <- create_aligned_wide_data(BASE_DATA)
WIDE_DATA <- WIDE_DATA_OBJ$wide_data

# Calculate CPCB indicative sub-indices on wide data
WIDE_WITH_SUBINDICES <- compute_cpcb_subindices(WIDE_DATA)

# 6. Source UI and Server Modules (Now that baseline datasets are initialized)
source("R/mod_overview.R")
source("R/mod_map.R")
source("R/mod_viz_studio.R")
source("R/mod_quality.R")
source("R/mod_trends.R")
source("R/mod_reports.R")

# Standard Pollutant Definitions and Units
POLLUTANT_UNITS <- c(
  "PM2.5" = "µg/m³ (24-hr avg)",
  "PM10"  = "µg/m³ (24-hr avg)",
  "NO2"   = "µg/m³ (24-hr avg)",
  "SO2"   = "µg/m³ (24-hr avg)",
  "CO"    = "mg/m³ / reported sub-index (8-hr avg)",
  "OZONE" = "µg/m³ (8-hr avg)",
  "NH3"   = "µg/m³ (24-hr avg)"
)

# Application Theme using bslib
APP_THEME <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1E3A8A",
  secondary = "#475569",
  success = "#059669",
  warning = "#D97706",
  danger = "#DC2626",
  base_font = font_google("Inter")
)
