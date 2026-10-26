# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/cpcb_subindex.R
# Purpose: Official CPCB sub-index breakpoint calculations with strict
#          methodological disclosure and eligibility checks.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# CPCB AQI Categories and Colors
AQI_CATEGORIES <- c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe")
AQI_COLORS <- c(
  "Good" = "#00B050",         # Green
  "Satisfactory" = "#92D050", # Light Green
  "Moderate" = "#FFFF00",     # Yellow
  "Poor" = "#FF7C80",         # Orange / Light Red
  "Very Poor" = "#C00000",    # Red
  "Severe" = "#7030A0"        # Purple / Dark Maroon
)

#' Get category label for a given numeric sub-index
get_aqi_category <- function(index_val) {
  if (is.na(index_val)) return(NA_character_)
  if (index_val <= 50) return("Good")
  if (index_val <= 100) return("Satisfactory")
  if (index_val <= 200) return("Moderate")
  if (index_val <= 300) return("Poor")
  if (index_val <= 400) return("Very Poor")
  return("Severe")
}

#' Official CPCB Sub-Index Linear Interpolation
#' Formula: I = I_lo + ((I_hi - I_lo) / (B_hi - B_lo)) * (C - B_lo)
calc_single_subindex <- function(pollutant, conc) {
  if (is.na(conc) || conc < 0) return(NA_real_)
  
  # Standard CPCB Breakpoints: c(B_lo, B_hi, I_lo, I_hi)
  breakpoints <- list(
    "PM2.5" = list(
      c(0, 30, 0, 50),
      c(30, 60, 51, 100),
      c(60, 90, 101, 200),
      c(90, 120, 201, 300),
      c(120, 250, 301, 400),
      c(250, 500, 401, 500)
    ),
    "PM10" = list(
      c(0, 50, 0, 50),
      c(50, 100, 51, 100),
      c(100, 250, 101, 200),
      c(250, 350, 201, 300),
      c(350, 430, 301, 400),
      c(430, 600, 401, 500)
    ),
    "NO2" = list(
      c(0, 40, 0, 50),
      c(40, 80, 51, 100),
      c(80, 180, 101, 200),
      c(180, 280, 201, 300),
      c(280, 400, 301, 400),
      c(400, 800, 401, 500)
    ),
    "SO2" = list(
      c(0, 40, 0, 50),
      c(40, 80, 51, 100),
      c(80, 380, 101, 200),
      c(380, 800, 201, 300),
      c(800, 1600, 301, 400),
      c(1600, 2000, 401, 500)
    ),
    "NH3" = list(
      c(0, 200, 0, 50),
      c(200, 400, 51, 100),
      c(400, 800, 101, 200),
      c(800, 1200, 201, 300),
      c(1200, 1800, 301, 400),
      c(1800, 2400, 401, 500)
    ),
    "OZONE" = list(
      c(0, 50, 0, 50),
      c(50, 100, 51, 100),
      c(100, 168, 101, 200),
      c(168, 208, 201, 300),
      c(208, 748, 301, 400),
      c(748, 1000, 401, 500)
    )
  )
  
  pol <- toupper(pollutant)
  if (!pol %in% names(breakpoints)) {
    # If CO or other pollutant lacks unambiguous linear concentration mapping, return NA
    return(NA_real_)
  }
  
  tiers <- breakpoints[[pol]]
  for (t in tiers) {
    b_lo <- t[1]; b_hi <- t[2]; i_lo <- t[3]; i_hi <- t[4]
    if (conc >= b_lo && conc <= b_hi) {
      subindex <- i_lo + ((i_hi - i_lo) / (b_hi - b_lo)) * (conc - b_lo)
      return(round(subindex, 0))
    }
  }
  
  # If concentration exceeds highest tier upper bound
  if (conc > tiers[[length(tiers)]][2]) {
    return(500)
  }
  
  return(NA_real_)
}

#' Compute sub-indices across an aligned dataset with CPCB eligibility checking
#' 
#' Note on CPCB Minimum Data Criteria:
#' CPCB requires at least 3 pollutants to be monitored, one of which MUST be either
#' PM2.5 or PM10. Without this, no overall sub-index maximum is declared.
compute_cpcb_subindices <- function(wide_df) {
  res <- wide_df
  
  # Calculate sub-indices for supported pollutants
  res$sub_PM25 <- sapply(res$PM2.5, function(x) calc_single_subindex("PM2.5", x))
  res$sub_PM10 <- sapply(res$PM10, function(x) calc_single_subindex("PM10", x))
  res$sub_NO2  <- sapply(res$NO2,  function(x) calc_single_subindex("NO2", x))
  res$sub_SO2  <- sapply(res$SO2,  function(x) calc_single_subindex("SO2", x))
  res$sub_NH3  <- sapply(res$NH3,  function(x) calc_single_subindex("NH3", x))
  res$sub_O3   <- sapply(res$OZONE, function(x) calc_single_subindex("OZONE", x))
  
  sub_cols <- c("sub_PM25", "sub_PM10", "sub_NO2", "sub_SO2", "sub_NH3", "sub_O3")
  
  # Check CPCB eligibility row by row
  indicative_aqi <- rep(NA_real_, nrow(res))
  dominant_pollutant <- rep(NA_character_, nrow(res))
  eligible_status <- rep(FALSE, nrow(res))
  
  for (i in seq_len(nrow(res))) {
    # Count valid measured pollutants
    has_pm25 <- !is.na(res$PM2.5[i])
    has_pm10 <- !is.na(res$PM10[i])
    has_particulate <- (has_pm25 || has_pm10)
    
    # Count total non-NA pollutants monitored at this station
    monitored_count <- sum(!is.na(c(res$PM2.5[i], res$PM10[i], res$NO2[i], res$SO2[i], res$CO[i], res$OZONE[i], res$NH3[i])))
    
    # Check CPCB criteria: >= 3 pollutants and at least one particulate
    if (monitored_count >= 3 && has_particulate) {
      eligible_status[i] <- TRUE
      row_subs <- c(
        "PM2.5" = res$sub_PM25[i],
        "PM10"  = res$sub_PM10[i],
        "NO2"   = res$sub_NO2[i],
        "SO2"   = res$sub_SO2[i],
        "NH3"   = res$sub_NH3[i],
        "OZONE" = res$sub_O3[i]
      )
      valid_subs <- row_subs[!is.na(row_subs)]
      if (length(valid_subs) > 0) {
        max_val <- max(valid_subs)
        indicative_aqi[i] <- max_val
        dominant_pollutant[i] <- names(valid_subs)[which.max(valid_subs)]
      }
    }
  }
  
  res$cpcb_eligible <- eligible_status
  res$indicative_aqi <- indicative_aqi
  res$dominant_pollutant <- dominant_pollutant
  res$indicative_category <- sapply(res$indicative_aqi, get_aqi_category)
  
  return(res)
}
