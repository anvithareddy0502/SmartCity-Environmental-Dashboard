# ==============================================================================
# Smart City Environmental Intelligence Dashboard
# File: R/assistant_engine.R
# Purpose: Advanced Natural Language Query (NLQ) & Analytical Engine
#          - Compound & multi-part question parsing
#          - Dual-engine: Configurable AI Language Model Planner + Deterministic Semantic Parser
#          - Strictly safe execution: Pure R & dplyr pipelines (zero arbitrary code execution)
#          - Evidence grounding, ambiguity clarification, and multi-turn context retention
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(scales)
  library(httr)
  library(jsonlite)
})

# ------------------------------------------------------------------------------
# 1. Dataset Schema & Capability Allowlist Registry
# ------------------------------------------------------------------------------

ASSISTANT_SCHEMA <- list(
  columns_base = c(
    "country", "state", "city", "station", "last_update",
    "latitude", "longitude", "pollutant_id", "pollutant_min",
    "pollutant_max", "pollutant_avg", "observation_time", "collection_time"
  ),
  columns_wide = c(
    "station", "city", "state", "latitude", "longitude",
    "PM2.5", "PM10", "NO2", "SO2", "CO", "OZONE", "NH3",
    "sub_PM25", "sub_PM10", "sub_NO2", "sub_SO2", "sub_NH3", "sub_O3",
    "indicative_aqi", "indicative_category", "dominant_pollutant", "cpcb_eligible"
  ),
  valid_pollutants = c("PM2.5", "PM10", "NO2", "SO2", "CO", "OZONE", "NH3"),
  pollutant_units = list(
    "PM2.5" = "µg/m³",
    "PM10"  = "µg/m³",
    "NO2"   = "µg/m³",
    "SO2"   = "µg/m³",
    "CO"    = "mg/m³",
    "OZONE" = "µg/m³",
    "NH3"   = "µg/m³"
  ),
  valid_operations = c(
    "rank", "aggregate", "compare", "compare_stations_pollutant",
    "ratio", "missing", "outlier", "flatline", "aqi_breakdown",
    "lookup", "temporal", "metadata", "clarification", "unsupported"
  ),
  valid_groupings = c("state", "city", "station", "pollutant_id", "category"),
  cpcb_categories = c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe"),
  snapshot_timestamp = "23-10-2026 18:00 IST",
  is_single_snapshot = TRUE
)

# ------------------------------------------------------------------------------
# 2. Entity Dictionaries & Canonical Mappings
# ------------------------------------------------------------------------------

POLLUTANT_PATTERNS <- list(
  "PM2.5" = c("pm2.5", "pm25", "pm 2.5", "pm2,5", "fine particulate", "fine particles", "pm 25"),
  "PM10"  = c("pm10", "pm 10", "respirable particulate", "coarse particles", "dust"),
  "NO2"   = c("no2", "nitrogen dioxide", "nitric oxide", "nitrogen oxides"),
  "SO2"   = c("so2", "sulfur dioxide", "sulphur dioxide"),
  "CO"    = c("co", "carbon monoxide"),
  "OZONE" = c("ozone", "o3", "tropospheric ozone"),
  "NH3"   = c("nh3", "ammonia")
)

STATE_ALIASES <- list(
  "Delhi" = c("delhi", "nct of delhi", "new delhi", "delhi ncr"),
  "Uttar Pradesh" = c("uttar pradesh", "up", "u.p."),
  "Tamil Nadu" = c("tamil nadu", "tamilnadu", "tn", "t.n."),
  "Maharashtra" = c("maharashtra", "mh"),
  "Karnataka" = c("karnataka", "ka"),
  "West Bengal" = c("west bengal", "wb", "w.b."),
  "Andhra Pradesh" = c("andhra pradesh", "ap", "a.p."),
  "Madhya Pradesh" = c("madhya pradesh", "mp", "m.p."),
  "Himachal Pradesh" = c("himachal pradesh", "hp", "h.p."),
  "Jammu and Kashmir" = c("jammu and kashmir", "jammu & kashmir", "j&k", "jk"),
  "Punjab" = c("punjab"),
  "Haryana" = c("haryana"),
  "Rajasthan" = c("rajasthan"),
  "Gujarat" = c("gujarat"),
  "Kerala" = c("kerala"),
  "Bihar" = c("bihar"),
  "Odisha" = c("odisha", "orissa"),
  "Telangana" = c("telangana"),
  "Assam" = c("assam"),
  "Jharkhand" = c("jharkhand"),
  "Chhattisgarh" = c("chhattisgarh", "chattisgarh"),
  "Uttarakhand" = c("uttarakhand", "uttaranchal"),
  "Goa" = c("goa"),
  "Chandigarh" = c("chandigarh"),
  "Puducherry" = c("puducherry", "pondicherry"),
  "Meghalaya" = c("meghalaya"),
  "Mizoram" = c("mizoram"),
  "Nagaland" = c("nagaland"),
  "Tripura" = c("tripura"),
  "Sikkim" = c("sikkim"),
  "Arunachal Pradesh" = c("arunachal pradesh")
)

CITY_ALIASES <- list(
  "Bengaluru" = c("bengaluru", "bangalore"),
  "Kolkata"   = c("kolkata", "calcutta"),
  "Mumbai"    = c("mumbai", "bombay"),
  "Chennai"   = c("chennai", "madras"),
  "Delhi"     = c("delhi", "new delhi"),
  "Gurugram"  = c("gurugram", "gurgaon"),
  "Noida"     = c("noida", "greater noida"),
  "Hyderabad" = c("hyderabad"),
  "Prayagraj" = c("prayagraj", "allahabad"),
  "Varanasi"  = c("varanasi", "benares", "banaras"),
  "Patna"     = c("patna"),
  "Lucknow"   = c("lucknow"),
  "Kanpur"    = c("kanpur"),
  "Jaipur"    = c("jaipur"),
  "Ahmedabad" = c("ahmedabad"),
  "Pune"      = c("pune"),
  "Chandigarh"= c("chandigarh")
)

# ------------------------------------------------------------------------------
# 3. Compound Question & Sub-Question Clause Splitter
# ------------------------------------------------------------------------------

#' Split compound user inquiries into discrete analytical clauses
#'
#' @param query_text Character string of the raw user query
#' @return A list with `clauses` (vector of sub-questions) and `format_directive` ("chart", "table", "auto")
split_compound_question <- function(query_text) {
  raw_q <- trimws(query_text)
  if (!nzchar(raw_q)) {
    return(list(clauses = character(0), format_directive = "auto"))
  }
  
  # Normalize newlines and excess whitespace
  clean_text <- str_replace_all(raw_q, "[\r\n\t]+", " ")
  
  # Detect global format directives (e.g., "show in a chart", "plot this")
  format_directive <- "auto"
  if (grepl("\\b(chart|plot|graph|visualize|bar chart|scatterplot)\\b", tolower(clean_text))) {
    format_directive <- "chart"
  } else if (grepl("\\b(table|list|dataframe|records|directory)\\b", tolower(clean_text))) {
    format_directive <- "table"
  }
  
  # Delimit on numbered lists: "1. ", "2. ", "(1)", "(a)"
  delimited <- str_replace_all(clean_text, "\\b(\\d+)\\.\\s+", " ||| ")
  delimited <- str_replace_all(delimited, "\\(([a-z0-9]+)\\)\\s*", " ||| ")
  
  # Delimit on punctuation terminators
  delimited <- str_replace_all(delimited, "[?;]", " ||| ")
  
  # Delimit on compound conjunction clauses:
  # ", and which", ", and how", ", and what", ", and compare", ", and explain", ", and identify", ", and list"
  conjunction_regexes <- c(
    ",\\s*and\\s+(which|what|how|where|who|compare|explain|list|show|identify|give|is|are|tell|also)",
    ",\\s*(which|what|how|where|who|compare|explain|list|show|identify|tell)",
    "\\band\\s+(which|what|how|where|who|compare|explain|list|show|identify|tell)\\b",
    "\\b(as well as|additionally|furthermore|also)\\b"
  )
  for (re in conjunction_regexes) {
    delimited <- str_replace_all(delimited, regex(re, ignore_case = TRUE), " ||| \\1")
  }
  
  raw_clauses <- unlist(str_split(delimited, "\\|\\|\\|"))
  cleaned_clauses <- trimws(raw_clauses)
  cleaned_clauses <- cleaned_clauses[nzchar(cleaned_clauses)]
  
  # Strip leading conjunctions
  cleaned_clauses <- str_replace(cleaned_clauses, "^(?i)(and|also|as well as)\\s+", "")
  cleaned_clauses <- trimws(cleaned_clauses)
  
  # Filter out clauses that are purely formatting requests (e.g. "show the results in a chart")
  analytical_clauses <- character(0)
  for (cl in cleaned_clauses) {
    if (grepl("^(?i)(show|display|present|plot)\\s+(the\\s+)?(results|data|output)?\\s*(in|as)?\\s*(a\\s+)?(chart|plot|graph|table)\\.?$", cl)) {
      if (grepl("chart|plot|graph", tolower(cl))) format_directive <- "chart"
      if (grepl("table", tolower(cl))) format_directive <- "table"
    } else if (nzchar(cl)) {
      analytical_clauses <- c(analytical_clauses, cl)
    }
  }
  
  # Expand multi-pollutant comparison clauses:
  # e.g., "compare Delhi and Maharashtra for PM2.5 and PM10" -> Sub-clause PM2.5, Sub-clause PM10
  expanded <- character(0)
  for (cl in analytical_clauses) {
    pol_matches <- unlist(str_extract_all(tolower(cl), "\\b(pm2\\.5|pm10|no2|so2|co|ozone|nh3)\\b"))
    pol_matches <- unique(pol_matches)
    
    if (length(pol_matches) >= 2 &&
        grepl("\\b(compare|versus|vs|difference|between)\\b", tolower(cl)) &&
        !grepl("\\b(ratio|correlation|scatter)\\b", tolower(cl))) {
      # Split by pollutant so each gets evaluated cleanly
      for (pm in pol_matches) {
        other_pols <- setdiff(pol_matches, pm)
        sub_cl <- cl
        for (op in other_pols) {
          sub_cl <- str_replace_all(sub_cl, regex(paste0("\\b(and\\s+)?", op, "\\b"), ignore_case = TRUE), "")
        }
        sub_cl <- str_replace_all(sub_cl, regex("(?i)\\bfor\\s+and\\b"), "for")
        sub_cl <- str_replace_all(sub_cl, regex("(?i)\\band\\s+and\\b"), "and")
        sub_cl <- trimws(str_replace_all(sub_cl, "\\s+", " "))
        expanded <- c(expanded, sub_cl)
      }
    } else {
      expanded <- c(expanded, cl)
    }
  }
  
  if (length(expanded) == 0) expanded <- clean_text
  
  list(
    clauses = expanded,
    format_directive = format_directive
  )
}

# ------------------------------------------------------------------------------
# 4. Entity Extraction (Per-Clause & Context-Aware)
# ------------------------------------------------------------------------------

#' Extract analytical entities from a clause with context inheritance
extract_clause_entities <- function(clause_text, parent_entities = list(), context = list(), all_states = NULL, all_cities = NULL) {
  raw_q <- tolower(clause_text)
  
  detected_pollutants <- character(0)
  detected_states <- character(0)
  detected_cities <- character(0)
  detected_limit <- 5L
  detected_direction <- "desc"
  detected_group_by <- NULL
  requested_metric <- "avg"
  
  # 4.1 Pollutants
  for (pol in names(POLLUTANT_PATTERNS)) {
    patterns <- POLLUTANT_PATTERNS[[pol]]
    if (any(sapply(patterns, function(p) grepl(paste0("\\b", p, "\\b"), raw_q, perl = TRUE)))) {
      detected_pollutants <- c(detected_pollutants, pol)
    }
  }
  detected_pollutants <- unique(detected_pollutants)
  
  # Inherit pollutant from parent or context if clause doesn't specify one
  primary_pollutant <- if (length(detected_pollutants) > 0) {
    detected_pollutants[1]
  } else if (!is.null(parent_entities$pollutant)) {
    parent_entities$pollutant
  } else if (!is.null(context$pollutant) &&
             !grepl("\\b(most polluted|worst|cleanest|best|highest pollution|lowest pollution)\\b", raw_q)) {
    context$pollutant
  } else {
    NULL
  }
  
  # 4.2 States
  for (st_name in names(STATE_ALIASES)) {
    aliases <- STATE_ALIASES[[st_name]]
    if (any(sapply(aliases, function(a) grepl(paste0("\\b", a, "\\b"), raw_q, perl = TRUE)))) {
      detected_states <- c(detected_states, st_name)
    }
  }
  if (!is.null(all_states)) {
    for (st in all_states) {
      if (grepl(paste0("\\b", tolower(st), "\\b"), raw_q, perl = TRUE)) {
        if (!st %in% detected_states) detected_states <- c(detected_states, st)
      }
    }
  }
  detected_states <- unique(detected_states)
  
  # Inherit states if none detected and not an overarching national inquiry
  if (length(detected_states) == 0) {
    if (!is.null(parent_entities$states) && length(parent_entities$states) > 0) {
      detected_states <- parent_entities$states
    } else if (!is.null(context$state) &&
               !grepl("\\b(national|across india|all states|which state|most polluted state)\\b", raw_q)) {
      detected_states <- context$state
    }
  }
  
  # 4.3 Cities
  for (c_name in names(CITY_ALIASES)) {
    aliases <- CITY_ALIASES[[c_name]]
    if (any(sapply(aliases, function(a) grepl(paste0("\\b", a, "\\b"), raw_q, perl = TRUE)))) {
      detected_cities <- c(detected_cities, c_name)
    }
  }
  if (!is.null(all_cities)) {
    for (ct in all_cities) {
      if (grepl(paste0("\\b", tolower(ct), "\\b"), raw_q, perl = TRUE)) {
        if (!ct %in% detected_cities) detected_cities <- c(detected_cities, ct)
      }
    }
  }
  detected_cities <- unique(detected_cities)
  
  # Inherit cities
  if (length(detected_cities) == 0) {
    if (!is.null(parent_entities$cities) && length(parent_entities$cities) > 0) {
      detected_cities <- parent_entities$cities
    } else if (!is.null(context$city) &&
               !grepl("\\b(national|across india|all cities|which city|most polluted city)\\b", raw_q)) {
      detected_cities <- context$city
    }
  }
  
  # Check for explicit named location like "in Atlantis" or "for Gondor" not in known registry
  if (length(detected_states) == 0 && length(detected_cities) == 0) {
    loc_match <- str_match(clause_text, "(?i)\\b(?:in|for|at|near)\\s+([A-Za-z][a-zA-Z\\s]{2,20})\\b")
    if (!is.na(loc_match[1, 2])) {
      cand_loc <- trimws(loc_match[1, 2])
      cand_loc <- str_replace(cand_loc, "[^a-zA-Z\\s]+$", "")
      cand_loc <- trimws(cand_loc)
      ignore_words <- c("delhi", "india", "average", "general", "each", "all", "chart", "table", "both", "pm10", "pm25", "pm2.5", "no2", "so2", "co", "ozone", "nh3")
      if (nzchar(cand_loc) && !tolower(cand_loc) %in% ignore_words) {
        detected_cities <- cand_loc
      }
    }
  }
  
  # 4.4 Group By Target
  if (grepl("\\b(state|states)\\b", raw_q)) {
    detected_group_by <- "state"
  } else if (grepl("\\b(city|cities)\\b", raw_q)) {
    detected_group_by <- "city"
  } else if (grepl("\\b(station|stations|monitors|sensor|sensors)\\b", raw_q)) {
    detected_group_by <- "station"
  }
  
  # 4.5 Limit (e.g. "top 5", "five", "10")
  word_to_num <- list("one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5, "ten" = 10)
  for (w in names(word_to_num)) {
    if (grepl(paste0("\\b(top|first|lowest|highest)\\s+", w, "\\b"), raw_q)) {
      detected_limit <- word_to_num[[w]]
      break
    }
  }
  num_match <- str_match(raw_q, "\\b(?:top|first|lowest|highest|best|worst)?\\s*(\\d+)\\s*(?:stations|cities|states)?\\b")
  if (!is.na(num_match[1, 2])) {
    v <- as.integer(num_match[1, 2])
    if (v >= 1 && v <= 50) detected_limit <- v
  }
  
  # 4.6 Direction
  if (grepl("\\b(lowest|least|best|cleanest|minimum|min|bottom|safest)\\b", raw_q)) {
    detected_direction <- "asc"
  } else if (grepl("\\b(highest|most|worst|maximum|max|polluted|top|elevated)\\b", raw_q)) {
    detected_direction <- "desc"
  }
  
  # 4.7 Metric
  if (grepl("\\b(maximum|max|highest reading)\\b", raw_q)) {
    requested_metric <- "max"
  } else if (grepl("\\b(minimum|min|lowest reading)\\b", raw_q)) {
    requested_metric <- "min"
  } else if (grepl("\\b(median)\\b", raw_q)) {
    requested_metric <- "median"
  } else if (grepl("\\b(completeness|missing|reporting rate)\\b", raw_q)) {
    requested_metric <- "completeness"
  } else {
    requested_metric <- "avg"
  }
  
  list(
    pollutant = primary_pollutant,
    pollutants = detected_pollutants,
    state = if (length(detected_states) == 1) detected_states[1] else NULL,
    states = detected_states,
    city = if (length(detected_cities) == 1) detected_cities[1] else NULL,
    cities = detected_cities,
    group_by = detected_group_by,
    limit = detected_limit,
    direction = detected_direction,
    metric = requested_metric,
    raw_clause = clause_text
  )
}

# ------------------------------------------------------------------------------
# 5. Semantic Clause Planner (Deterministic Fallback)
# ------------------------------------------------------------------------------

#' Construct a validated analytical plan for a single clause
plan_single_clause <- function(clause_text, entities, parent_entities = list(), context = list(), metadata = list()) {
  raw_q <- tolower(clause_text)
  
  # 5.1 Adversarial Code Injection & System Commands
  if (grepl("system\\(|eval\\(|parse\\(|rm\\s+-rf|<script|drop\\s+table|exec\\(", raw_q)) {
    return(list(
      operation = "unsupported",
      reason = "code_injection",
      raw_clause = clause_text
    ))
  }
  
  # 5.2 Out-of-Scope Queries (Weather, Prediction, Traffic, Industrial Liability)
  unsupported_words <- c(
    "tomorrow", "forecast", "predict", "next week", "future", "yesterday",
    "weather", "temperature", "humidity", "rain", "wind", "wind speed", "pressure",
    "traffic", "factory", "cause", "who is to blame", "penalty", "fine",
    "hospital", "health outcome", "stock", "mandate"
  )
  if (any(sapply(unsupported_words, function(w) grepl(paste0("\\b", w, "\\b"), raw_q)))) {
    return(list(
      operation = "unsupported",
      reason = "out_of_scope",
      raw_clause = clause_text
    ))
  }
  
  # 5.3 Temporal & Time-Based Trends
  if (grepl("\\b(trend|trends|over time|history|historical|longitudinal|progression|time series|daily change)\\b", raw_q)) {
    return(list(
      operation = "temporal",
      pollutant = if (!is.null(entities$pollutant)) entities$pollutant else "PM2.5",
      state = entities$state,
      city = entities$city,
      raw_clause = clause_text
    ))
  }
  
  # 5.4 Metadata & Provenance
  if (grepl("\\b(when|timestamp|provenance|source|dataset|cpcb|offline|baseline|how many stations|total stations|coverage)\\b", raw_q) &&
      !grepl("\\b(highest|lowest|average|mean|outlier|compare|missing|gap|flatline|severe|reading)\\b", raw_q)) {
    return(list(
      operation = "metadata",
      raw_clause = clause_text
    ))
  }
  
  # 5.5 Ambiguity Check 1: "Which is the best/worst city/state?" without a specified metric
  if (is.null(entities$pollutant) &&
      grepl("\\b(most polluted|worst|cleanest|best|highest pollution|lowest pollution)\\b", raw_q) &&
      !grepl("\\b(aqi|subindex|overall|category|completeness)\\b", raw_q)) {
    target_dim <- if (!is.null(entities$group_by)) entities$group_by else "city"
    return(list(
      operation = "clarification",
      clarification_type = "POLLUTANT_CHOICE",
      question = paste0("Pollution severity varies significantly by metric. How would you like to evaluate the ", target_dim, "s?"),
      options = c("Rank by PM2.5", "Rank by PM10", "Rank by NO2", "Rank by Indicative AQI", "Rank by Data Completeness"),
      raw_clause = clause_text
    ))
  }
  
  # 5.6 Ambiguity Check 2: Bare Location Name
  if (is.null(entities$pollutant) &&
      (length(entities$states) > 0 || length(entities$cities) > 0) &&
      str_count(raw_q, "\\w+") <= 3 &&
      !grepl("\\b(average|mean|median|missing|outlier|rank|worst|best|highest|lowest)\\b", raw_q)) {
    loc <- if (!is.null(entities$city)) entities$city else entities$state
    return(list(
      operation = "clarification",
      clarification_type = "LOCATION_ACTION",
      question = paste0("You selected ", loc, ". What analytical summary would you like to inspect?"),
      options = c(
        paste0("All Pollutants Summary for ", loc),
        paste0("Top Stations Ranked by PM2.5 in ", loc),
        paste0("CPCB AQI Category Breakdown in ", loc),
        paste0("Missing Readings Audit in ", loc)
      ),
      raw_clause = clause_text
    ))
  }
  
  # 5.7 Particulate Mass Dynamics: PM2.5 vs PM10 Ratio
  if (grepl("\\b(ratio|pm2.5 to pm10|pm25 to pm10|particulate mass|fine aerosol)\\b", raw_q) ||
      (grepl("pm2.5", raw_q) && grepl("pm10", raw_q) && grepl("\\b(compare|versus|vs|correlation|scatter)\\b", raw_q))) {
    return(list(
      operation = "ratio",
      state = entities$state,
      raw_clause = clause_text
    ))
  }
  
  # 5.8 Dependent Comparison: "compare them with their PM10 readings"
  if (grepl("\\b(compare them|with their|for those stations)\\b", raw_q) && !is.null(entities$pollutant)) {
    return(list(
      operation = "compare_stations_pollutant",
      pollutant = entities$pollutant,
      raw_clause = clause_text
    ))
  }
  
  # 5.9 Missing Data & Reporting Completeness
  if (grepl("\\b(missing|gap|gaps|unreported|offline sensor|complete|completeness|reporting rate|data quality|how many readings are missing)\\b", raw_q)) {
    # If parent or entities have multiple states, pass them for comparative completeness
    comp_sts <- if (length(entities$states) >= 2) {
      entities$states
    } else if (!is.null(parent_entities$states) && length(parent_entities$states) >= 2) {
      parent_entities$states
    } else {
      entities$states
    }
    
    return(list(
      operation = "missing",
      pollutant = entities$pollutant,
      states = comp_sts,
      state = entities$state,
      city = entities$city,
      raw_clause = clause_text
    ))
  }
  
  # 5.10 Outliers & Statistical Anomalies
  if (grepl("\\b(outlier|outliers|anomaly|anomalies|extreme|fence|tukey)\\b", raw_q)) {
    return(list(
      operation = "outlier",
      pollutant = if (!is.null(entities$pollutant)) entities$pollutant else "PM2.5",
      state = entities$state,
      raw_clause = clause_text
    ))
  }
  
  # 5.11 Zero-Value Sensor Flatlines
  if (grepl("\\b(flatline|flatlines|stuck sensor|zero reading|zero flatline|zero value)\\b", raw_q)) {
    return(list(
      operation = "flatline",
      pollutant = entities$pollutant,
      state = entities$state,
      raw_clause = clause_text
    ))
  }
  
  # 5.12 CPCB NAQI Categories & Eligibility
  if (grepl("\\b(aqi|subindex|sub-index|category|categories|severe|very poor|poor|moderate|satisfactory|good|eligible|ineligible|dominant)\\b", raw_q)) {
    return(list(
      operation = "aqi_breakdown",
      state = entities$state,
      raw_clause = clause_text
    ))
  }
  
  # 5.13 Geographic Comparisons (2+ States or Cities)
  distinct_locs <- unique(c(entities$states, entities$cities))
  if (grepl("\\b(compare|versus|vs|difference between|relative to)\\b", raw_q) || length(distinct_locs) >= 2) {
    return(list(
      operation = "compare",
      pollutant = if (!is.null(entities$pollutant)) entities$pollutant else "PM2.5",
      states = entities$states,
      cities = entities$cities,
      raw_clause = clause_text
    ))
  }
  
  # 5.14 Rankings (State, City, Station)
  if (grepl("\\b(top|highest|lowest|worst|best|cleanest|maximum|max|minimum|min|rank|ranking|rankings|hottest)\\b", raw_q)) {
    target_group <- if (!is.null(entities$group_by)) {
      entities$group_by
    } else if (grepl("\\b(state|states)\\b", raw_q)) {
      "state"
    } else if (grepl("\\b(city|cities)\\b", raw_q)) {
      "city"
    } else {
      "station"
    }
    
    return(list(
      operation = "rank",
      group_by = target_group,
      pollutant = if (!is.null(entities$pollutant)) entities$pollutant else "PM2.5",
      state = entities$state,
      city = entities$city,
      limit = entities$limit,
      direction = entities$direction,
      metric = entities$metric,
      raw_clause = clause_text
    ))
  }
  
  # 5.15 Aggregation & Statistical Summary
  if (grepl("\\b(average|mean|median|summary|distribution|spread|level|concentration)\\b", raw_q) || !is.null(entities$pollutant)) {
    return(list(
      operation = "aggregate",
      pollutant = if (!is.null(entities$pollutant)) entities$pollutant else "PM2.5",
      state = entities$state,
      city = entities$city,
      raw_clause = clause_text
    ))
  }
  
  # 5.16 Location Lookup
  if (!is.null(entities$city) || !is.null(entities$state)) {
    return(list(
      operation = "lookup",
      location = if (!is.null(entities$city)) entities$city else entities$state,
      raw_clause = clause_text
    ))
  }
  
  # Default Guide
  list(
    operation = "general_guide",
    raw_clause = clause_text
  )
}

# ------------------------------------------------------------------------------
# 6. Optional AI Language Model Planner (Configurable Provider)
# ------------------------------------------------------------------------------

#' Attempt to query an AI model API for analytical planning if configured
call_ai_model_if_available <- function(query_text, schema = ASSISTANT_SCHEMA, api_key_override = NULL) {
  # Check environment variable or explicit session override
  gemini_key <- if (!is.null(api_key_override) && nzchar(api_key_override)) {
    api_key_override
  } else if (nzchar(Sys.getenv("GEMINI_API_KEY"))) {
    Sys.getenv("GEMINI_API_KEY")
  } else {
    NULL
  }
  
  openai_key <- if (nzchar(Sys.getenv("OPENAI_API_KEY"))) Sys.getenv("OPENAI_API_KEY") else NULL
  
  if (is.null(gemini_key) && is.null(openai_key)) {
    return(NULL) # No AI API configured; trigger fallback
  }
  
  # Prompt system instructions defining strict analytical translation
  system_instruction <- paste0(
    "You are a natural language query planner for an air quality dataset. ",
    "Translate user questions into a JSON array of analytical sub-question plans. ",
    "DO NOT calculate numbers or invent answers. Output ONLY raw JSON conforming to this schema:\n",
    "{\n",
    "  \"subquestions\": [\n",
    "    {\n",
    "      \"operation\": \"rank\" | \"aggregate\" | \"compare\" | \"ratio\" | \"missing\" | \"outlier\" | \"flatline\" | \"aqi_breakdown\" | \"lookup\" | \"temporal\" | \"metadata\" | \"clarification\" | \"unsupported\",\n",
    "      \"pollutant\": \"PM2.5\" | \"PM10\" | \"NO2\" | \"SO2\" | \"CO\" | \"OZONE\" | \"NH3\",\n",
    "      \"group_by\": \"state\" | \"city\" | \"station\" | null,\n",
    "      \"states\": [\"Delhi\", ...],\n",
    "      \"cities\": [\"Bengaluru\", ...],\n",
    "      \"direction\": \"desc\" | \"asc\",\n",
    "      \"limit\": 5,\n",
    "      \"metric\": \"avg\" | \"min\" | \"max\" | \"completeness\"\n",
    "    }\n",
    "  ]\n",
    "}\n",
    "Dataset: 7 criteria pollutants, 31 Indian states, 273 cities, 526 CAAQM stations. Offline snapshot 23-Oct-2026 18:00 IST."
  )
  
  tryCatch({
    if (!is.null(gemini_key)) {
      # Gemini API Call (gemini-2.0-flash or gemini-1.5-flash)
      url <- paste0(
        "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent?key=",
        gemini_key
      )
      
      payload <- list(
        contents = list(
          list(
            parts = list(
              list(text = paste(system_instruction, "\n\nUser Question:", query_text))
            )
          )
        ),
        generationConfig = list(
          temperature = 0.0,
          responseMimeType = "application/json"
        )
      )
      
      resp <- httr::POST(
        url,
        body = jsonlite::toJSON(payload, auto_unbox = TRUE),
        httr::content_type_json(),
        httr::timeout(4)
      )
      
      if (httr::status_code(resp) == 200) {
        resp_json <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
        raw_text <- resp_json$candidates$content$parts[[1]]$text
        parsed_plan <- jsonlite::fromJSON(raw_text)
        if (!is.null(parsed_plan$subquestions) && length(parsed_plan$subquestions) > 0) {
          return(parsed_plan$subquestions)
        }
      }
    }
  }, error = function(e) {
    # On network failure or malformed JSON, silently fall back
    NULL
  })
  
  return(NULL)
}

# ------------------------------------------------------------------------------
# 7. Validated Analytical Execution Functions (Pure R & dplyr)
# ------------------------------------------------------------------------------

#' Execute Ranking (Stations, Cities, States)
exec_op_rank <- function(plan, base_data) {
  pol <- if (!is.null(plan$pollutant)) plan$pollutant else "PM2.5"
  unit <- ASSISTANT_SCHEMA$pollutant_units[[pol]] %||% "µg/m³"
  dir <- if (!is.null(plan$direction) && plan$direction == "asc") "asc" else "desc"
  limit_n <- if (!is.null(plan$limit) && is.numeric(plan$limit)) as.integer(plan$limit) else 5L
  target_group <- if (!is.null(plan$group_by)) plan$group_by else "station"
  dir_label <- if (dir == "asc") "Lowest (Cleanest)" else "Highest (Most Elevated)"
  
  sub_df <- base_data %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
  filter_desc <- "National Scope"
  
  if (!is.null(plan$state)) {
    sub_df <- sub_df %>% filter(state == plan$state)
    filter_desc <- paste("State:", plan$state)
  }
  if (!is.null(plan$city)) {
    sub_df <- sub_df %>% filter(city == plan$city)
    filter_desc <- paste("City:", plan$city)
  }
  
  if (nrow(sub_df) == 0) {
    return(list(
      title = paste("Ranking by", pol),
      text = paste0("No reporting monitoring records found for <strong>", pol, "</strong> matching ", filter_desc, "."),
      evidence = list(records_analyzed = 0, filters = filter_desc),
      plot = NULL,
      table = NULL
    ))
  }
  
  if (target_group == "state") {
    ranked_df <- sub_df %>%
      group_by(state) %>%
      summarise(
        mean_avg = round(mean(pollutant_avg, na.rm = TRUE), 1),
        median_avg = round(median(pollutant_avg, na.rm = TRUE), 1),
        stations_count = n(),
        .groups = "drop"
      )
    ranked_df <- if (dir == "asc") ranked_df %>% arrange(mean_avg) %>% head(limit_n) else ranked_df %>% arrange(desc(mean_avg)) %>% head(limit_n)
    
    top_name <- ranked_df$state[1]
    top_val <- ranked_df$mean_avg[1]
    
    summary_text <- paste0(
      "<strong>Top ", nrow(ranked_df), " States Ranked by Mean ", pol, " (", dir_label, "):</strong><br/>",
      "Rank #1 is <strong>", top_name, "</strong> with a state-wide mean concentration of <strong>",
      top_val, " ", unit, "</strong> across ", ranked_df$stations_count[1], " monitoring stations."
    )
    
    p <- ggplot(ranked_df, aes(x = mean_avg, y = reorder(state, if (dir == "asc") -mean_avg else mean_avg))) +
      geom_col(fill = if (dir == "asc") "#059669" else "#DC2626", width = 0.65) +
      geom_text(aes(label = paste0(mean_avg, " ", unit)), hjust = -0.15, size = 3.2, color = "#1E293B") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = paste0("State Mean (", unit, ")"), y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
    
    ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 20, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
    
    tbl <- ranked_df %>%
      mutate(Rank = row_number()) %>%
      select(Rank, State = state, `Mean Average` = mean_avg, `Median` = median_avg, `Stations` = stations_count)
    
  } else if (target_group == "city") {
    ranked_df <- sub_df %>%
      group_by(city, state) %>%
      summarise(
        mean_avg = round(mean(pollutant_avg, na.rm = TRUE), 1),
        max_reading = max(pollutant_max, na.rm = TRUE),
        stations_count = n(),
        .groups = "drop"
      )
    ranked_df <- if (dir == "asc") ranked_df %>% arrange(mean_avg) %>% head(limit_n) else ranked_df %>% arrange(desc(mean_avg)) %>% head(limit_n)
    
    top_name <- paste0(ranked_df$city[1], " (", ranked_df$state[1], ")")
    top_val <- ranked_df$mean_avg[1]
    
    summary_text <- paste0(
      "<strong>Top ", nrow(ranked_df), " Cities Ranked by ", pol, " (", dir_label, "):</strong><br/>",
      "Rank #1 is <strong>", top_name, "</strong> with an average concentration of <strong>",
      top_val, " ", unit, "</strong> (peak recorded maximum: ", ranked_df$max_reading[1], " ", unit, " across ", ranked_df$stations_count[1], " stations)."
    )
    
    p <- ggplot(ranked_df, aes(x = mean_avg, y = reorder(paste0(city, " (", state, ")"), if (dir == "asc") -mean_avg else mean_avg))) +
      geom_col(fill = if (dir == "asc") "#059669" else "#EA580C", width = 0.65) +
      geom_text(aes(label = paste0(mean_avg, " ", unit)), hjust = -0.15, size = 3.2, color = "#1E293B") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = paste0("City Mean (", unit, ")"), y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
    
    ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 20, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
    
    tbl <- ranked_df %>%
      mutate(Rank = row_number()) %>%
      select(Rank, City = city, State = state, `Mean Average` = mean_avg, `Peak Max` = max_reading, Stations = stations_count)
    
  } else {
    # Default: Station ranking
    ranked_df <- if (dir == "asc") {
      sub_df %>% arrange(pollutant_avg) %>% head(limit_n)
    } else {
      sub_df %>% arrange(desc(pollutant_avg)) %>% head(limit_n)
    }
    
    top_name <- paste0(ranked_df$station[1], " (", ranked_df$city[1], ", ", ranked_df$state[1], ")")
    top_val <- ranked_df$pollutant_avg[1]
    
    summary_text <- paste0(
      "<strong>Top ", nrow(ranked_df), " Monitoring Stations for ", pol, " (", dir_label, ", ", filter_desc, "):</strong><br/>",
      "Rank #1 station is <strong>", top_name, "</strong> with a recorded average of <strong>",
      top_val, " ", unit, "</strong> (min: ", ranked_df$pollutant_min[1], " | max: ", ranked_df$pollutant_max[1], " ", unit, ")."
    )
    
    ranked_df <- ranked_df %>%
      mutate(stn_short = paste0(str_trunc(station, 20), " (", city, ")"))
    
    p <- ggplot(ranked_df, aes(x = pollutant_avg, y = reorder(stn_short, if (dir == "asc") -pollutant_avg else pollutant_avg))) +
      geom_col(fill = if (dir == "asc") "#059669" else "#DC2626", width = 0.65) +
      geom_text(aes(label = paste0(pollutant_avg, " ", unit)), hjust = -0.15, size = 3.2, color = "#1E293B") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
      labs(x = paste0("Recorded Average (", unit, ")"), y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
    
    ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 20, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
    
    tbl <- ranked_df %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Station = station, City = city, State = state, Min = pollutant_min, Average = pollutant_avg, Max = pollutant_max)
  }
  
  list(
    title = paste("Ranking by", pol),
    text = summary_text,
    evidence = list(
      metric = paste(target_group, "Ranking by", pol),
      top_entity = top_name,
      top_value = paste(top_val, unit),
      direction = dir_label,
      records_analyzed = nrow(sub_df),
      filters = filter_desc,
      unit = unit
    ),
    plot = ply,
    table = tbl,
    last_stations = if (target_group == "station") ranked_df$station else NULL
  )
}

#' Execute Aggregation / Summary
exec_op_aggregate <- function(plan, base_data) {
  pol <- if (!is.null(plan$pollutant)) plan$pollutant else "PM2.5"
  unit <- ASSISTANT_SCHEMA$pollutant_units[[pol]] %||% "µg/m³"
  
  sub_df <- base_data %>% filter(pollutant_id == pol)
  filter_desc <- "National Scope"
  if (!is.null(plan$state)) {
    sub_df <- sub_df %>% filter(state == plan$state)
    filter_desc <- paste("State:", plan$state)
  }
  if (!is.null(plan$city)) {
    sub_df <- sub_df %>% filter(city == plan$city)
    filter_desc <- paste("City:", plan$city)
  }
  
  valid_recs <- sub_df %>% filter(!is.na(pollutant_avg))
  if (nrow(valid_recs) == 0) {
    loc_name <- plan$city %||% plan$state %||% "the specified region"
    return(list(
      title = paste(pol, "Summary"),
      text = paste0("Location '", loc_name, "' was not found in the CPCB monitoring network, or has no reporting measurements for <strong>", pol, "</strong>."),
      evidence = list(records = 0, filters = filter_desc),
      plot = NULL, table = NULL
    ))
  }
  
  mean_val <- round(mean(valid_recs$pollutant_avg), 1)
  med_val <- round(median(valid_recs$pollutant_avg), 1)
  min_val <- min(valid_recs$pollutant_avg)
  max_val <- max(valid_recs$pollutant_avg)
  iqr_val <- round(IQR(valid_recs$pollutant_avg), 1)
  comp_pct <- round(100 * nrow(valid_recs) / nrow(sub_df), 1)
  
  summary_text <- paste0(
    "<strong>Statistical Summary for ", pol, " (", filter_desc, "):</strong><br/>",
    "• <strong>Mean Concentration:</strong> ", mean_val, " ", unit, "<br/>",
    "• <strong>Median Concentration:</strong> ", med_val, " ", unit, "<br/>",
    "• <strong>Observed Range:</strong> ", min_val, " to ", max_val, " ", unit, " (IQR spread: ", iqr_val, " ", unit, ")<br/>",
    "• <strong>Monitoring Completeness:</strong> ", nrow(valid_recs), " of ", nrow(sub_df), " stations reporting (", comp_pct, "%)."
  )
  
  p <- ggplot(valid_recs, aes(x = pollutant_avg)) +
    geom_histogram(bins = 20, fill = "#1E3A8A", color = "white", alpha = 0.8) +
    geom_vline(aes(xintercept = mean_val), color = "#DC2626", linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = mean_val, y = Inf, label = paste("Mean:", mean_val), vjust = 2, hjust = -0.1, color = "#DC2626", size = 3.2) +
    labs(x = paste0(pol, " (", unit, ")"), y = "Stations Count") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
  
  tbl <- data.frame(
    Metric = c("Mean Concentration", "Median Concentration", "Minimum", "Maximum", "IQR Spread", "Reporting Stations", "Data Completeness"),
    Value = c(paste(mean_val, unit), paste(med_val, unit), paste(min_val, unit), paste(max_val, unit), paste(iqr_val, unit), paste(nrow(valid_recs), "stations"), paste0(comp_pct, "%"))
  )
  
  list(
    title = paste(pol, "Summary"),
    text = summary_text,
    evidence = list(
      metric = paste(pol, "Summary"),
      mean = paste(mean_val, unit),
      median = paste(med_val, unit),
      observed_range = paste(min_val, "to", max_val, unit),
      completeness = paste0(comp_pct, "%"),
      sample_size_n = nrow(valid_recs),
      filters = filter_desc
    ),
    plot = ply,
    table = tbl
  )
}

#' Execute Geographic Comparison (States or Cities)
exec_op_compare <- function(plan, base_data, wide_data) {
  pol <- if (!is.null(plan$pollutant)) plan$pollutant else "PM2.5"
  unit <- ASSISTANT_SCHEMA$pollutant_units[[pol]] %||% "µg/m³"
  
  comp_states <- plan$states
  comp_cities <- plan$cities
  
  if (length(comp_states) == 0 && length(comp_cities) == 0) {
    comp_states <- c("Delhi", "Maharashtra") # Baseline comparison
  }
  
  use_cities <- length(comp_cities) >= 2
  group_col <- if (use_cities) "city" else "state"
  items <- if (use_cities) comp_cities else comp_states
  
  sub_df <- base_data %>%
    filter(pollutant_id == pol, .data[[group_col]] %in% items, !is.na(pollutant_avg))
  
  if (nrow(sub_df) == 0) {
    return(list(
      title = paste("Comparison by", pol),
      text = paste0("No valid observations for ", pol, " across the requested entities: ", paste(items, collapse = ", ")),
      evidence = list(records = 0), plot = NULL, table = NULL
    ))
  }
  
  comp_summary <- sub_df %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      mean_val = round(mean(pollutant_avg), 1),
      median_val = round(median(pollutant_avg), 1),
      stations = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_val))
  
  hi_item <- comp_summary[[group_col]][1]
  hi_val <- comp_summary$mean_val[1]
  lo_item <- comp_summary[[group_col]][nrow(comp_summary)]
  lo_val <- comp_summary$mean_val[nrow(comp_summary)]
  diff_val <- round(hi_val - lo_val, 1)
  ratio_val <- round(hi_val / max(0.1, lo_val), 1)
  
  text <- paste0(
    "<strong>Comparative Breakdown for ", pol, " across ", paste(items, collapse = " vs "), ":</strong><br/>",
    "• <strong>", hi_item, "</strong> reports an average concentration of <strong>", hi_val, " ", unit, "</strong> (median: ", comp_summary$median_val[1], " ", unit, ", N=", comp_summary$stations[1], ").<br/>",
    "• <strong>", lo_item, "</strong> reports an average concentration of <strong>", lo_val, " ", unit, "</strong> (median: ", comp_summary$median_val[nrow(comp_summary)], " ", unit, ", N=", comp_summary$stations[nrow(comp_summary)], ").<br/>",
    "• Absolute difference: <strong>+", diff_val, " ", unit, "</strong> (", ratio_val, "× relative elevated ratio in ", hi_item, ")."
  )
  
  p <- ggplot(sub_df, aes(x = .data[[group_col]], y = pollutant_avg, fill = .data[[group_col]])) +
    geom_boxplot(alpha = 0.75, width = 0.5, outlier.color = "#DC2626") +
    scale_fill_viridis_d(option = "turbo", guide = "none") +
    labs(x = NULL, y = paste0(pol, " (", unit, ")")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
  
  tbl <- comp_summary %>%
    rename(Location = 1, `Mean Average` = mean_val, `Median` = median_val, `Reporting Stations` = stations)
  
  list(
    title = paste(pol, "Comparison:", paste(items, collapse = " vs ")),
    text = text,
    evidence = list(
      higher_entity = paste0(hi_item, " (", hi_val, " ", unit, ")"),
      lower_entity = paste0(lo_item, " (", lo_val, " ", unit, ")"),
      difference = paste(diff_val, unit),
      relative_ratio = paste0(ratio_val, "x"),
      total_stations = nrow(sub_df)
    ),
    plot = ply,
    table = tbl
  )
}

#' Execute Dependent Comparison: Compare Top Stations with Second Pollutant
exec_op_compare_stations_pollutant <- function(plan, base_data, previous_stations = NULL) {
  pol <- if (!is.null(plan$pollutant)) plan$pollutant else "PM10"
  unit <- ASSISTANT_SCHEMA$pollutant_units[[pol]] %||% "µg/m³"
  
  if (is.null(previous_stations) || length(previous_stations) == 0) {
    # Default to top 5 PM2.5 stations
    top_stns <- base_data %>%
      filter(pollutant_id == "PM2.5", !is.na(pollutant_avg)) %>%
      arrange(desc(pollutant_avg)) %>%
      head(5) %>%
      pull(station)
  } else {
    top_stns <- head(previous_stations, 5)
  }
  
  comp_df <- base_data %>%
    filter(station %in% top_stns, pollutant_id %in% c("PM2.5", pol)) %>%
    select(station, city, state, pollutant_id, pollutant_avg) %>%
    tidyr::pivot_wider(names_from = pollutant_id, values_from = pollutant_avg)
  
  text <- paste0(
    "<strong>Co-located Particulate Comparison for Top Stations with ", pol, ":</strong><br/>",
    paste0(
      "• <strong>", comp_df$station, "</strong> (", comp_df$city, "): PM2.5 = ",
      ifelse(is.na(comp_df$PM2.5), "NA", paste0(comp_df$PM2.5, " µg/m³")), " | ",
      pol, " = ", ifelse(is.na(comp_df[[pol]]), "NA", paste0(comp_df[[pol]], " ", unit)),
      collapse = "<br/>"
    )
  )
  
  plot_df <- base_data %>%
    filter(station %in% top_stns, pollutant_id %in% c("PM2.5", pol), !is.na(pollutant_avg)) %>%
    mutate(stn_short = paste0(str_trunc(station, 18), " (", city, ")"))
  
  p <- ggplot(plot_df, aes(x = pollutant_avg, y = stn_short, fill = pollutant_id)) +
    geom_col(position = "dodge", width = 0.6) +
    scale_fill_manual(values = c("PM2.5" = "#DC2626", "PM10" = "#2563EB")) +
    labs(x = "Concentration (µg/m³)", y = NULL, fill = "Pollutant") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
  
  list(
    title = paste("PM2.5 vs", pol, "Comparison for Top Stations"),
    text = text,
    evidence = list(stations_compared = nrow(comp_df), secondary_pollutant = pol),
    plot = ply,
    table = comp_df
  )
}

#' Execute Particulate Dynamics (PM2.5 vs PM10 Ratio & Regression)
exec_op_ratio <- function(plan, wide_data) {
  sub_df <- wide_data %>% filter(!is.na(PM2.5), !is.na(PM10), PM10 > 0)
  filter_desc <- "National Scope"
  if (!is.null(plan$state)) {
    sub_df <- sub_df %>% filter(state == plan$state)
    filter_desc <- paste("State:", plan$state)
  }
  
  if (nrow(sub_df) < 2) {
    return(list(
      title = "Particulate Ratio",
      text = "Insufficient co-located stations reporting concurrent PM2.5 and PM10.",
      evidence = list(records = nrow(sub_df)), plot = NULL, table = NULL
    ))
  }
  
  r_val <- round(cor(sub_df$PM2.5, sub_df$PM10, method = "pearson"), 2)
  med_ratio <- round(median(sub_df$PM2.5 / sub_df$PM10), 2)
  mean_pm25 <- round(mean(sub_df$PM2.5), 1)
  mean_pm10 <- round(mean(sub_df$PM10), 1)
  
  text <- paste0(
    "<strong>Particulate Dynamics: PM2.5 vs PM10 (", filter_desc, "):</strong><br/>",
    "• <strong>Co-located Stations:</strong> ", nrow(sub_df), " CAAQM stations concurrently recording both fractions.<br/>",
    "• <strong>Pearson Correlation (r):</strong> <strong>", r_val, "</strong> (strong co-variation).<br/>",
    "• <strong>Median Mass Ratio (PM2.5 / PM10):</strong> <strong>", med_ratio, "</strong> (fine aerosols constitute ", round(med_ratio * 100), "% of respirable mass).<br/>",
    "• <strong>Mean Concentrations:</strong> PM2.5 mean = ", mean_pm25, " µg/m³; PM10 mean = ", mean_pm10, " µg/m³."
  )
  
  p <- ggplot(sub_df, aes(x = PM10, y = PM2.5)) +
    geom_point(color = "#2563EB", alpha = 0.65, size = 2) +
    geom_smooth(method = "lm", se = FALSE, color = "#DC2626", linewidth = 0.8) +
    labs(x = "PM10 (µg/m³)", y = "PM2.5 (µg/m³)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
  
  tbl <- sub_df %>%
    mutate(Ratio = round(PM2.5 / PM10, 2)) %>%
    select(Station = station, City = city, State = state, PM2.5, PM10, Ratio) %>%
    arrange(desc(Ratio)) %>%
    head(10)
  
  list(
    title = "PM2.5 vs PM10 Dynamics",
    text = text,
    evidence = list(
      pearson_r = r_val,
      median_ratio = med_ratio,
      co_located_n = nrow(sub_df),
      filters = filter_desc
    ),
    plot = ply,
    table = tbl
  )
}

#' Execute Missing Data & Reporting Completeness
exec_op_missing <- function(plan, base_data) {
  sub_df <- base_data
  filter_desc <- "National Scope"
  
  # Check if comparison between states is requested for data completeness
  if (!is.null(plan$states) && length(plan$states) >= 2) {
    comp_states <- plan$states
    st_comp <- sub_df %>%
      filter(state %in% comp_states) %>%
      group_by(state) %>%
      summarise(
        total = n(),
        valid = sum(!is.na(pollutant_avg)),
        missing = sum(is.na(pollutant_avg)),
        completeness_pct = round(100 * valid / total, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(completeness_pct))
    
    more_comp <- st_comp$state[1]
    more_pct <- st_comp$completeness_pct[1]
    less_comp <- st_comp$state[nrow(st_comp)]
    less_pct <- st_comp$completeness_pct[nrow(st_comp)]
    
    text <- paste0(
      "<strong>Data Completeness Comparison across ", paste(comp_states, collapse = " vs "), ":</strong><br/>",
      "• <strong>", more_comp, "</strong> exhibits more complete telemetry at <strong>", more_pct, "%</strong> (",
      st_comp$valid[1], " of ", st_comp$total[1], " records valid; ", st_comp$missing[1], " missing).<br/>",
      "• <strong>", less_comp, "</strong> reports <strong>", less_pct, "%</strong> completeness (",
      st_comp$valid[nrow(st_comp)], " of ", st_comp$total[nrow(st_comp)], " records valid; ", st_comp$missing[nrow(st_comp)], " missing).<br/>",
      "• <strong>Fidelity standard:</strong> All missing observations are preserved as genuine NAs and never replaced with zero."
    )
    
    p <- ggplot(st_comp, aes(x = completeness_pct, y = state, fill = state)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = paste0(completeness_pct, "% (", missing, " NA)")), hjust = -0.1, size = 3.2) +
      scale_x_continuous(limits = c(0, 115)) +
      labs(x = "Completeness (%)", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(), legend.position = "none")
    
    ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
    
    tbl <- st_comp %>%
      select(State = state, `Total Records` = total, `Valid Readings` = valid, `Missing (NA)` = missing, `Completeness %` = completeness_pct)
    
    return(list(
      title = "Data Completeness Comparison",
      text = text,
      evidence = list(
        more_complete_state = paste0(more_comp, " (", more_pct, "%)"),
        less_complete_state = paste0(less_comp, " (", less_pct, "%)")
      ),
      plot = ply,
      table = tbl
    ))
  }
  
  # Standard Pollutant Missingness Audit
  if (!is.null(plan$state)) {
    sub_df <- sub_df %>% filter(state == plan$state)
    filter_desc <- paste("State:", plan$state)
  }
  
  completeness_df <- sub_df %>%
    group_by(pollutant_id) %>%
    summarise(
      total = n(),
      valid = sum(!is.na(pollutant_avg)),
      missing = sum(is.na(pollutant_avg)),
      completeness_pct = round(100 * valid / total, 1),
      .groups = "drop"
    ) %>%
    arrange(completeness_pct)
  
  tot_recs <- nrow(sub_df)
  tot_missing <- sum(is.na(sub_df$pollutant_avg))
  tot_valid <- tot_recs - tot_missing
  overall_pct <- round(100 * tot_valid / tot_recs, 1)
  
  text <- paste0(
    "<strong>Data Quality & Missing Measurement Audit (", filter_desc, "):</strong><br/>",
    "• <strong>Overall Completeness:</strong> <strong>", overall_pct, "%</strong> (", scales::comma(tot_valid), " of ", scales::comma(tot_recs), " records valid).<br/>",
    "• <strong>Total Missing Readings:</strong> <strong>", tot_missing, " measurements</strong> preserved strictly as genuine `NA` values without synthetic zero imputation.<br/>",
    "• <strong>Lowest Reporting Pollutant:</strong> <strong>", completeness_df$pollutant_id[1], "</strong> at ", completeness_df$completeness_pct[1], "% availability.<br/>",
    "• <strong>Highest Reporting Pollutant:</strong> <strong>", completeness_df$pollutant_id[nrow(completeness_df)], "</strong> at ", completeness_df$completeness_pct[nrow(completeness_df)], "% availability."
  )
  
  p <- ggplot(completeness_df, aes(x = completeness_pct, y = reorder(pollutant_id, completeness_pct))) +
    geom_col(fill = "#2563EB", width = 0.6) +
    geom_text(aes(label = paste0(completeness_pct, "% (", missing, " NA)")), hjust = -0.1, size = 3.2, color = "#1E293B") +
    scale_x_continuous(limits = c(0, 115)) +
    labs(x = "Completeness Percentage (%)", y = "Pollutant") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 20, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
  
  tbl <- completeness_df %>%
    select(Pollutant = pollutant_id, `Total Monitors` = total, `Valid Readings` = valid, `Missing (NA)` = missing, `Completeness %` = completeness_pct)
  
  list(
    title = "Missing Data & Completeness Audit",
    text = text,
    evidence = list(
      overall_completeness = paste0(overall_pct, "%"),
      total_missing_na = tot_missing,
      total_records = tot_recs,
      filters = filter_desc
    ),
    plot = ply,
    table = tbl
  )
}

#' Execute Outlier Detection (Tukey IQR)
exec_op_outlier <- function(plan, base_data) {
  pol <- if (!is.null(plan$pollutant)) plan$pollutant else "PM2.5"
  unit <- ASSISTANT_SCHEMA$pollutant_units[[pol]] %||% "µg/m³"
  
  sub_df <- base_data %>% filter(pollutant_id == pol, !is.na(pollutant_avg))
  filter_desc <- "National Scope"
  if (!is.null(plan$state)) {
    sub_df <- sub_df %>% filter(state == plan$state)
    filter_desc <- paste("State:", plan$state)
  }
  
  q1 <- quantile(sub_df$pollutant_avg, 0.25)
  q3 <- quantile(sub_df$pollutant_avg, 0.75)
  iqr_val <- q3 - q1
  upper_fence <- q3 + 1.5 * iqr_val
  
  outliers_df <- sub_df %>% filter(pollutant_avg > upper_fence) %>% arrange(desc(pollutant_avg))
  n_outliers <- nrow(outliers_df)
  
  text <- paste0(
    "<strong>Statistical Outlier Audit for ", pol, " (", filter_desc, "):</strong><br/>",
    "• <strong>Method:</strong> Tukey 1.5 × IQR Fence (Upper threshold = Q3 + 1.5 × IQR = <strong>", round(upper_fence, 1), " ", unit, "</strong>).<br/>",
    "• <strong>Flagged Outliers:</strong> <strong>", n_outliers, " stations</strong> (", round(100 * n_outliers / nrow(sub_df), 1), "% of valid records) exhibit concentration spikes exceeding the fence.<br/>",
    if (n_outliers > 0) {
      paste0("• <strong>Peak Flagged Outlier:</strong> <strong>", outliers_df$station[1], "</strong> (", outliers_df$city[1], ", ", outliers_df$state[1], ") with <strong>", outliers_df$pollutant_avg[1], " ", unit, "</strong> (+", round(outliers_df$pollutant_avg[1] - upper_fence, 1), " above threshold).<br/>")
    } else "",
    "• <strong>Interpretation:</strong> Statistical outliers reflect localized concentration spikes and do not imply confirmed hardware malfunction."
  )
  
  tbl <- outliers_df %>%
    mutate(
      Threshold = round(upper_fence, 1),
      Exceedance = round(pollutant_avg - upper_fence, 1)
    ) %>%
    select(Station = station, City = city, State = state, Reading = pollutant_avg, Threshold, `Exceedance Above Fence` = Exceedance) %>%
    head(10)
  
  list(
    title = paste(pol, "Outlier Audit"),
    text = text,
    evidence = list(
      upper_fence = paste(round(upper_fence, 1), unit),
      flagged_outliers = n_outliers,
      sample_size_n = nrow(sub_df),
      filters = filter_desc
    ),
    plot = NULL,
    table = tbl
  )
}

#' Execute Zero Flatlines
exec_op_flatline <- function(plan, base_data) {
  sub_df <- base_data %>%
    filter(!is.na(pollutant_avg), pollutant_min == 0, pollutant_max == 0, pollutant_avg == 0)
  filter_desc <- "National Scope"
  if (!is.null(plan$pollutant)) {
    sub_df <- sub_df %>% filter(pollutant_id == plan$pollutant)
    filter_desc <- paste("Pollutant:", plan$pollutant)
  }
  
  text <- paste0(
    "<strong>Zero-Value Sensor Flatline Audit (", filter_desc, "):</strong><br/>",
    "• <strong>Definition:</strong> Sensor records reporting Minimum == Maximum == Average == 0 during the entire snapshot window.<br/>",
    "• <strong>Flagged Monitors:</strong> <strong>", nrow(sub_df), " observation records</strong> exhibit zero flatlines.<br/>",
    "• <strong>Fidelity standard:</strong> While low trace concentrations occur legitimately for some gases (e.g. SO2), persistent flatlines warrant physical telemetry verification."
  )
  
  tbl <- sub_df %>%
    select(Station = station, City = city, State = state, Pollutant = pollutant_id, Min = pollutant_min, Avg = pollutant_avg, Max = pollutant_max) %>%
    head(15)
  
  list(
    title = "Zero Flatline Audit",
    text = text,
    evidence = list(flatline_records = nrow(sub_df), filters = filter_desc),
    plot = NULL,
    table = tbl
  )
}

#' Execute CPCB Indicative AQI Breakdown
exec_op_aqi <- function(plan, wide_data) {
  sub_df <- wide_data
  filter_desc <- "National Scope"
  if (!is.null(plan$state)) {
    sub_df <- sub_df %>% filter(state == plan$state)
    filter_desc <- paste("State:", plan$state)
  }
  
  eligible_df <- sub_df %>% filter(cpcb_eligible)
  ineligible_n <- nrow(sub_df) - nrow(eligible_df)
  
  cat_counts <- eligible_df %>%
    count(indicative_category) %>%
    mutate(pct = round(100 * n / nrow(eligible_df), 1))
  
  dom_pol <- eligible_df %>%
    count(dominant_pollutant) %>%
    arrange(desc(n))
  
  top_dom <- dom_pol$dominant_pollutant[1]
  top_dom_pct <- round(100 * dom_pol$n[1] / nrow(eligible_df), 1)
  
  text <- paste0(
    "<strong>CPCB Indicative AQI Category & Eligibility Breakdown (", filter_desc, "):</strong><br/>",
    "• <strong>Eligibility Criteria:</strong> Station must monitor ≥3 pollutants, including at least one particulate (PM2.5 or PM10).<br/>",
    "• <strong>Eligible Stations:</strong> <strong>", nrow(eligible_df), " of ", nrow(sub_df), " stations</strong> (", round(100 * nrow(eligible_df) / nrow(sub_df), 1), "% eligible). ",
    ineligible_n, " stations are classified as Ineligible.<br/>",
    "• <strong>Dominant Pollutant:</strong> <strong>", top_dom, "</strong> governs the peak sub-index across <strong>", top_dom_pct, "%</strong> of eligible stations.<br/>",
    "• <strong>Regulatory Caveat:</strong> Snapshot sub-indices are indicative; certified regulatory CPCB AQI mandates 24-hr continuous averages with ≥16 valid hourly readings."
  )
  
  cat_order <- c("Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe")
  cat_colors <- c("Good" = "#00B050", "Satisfactory" = "#92D050", "Moderate" = "#D4AC0D", "Poor" = "#E67E22", "Very Poor" = "#C0392B", "Severe" = "#7D3C98")
  
  plot_df <- cat_counts %>%
    filter(indicative_category %in% cat_order) %>%
    mutate(indicative_category = factor(indicative_category, levels = cat_order))
  
  p <- ggplot(plot_df, aes(x = indicative_category, y = n, fill = indicative_category)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(n, " (", pct, "%)")), vjust = -0.3, size = 3.2) +
    scale_fill_manual(values = cat_colors, guide = "none") +
    labs(x = "CPCB Indicative Category", y = "Stations Count") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  ply <- ggplotly(p) %>% layout(margin = list(l = 10, r = 10, t = 10, b = 20)) %>% plotly::config(displayModeBar = FALSE)
  
  tbl <- plot_df %>% select(Category = indicative_category, `Stations Count` = n, `Percentage %` = pct)
  
  list(
    title = "CPCB AQI Category Breakdown",
    text = text,
    evidence = list(
      eligible_stations = nrow(eligible_df),
      ineligible_stations = ineligible_n,
      dominant_pollutant = paste0(top_dom, " (", top_dom_pct, "%)")
    ),
    plot = ply,
    table = tbl
  )
}

#' Execute Location Telemetry Lookup
exec_op_lookup <- function(plan, base_data) {
  loc <- plan$location
  sub_df <- base_data %>% filter(city == loc | state == loc | station == loc)
  
  if (nrow(sub_df) == 0) {
    return(list(
      title = paste("Lookup:", loc),
      text = paste0("Location '", loc, "' was not found in the CPCB CAAQM monitoring registry."),
      evidence = list(records = 0), plot = NULL, table = NULL
    ))
  }
  
  stn_n <- length(unique(sub_df$station))
  pol_summary <- sub_df %>%
    group_by(pollutant_id) %>%
    summarise(
      mean_val = round(mean(pollutant_avg, na.rm = TRUE), 1),
      median_val = round(median(pollutant_avg, na.rm = TRUE), 1),
      valid = sum(!is.na(pollutant_avg)),
      total = n(),
      .groups = "drop"
    ) %>%
    mutate(unit = ifelse(pollutant_id == "CO", "mg/m³", "µg/m³"))
  
  text <- paste0(
    "<strong>Environmental Telemetry for ", loc, ":</strong><br/>",
    "Representing <strong>", stn_n, " continuous monitoring station(s)</strong>.<br/>",
    paste0("• <strong>", pol_summary$pollutant_id, ":</strong> Average ", pol_summary$mean_val, " ", pol_summary$unit, " (Reporting: ", pol_summary$valid, "/", pol_summary$total, ")", collapse = "<br/>")
  )
  
  tbl <- pol_summary %>%
    select(Pollutant = pollutant_id, Unit = unit, `Mean Average` = mean_val, `Median` = median_val, `Valid Readings` = valid, `Total Monitors` = total)
  
  list(
    title = paste("Telemetry for", loc),
    text = text,
    evidence = list(location = loc, stations = stn_n, records_examined = nrow(sub_df)),
    plot = NULL,
    table = tbl
  )
}

#' Execute Temporal Query (Truthful Gating)
exec_op_temporal <- function(plan, metadata) {
  has_multi <- isTRUE(metadata$has_multi_timestamps)
  
  if (!has_multi) {
    text <- paste0(
      "<strong>Historical Trend Analysis Gating Notice:</strong><br/>",
      "• <strong>Dataset Scope:</strong> The current active dataset is a single static snapshot captured on <strong>23-Oct-2026 at 18:00 IST</strong>.<br/>",
      "• <strong>Scientific Ethics:</strong> To avoid fabricating synthetic trends, multi-hour longitudinal trajectory charts require at least two distinct genuine observation snapshots.<br/>",
      "• <strong>How to Enable:</strong> Use the <strong>Historical Trends</strong> page to import timestamped CPCB snapshots or trigger the official API harvester once data.gov.in restores live access."
    )
    
    return(list(
      title = "Historical Trends (Gated)",
      text = text,
      evidence = list(
        dataset_mode = "Single Offline Snapshot",
        observation_time = "23-10-2026 18:00 IST",
        distinct_timestamps_t = 1,
        historical_charts_status = "Gated to prevent data fabrication"
      ),
      plot = NULL,
      table = NULL
    ))
  }
  
  list(
    title = "Historical Trends",
    text = "Multi-timestamp longitudinal observations are loaded. Please inspect the Historical Trends module for interactive trajectories.",
    evidence = list(has_multi = TRUE), plot = NULL, table = NULL
  )
}

#' Execute Metadata & Provenance
exec_op_metadata <- function(plan, base_data, metadata) {
  tot_stns <- length(unique(base_data$station))
  tot_cities <- length(unique(base_data$city))
  tot_states <- length(unique(base_data$state))
  tot_recs <- nrow(base_data)
  valid_recs <- sum(!is.na(base_data$pollutant_avg))
  
  text <- paste0(
    "<strong>CPCB CAAQM Dataset Specification & Provenance:</strong><br/>",
    "• <strong>Source Authority:</strong> Central Pollution Control Board (CPCB) via Open Government Data Platform India (`data.gov.in`).<br/>",
    "• <strong>Observation Snapshot:</strong> 23-Oct-2026 18:00 IST (Resource ID: <code>3b01bcb8</code>).<br/>",
    "• <strong>Network Breadth:</strong> ", tot_stns, " monitoring stations across ", tot_cities, " cities in ", tot_states, " States & UTs.<br/>",
    "• <strong>Records:</strong> ", scales::comma(tot_recs), " observations (", scales::comma(valid_recs), " valid readings, ",
    round(100 * valid_recs / tot_recs, 1), "% reporting completeness).<br/>",
    "• <strong>Monitored Parameters:</strong> PM2.5, PM10, NO2, SO2, CO, OZONE, NH3."
  )
  
  list(
    title = "Dataset Provenance",
    text = text,
    evidence = list(
      source = "CPCB / data.gov.in",
      stations = tot_stns,
      cities = tot_cities,
      states = tot_states,
      total_records = tot_recs
    ),
    plot = NULL,
    table = NULL
  )
}

#' Execute Clarification Needed
exec_op_clarification <- function(plan) {
  list(
    title = "Clarification Needed",
    text = plan$question,
    options = plan$options,
    evidence = list(status = "Awaiting user selection"),
    plot = NULL,
    table = NULL
  )
}

#' Execute Unsupported Query
exec_op_unsupported <- function(plan) {
  reason <- if (!is.null(plan$reason)) plan$reason else "out_of_scope"
  
  if (reason == "code_injection") {
    text <- paste0(
      "<strong>Security Restriction:</strong> Arbitrary code execution, shell commands, and un-sanitized script evaluation are strictly disallowed. ",
      "All assistant queries execute via pre-validated, immutable `dplyr` templates."
    )
  } else {
    text <- paste0(
      "<strong>Domain Boundary Notice:</strong> The assistant cannot perform predictive weather forecasting, meteorological simulations (wind, humidity, rain), or causal industrial blame apportionment. ",
      "The dataset contains empirical ambient air quality measurements from continuous CAAQM sensors captured on <strong>23-Oct-2026 at 18:00 IST</strong>."
    )
  }
  
  list(
    title = "Unsupported Query",
    text = text,
    evidence = list(reason = reason, supported_operations = "Empirical CAAQM telemetry analysis only"),
    plot = NULL,
    table = NULL
  )
}

#' Execute General Guide
exec_op_guide <- function(plan) {
  text <- paste0(
    "I am your <strong>Environmental Data Assistant</strong> grounded strictly in the genuine CPCB continuous air quality monitoring dataset.<br/><br/>",
    "You can ask me questions such as:<br/>",
    "• <em>'Which state has the highest PM2.5 average, which city has the highest PM10 reading, and how many readings are missing?'</em><br/>",
    "• <em>'Compare Delhi and Maharashtra for PM2.5 and PM10, and explain which state has more complete data.'</em><br/>",
    "• <em>'List the five stations with the highest PM2.5 readings, compare them with their PM10 readings, and identify any flagged outliers.'</em><br/>",
    "• <em>'What is the particulate ratio between PM2.5 and PM10 in Delhi?'</em><br/>",
    "• <em>'Show statistical outliers for NO2'</em><br/>",
    "• <em>'How many stations are in the Severe category?'</em>"
  )
  
  list(
    title = "Assistant Capabilities",
    text = text,
    evidence = list(status = "Ready for inquiries"),
    plot = NULL,
    table = NULL
  )
}

# ------------------------------------------------------------------------------
# 8. Master Pipeline Coordinator & Compound Response Synthesizer
# ------------------------------------------------------------------------------

#' Execute the full Environmental Data Assistant analytical pipeline
#'
#' @param query_text User prompt
#' @param context Active session conversational context
#' @param base_data Genuine CPCB baseline observations
#' @param wide_data Pivoted CAAQM dataframe with CPCB sub-indices
#' @param metadata Metadata list
#' @param api_key_override Optional user-configured API key
#' @return A synthesized response object with text, evidence, plots, and tables
execute_assistant_pipeline <- function(query_text, context = list(), base_data = BASE_DATA, wide_data = WIDE_WITH_SUBINDICES, metadata = METADATA, api_key_override = NULL) {
  clean_q <- trimws(query_text)
  if (!nzchar(clean_q)) return(NULL)
  
  all_states <- sort(unique(base_data$state))
  all_cities <- sort(unique(base_data$city))
  
  # Step 1: Attempt AI Language Model Plan Translation if API key is configured
  ai_plans <- call_ai_model_if_available(clean_q, ASSISTANT_SCHEMA, api_key_override)
  
  # Step 2: Fallback to Deterministic Compound Parser if AI is unavailable or returned NULL
  plans_to_execute <- list()
  engine_used <- "deterministic_fallback"
  
  if (!is.null(ai_plans) && length(ai_plans) > 0) {
    # Validate each AI plan against allowlist
    valid_ai <- TRUE
    for (p in ai_plans) {
      if (is.null(p$operation) || !p$operation %in% ASSISTANT_SCHEMA$valid_operations) {
        valid_ai <- FALSE; break
      }
    }
    if (valid_ai) {
      plans_to_execute <- ai_plans
      engine_used <- "ai_language_model"
    }
  }
  
  if (length(plans_to_execute) == 0) {
    # Decompose into compound sub-questions
    decomp <- split_compound_question(clean_q)
    clauses <- decomp$clauses
    format_dir <- decomp$format_directive
    
    # Parse each clause into a structured plan with context inheritance
    parent_ent <- list()
    for (i in seq_along(clauses)) {
      cl <- clauses[i]
      ent <- extract_clause_entities(cl, parent_ent, context, all_states, all_cities)
      if (format_dir != "auto") ent$format <- format_dir
      
      pl <- plan_single_clause(cl, ent, parent_ent, context, metadata)
      plans_to_execute[[length(plans_to_execute) + 1]] <- pl
      
      # Update parent entities for subsequent clauses in the same prompt
      if (!is.null(ent$pollutant)) parent_ent$pollutant <- ent$pollutant
      if (length(ent$states) > 0) parent_ent$states <- ent$states
      if (length(ent$cities) > 0) parent_ent$cities <- ent$cities
    }
  }
  
  # Step 3: Execute Each Plan
  sub_results <- list()
  last_stns <- NULL
  
  for (i in seq_along(plans_to_execute)) {
    pl <- plans_to_execute[[i]]
    op <- pl$operation %||% "general_guide"
    
    res <- switch(
      op,
      "rank" = exec_op_rank(pl, base_data),
      "aggregate" = exec_op_aggregate(pl, base_data),
      "compare" = exec_op_compare(pl, base_data, wide_data),
      "compare_stations_pollutant" = exec_op_compare_stations_pollutant(pl, base_data, last_stns),
      "ratio" = exec_op_ratio(pl, wide_data),
      "missing" = exec_op_missing(pl, base_data),
      "outlier" = exec_op_outlier(pl, base_data),
      "flatline" = exec_op_flatline(pl, base_data),
      "aqi_breakdown" = exec_op_aqi(pl, wide_data),
      "lookup" = exec_op_lookup(pl, base_data),
      "temporal" = exec_op_temporal(pl, metadata),
      "metadata" = exec_op_metadata(pl, base_data, metadata),
      "clarification" = exec_op_clarification(pl),
      "unsupported" = exec_op_unsupported(pl),
      exec_op_guide(pl)
    )
    
    if (!is.null(res$last_stations)) last_stns <- res$last_stations
    sub_results[[length(sub_results) + 1]] <- res
  }
  
  # Step 4: Synthesize Compound Response
  n_sub <- length(sub_results)
  is_clarification <- any(sapply(sub_results, function(r) !is.null(r$options)))
  
  # If any sub-question requires clarification, surface the clarification prompt directly
  if (is_clarification) {
    clar_idx <- which(sapply(sub_results, function(r) !is.null(r$options)))[1]
    clar_res <- sub_results[[clar_idx]]
    return(list(
      type = "clarification",
      text = clar_res$text,
      options = clar_res$options,
      evidence = clar_res$evidence,
      plots = list(),
      tables = list(),
      subquestion_count = n_sub,
      engine_used = engine_used,
      context = context
    ))
  }
  
  # Assemble Text
  combined_text <- ""
  if (n_sub == 1) {
    combined_text <- sub_results[[1]]$text
  } else {
    combined_text <- "<strong>Multi-Part Inquiry Analysis:</strong><br/><br/>"
    for (i in seq_along(sub_results)) {
      sr <- sub_results[[i]]
      combined_text <- paste0(
        combined_text,
        "<strong>Part ", i, " (", sr$title, "):</strong><br/>",
        sr$text,
        if (i < n_sub) "<br/><br/>" else ""
      )
    }
  }
  
  # Consolidate Evidence
  consolidated_evidence <- list(
    dataset_mode = "CPCB CAAQM Baseline Snapshot",
    observation_timestamp = "23-10-2026 18:00 IST",
    subquestions_evaluated = n_sub,
    engine = if (engine_used == "ai_language_model") "AI Plan Translation + Safe R Engine" else "Semantic Parser + Safe R Engine"
  )
  for (i in seq_along(sub_results)) {
    sr_ev <- sub_results[[i]]$evidence
    if (!is.null(sr_ev)) {
      for (k in names(sr_ev)) {
        consolidated_evidence[[paste0("part", i, "_", k)]] <- sr_ev[[k]]
      }
    }
  }
  
  # Collect Visualizations & Tables
  collected_plots <- list()
  collected_tables <- list()
  for (i in seq_along(sub_results)) {
    sr <- sub_results[[i]]
    lbl <- if (n_sub > 1) paste0("Part ", i, ": ", sr$title) else sr$title
    if (!is.null(sr$plot)) collected_plots[[lbl]] <- sr$plot
    if (!is.null(sr$table)) collected_tables[[lbl]] <- sr$table
  }
  
  # Update Conversational Context
  updated_context <- context
  for (pl in plans_to_execute) {
    if (!is.null(pl$pollutant)) updated_context$pollutant <- pl$pollutant
    if (!is.null(pl$state)) updated_context$state <- pl$state
    if (!is.null(pl$city)) updated_context$city <- pl$city
    if (!is.null(pl$operation) && !pl$operation %in% c("clarification", "unsupported")) {
      updated_context$last_operation <- pl$operation
    }
  }
  if (!is.null(last_stns)) updated_context$last_stations <- last_stns
  
  list(
    type = "answer",
    text = combined_text,
    evidence = consolidated_evidence,
    options = NULL,
    plots = collected_plots,
    tables = collected_tables,
    subquestion_count = n_sub,
    engine_used = engine_used,
    context = updated_context,
    # Backward compatibility fields
    plot = if (length(collected_plots) > 0) collected_plots[[1]] else NULL,
    table = if (length(collected_tables) > 0) collected_tables[[1]] else NULL
  )
}

# ------------------------------------------------------------------------------
# 9. Backward Compatibility Wrappers (For Existing Audit Suites & Tests)
# ------------------------------------------------------------------------------

extract_assistant_entities <- function(query_text, current_context = list(), all_states = NULL, all_cities = NULL) {
  extract_clause_entities(query_text, list(), current_context, all_states, all_cities)
}

classify_assistant_intent <- function(query_text, entities, current_context = list()) {
  pl <- plan_single_clause(query_text, entities, list(), current_context, METADATA)
  intent_name <- switch(
    pl$operation,
    "rank" = if (!is.null(pl$group_by) && pl$group_by == "city") "RANKING_CITIES" else "RANKING_STATIONS",
    "aggregate" = "SUMMARY_POLLUTANT",
    "compare" = "COMPARISON_GEOGRAPHY",
    "ratio" = "COMPARISON_PARTICULATES",
    "missing" = "MISSING_DATA",
    "outlier" = "ANOMALY_OUTLIER",
    "flatline" = "ANOMALY_FLATLINE",
    "aqi_breakdown" = "AQI_BREAKDOWN",
    "lookup" = "LOCATION_LOOKUP",
    "temporal" = "HISTORICAL_TRENDS",
    "metadata" = "METADATA",
    "clarification" = "CLARIFICATION_NEEDED",
    "unsupported" = "UNSUPPORTED",
    "GENERAL_GUIDE"
  )
  list(
    intent = intent_name,
    clarification_type = pl$clarification_type,
    question = pl$question,
    options = pl$options,
    reason = pl$reason
  )
}

execute_assistant_query <- function(intent_obj, entities, base_data = BASE_DATA, wide_data = WIDE_WITH_SUBINDICES, metadata = METADATA) {
  # Wrap into execute_assistant_pipeline
  res <- execute_assistant_pipeline(
    query_text = entities$raw_clause %||% entities$raw_query %||% "General summary",
    context = list(pollutant = entities$pollutant, state = entities$state, city = entities$city),
    base_data = base_data,
    wide_data = wide_data,
    metadata = metadata
  )
  
  if (is.null(res)) {
    return(list(type = "answer", text = "No result produced.", evidence = list(), plot = NULL, table = NULL))
  }
  
  list(
    type = res$type,
    text = res$text,
    evidence = res$evidence,
    options = res$options,
    plot = res$plot,
    table = res$table,
    plots = res$plots,
    tables = res$tables
  )
}
