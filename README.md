# Smart City Environmental Intelligence Dashboard
### Advanced Data Visualization (CSI3005) — Lab Assessment Project
**Vellore Institute of Technology (VIT), Vellore**

---

## 1. Project Title & Overview
**Smart City Environmental Intelligence Dashboard: Real-Time Air Quality Monitoring, Historical Trend Analysis, and Pollution Hotspot Detection Across India.**

This project delivers an interactive environmental intelligence dashboard developed in **R and R Shiny**. Grounded strictly in authentic, empirical monitoring data published by the **Central Pollution Control Board (CPCB)** via the **Open Government Data (OGD) Platform India (`data.gov.in`)**, the application provides spatial hotspot analysis, telemetry health audits, criteria pollutant distribution tracking, and evidence-grounded environmental insights.

---

## 2. Dataset Documentation & Provenance
* **Official Source:** Central Pollution Control Board (CPCB), Ministry of Environment, Forest and Climate Change, Government of India.
* **Portal Page:** [data.gov.in: Real-time Air Quality Index from various locations](https://www.data.gov.in/resource/real-time-air-quality-index-various-locations)
* **Primary Resource File:** `data/air_quality.csv` (Preserved unaltered)
* **Dataset Characteristics:**
  * **3,682 observations** across **11 variables**.
  * **526 continuous ambient air quality monitoring stations (CAAQM)** represented in the dataset.
  * **273 cities** across **31 States and Union Territories**.
  * **7 Criteria Pollutants:** $PM_{2.5}$, $PM_{10}$, $NO_2$, $SO_2$, $CO$, $OZONE$, $NH_3$.
  * **Geographic Conformance:** 100% of station coordinates fall within Indian territorial boundaries (Latitude: $8.515^\circ - 34.147^\circ\text{N}$, Longitude: $70.780^\circ - 94.640^\circ\text{E}$).
  * **Missing Value Integrity:** 328 measurements ($8.91\%$) are missing in the raw feed. Missing readings are preserved as genuine `NA` values without silent zero-imputation.
  * **Baseline Snapshot Timestamp:** `23-10-2026 18:00 IST`.
  * **Data Status:** Offline Baseline Snapshot (data.gov.in upstream API status: HTTP 500 error handled gracefully).

---

## 3. Methodological Framework: CPCB Sub-Index Calculation
In strict accordance with the official CPCB National Air Quality Index (NAQI) guidelines:
1. **Sub-Index Formula:**
   $$I = I_{lo} + \frac{I_{hi} - I_{lo}}{B_{hi} - B_{lo}} \times (C - B_{lo})$$
2. **CPCB Minimum Data Criteria:**
   * An overall station sub-index is computed **only** if the station monitors at least **3 criteria pollutants**, of which at least **one must be a particulate** ($PM_{2.5}$ or $PM_{10}$).
   * Stations with fewer than 3 pollutants or missing particulate data are transparently classified as **Ineligible per CPCB criteria** (36 stations nationally).
3. **Caveat on Regulatory AQI vs Instantaneous Snapshots:**
   * Certified regulatory CPCB AQI values require 24-hour continuous rolling averages with at least 16 hours of valid data.
   * Single-timestamp snapshot values provide **indicative instantaneous sub-indices** and are labeled accordingly.

---

## 4. Project Directory Structure
```
advt project/
├── app.R                      # Main Shiny application entry point
├── global.R                   # Global environment, package loading & data loading
├── ui.R                       # bslib Bootstrap 5 navigation layout
├── server.R                   # Reactive server pipeline and module execution
├── R/
│   ├── package_check.R        # Automated check for required R packages
│   ├── data_loader.R          # Strict readr parsing, metadata tagging
│   ├── data_validator.R       # Geographic bounding box, NA audit, anomaly detection
│   ├── cpcb_subindex.R        # Official CPCB linear interpolation & eligibility check
│   ├── data_collector.R       # Official API fetcher, HTTP error handling & archival
│   ├── mod_overview.R         # Module 1: Executive Overview (KPIs, Leaflet map, ranking, DT table)
│   ├── mod_map.R              # Module 2: Geographic Hotspot Explorer (Leaflet, station telemetry, directory)
│   ├── mod_viz_studio.R       # Module 3: Advanced Visualization Studio (boxplots, density, scatter, heatmap, parcoords)
│   ├── mod_quality.R          # Module 4: Anomaly & Data Quality Monitor (missing heatmap, IQR/Z-score outliers, flatlines, CSV export)
│   ├── mod_trends.R           # Module 5: Temporal Trends & Gating Notice (longitudinal ingestion architecture)
│   └── mod_reports.R          # Module 6: Insights, Policy Evidence & Reports (extremes, benchmark DT, HTML executive briefing)
├── data/
│   ├── air_quality.csv        # Genuine CPCB baseline dataset
│   └── archive/               # Persistent time-series archival directory
├── www/
│   └── custom.css             # Accessible styling, glassmorphism cards, badges
└── README.md                  # Project documentation and execution guide
```

---

## 5. How to Run the Application in RStudio

### Step 1: Open the Project
1. Launch **RStudio**.
2. Open the project directory (`c:/Users/Anu/OneDrive/Desktop/advt project`) using **File > Open Project...** or set your working directory:
   ```r
   setwd("c:/Users/Anu/OneDrive/Desktop/advt project")
   ```

### Step 2: Verify Packages (One-Time Setup)
All required packages are pre-verified. If running on a new computer:
```r
install.packages(c(
  "shiny", "bslib", "shinyWidgets", "DT",
  "ggplot2", "plotly", "leaflet", "sf",
  "dplyr", "tidyr", "readr", "lubridate", "stringr",
  "scales", "viridis", "corrplot", "httr", "jsonlite", "curl"
))
```

### Step 3: Run the Application
In the RStudio console, execute:
```r
shiny::runApp()
```
Or open [app.R](file:///c:/Users/Anu/OneDrive/Desktop/advt%20project/app.R) and click the **Run App** button in the top-right corner of the RStudio editor pane.

---

## 6. Dashboard Modules & Pages

1. **Executive Overview (`mod_overview.R`):**
   * High-level environmental health KPIs (Stations, Cities, States/UTs, Completeness %, CPCB Category count).
   * Interactive cascading State/City and Pollutant filters with instant reset.
   * Top 15 ranked station Plotly bar chart with pollutant-specific unit formatting.
   * Interactive Leaflet overview map with dynamic color coding by pollutant concentration.
   * CPCB NAQI Category distribution donut chart (Good, Satisfactory, Moderate, Poor, Very Poor, Severe, Ineligible).
   * Criteria Pollutants tabular audit with min, mean, max, and completeness percentages.

2. **Geographic Hotspot Explorer (`mod_map.R`):**
   * Full-screen interactive Leaflet map of India featuring CartoDB-free ESRI and OSM tile layers.
   * Dual-mode station visualization: Continuous concentration gradient vs CPCB discrete categories.
   * Interactive marker selection triggering a comprehensive station telemetry inspection panel (displaying all 7 criteria pollutants).
   * Searchable and sortable DataTables station directory synchronized bidirectionally with the map markers.
   * Mini ranked comparison chart for immediate contextual benchmarking.

3. **Advanced Visualization Studio (`mod_viz_studio.R`):**
   * **Comparative Distributions:** Box plots and kernel density curves across top states/cities with skewness and dispersion analysis.
   * **Particulate Dynamics:** Co-located $PM_{2.5}$ vs $PM_{10}$ scatter plot with linear regression ($R^2 = 0.710, r = 0.843, \text{Slope} = 0.73$) and $PM_{2.5}/PM_{10}$ mass ratio histogram (national median 0.77).
   * **Inter-Pollutant Correlation:** Pearson and Spearman correlation heatmap with pairwise sample size ($N$) disclosure table.
   * **Small Multiples & Dimensionality:** Grouped bar charts with independent scales to avoid mixing $\mu\text{g/m}^3$ and $\text{mg/m}^3$, alongside normalized parallel coordinates.

4. **Anomaly Detection & Data Quality Monitor (`mod_quality.R`):**
   * 6 Data Quality KPI cards ($3,682$ records, $328$ missing NAs, $0$ duplicates, $526/526$ valid coordinates, $135$ IQR outliers, $12$ zero flatlines).
   * State-wise vs Pollutant-wise missingness heatmap and completeness audit.
   * Tukey Fence ($1.5 \times \text{IQR}$) and Z-score outlier detection tables with scientific explanations.
   * Sensor zero-flatline detection table ($12$ suspected sensor flatlines identified).
   * Geographic bounding box verification map confirming $100\%$ spatial validity within Indian borders.
   * Filtered quality audit CSV export handler.

5. **Historical Trends & Longitudinal Ingestion Engine (`mod_trends.R`):**
   * **Telemetry Dashboard:** Live tracking of distinct observation timestamps ($T$), total historical records, archived files, and supported analysis mode.
   * **Dual Ingestion Command Center:**
     * *Pathway 1 (Manual CSV Upload):* Drag-and-drop file uploader for genuine CPCB CSV snapshots with automated column validation and timestamp tagging.
     * *Pathway 2 (API Harvesting):* On-demand retrieval trigger querying the official CPCB CAAQM endpoint (Resource ID: 3b01bcb8) with explicit rate-limit (HTTP 429) and server-error (HTTP 500) diagnostic feedback.
   * **Deduplication Engine:** Enforces composite primary key `(station, pollutant_id, observation_time)` to strictly prevent duplicate records across multiple ingestions.
   * **Transparent State Machine:**
     * *Baseline Mode ($T = 1$):* Educates users on data ethics, explains why multi-hour trajectories require $\ge 2$ timestamps to avoid synthetic data fabrication, and keeps the page fully visible with live ingestion controls.
     * *Longitudinal Mode ($T \ge 2$):* Automatically unlocks interactive time-series trajectory line charts, station shift ranking bar charts ($\Delta$ accumulation vs dispersion), state-level progression curves, and longitudinal CSV data exports.

6. **Insights, Policy Evidence & Reports (`mod_reports.R`):**
   * Extreme contrast cards (Highest and Lowest recorded station averages for selected pollutant).
   * CPCB NAQI eligibility breakdown ($490/526$ stations eligible; $36$ stations transparently labeled Ineligible).
   * State-level comparison ranked bar chart with sample station count annotations.
   * Criteria pollutant regulatory benchmark comparison table.
   * Dynamic analytical insights container updated reactively upon filter changes.
   * CSV export handlers for filtered station observations and criteria pollutant summaries.
   * Self-contained, publication-ready Executive Briefing HTML report download handler.

---

## 7. Verification and Testing Summary
The entire application pipeline has been validated through rigorous automated unit tests and headless server tests:
* `scratch/test_stage1.R`: Validated package imports, strict data loading, schema integrity, missing-data audit, and CPCB sub-index math.
* `scratch/test_stage2.R`: Verified the reactive server pipeline, cascading state/city filters, pollutant selection, reset button, and HTML container rendering.
* `scratch/test_stage3.R`: Validated the Geographic Hotspot Explorer, marker color scales, popups, and DT directory synchronization.
* `scratch/test_stage4.R`: Verified all 7 Plotly charts (boxplots, density, scatter, correlation heatmap, parallel coordinates) and dynamic interpretation panels.
* `scratch/test_stage5.R`: Tested data quality KPI cards, Tukey fences, zero flatlines, and CSV download handler.
* `scratch/test_stage6.R`: Validated extreme contrast calculations, state comparison charts, dynamic briefings, CSV export handlers, and executive HTML briefing generator.
* `scratch/test_historical_trends.R`: Verified the historical ingestion workflow, testing both empty archive gating ($T = 1$) and unlocked multi-timestamp mode ($T = 2$) with deduplication and trajectory plotting.
* `scratch/audit_suite.R`: Executed the 11-point end-to-end evaluation audit covering all navigation pages, filters, genuine values, map markers, anomaly detection, exports, and offline status.
* `scratch/test_live_http_request.R`: Tested running the background Shiny application and confirmed HTTP Status 200 response on localhost.
