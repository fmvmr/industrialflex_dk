# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
#!/usr/bin/env Rscript
# ============================================================================
# Geographical Weighted Average: Spatial Disaggregation of Danish Industrial
# Electricity Consumption
#
# Methodology : Proportional (pro-rata) spatial disaggregation
#               (Eurostat, 2018; Hewings, 1977)
# Assumption  : Temporal stability of spatial shares — annual municipal
#               consumption shares within each DK10 group serve as valid
#               spatial allocation weights for hourly DK36/DK19 data.
#
# Data Sources (Energi Data Service / Energinet):
#   Dataset A : ConsumptionDK3619codehour  (hourly, DK36/DK19, DK1/DK2)
#   Dataset B : ConsumptionDK10            (annual, DK10, municipality)
#   Dataset C : ConsumptionIndustry        (hourly, 3 categories, municipality)
#
# Classification: Danmarks Statistik DB07 standard groupings
#   DK36 (36 groups) ⊂ DK19 (19 groups) ⊂ DK10 (10 groups)
#
# References:
#   Eurostat. (2018). ESS guidelines on temporal disaggregation, benchmarking
#     and reconciliation. Publications Office of the European Union.
#   Danmarks Statistik. (2007). Dansk Branchekode 2007 (DB07). Rev. Dec 2015.
#     https://www.dst.dk/da/Statistik/dokumentation/nomenklaturer/db07
#   Hewings, G. J. D. (1977). Regional industrial analysis and development.
#
# Author : [Your name]
# Date   : February 2026
# ============================================================================

# --- Package requirements ---------------------------------------------------
required_pkgs <- c("data.table", "httr", "jsonlite")
missing <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(missing) > 0L) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}
library(data.table)
library(httr)
library(jsonlite)

# ============================================================================
# 0. CONFIGURATION
# ============================================================================
API_BASE    <- "https://api.energidataservice.dk/dataset"
BATCH_LIMIT <- 100000L    # Max rows per API request
RATE_WAIT   <- 1.0        # Seconds between paginated requests
DATA_START  <- "2021-01-01"   # Complete hourly metering begins 2021-01-01
OUTPUT_DIR  <- "output"

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("================================================================\n")
cat("  Geographical Weighted Average: Spatial Disaggregation\n")
cat("  Danish Industrial Electricity Consumption\n")
cat("  Energi Data Service (Energinet)\n")
cat("================================================================\n\n")


# ============================================================================
# 1. DB07 CONCORDANCE TABLE: DK36 -> DK19 -> DK10
#    Source: Danmarks Statistik (2007). DB07 Standardgrupperinger.
#    https://www.dst.dk/da/Statistik/dokumentation/nomenklaturer/db07
#    https://www.dst.dk/pubfile/16252/21dbs
# ============================================================================

concordance <- data.table(
  DK36Code = c(
    # DK10 = 1: Landbrug, skovbrug og fiskeri
    "A",
    # DK10 = 2: Industri, raastoffer, forsyning
    "B", "CA", "CB", "CC", "CD", "CE", "CF", "CG", "CH", "CI", "CJ",
    "CK", "CL", "CM", "D", "E",
    # DK10 = 3: Bygge og anlaeg
    "F",
    # DK10 = 4: Handel og transport mv.
    "G", "H", "I",
    # DK10 = 5: Information og kommunikation
    "JA", "JB", "JC",
    # DK10 = 6: Finansiering og forsikring
    "K",
    # DK10 = 7: Ejendomshandel og udlejning
    "L",
    # DK10 = 8: Erhvervsservice
    "MA", "MB", "MC", "N",
    # DK10 = 9: Off. adm., undervisning, sundhed
    "O", "P", "QA", "QB",
    # DK10 = 10: Kultur, fritid, anden service
    "R", "S",
    # DK10 = 11: Uoplyst aktivitet
    "X"
  ),
  DK19Code = c(
    "A",
    "B", rep("C", 13), "D", "E",
    "F",
    "G", "H", "I",
    rep("J", 3),
    "K",
    "L",
    rep("M", 3), "N",
    "O", "P", "Q", "Q",
    "R", "S",
    "X"
  ),
  DK10Code = c(
    1L,
    rep(2L, 16),
    3L,
    rep(4L, 3),
    rep(5L, 3),
    6L,
    7L,
    rep(8L, 4),
    rep(9L, 4),
    rep(10L, 2),
    11L
  ),
  DK10Name = c(
    "Landbrug, skovbrug og fiskeri",
    rep("Industri, raastoffer, forsyning", 16),
    "Bygge og anlaeg",
    rep("Handel og transport mv.", 3),
    rep("Information og kommunikation", 3),
    "Finansiering og forsikring",
    "Ejendomshandel og udlejning",
    rep("Erhvervsservice", 4),
    rep("Off. adm., undervisning, sundhed", 4),
    rep("Kultur, fritid, anden service", 2),
    "Uoplyst aktivitet"
  )
)

setkey(concordance, DK36Code)
cat("Concordance table: ", nrow(concordance), " DK36 codes mapped to ",
    uniqueN(concordance$DK10Code), " DK10 groups\n\n")


# ============================================================================
# 2. API FETCH FUNCTIONS
# ============================================================================

#' Fetch a single page from the Energi Data Service API
#'
#' @param dataset Character. Dataset name (e.g. "ConsumptionDK10")
#' @param start   Character or NULL. ISO date start filter
#' @param end     Character or NULL. ISO date end filter
#' @param limit   Integer. Max rows to return
#' @param offset  Integer. Row offset for pagination
#' @param filters Named list or NULL. Column filters as list
#' @return List with $records (data.table) and $total (integer)
fetch_page <- function(dataset, start = NULL, end = NULL,
                       limit = BATCH_LIMIT, offset = 0L,
                       filters = NULL) {
  
  url <- paste0(API_BASE, "/", dataset)
  
  query <- list(limit = limit, offset = offset)
  if (!is.null(start))   query$start   <- start
  if (!is.null(end))     query$end     <- end
  if (!is.null(filters)) query$filter  <- toJSON(filters, auto_unbox = TRUE)
  
  msg <- sprintf("  GET %s (offset=%d, limit=%d)", dataset, offset, limit)
  cat(msg, "... ")
  
  resp <- GET(url, query = query, timeout(120))
  stop_for_status(resp, task = paste("fetch", dataset))
  body <- content(resp, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(body, flatten = TRUE)
  
  records <- as.data.table(parsed$records)
  total   <- as.integer(parsed$total)
  
  cat(sprintf("got %d rows (total: %s)\n", nrow(records),
              format(total, big.mark = ",")))
  
  list(records = records, total = total)
}


#' Fetch all records with automatic pagination
#'
#' @param dataset Character. Dataset name
#' @param ...     Additional arguments passed to fetch_page
#' @return data.table with all records
fetch_all <- function(dataset, ...) {
  cat(sprintf("\n--- Fetching full dataset: %s ---\n", dataset))
  
  all_records <- list()
  offset <- 0L
  total  <- NA_integer_
  batch  <- 1L
  
  repeat {
    page <- fetch_page(dataset, offset = offset, ...)
    all_records[[batch]] <- page$records
    total <- page$total
    
    offset <- offset + nrow(page$records)
    batch  <- batch + 1L
    
    if (offset >= total || nrow(page$records) == 0L) break
    Sys.sleep(RATE_WAIT)
  }
  
  dt <- rbindlist(all_records, use.names = TRUE, fill = TRUE)
  cat(sprintf("  => Total fetched: %s rows\n\n",
              format(nrow(dt), big.mark = ",")))
  dt
}


# ============================================================================
# 3. STEP 1 — INSPECT DATASET SCHEMAS
#    Fetch small samples to discover column names and data structure.
# ============================================================================

step1_inspect <- function() {
  cat("================================================================\n")
  cat("STEP 1: Inspecting dataset schemas\n")
  cat("================================================================\n")
  
  # --- Dataset A: DK36/DK19 hourly ---
  cat("\n[A] ConsumptionDK3619codehour\n")
  page_a <- fetch_page("ConsumptionDK3619codehour",
                       start = "2024-01-01", end = "2024-01-02",
                       limit = 50L)
  cat("  Columns:", paste(names(page_a$records), collapse = ", "), "\n")
  if ("DK36Code" %in% names(page_a$records)) {
    cat("  DK36 codes in sample:", paste(unique(page_a$records$DK36Code),
                                         collapse = ", "), "\n")
  }
  
  # --- Dataset B: DK10 annual regional ---
  cat("\n[B] ConsumptionDK10\n")
  page_b <- fetch_page("ConsumptionDK10", limit = 50L)
  cat("  Columns:", paste(names(page_b$records), collapse = ", "), "\n")
  cat("  Total records:", format(page_b$total, big.mark = ","), "\n")
  
  # --- Dataset C: ConsumptionIndustry (control) ---
  cat("\n[C] ConsumptionIndustry\n")
  page_c <- fetch_page("ConsumptionIndustry",
                       start = "2024-01-01", end = "2024-01-02",
                       limit = 50L)
  cat("  Columns:", paste(names(page_c$records), collapse = ", "), "\n")
  cat("  Total records:", format(page_c$total, big.mark = ","), "\n")
  
  list(sample_a = page_a$records,
       sample_b = page_b$records,
       sample_c = page_c$records)
}


# ============================================================================
# 4. COLUMN NAME DETECTION
#    API column names may vary (camelCase, mixed case). These helpers find
#    the actual column name matching a pattern, falling back gracefully.
# ============================================================================

find_col <- function(dt, patterns, required = TRUE) {
  nms <- names(dt)
  for (p in patterns) {
    match <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    if (length(match) >= 1L) return(match[1L])
  }
  if (required) {
    stop("Could not find column matching: ",
         paste(patterns, collapse = " / "),
         "\n  Available: ", paste(nms, collapse = ", "))
  }
  NA_character_
}


# ============================================================================
# 5. STEP 2 — BUILD REGIONAL ALLOCATION WEIGHTS
#
#    For each DK10 group k, municipality r, year y, price area p:
#        w_{k,r,y} = C^B_{k,r,y} / sum_{r' in p} C^B_{k,r',y}
#
#    Weights are computed WITHIN each price area so that the disaggregated
#    hourly values sum correctly to the price-area totals from Dataset A.
# ============================================================================

step2_build_weights <- function(dt_dk10, target_year = NULL) {
  
  cat("================================================================\n")
  cat("STEP 2: Computing regional allocation weights\n")
  cat("================================================================\n")
  
  # --- Detect column names ---
  col_dk10  <- find_col(dt_dk10, c("^DK10", "dk10"))
  col_muni  <- find_col(dt_dk10, c("MunicipalityNo", "municipalno",
                                   "municipality.*no"))
  col_cons  <- find_col(dt_dk10, c("Consumption", "kwh", "kWh"))
  col_year  <- find_col(dt_dk10, c("^Year$", "year"), required = FALSE)
  col_pa    <- find_col(dt_dk10, c("PriceArea", "pricearea"), required = FALSE)
  col_mname <- find_col(dt_dk10, c("MunicipalityName", "municipality.*name"),
                        required = FALSE)
  
  cat(sprintf("  Column mapping:\n"))
  cat(sprintf("    DK10 code    : %s\n", col_dk10))
  cat(sprintf("    Municipality : %s\n", col_muni))
  cat(sprintf("    Consumption  : %s\n", col_cons))
  cat(sprintf("    Year         : %s\n", ifelse(is.na(col_year), "[none]", col_year)))
  cat(sprintf("    Price area   : %s\n", ifelse(is.na(col_pa), "[none — national weights]", col_pa)))
  
  # --- Standardise to working names using base R ---
  dt <- copy(dt_dk10)
  setnames(dt, col_dk10, "dk10_code", skip_absent = TRUE)
  setnames(dt, col_muni, "muni_no", skip_absent = TRUE)
  setnames(dt, col_cons, "cons_kwh", skip_absent = TRUE)
  if (!is.na(col_year))  setnames(dt, col_year, "year", skip_absent = TRUE)
  if (!is.na(col_pa))    setnames(dt, col_pa, "price_area", skip_absent = TRUE)
  if (!is.na(col_mname)) setnames(dt, col_mname, "muni_name", skip_absent = TRUE)
  
  # Coerce consumption to numeric (API sometimes returns character)
  dt[, cons_kwh := as.numeric(cons_kwh)]
  
  # --- Filter to target year ---
  if (!is.null(target_year) && "year" %in% names(dt)) {
    dt <- dt[year == target_year]
    cat(sprintf("  Filtered to year %d: %s rows\n", target_year,
                format(nrow(dt), big.mark = ",")))
  } else if (!is.null(target_year)) {
    cat("  Warning: no 'year' column — using all available data\n")
  }
  
  available_years <- if ("year" %in% names(dt)) sort(unique(dt$year)) else "all"
  cat(sprintf("  Available years: %s\n", paste(available_years, collapse = ", ")))
  
  # --- Remove rows with zero or NA consumption ---
  dt <- dt[!is.na(cons_kwh) & cons_kwh > 0]
  
  # --- Compute weights within price area (or nationally if no PA column) ---
  if ("price_area" %in% names(dt)) {
    cat("  Computing weights WITHIN each price area (DK1/DK2)\n")
    group_totals <- dt[, .(group_total = sum(cons_kwh, na.rm = TRUE)),
                       by = .(dk10_code, price_area)]
    dt <- merge(dt, group_totals, by = c("dk10_code", "price_area"), all.x = TRUE)
  } else {
    cat("  No PriceArea column — computing NATIONAL weights\n")
    group_totals <- dt[, .(group_total = sum(cons_kwh, na.rm = TRUE)),
                       by = .(dk10_code)]
    dt <- merge(dt, group_totals, by = "dk10_code", all.x = TRUE)
  }
  
  dt[, weight := fifelse(group_total > 0, cons_kwh / group_total, 0)]
  
  # --- Validate: weights must sum to 1 within each DK10 × PriceArea ---
  if ("price_area" %in% names(dt)) {
    weight_sums <- dt[, .(wsum = sum(weight)), by = .(dk10_code, price_area)]
  } else {
    weight_sums <- dt[, .(wsum = sum(weight)), by = .(dk10_code)]
  }
  
  cat(sprintf("\n  Weight-sum validation (should all be ~1.0):\n"))
  cat(sprintf("    Min  : %.6f\n", min(weight_sums$wsum)))
  cat(sprintf("    Max  : %.6f\n", max(weight_sums$wsum)))
  cat(sprintf("    Mean : %.6f\n", mean(weight_sums$wsum)))
  
  # Flag any problematic groups
  bad <- weight_sums[abs(wsum - 1) > 0.001]
  if (nrow(bad) > 0L) {
    cat(sprintf("  WARNING: %d groups have weight sums deviating > 0.001:\n",
                nrow(bad)))
    print(bad)
  }
  
  # --- Build output weight table ---
  keep_cols <- c("dk10_code", "muni_no", "weight")
  if ("price_area" %in% names(dt)) keep_cols <- c(keep_cols, "price_area")
  if ("muni_name" %in% names(dt))  keep_cols <- c(keep_cols, "muni_name")
  if ("year" %in% names(dt))       keep_cols <- c(keep_cols, "year")
  weights <- dt[, ..keep_cols]
  
  cat(sprintf("\n  => %d weights across %d municipalities, %d DK10 groups\n",
              nrow(weights), uniqueN(weights$muni_no),
              uniqueN(weights$dk10_code)))
  
  weights
}


# ============================================================================
# 6. STEP 3 — SPATIAL DISAGGREGATION
#
#    For each DK36 code i, municipality r, hour t (within year y):
#        C_hat_{i,r,t} = C^A_{i,t} * w_{k(i),r,y}
#
#    where k(i) maps DK36 code i to its DK10 parent via the concordance.
# ============================================================================

step3_disaggregate <- function(dt_hourly, weights) {
  
  cat("================================================================\n")
  cat("STEP 3: Spatial disaggregation of hourly consumption\n")
  cat("================================================================\n")
  
  # --- Detect columns in hourly data ---
  col_dk36 <- find_col(dt_hourly, c("^DK36", "dk36"))
  col_dk19 <- find_col(dt_hourly, c("^DK19", "dk19"), required = FALSE)
  col_hour <- find_col(dt_hourly, c("HourUTC", "hourutc"))
  col_cons <- find_col(dt_hourly, c("Consumption", "kwh", "kWh"))
  col_pa_a <- find_col(dt_hourly, c("PriceArea", "pricearea"), required = FALSE)
  
  cat(sprintf("  Hourly data columns: DK36=%s, Hour=%s, Consumption=%s\n",
              col_dk36, col_hour, col_cons))
  
  # --- Standardise column names ---
  dt <- copy(dt_hourly)
  setnames(dt, col_dk36, "dk36_code", skip_absent = TRUE)
  setnames(dt, col_hour, "hour_utc", skip_absent = TRUE)
  setnames(dt, col_cons, "cons_kwh_national", skip_absent = TRUE)
  if (!is.na(col_dk19)) setnames(dt, col_dk19, "dk19_code", skip_absent = TRUE)
  if (!is.na(col_pa_a)) setnames(dt, col_pa_a, "price_area_a", skip_absent = TRUE)
  
  dt[, cons_kwh_national := as.numeric(cons_kwh_national)]
  
  # --- Map DK36 -> DK10 via concordance ---
  dt <- merge(dt, concordance[, .(DK36Code, DK19Code, DK10Code, DK10Name)],
              by.x = "dk36_code", by.y = "DK36Code",
              all.x = TRUE, sort = FALSE)
  
  # Rename merged columns
  setnames(dt, c("DK10Code", "DK10Name", "DK19Code"),
           c("dk10_code", "dk10_name", "dk19_code_conc"),
           skip_absent = TRUE)
  
  # Report unmapped codes
  unmapped <- unique(dt[is.na(dk10_code), dk36_code])
  if (length(unmapped) > 0L) {
    cat(sprintf("  WARNING: %d DK36 code(s) not in concordance (GDPR-merged?): %s\n",
                length(unmapped), paste(unmapped, collapse = ", ")))
    cat("    => These will be EXCLUDED from disaggregation.\n")
    cat("    => You may need to manually map them to a DK10 parent.\n")
    dt <- dt[!is.na(dk10_code)]
  }
  
  n_dk36 <- uniqueN(dt$dk36_code)
  n_hours <- uniqueN(dt$hour_utc)
  cat(sprintf("  Mapped: %d unique DK36 codes, %d hours\n", n_dk36, n_hours))
  
  # --- Merge with regional weights ---
  # Join on dk10_code (and price_area if both datasets have it)
  merge_keys <- "dk10_code"
  if ("price_area" %in% names(weights) && "price_area_a" %in% names(dt)) {
    setnames(dt, "price_area_a", "price_area", skip_absent = TRUE)
    merge_keys <- c(merge_keys, "price_area")
    cat("  Merging on: dk10_code + price_area (within-area weights)\n")
  } else {
    cat("  Merging on: dk10_code only (national weights)\n")
  }
  
  merged <- merge(dt, weights, by = merge_keys,
                  all.x = TRUE, allow.cartesian = TRUE, sort = FALSE)
  
  # Check for unmatched DK10 codes
  n_na_weight <- sum(is.na(merged$weight))
  if (n_na_weight > 0L) {
    cat(sprintf("  WARNING: %d rows have no matching weight (missing municipalities)\n",
                n_na_weight))
    merged <- merged[!is.na(weight)]
  }
  
  # --- Apply disaggregation formula ---
  merged[, cons_kwh_regional := cons_kwh_national * weight]
  
  n_munis <- uniqueN(merged$muni_no)
  cat(sprintf("\n  => Output: %s rows (DK36 x municipality x hour)\n",
              format(nrow(merged), big.mark = ",")))
  cat(sprintf("     %d DK36 codes x %d municipalities x %d hours\n",
              n_dk36, n_munis, n_hours))
  
  merged
}


# ============================================================================
# 7. STEP 4 — VALIDATION AGAINST DATASET C
#
#    Sum disaggregated consumption per municipality per hour and compare
#    against ConsumptionIndustry totals.
#
#    Metric: delta_{r,t} = (C_hat - C_control) / C_control * 100
# ============================================================================

step4_validate <- function(disaggregated, dt_control) {
  
  cat("================================================================\n")
  cat("STEP 4: Validation against ConsumptionIndustry (Dataset C)\n")
  cat("================================================================\n")
  
  # --- Aggregate disaggregated data: sum all DK36 per municipality per hour ---
  constructed <- disaggregated[, .(constructed_kwh = sum(cons_kwh_regional,
                                                         na.rm = TRUE)),
                               by = .(hour_utc, muni_no)]
  
  # --- Prepare control dataset ---
  col_hour_c <- find_col(dt_control, c("HourUTC", "hourutc"))
  col_muni_c <- find_col(dt_control, c("MunicipalityNo", "municipalno",
                                       "municipality.*no"))
  col_cons_c <- find_col(dt_control, c("Consumption", "kwh", "kWh"))
  
  ctrl <- copy(dt_control)
  setnames(ctrl, col_hour_c, "hour_utc", skip_absent = TRUE)
  setnames(ctrl, col_muni_c, "muni_no", skip_absent = TRUE)
  setnames(ctrl, col_cons_c, "control_kwh", skip_absent = TRUE)
  ctrl[, control_kwh := as.numeric(control_kwh)]
  
  # Sum across consumer categories per municipality per hour
  ctrl_agg <- ctrl[, .(control_kwh = sum(control_kwh, na.rm = TRUE)),
                   by = .(hour_utc, muni_no)]
  
  # --- Merge constructed and control ---
  validation <- merge(constructed, ctrl_agg,
                      by = c("hour_utc", "muni_no"),
                      all = FALSE)
  
  # --- Compute deviation ---
  validation[, deviation_pct := fifelse(
    control_kwh != 0,
    (constructed_kwh - control_kwh) / control_kwh * 100,
    NA_real_
  )]
  
  validation[, abs_dev := abs(deviation_pct)]
  
  # --- Summary statistics ---
  n_obs <- nrow(validation)
  cat(sprintf("\n  Validation sample: %s municipality-hour observations\n",
              format(n_obs, big.mark = ",")))
  cat(sprintf("  Municipalities matched: %d\n", uniqueN(validation$muni_no)))
  cat(sprintf("  Hours matched: %d\n\n", uniqueN(validation$hour_utc)))
  
  if (n_obs > 0L) {
    cat("  Deviation statistics (constructed vs. control):\n")
    cat(sprintf("    Mean absolute deviation     : %7.2f%%\n",
                mean(validation$abs_dev, na.rm = TRUE)))
    cat(sprintf("    Median absolute deviation   : %7.2f%%\n",
                median(validation$abs_dev, na.rm = TRUE)))
    cat(sprintf("    90th percentile             : %7.2f%%\n",
                quantile(validation$abs_dev, 0.90, na.rm = TRUE)))
    cat(sprintf("    95th percentile             : %7.2f%%\n",
                quantile(validation$abs_dev, 0.95, na.rm = TRUE)))
    cat(sprintf("    Max absolute deviation      : %7.2f%%\n",
                max(validation$abs_dev, na.rm = TRUE)))
    cat(sprintf("    Within +/- 5%%               : %6.1f%%\n",
                mean(validation$abs_dev < 5, na.rm = TRUE) * 100))
    cat(sprintf("    Within +/- 10%%              : %6.1f%%\n",
                mean(validation$abs_dev < 10, na.rm = TRUE) * 100))
    
    # Per-municipality summary
    cat("\n  Per-municipality mean absolute deviation (top 10 worst):\n")
    muni_dev <- validation[, .(mean_abs_dev = mean(abs_dev, na.rm = TRUE),
                               n_hours = .N),
                           by = muni_no]
    setorder(muni_dev, -mean_abs_dev)
    print(head(muni_dev, 10L))
  } else {
    cat("  WARNING: No matching municipality-hour pairs found.\n")
    cat("  Check that hour_utc formats and muni_no values align.\n")
  }
  
  validation
}


# ============================================================================
# 8. DIAGNOSTIC PLOTS (optional, requires ggplot2)
# ============================================================================

plot_diagnostics <- function(validation, weights, output_dir = OUTPUT_DIR) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cat("  ggplot2 not installed — skipping diagnostic plots.\n")
    return(invisible(NULL))
  }
  library(ggplot2)
  
  # --- Plot 1: Distribution of deviations ---
  p1 <- ggplot(validation, aes(x = deviation_pct)) +
    geom_histogram(binwidth = 1, fill = "#1F4E79", alpha = 0.8) +
    geom_vline(xintercept = 0, colour = "red", linewidth = 0.8) +
    labs(title = "Distribution of Disaggregation Deviations",
         subtitle = "Constructed (weighted avg) vs. Control (ConsumptionIndustry)",
         x = "Deviation (%)", y = "Count") +
    theme_minimal(base_size = 12)
  ggsave(file.path(output_dir, "deviation_distribution.png"),
         p1, width = 8, height = 5, dpi = 150)
  
  # --- Plot 2: Weight concentration by DK10 group ---
  if ("dk10_code" %in% names(weights)) {
    w_summary <- weights[, .(
      max_weight = max(weight),
      hhi = sum(weight^2),
      n_munis = .N
    ), by = dk10_code]
    
    p2 <- ggplot(w_summary, aes(x = factor(dk10_code), y = hhi)) +
      geom_col(fill = "#2E7D32", alpha = 0.8) +
      labs(title = "Geographic Concentration (HHI) by DK10 Industry Group",
           subtitle = "Higher HHI = consumption concentrated in fewer municipalities",
           x = "DK10 Group", y = "Herfindahl-Hirschman Index") +
      theme_minimal(base_size = 12)
    ggsave(file.path(output_dir, "weight_concentration_hhi.png"),
           p2, width = 8, height = 5, dpi = 150)
  }
  
  cat("  Diagnostic plots saved to:", output_dir, "\n")
}


# ============================================================================
# 9. MAIN EXECUTION PIPELINE
# ============================================================================

run_pipeline <- function(
    sample_start = "2024-06-01",  # Sample period for validation test
    sample_end   = "2024-06-08",
    weight_year  = 2024L,
    full_run     = FALSE          # Set TRUE to process full date range
) {
  t0 <- Sys.time()
  
  # ---- Phase 1: Inspect schemas -----------------------------------------
  cat("\n### PHASE 1: Inspect dataset schemas ###\n\n")
  schemas <- tryCatch(
    step1_inspect(),
    error = function(e) {
      cat("\n  ERROR: API request failed:\n  ", conditionMessage(e), "\n")
      cat("\n  Possible causes:\n")
      cat("    - No internet access to api.energidataservice.dk\n")
      cat("    - API rate limit exceeded (1 req/min/dataset)\n")
      cat("    - Dataset name changed on the platform\n")
      cat("\n  To run from local CSV files, use:\n")
      cat("    dt_dk10    <- fread('ConsumptionDK10.csv')\n")
      cat("    dt_dk36    <- fread('ConsumptionDK3619codehour.csv')\n")
      cat("    dt_control <- fread('ConsumptionIndustry.csv')\n")
      cat("  Then call the step functions individually.\n")
      return(NULL)
    }
  )
  if (is.null(schemas)) return(invisible(NULL))
  
  # ---- Phase 2: Fetch DK10 and compute weights --------------------------
  cat("\n### PHASE 2: Fetch DK10 data & compute regional weights ###\n\n")
  dt_dk10 <- fetch_all("ConsumptionDK10")
  
  # Determine available years
  year_col <- find_col(dt_dk10, c("^Year$", "year"), required = FALSE)
  if (!is.na(year_col)) {
    available_years <- sort(unique(dt_dk10[[year_col]]))
    cat("  Available years in DK10 data:", paste(available_years, collapse = ", "), "\n")
    if (!(weight_year %in% available_years)) {
      weight_year <- max(available_years)
      cat(sprintf("  Requested year not available — using %d instead\n", weight_year))
    }
  }
  
  weights <- step2_build_weights(dt_dk10, target_year = weight_year)
  
  # Save weights
  fwrite(weights, file.path(OUTPUT_DIR, sprintf("regional_weights_%d.csv", weight_year)))
  cat("  Weights saved to:", file.path(OUTPUT_DIR,
                                       sprintf("regional_weights_%d.csv\n", weight_year)))
  
  # ---- Phase 3: Fetch hourly DK36 and disaggregate ----------------------
  cat("\n### PHASE 3: Fetch hourly DK36 data & disaggregate ###\n\n")
  
  if (full_run) {
    cat("  FULL RUN: Fetching all data from", DATA_START, "onwards\n")
    cat("  This may take a long time and substantial memory.\n")
    dt_dk36 <- fetch_all("ConsumptionDK3619codehour", start = DATA_START)
  } else {
    cat(sprintf("  SAMPLE RUN: %s to %s\n", sample_start, sample_end))
    cat("  Set full_run = TRUE for complete processing.\n")
    dt_dk36 <- fetch_all("ConsumptionDK3619codehour",
                         start = sample_start, end = sample_end)
  }
  
  disaggregated <- step3_disaggregate(dt_dk36, weights)
  
  # Save disaggregated (sample or full)
  out_file <- ifelse(full_run,
                     "disaggregated_consumption_full.csv",
                     "disaggregated_consumption_sample.csv")
  fwrite(disaggregated, file.path(OUTPUT_DIR, out_file))
  cat("  Disaggregated data saved to:", file.path(OUTPUT_DIR, out_file), "\n")
  
  # ---- Phase 4: Fetch control and validate -------------------------------
  cat("\n### PHASE 4: Fetch control data & validate ###\n\n")
  
  if (full_run) {
    dt_control <- fetch_all("ConsumptionIndustry", start = DATA_START)
  } else {
    dt_control <- fetch_all("ConsumptionIndustry",
                            start = sample_start, end = sample_end)
  }
  
  validation <- step4_validate(disaggregated, dt_control)
  fwrite(validation, file.path(OUTPUT_DIR, "validation_results.csv"))
  cat("  Validation saved to:", file.path(OUTPUT_DIR, "validation_results.csv\n"))
  
  # ---- Phase 5: Diagnostic plots ----------------------------------------
  cat("\n### PHASE 5: Diagnostic plots ###\n\n")
  plot_diagnostics(validation, weights)
  
  # ---- Summary -----------------------------------------------------------
  elapsed <- round(difftime(Sys.time(), t0, units = "mins"), 1)
  cat("\n================================================================\n")
  cat("  PIPELINE COMPLETE\n")
  cat(sprintf("  Elapsed: %s minutes\n", elapsed))
  cat(sprintf("  Output directory: %s/\n", OUTPUT_DIR))
  cat("  Files:\n")
  cat(sprintf("    - regional_weights_%d.csv\n", weight_year))
  cat(sprintf("    - %s\n", out_file))
  cat("    - validation_results.csv\n")
  cat("    - deviation_distribution.png (if ggplot2 available)\n")
  cat("    - weight_concentration_hhi.png (if ggplot2 available)\n")
  cat("================================================================\n")
  
  invisible(list(
    weights       = weights,
    disaggregated = disaggregated,
    validation    = validation,
    concordance   = concordance
  ))
}


# ============================================================================
# 10. ENTRY POINT
# ============================================================================

# Run the pipeline with a one-week sample for validation
# Change full_run = TRUE and adjust dates for production use
results <- run_pipeline(
  sample_start = "2024-06-01",
  sample_end   = "2024-06-08",
  weight_year  = 2024L,
  full_run     = FALSE
)
view(results)
