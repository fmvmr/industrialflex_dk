# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# IV INSTRUMENT FINAL VERSION:
fixest::setFixest_notes(FALSE)

# (commented out for reproducibility: refers to a model created later in the script)
# summary(IV_model_het_DK10_W$`sample.var: DK36_en; sample: Consulting services`, diagnostics = TRUE)
# =============================================================================
# = PART ONE: DATA PREPARATION  ==============================================
# =============================================================================
# ── Packages ───────────────────────────────────────────────────────────────####

invisible(lapply(
  c("arrow", "dplyr", "lubridate", "stringr", "purrr", "glue", "readxl",
    "tidyr", "fixest", "readr", "ggplot2", "rlang", "tibble", "patchwork",
    "progress", "plm", "AER", "ivreg", "lmtest", "sandwich", "stargazer",
    "kableExtra", "scales", "sensemakr", "writexl", "knitr"),
  library, character.only = TRUE
))


# ── Load data ──────────────────────────────────────────────────────────────####
load_data <- function(years, months = 1:12) {
  safe_read <- function(path) if (file.exists(path)) arrow::read_parquet(path)
  bind_nn   <- function(lst)  dplyr::bind_rows(Filter(Negate(is.null), lst))
  yms       <- unlist(lapply(years, function(y) sprintf("%d_%02d", y, months)))
  monthly_types <- list(
    prices      = "elspotprices_monthly/elspot",
    forecast    = "forecast_hourly_monthly_compact/forecast_compact",
    temp        = "temp_zone_hourly_monthly/temp_zone",
    industry    = "consumption_industry_hourly_monthly/consumption_industry_hour"
  )
  c(
    lapply(monthly_types, function(p)
      bind_nn(lapply(yms, function(ym) safe_read(glue("raw/{p}_{ym}.parquet"))))
    ),
    list(
      industry_annual = bind_nn(lapply(years, function(y)
        safe_read(glue("raw/consumption_dk10_region_year/consumption_dk10_region_{y}.parquet"))
      )),
      generation = bind_nn(lapply(years, function(y)
        bind_nn(lapply(c("DK1", "DK2"), function(area)
          safe_read(glue("raw/generation_prod_type_exchange/generation_{area}_{y}.parquet"))
        ))
      )),
      gas    = arrow::read_parquet("raw/controls/gas_daily_2020_2025.parquet"),
      carbon = arrow::read_parquet("raw/controls/carbon_daily_2020_2025.parquet"),
      coal   = arrow::read_parquet("raw/controls/coal_daily_2020_2025.parquet")
    )
  )
}

# ── Pull into environment ──────────────────────────────────────────────────####
d <- load_data(2021:2025)
list2env(d[c("prices","forecast","temp","industry","industry_annual","generation",
             "gas","carbon","coal")], .GlobalEnv)


# ── DK19 → DK10 mapping ────────────────────────────────────────────────────####
dk19_to_dk10 <- tibble::tribble(
  ~DK19Title,                                               ~DK10Title,
  "Landbrug, skovbrug og fiskeri",                          "Landbrug, skovbrug og fiskeri",
  "Råstofindvinding & Vandforsyning og renovation",         "Industri, råstofindvinding og forsyningsvirksomhed",
  "Industri",                                               "Industri, råstofindvinding og forsyningsvirksomhed",
  "Energiforsyning",                                        "Industri, råstofindvinding og forsyningsvirksomhed",
  "Bygge og anlæg",                                         "Bygge og anlæg",
  "Handel",                                                 "Handel og transport",
  "Transport",                                              "Handel og transport",
  "Hoteller og restauranter",                               "Handel og transport",
  "Information og kommunikation",                           "Information og kommunikation",
  "Finansiering og forsikring",                             "Finansiering og forsikring",
  "Ejendomshandel og udlejning",                            "Ejendomshandel og udlejning",
  "Videnservice",                                           "Erhvervsservice",
  "Rejsebureauer, rengøring og anden operationel service",  "Erhvervsservice",
  "Offentlig administration, forsvar og politi",            "Offentlig administration, undervisning og sundhed",
  "Undervisning",                                           "Offentlig administration, undervisning og sundhed",
  "Sundhed og socialvæsen",                                 "Offentlig administration, undervisning og sundhed",
  "Kultur og fritid",                                       "Kultur, fritid og anden service",
  "Andre serviceydelser mv",                                "Kultur, fritid og anden service",
  "Privat",                                                 NA_character_,
  "Uoplyst aktivitet",                                      NA_character_
)

DK2_REGIONS <- c("Region Hovedstaden", "Region Sjælland")
DK1_REGIONS <- c("Region Syddanmark", "Region Midtjylland", "Region Nordjylland")

# ── Annual consumption zone weights ────────────────────────────────────────####
annual_zone_share <- industry_annual %>%
  filter(DK10Title != "Privat") %>%
  mutate(
    Year = as.integer(format(Year, "%Y")),
    Zone = case_when(
      RegionName %in% DK2_REGIONS ~ "DK2",
      RegionName %in% DK1_REGIONS ~ "DK1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Zone)) %>%
  group_by(Year, DK10Title, Zone) %>%
  summarise(Cons = sum(ConsumptionkWh, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Zone, values_from = Cons, values_fill = 0) %>%
  mutate(w_DK1_c = if_else(DK1 + DK2 > 0, DK1 / (DK1 + DK2), NA_real_)) %>%
  select(Year, DK10Title, w_DK1_c)

industry <- industry %>%
  left_join(dk19_to_dk10, by = "DK19Title") %>%
  mutate(Year = as.integer(format(TimeDK, "%Y"))) %>%
  left_join(annual_zone_share, by = c("Year", "DK10Title"))

# ── DK36 mapping ───────────────────────────────────────────────────────────####
dk36_mapping <- tibble(DK36_group = unique(industry$DK36Code)) %>%
  filter(DK36_group != "-") %>%
  mutate(IndustryCode = str_split(DK36_group, "_")) %>%
  unnest(IndustryCode)

# ── Employee weights ───────────────────────────────────────────────────────####
total_employees <- bind_rows(lapply(
  c("employees2021.csv", "Employees2022.csv", "employees2023.csv", "employees2024.csv"),
  read_delim, delim = ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE
)) %>%
  select(-X1) %>%
  rename(year = X2, status = X3, age = X4, industry = X5,
         region_hovedstaden = X6, region_sjaelland = X7,
         region_syddanmark = X8, region_midtjylland = X9, region_nordjylland = X10) %>%
  group_by(year, industry) %>%
  summarise(across(starts_with("region_"), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    DK1          = region_syddanmark + region_midtjylland + region_nordjylland,
    DK2          = region_hovedstaden + region_sjaelland,
    IndustryCode = str_extract(industry, "^[A-Z]+")
  ) %>%
  left_join(dk36_mapping, by = "IndustryCode") %>%
  group_by(year, DK36_group) %>%
  summarise(DK1 = sum(DK1, na.rm = TRUE), DK2 = sum(DK2, na.rm = TRUE), .groups = "drop") %>%
  mutate(w_DK1_emp = DK1 / (DK1 + DK2)) %>%
  {bind_rows(., filter(., year == 2024) %>% mutate(year = 2025))}

# ── Firm-count weights ─────────────────────────────────────────────────────####
firms_long <- read_excel("DK36 Regional Split.xlsx") %>%
  pivot_longer(cols = c(`2021`, `2022`, `2023`), names_to = "year", values_to = "n_firms") %>%
  mutate(
    year = as.integer(year),
    zone = case_when(
      Region %in% DK2_REGIONS ~ "DK2",
      Region %in% DK1_REGIONS ~ "DK1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(zone)) %>%
  group_by(DK36_Code, year, zone) %>%
  summarise(firms = sum(n_firms, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = zone, values_from = firms, values_fill = 0) %>%
  left_join(dk36_mapping, by = c("DK36_Code" = "IndustryCode")) %>%
  filter(!is.na(DK36_group)) %>%
  group_by(year, DK36_group) %>%
  summarise(DK1 = sum(DK1, na.rm = TRUE), DK2 = sum(DK2, na.rm = TRUE), .groups = "drop") %>%
  mutate(w_DK1_firm = DK1 / (DK1 + DK2)) %>%
  {bind_rows(.,
             filter(., year == 2023) %>% mutate(year = 2024),
             filter(., year == 2023) %>% mutate(year = 2025)
  )} %>%
  select(w_DK1_firm, year, DK36_group)
# ── Attach all weights to industry ─────────────────────────────────────────####
industry <- industry %>%
  mutate(Year = as.integer(format(TimeUTC, "%Y"))) %>%
  left_join(select(total_employees, year, DK36_group, w_DK1_emp),
            by = c("Year" = "year", "DK36Code" = "DK36_group")) %>%
  left_join(firms_long,
            by = c("Year" = "year", "DK36Code" = "DK36_group"))
# ── Filter invalid sector codes ────────────────────────────────────────────####
industry <- industry %>%
  filter(DK36Code != "PR",
         !(DK36Code == "-" & DK19Code == "-"))

exclude_sectors <- c("Energiforsyning", "Råstofindvinding & Vandforsyning og renovation")
industry <- industry %>%
  dplyr::filter(!DK36Title %in% exclude_sectors)


# ── Weighted electricity prices ────────────────────────────────────────────####
prices_wide <- prices %>%
  arrange(HourDK, PriceArea) %>%
  select(HourUTC, PriceArea, SpotPriceEUR) %>%
  pivot_wider(names_from = PriceArea, values_from = SpotPriceEUR, values_fn = first)

weighted_prices <- industry %>%
  distinct(TimeUTC, DK10Title, DK19Title, DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  left_join(prices_wide, by = c("TimeUTC" = "HourUTC")) %>%
  mutate(
    P_weighted_c    = w_DK1_c    * DK1 + (1 - w_DK1_c)    * DK2,
    P_weighted_emp  = w_DK1_emp  * DK1 + (1 - w_DK1_emp)  * DK2,
    P_weighted_firm = w_DK1_firm * DK1 + (1 - w_DK1_firm) * DK2
  ) %>%
  rename(DK1_P = DK1, DK2_P = DK2)


# ── Wind and temperature instruments ───────────────────────────────────────####
wind_supply <- forecast %>%
  select(HourUTC, PriceArea, Wind_DayAhead) %>%
  pivot_wider(names_from = PriceArea, values_from = Wind_DayAhead, values_fn = first) %>%
  rename(Wind_DK1 = DK1, Wind_DK2 = DK2)

temp_combined <- temp %>%
  select(HourUTC, PriceArea, TempC) %>%
  pivot_wider(names_from = PriceArea, values_from = TempC, values_fn = first) %>%
  rename(Temp_DK1 = DK1, Temp_DK2 = DK2)

wind_temp <- left_join(temp_combined, wind_supply, by = "HourUTC")

industry <- industry %>%
  left_join(wind_temp, by = c("TimeUTC" = "HourUTC")) %>%
  mutate(
    Wind_c    = w_DK1_c    * Wind_DK1 + (1 - w_DK1_c)    * Wind_DK2,
    Wind_emp  = w_DK1_emp  * Wind_DK1 + (1 - w_DK1_emp)  * Wind_DK2,
    Wind_firm = w_DK1_firm * Wind_DK1 + (1 - w_DK1_firm) * Wind_DK2,
    Temp_c    = w_DK1_c    * Temp_DK1 + (1 - w_DK1_c)    * Temp_DK2,
    Temp_emp  = w_DK1_emp  * Temp_DK1 + (1 - w_DK1_emp)  * Temp_DK2,
    Temp_firm = w_DK1_firm * Temp_DK1 + (1 - w_DK1_firm) * Temp_DK2
  )


# ── Fuel controls (pre-joined) ─────────────────────────────────────────────####
fuel <- gas %>%
  mutate(Date = as.Date(Date)) %>% select(Date, Gas_EUR_MWh) %>%
  left_join(carbon %>% mutate(Date = as.Date(Date)) %>% select(Date, EUA_EUR_ton),  by = "Date") %>%
  left_join(coal   %>% mutate(Date = as.Date(Date)) %>% select(Date, Coal_USD_ton), by = "Date")

# ── Main consumption panel ─────────────────────────────────────────────────####
consumption_panel <- industry %>%
  left_join(
    weighted_prices %>%
      select(TimeUTC, DK36Title, P_weighted_c, P_weighted_emp, P_weighted_firm, DK1_P, DK2_P),
    by = c("TimeUTC", "DK36Title")
  ) %>%
  mutate(
    fe_hour  = factor(lubridate::hour(TimeUTC)),
    fe_month = factor(format(TimeUTC, "%Y-%m")),
    fe_week  = factor(format(TimeUTC, "%Y-%U"))
  ) %>%
  filter(P_weighted_c > 0, P_weighted_emp > 0, P_weighted_firm > 0, Consumption_MWh > 0) %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  left_join(fuel, by = "Date") %>%
  mutate(
    log_gas         = log(Gas_EUR_MWh),
    log_carbon      = log(EUA_EUR_ton),
    log_coal        = log(Coal_USD_ton),
    log_consumption = log(Consumption_MWh),
    log_P_c         = log(P_weighted_c),
    log_P_emp       = log(P_weighted_emp),
    log_P_firm      = log(P_weighted_firm)
  )

names(forecast)
names(consumption_panel)

consumption_panel <- consumption_panel %>%
  mutate(fe_dow = factor(lubridate::wday(TimeUTC, label = FALSE)))



dk36_translate <- tibble::tibble(
  DK36Title = c(
    "Andre serviceydelser  mv.",
    "Bygge og anlæg",
    "Ejendomshandel og udlejning",
    "Elektronikindustri",
    "Finansiering og forsikring",
    "Føde-, drikke- og tobaksvareindustri",
    "Forlag, tv og radio",
    "Forskning og udvikling",
    "Fremst. af elektrisk udstyr",
    "Handel",
    "Hoteller og restauranter",
    "Kemiskindustri, Olieraffinaderier og Medicinalindustri",
    "Kultur og fritid",
    "Landbrug, skovbrug og fiskeri",
    "Maskinindustri",
    "Metalindustri",
    "Offentlig administration, forsvar og politi",
    "Plast-, glas- og betonindustri",
    "Rådgivning mv.",
    "Rejsebureauer, rengøring og anden operationel service",
    "Reklame og øvrig erhvervsservice",
    "Sociale institutioner",
    "Sundhedsvæsen",
    "Tekstil - og læderindustri & Møbel og anden industri mv.",
    "Telekommunikation &It - og informationstjenester",
    "Træ- og papirindustri, trykkerier",
    "Transport",
    "Transportmiddelindustri",
    "Undervisning"
  ),
  DK36_en = c(
    "Other services",
    "Construction",
    "Real estate",
    "Electronics manufacturing",
    "Finance and insurance",
    "Food, beverages and tobacco",
    "Publishing, TV and radio",
    "Research and development",
    "Electrical equipment manufacturing",
    "Trade",
    "Hotels and restaurants",
    "Chemicals, oil refining and pharmaceuticals",
    "Culture and recreation",
    "Agriculture, forestry and fishing",
    "Machinery manufacturing",
    "Metal manufacturing",
    "Public administration and defence",
    "Plastics, glass and concrete",
    "Consulting services",
    "Travel agencies and operational services",
    "Advertising and business services",
    "Social institutions",
    "Healthcare",
    "Textiles, leather and furniture",
    "Telecommunications and IT services",
    "Wood, paper and printing",
    "Transport",
    "Transport equipment manufacturing",
    "Education"
  )
)

consumption_panel <- consumption_panel %>% 
  left_join(dk36_translate, by = "DK36Title")

consumption_panel %>%
  group_by(TimeDK) %>%
  summarise(Total_MWh = sum(Consumption_MWh, na.rm = TRUE)) %>%
  summarise(Avg_Hourly_Total = mean(Total_MWh))



# ── PRICE Correlation ──────────────────────────────────────────────────────####
price_correlations <- consumption_panel %>%
  dplyr::select(log_P_c, log_P_emp, log_P_firm) %>%
  cor(use = "complete.obs")

print(round(price_correlations, 4))

# ── Sector breakdown ───────────────────────────────────────────────────────####
consumption_panel %>%
  group_by(DK36_en) %>%
  summarise(N_obs = n(), Total_MWh = sum(Consumption_MWh, na.rm = TRUE),
            Mean_MWh = mean(Consumption_MWh, na.rm = TRUE),
            SD_MWh   = sd(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(Share_pct = round(100 * Total_MWh / sum(Total_MWh), 2)) %>%
  arrange(desc(Total_MWh)) %>%
  print(n = Inf)
# ============================================================================
# = PART TWO: IV ESTIMATION  =================================================
# ============================================================================
# ── OLS model specifications (week-clustered) ──────────────────────────────####

OLS_model_het_c_w <- feols(
  log_consumption ~ log_P_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

OLS_model_het_emp_w <- feols(
  log_consumption ~ log_P_emp + Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

OLS_model_het_firm_w <- feols(
  log_consumption ~ log_P_firm + Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)
# ── IV model specifications ────────────────────────────────────────────────####

IV_model_het_DK10_W <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week  split = ~ DK36_en
)


IV_model_het_Emp_w <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)


IV_model_het_firm_w <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)


# ── Extract results ────────────────────────────────────────────────────────####
extract_iv <- function(model, weight, cluster) {
  sector_names <- names(model)
  purrr::map_dfr(seq_along(sector_names), function(i) {
    m      <- model[[i]]
    s      <- sector_names[i]
    ct     <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    if (length(iv_row) == 0) return(NULL)
    tibble::tibble(
      sector   = s,
      weight   = weight,
      cluster  = cluster,
      estimate = as.numeric(ct[iv_row, "Estimate"]),
      se       = as.numeric(ct[iv_row, "Std. Error"]),
      p_value  = as.numeric(ct[iv_row, "Pr(>|t|)"]),
      fs_f     = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                          error = function(e) NA_real_)
    )
  })
}

results <- bind_rows(
  extract_iv(IV_model_het_DK10_W, "Consumption", "Week"),
  extract_iv(IV_model_het_Emp_w,  "Employment",  "Week"),
  extract_iv(IV_model_het_firm_w, "Firm count",  "Week")
) %>%
  mutate(
    sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    ci_lo   = estimate - 1.96 * se,
    ci_hi   = estimate + 1.96 * se,
    weight  = factor(weight,  levels = c("Consumption", "Employment", "Firm count")),
    cluster = factor(cluster)
  )

# ── Sector name cleaning ───────────────────────────────────────────────────####
results <- results %>%
  mutate(sector = stringr::str_remove(sector,
                                      "^sample\\.var: DK36_en; sample: "))

sector_order <- results %>%
  filter(weight == "Consumption") %>%
  arrange(estimate) %>%
  pull(sector) %>%
  unique()

results <- results %>%
  mutate(sector = factor(sector, levels = sector_order))


# ── Estimate tables: Structural effect ─────────────────────────────────────####
est_wide <- results %>%
  mutate(
    cell = sprintf("%.3f%s (%.3f)", estimate, sig, se),
    col  = as.character(weight)
  ) %>%
  select(sector, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell) %>%
  arrange(sector)

print(knitr::kable(est_wide, format = "simple",
                   col.names = c("Sector",
                                 "Consumption",
                                 "Employment",
                                 "Firm count")))

# ── Estimate tables: first stage ───────────────────────────────────────────####


extract_first_stage <- function(model, weight, cluster, granularity = "DK36") {
  sector_names <- names(model)
  
  purrr::map_dfr(seq_along(sector_names), function(i) {
    m <- model[[i]]
    s <- sector_names[i]
    
    # First-stage model
    fs_model <- tryCatch(m$iv_first_stage[[1]], error = function(e) NULL)
    if (is.null(fs_model)) return(NULL)
    
    fs_ct <- fixest::coeftable(fs_model)
    
    # Find instrument row robustly
    inst_row <- rownames(fs_ct)[stringr::str_detect(rownames(fs_ct), "Wind")]
    if (length(inst_row) == 0) return(NULL)
    
    # Cluster-robust partial F = t^2 (just-identified case)
    fs_t     <- as.numeric(fs_ct[inst_row[1], "t value"])
    fs_f_rob <- fs_t^2
    
    tibble::tibble(
      sector        = stringr::str_remove(s, "^sample\\.var: DK36_en; sample: "),
      granularity   = granularity,
      weight        = weight,
      cluster       = cluster,
      instrument    = inst_row[1],
      fs_estimate   = as.numeric(fs_ct[inst_row[1], "Estimate"]),
      fs_se         = as.numeric(fs_ct[inst_row[1], "Std. Error"]),
      fs_p_value    = as.numeric(fs_ct[inst_row[1], "Pr(>|t|)"]),
      fs_t_value    = fs_t,
      fs_f          = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                               error = function(e) NA_real_),
      fs_f_robust   = fs_f_rob   # cluster-robust partial F (t^2, just-identified)
    )
  })
}

first_stage_results <- dplyr::bind_rows(
  extract_first_stage(IV_model_het_DK10_W, "Consumption", "Week",  "DK36"),
  extract_first_stage(IV_model_het_Emp_w,  "Employment",  "Week",  "DK36"),
  extract_first_stage(IV_model_het_firm_w, "Firm count",  "Week",  "DK36")
) %>%
  dplyr::group_by(sector) %>%
  dplyr::mutate(
    mean_first_stage_sector = mean(fs_estimate, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

# Diagnostics: compare homoskedastic vs robust F
first_stage_results %>%
  filter(cluster == "Week") %>%
  select(sector, weight, fs_f, fs_f_robust) %>%
  mutate(ratio = round(fs_f / fs_f_robust, 2)) %>%
  arrange(desc(ratio)) %>%
  print(n = Inf)

print(first_stage_results, n = Inf)


# ── Estimate tables with OLS  ──────────────────────────────────────────────####
clean_sector <- function(model) {
  sub(".*sample: ", "", names(model))
}

# ── Helper: extract price coefficient from a split feols object ──────────────
extract_split_coef <- function(model, coef_name) {
  map_dfr(seq_along(model), function(i) {
    ct <- coeftable(model[[i]])
    tibble(
      DK36_en = sub(".*sample: ", "", names(model)[i]),
      coef    = ct[coef_name, "Estimate"],
      se      = ct[coef_name, "Std. Error"],
      pval    = ct[coef_name, "Pr(>|t|)"],
      stars   = case_when(
        pval < 0.01 ~ "***",
        pval < 0.05 ~ "**",
        pval < 0.10 ~ "*",
        TRUE        ~ ""
      )
    )
  })
}

# ── Helper: extract first-stage F-stat from a split IV feols object ──────────
extract_fstat <- function(model) {
  map_dfr(seq_along(model), function(i) {
    tibble(
      DK36_en = sub(".*sample: ", "", names(model)[i]),
      fstat   = fitstat(model[[i]], "ivf")[[1]]$stat
    )
  })
}

# ── Extract coefficients ──────────────────────────────────────────────────────
iv_c    <- extract_split_coef(IV_model_het_DK10_W,  "fit_log_P_c")
iv_emp  <- extract_split_coef(IV_model_het_Emp_w,   "fit_log_P_emp")
iv_firm <- extract_split_coef(IV_model_het_firm_w,  "fit_log_P_firm")
ols_c   <- extract_split_coef(OLS_model_het_c_w,    "log_P_c")

fstat_c    <- extract_fstat(IV_model_het_DK10_W)
fstat_emp  <- extract_fstat(IV_model_het_Emp_w)
fstat_firm <- extract_fstat(IV_model_het_firm_w)

# ── Format: coefficient (SE***) ──────────────────────────────────────────────
fmt <- function(df, colname) {
  df |>
    mutate(cell = paste0(
      formatC(coef, digits = 3, format = "f"), stars,
      " (", formatC(se, digits = 3, format = "f"), ")"
    )) |>
    select(DK36_en, cell) |>
    rename(!!colname := cell)
}

# ── Build master table ────────────────────────────────────────────────────────
results_table <- fmt(iv_c,   "IV_cons") |>
  left_join(fmt(iv_emp,  "IV_emp"),  by = "DK36_en") |>
  left_join(fmt(iv_firm, "IV_firm"), by = "DK36_en") |>
  left_join(fmt(ols_c,   "OLS"),     by = "DK36_en") |>
  mutate(across(starts_with("F_"), ~ formatC(.x, digits = 1, format = "f"))) |>
  arrange(DK36_en)

results_table
# ============================================================================
# = PART THREE: Robustness of specifications  ================================
# ============================================================================
# ── Robustness panel (levels price, same sample as main) ───────────────────####
rob_panel <- industry %>%
  left_join(
    weighted_prices %>%
      select(TimeUTC, DK36Title, P_weighted_c, P_weighted_emp, P_weighted_firm,
             DK1_P, DK2_P),
    by = c("TimeUTC", "DK36Title")
  ) %>%
  mutate(
    fe_hour  = factor(lubridate::hour(TimeUTC)),
    fe_month = factor(format(TimeUTC, "%Y-%m")),
    fe_week  = factor(format(TimeUTC, "%Y-%U")),
    fe_dow   = factor(lubridate::wday(TimeUTC, label = FALSE))
  ) %>%
  filter(P_weighted_c > 0, P_weighted_emp > 0, P_weighted_firm > 0,
         Consumption_MWh > 0) %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  left_join(fuel, by = "Date") %>%
  mutate(
    # Outcome
    log_consumption = log(Consumption_MWh),
    log_P_c         = log(P_weighted_c),
    log_P_emp       = log(P_weighted_emp),
    log_P_firm      = log(P_weighted_firm)
  ) %>% 
  rename(
    gas = Gas_EUR_MWh,
    carbon = EUA_EUR_ton,
    coal = Coal_USD_ton
  )

rob_panel <- rob_panel %>% 
  left_join(dk36_translate, by = "DK36Title")

# ── Models ─────────────────────────────────────────────────────────────────####

# Log-linear (week-clustered)
IV_log_DK10_W <- feols(
  log_consumption ~ Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_c ~ Wind_c,
  data = rob_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_log_Emp_w <- feols(
  log_consumption ~ Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_emp ~ Wind_emp,
  data = rob_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_log_firm_w <- feols(
  log_consumption ~ Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_firm ~ Wind_firm,
  data = rob_panel, cluster = ~ fe_week, split = ~ DK36_en
)

# Linear (week-clustered)
IV_lin_DK10_W <- feols(
  Consumption_MWh ~ Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_c ~ Wind_c,
  data = rob_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_lin_Emp_w <- feols(
  Consumption_MWh ~ Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_emp ~ Wind_emp,
  data = rob_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_lin_firm_w <- feols(
  Consumption_MWh ~ Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_firm ~ Wind_firm,
  data = rob_panel, cluster = ~ fe_week, split = ~ DK36_en
)

# ── Extract results ────────────────────────────────────────────────────────####
extract_rob <- function(model, weight, spec) {
  sector_names <- names(model)
  purrr::map_dfr(seq_along(sector_names), function(i) {
    m      <- model[[i]]
    ct     <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    if (length(iv_row) == 0) return(NULL)
    tibble::tibble(
      sector   = stringr::str_remove(sector_names[i],
                                     "^sample\\.var: DK36_en; sample: "),
      weight   = weight,
      spec     = spec,
      estimate = as.numeric(ct[iv_row, "Estimate"]),
      se       = as.numeric(ct[iv_row, "Std. Error"]),
      p_value  = as.numeric(ct[iv_row, "Pr(>|t|)"]),
      fs_f     = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                          error = function(e) NA_real_)
    )
  })
}

results_rob <- bind_rows(
  extract_rob(IV_log_DK10_W, "Consumption", "Log-linear"),
  extract_rob(IV_log_Emp_w,  "Employment",  "Log-linear"),
  extract_rob(IV_log_firm_w, "Firm count",  "Log-linear"),
  extract_rob(IV_lin_DK10_W, "Consumption", "Linear"),
  extract_rob(IV_lin_Emp_w,  "Employment",  "Linear"),
  extract_rob(IV_lin_firm_w, "Firm count",  "Linear")
) %>%
  mutate(
    sig    = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    weight = factor(weight, levels = c("Consumption", "Employment", "Firm count")),
    spec   = factor(spec,   levels = c("Log-linear", "Linear"))
  )

# ── Sector-specific means for elasticity conversion ────────────────────────####
sector_means <- rob_panel %>%
  group_by(DK36_en) %>%
  summarise(
    P_mean_sector = mean(P_weighted_c,   na.rm = TRUE),
    Q_mean_sector = mean(Consumption_MWh, na.rm = TRUE),
    .groups = "drop"
  )

results_rob <- results_rob %>%
  left_join(sector_means, by = c("sector" = "DK36_en")) %>%
  mutate(
    elasticity = case_when(
      spec == "Log-linear" ~ estimate * P_mean_sector,
      spec == "Linear"     ~ estimate * P_mean_sector / Q_mean_sector
    )
  )

# Sanity check
results_rob %>%
  filter(weight == "Consumption") %>%
  select(sector, spec, estimate, P_mean_sector, Q_mean_sector, elasticity) %>%
  arrange(spec, elasticity) %>%
  print(n = Inf)

# ── comparison with LOG-LOG ────────────────────────────────────────────────####
loglog <- results %>%
  filter(weight == "Consumption", cluster == "Week") %>%
  select(sector, estimate, sig) %>%
  rename(loglog_est = estimate, loglog_sig = sig)

# ── Pull log-linear and linear elasticities (consumption weight only)
rob_wide <- results_rob %>%
  filter(weight == "Consumption") %>%
  mutate(
    cell = sprintf("%.3f%s", elasticity, sig),
    col  = as.character(spec)
  ) %>%
  select(sector, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell)

# ── Combine
comparison <- loglog %>%
  mutate(loglog_cell = sprintf("%.3f%s", loglog_est, loglog_sig)) %>%
  select(sector, loglog_cell) %>%
  left_join(rob_wide, by = "sector") %>%
  arrange(sector)

print(knitr::kable(
  comparison,
  format     = "simple",
  col.names  = c("Sector", "Log-log (primary)", "Log-linear", "Linear"),
  caption    = "Implied elasticities by specification (consumption weights, week-clustered). Log-log coefficient is the direct elasticity. Log-linear and linear converted to dimensionless elasticity at sector-specific means."
))

# First stage effects for modelling 
fs_sector <- extract_first_stage(IV_log_DK10_W, "Consumption", "Week") %>%
  select(sector, fs_estimate, fs_f_robust) %>%
  arrange(fs_estimate)

print(fs_sector, n = Inf)


# ============================================================================
# = PART FOUR: Anderson-Rubin  ===============================================
# ============================================================================
# ── Anderson Rubin Industry Split ──────────────────────────────────────────####
compute_AR_cs <- function(data, log_price_var, wind_var, temp_var,
                          beta_grid = seq(-0.5, 0.1, by = 0.001),
                          alpha = 0.05,
                          cluster_var = "fe_week") {
  
  # Pre-compute adjusted Y for each grid point
  Y <- data$log_consumption
  D <- data[[log_price_var]]
  
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  # Build a temporary data frame with all needed variables
  ar_data <- data.frame(
    Z      = data[[wind_var]],
    Temp   = data[[temp_var]],
    Gas    = data$log_gas,
    Coal   = data$log_coal,
    Carbon = data$log_carbon,
    fe_hour  = data$fe_hour,
    fe_month = data$fe_month,
    fe_dow   = data$fe_dow,
    cl_var   = data[[cluster_var]]
  )
  
  ar_results <- purrr::map(beta_grid, function(b0) {
    ar_data$Y_adj <- Y - b0 * D
    
    fit <- tryCatch(
      fixest::feols(Y_adj ~ Z + Temp + Gas + Coal + Carbon | fe_hour + fe_month + fe_dow,
                    data = ar_data, cluster = ~cl_var),
      error = function(e) NULL
    )
    if (is.null(fit)) return(NULL)
    
    ct <- fixest::coeftable(fit)
    if (!"Z" %in% rownames(ct)) return(NULL)
    
    tibble::tibble(
      beta0   = b0,
      t_stat  = ct["Z", "t value"],
      p_value = ct["Z", "Pr(>|t|)"],
      in_CS   = ct["Z", "Pr(>|t|)"] >= alpha
    )
  }) %>% dplyr::bind_rows()
  
  ar_results
}

industries <- sort(unique(consumption_panel$DK36_en))
length(industries)



run_split_ar <- function(price_var, wind_var, temp_var, weight_label) {
  cat("Running split AR (", weight_label, ")...\n")
  
  map_dfr(industries, function(ind) {
    cat("  ", ind, "\n")
    sub <- consumption_panel %>% filter(DK36_en == ind)
    
    ar <- compute_AR_cs(
      data = sub,
      log_price_var = price_var,
      wind_var = wind_var,
      temp_var = temp_var,
      beta_grid = seq(-0.6, 0.15, by = 0.001)
    )
    
    cs <- ar %>% filter(in_CS == TRUE)
    
    tibble(
      industry = ind,
      weight   = weight_label,
      AR_Lower = ifelse(nrow(cs) > 0, min(cs$beta0), NA_real_),
      AR_Upper = ifelse(nrow(cs) > 0, max(cs$beta0), NA_real_),
      AR_Width = ifelse(nrow(cs) > 0, max(cs$beta0) - min(cs$beta0), NA_real_),
      AR_Empty = nrow(cs) == 0,
      N_obs    = nrow(sub)
    )
  })
}

ar_split_c    <- run_split_ar("log_P_c",    "Wind_c",    "Temp_c",    "Consumption")
ar_split_emp  <- run_split_ar("log_P_emp",  "Wind_emp",  "Temp_emp",  "Employment")
ar_split_firm <- run_split_ar("log_P_firm", "Wind_firm", "Temp_firm", "Firm count")

ar_split_all <- bind_rows(ar_split_c, ar_split_emp, ar_split_firm)

# Merge with feols point estimates from results 
results_clean <- results %>%
  mutate(sector = as.character(sector)) %>%
  select(sector, weight, estimate, se)

ar_with_estimates <- ar_split_all %>%
  left_join(
    results_clean,
    by = c("industry" = "sector", "weight" = "weight")
  ) %>%
  mutate(
    Inside_CS  = !AR_Empty & estimate >= AR_Lower & estimate <= AR_Upper,
    Wald_Lower = estimate - 1.96 * se,
    Wald_Upper = estimate + 1.96 * se,
    Wald_Width = 2 * 1.96 * se,
    AR_vs_Wald = ifelse(!AR_Empty & Wald_Width > 0, AR_Width / Wald_Width, NA_real_)
  )

ar_with_estimates <- readRDS("ar_with_estimates.rds")

# ── 1.3 AR Reporting ───────────────────────────────────────────────────────####
print(
  ar_with_estimates %>%
    filter(weight == "Consumption") %>%
    select(industry, estimate, AR_Lower, AR_Upper, AR_Width,
           Wald_Width, AR_vs_Wald, Inside_CS) %>%
    arrange(estimate),
  n = 35
)

print(
  ar_with_estimates %>%
    filter(weight == "Employment") %>%
    select(industry, estimate, AR_Lower, AR_Upper, AR_Width,
           Wald_Width, AR_vs_Wald, Inside_CS) %>%
    arrange(estimate),
  n = 35
)

print(
  ar_with_estimates %>%
    filter(weight == "Firm count") %>%
    select(industry, estimate, AR_Lower, AR_Upper, AR_Width,
           Wald_Width, AR_vs_Wald, Inside_CS) %>%
    arrange(estimate),
  n = 35
)

nrow(ar_with_estimates)
sum(ar_with_estimates$AR_Empty, na.rm = TRUE)
cat("  Point estimate inside AR CS:",
    sum(ar_with_estimates$Inside_CS, na.rm = TRUE), "/",
    sum(!is.na(ar_with_estimates$Inside_CS)), "\n")

ar_c_valid <- ar_with_estimates %>% filter(weight == "Consumption", !AR_Empty)
cat("\nAR vs Wald width ratio (consumption weights):\n")
cat("  Mean:", round(mean(ar_c_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")
cat("  Min: ", round(min(ar_c_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")
cat("  Max: ", round(max(ar_c_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")

ar_e_valid <- ar_with_estimates %>% filter(weight == "Employment", !AR_Empty)
cat("\nAR vs Wald width ratio (Employment weights):\n")
cat("  Mean:", round(mean(ar_e_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")
cat("  Min: ", round(min(ar_e_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")
cat("  Max: ", round(max(ar_e_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")

ar_f_valid <- ar_with_estimates %>% filter(weight == "Firm count", !AR_Empty)
cat("\nAR vs Wald width ratio (Firm Count weights):\n")
cat("  Mean:", round(mean(ar_f_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")
cat("  Min: ", round(min(ar_f_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")
cat("  Max: ", round(max(ar_f_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")



wide_ar <- ar_c_valid %>% filter(AR_vs_Wald > 1.5)
if (nrow(wide_ar) > 0) {
  cat("  Industries with AR/Wald > 1.5:\n")
  print(wide_ar %>% select(industry, estimate, AR_Width, Wald_Width, AR_vs_Wald))
} else {
  cat("  No industries with AR/Wald > 1.5 — strong instrument confirmed.\n")
}

# Count how many AR sets exclude zero, by weight
ar_with_estimates %>%
  mutate(AR_excludes_zero = !AR_Empty & (AR_Upper < 0 | AR_Lower > 0)) %>%
  group_by(weight) %>%
  summarise(
    n_excludes_zero = sum(AR_excludes_zero),
    n_total = n()
  )

# ============================================================================
# = PART FIVE: Sensitivity Analysis ==========================================
# ============================================================================
# ── Run reduced-form feols with cluster-robust SE ──────────────────────────####
run_corrected_sensitivity <- function(data, industry_col, wind_var, temp_var,
                                      weight_label, cluster_var = "fe_week") {
  
  industries <- sort(unique(data[[industry_col]]))
  
  # Reduced-form specification: mirrors main 2SLS specification
  # Main spec: fe_hour + fe_month + fe_dow, week-clustered SEs
  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow"
  ))
  
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  map(industries, function(ind) {
    sub <- data %>% filter(.data[[industry_col]] == ind)
    
    # ── Cluster-robust reduced form (main inferential framework) ──
    rf <- tryCatch(
      feols(fml, data = sub, cluster = cluster_fml),
      error = function(e) NULL
    )
    if (is.null(rf)) return(NULL)
    
    ct <- coeftable(rf)
    if (!(wind_var %in% rownames(ct))) return(NULL)
    
    t_cluster  <- ct[wind_var, "t value"]
    se_cluster <- ct[wind_var, "Std. Error"]
    est        <- ct[wind_var, "Estimate"]
    
    n_obs   <- nobs(rf)
    n_clust <- length(unique(sub[[cluster_var]]))
    dof     <- n_clust - 1  # cluster-adjusted df for critical value
    
    # ── OLS reduced form for sensemakr (same specification as feols) ──
    # Partial R² is invariant to SE choice; the OLS lm is required because
    # sensemakr operates on lm objects, not feols.
    rf_ols <- tryCatch(
      lm(as.formula(paste0(
        "log_consumption ~ ", wind_var, " + ", temp_var,
        " + log_gas + log_coal + log_carbon + factor(fe_hour) + factor(fe_month) + factor(fe_dow)"
      )), data = sub),
      error = function(e) NULL
    )
    if (is.null(rf_ols)) return(NULL)
    
    ols_ct  <- summary(rf_ols)$coefficients
    t_ols   <- ols_ct[wind_var, "t value"]
    dof_ols <- rf_ols$df.residual
    partial_r2 <- t_ols^2 / (t_ols^2 + dof_ols)
    
    # ── Robustness values ──
    # RV_q1: point-estimate-to-zero, OLS framework (no significance threshold)
    rv_q1 <- robustness_value(t_statistic = t_ols, dof = dof_ols, q = 1)
    
    # RV_q1_alpha: CI-to-zero, cluster-robust framework
    rv_q1_alpha <- robustness_value(t_statistic = t_cluster, dof = dof, q = 1, alpha = 0.05)
    
    # ── Sensemakr bounds for benchmarking ──
    sens_ols <- tryCatch(
      sensemakr(model = rf_ols, treatment = wind_var,
                benchmark_covariates = temp_var, kd = 1:5, alpha = 0.05),
      error = function(e) NULL
    )
    
    # ── Critical k: cluster-robust framework ──
    # The bias-adjusted point estimate from sensemakr is invariant to SE choice.
    # We divide by the cluster-robust SE to get the cluster-robust adjusted t-statistic,
    # then compare against the cluster-robust 5% critical value.
    critical_kd <- NA
    if (!is.null(sens_ols) && !is.null(sens_ols$bounds) && nrow(sens_ols$bounds) > 0) {
      bounds_df <- as.data.frame(sens_ols$bounds)
      bounds_df$kd <- as.numeric(stringr::str_extract(bounds_df$bound_label, "^[0-9]+"))
      bounds_df$adjusted_t_cluster <- bounds_df$adjusted_estimate / se_cluster
      
      t_crit <- qt(0.975, df = dof)
      sig_lost <- bounds_df %>%
        filter(abs(adjusted_t_cluster) < t_crit) %>%
        slice_min(kd, n = 1)
      
      critical_kd <- if (nrow(sig_lost) > 0) sig_lost$kd[1] else ">5"
    }
    
    tibble(
      Industry    = ind,
      Weight      = weight_label,
      t_OLS       = round(t_ols, 2),
      t_cluster   = round(t_cluster, 2),
      inflation   = round(abs(t_ols / t_cluster), 2),
      Partial_R2  = round(partial_r2, 6),
      RV_q1       = round(rv_q1, 4),
      RV_q1_alpha = round(rv_q1_alpha, 4),
      Critical_kd = as.character(critical_kd),
      N_obs       = n_obs,
      N_clusters  = n_clust
    )
  }) %>% bind_rows()
}

# Run for all three weighting schemes
sens_corrected_c <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_c", "Temp_c", "Consumption"
)

sens_corrected_emp <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_emp", "Temp_emp", "Employment"
)

sens_corrected_firm <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_firm", "Temp_firm", "Firm count"
)

# ── Combine all three weighting schemes ────────────────────────────────────####
sens_corrected_all <- bind_rows(
  sens_corrected_c, 
  sens_corrected_emp, 
  sens_corrected_firm
)

# ============================================================================
# = PART SIX: Placebo Test  ==================================================
# ============================================================================
# ── Placebo Test ───────────────────────────────────────────────────────────#### 
# Create lead variables
consumption_panel_placebo <- consumption_panel %>%
  group_by(DK36Title) %>%
  arrange(TimeUTC, .by_group = TRUE) %>%
  mutate(
    Wind_c_lead24 = dplyr::lead(Wind_c, 24),
    Wind_c_lead48 = dplyr::lead(Wind_c, 48)
  ) %>%
  ungroup()

# Placebo regressions
placebo_24 <- feols(
  log_consumption ~ Wind_c_lead24 + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_week,
  data = consumption_panel_placebo,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

placebo_48 <- feols(
  log_consumption ~ Wind_c_lead48 + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_week,
  data = consumption_panel_placebo,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

extract_placebo <- function(fx_multi, wind_var, label) {
  models <- as.list(fx_multi)
  ids <- names(models)
  
  map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    ct <- fixest::coeftable(m)
    if (!(wind_var %in% rownames(ct))) return(NULL)
    
    est <- ct[wind_var, "Estimate"]
    se  <- ct[wind_var, "Std. Error"]
    pv  <- ct[wind_var, "Pr(>|t|)"]
    
    tibble(
      industry = str_remove(ids[i], "^.*sample:\\s*"),
      placebo_coef = est,
      placebo_se = se,
      placebo_pvalue = pv,
      placebo_sig = pv < 0.05,
      lag = label
    )
  })
}

plac_24 <- extract_placebo(placebo_24, "Wind_c_lead24", "24h lead")
plac_48 <- extract_placebo(placebo_48, "Wind_c_lead48", "48h lead")
plac_all <- bind_rows(plac_24, plac_48)


cat("24h lead - significant at 5%:", sum(plac_24$placebo_sig), "/", nrow(plac_24), "\n")
cat("48h lead - significant at 5%:", sum(plac_48$placebo_sig), "/", nrow(plac_48), "\n")

print(plac_all, n = 70)


wind <- na.omit(consumption_panel$Wind_c)

acf_vals <- acf(wind, lag.max = 72, plot = FALSE)
acf_vals$acf[25]  # lag 24
acf_vals$acf[49]  # lag 48
acf_vals$acf[72]  # lag 48

# Or just plot it
acf(wind, lag.max = 72, main = "Wind Forecast Autocorrelation")



# ── Export ─────────────────────────────────────────────────────────────────####

write_xlsx(
  list("Sensitivity Results" = as.data.frame(table_wide)),
  path = "tables/sensitivity_results_wide.xlsx"
)

# ============================================================================
# = PART SEVEN: Ecological Inference  ========================================
# ============================================================================


cat("\n============================================================\n")
cat("  ECOLOGICAL INFERENCE DIAGNOSTICS\n")
cat("============================================================\n")


# ── EI.1 Weight correlations across schemes ────────────────────────────────####
# Extract unique industry-year weight triples

w_tbl <- consumption_panel %>%
  distinct(Year, DK36_en, DK36Code, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  filter(!is.na(w_DK1_c), !is.na(w_DK1_emp), !is.na(w_DK1_firm))

# Pairwise weight correlations by year
weight_cors <- w_tbl %>%
  group_by(Year) %>%
  summarise(
    cor_c_emp    = cor(w_DK1_c, w_DK1_emp,  use = "complete.obs"),
    cor_c_firm   = cor(w_DK1_c, w_DK1_firm, use = "complete.obs"),
    cor_emp_firm = cor(w_DK1_emp, w_DK1_firm, use = "complete.obs"),
    n_industries = n(),
    .groups = "drop"
  )

# Pooled (across all years)
weight_cors_pooled <- w_tbl %>%
  summarise(
    cor_c_emp    = cor(w_DK1_c, w_DK1_emp,  use = "complete.obs"),
    cor_c_firm   = cor(w_DK1_c, w_DK1_firm, use = "complete.obs"),
    cor_emp_firm = cor(w_DK1_emp, w_DK1_firm, use = "complete.obs")
  )

cat("\n--- Pairwise Weight Correlations (by Year) ---\n")
print(weight_cors)
cat("\n--- Pooled Weight Correlations ---\n")
print(weight_cors_pooled)


# ── EI.2 Weight dispersion by industry ─────────────────────────────────────####
# Average across all years to avoid single-year artifacts

weight_dispersion <- w_tbl %>%
  group_by(DK36_en) %>%
  summarise(
    w_c_mean    = mean(w_DK1_c,    na.rm = TRUE),
    w_emp_mean  = mean(w_DK1_emp,  na.rm = TRUE),
    w_firm_mean = mean(w_DK1_firm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    w_range = pmax(w_c_mean, w_emp_mean, w_firm_mean) -
      pmin(w_c_mean, w_emp_mean, w_firm_mean)
  ) %>%
  arrange(desc(w_range))

cat("\n--- Weight Dispersion (Averaged Across Years) ---\n")
print(weight_dispersion, n = 35)
cat("\nWeight range summary:\n")
cat("  Mean:", round(mean(weight_dispersion$w_range, na.rm = TRUE), 4), "\n")
cat("  Median:", round(median(weight_dispersion$w_range, na.rm = TRUE), 4), "\n")
cat("  Max:", round(max(weight_dispersion$w_range, na.rm = TRUE), 4),
    "(", weight_dispersion$DK36_en[which.max(weight_dispersion$w_range)], ")\n")


# ── EI.3 Elasticity divergence across weighting schemes ────────────────────####
# Use preferred specification: week clustering (matches main results)


eco_results <- bind_rows(
  extract_iv(IV_model_het_DK10_W, "Consumption", "Week"),
  extract_iv(IV_model_het_Emp_w,  "Employment",  "Week"),
  extract_iv(IV_model_het_firm_w, "Firm count",  "Week")
) %>%
  mutate(sector = str_remove(sector, "^sample\\.var: DK36_en; sample: ")) %>%
  select(sector, weight, estimate, se)

# Pivot to wide: one row per industry
elasticity_wide <- eco_results %>%
  pivot_wider(
    id_cols     = sector,
    names_from  = weight,
    values_from = c(estimate, se)
  )

# Compute divergence metrics
elasticity_divergence <- elasticity_wide %>%
  mutate(
    range_elasticity = pmax(estimate_Consumption, estimate_Employment,
                            `estimate_Firm count`, na.rm = TRUE) -
      pmin(estimate_Consumption, estimate_Employment,
           `estimate_Firm count`, na.rm = TRUE),
    mean_elasticity  = (estimate_Consumption + estimate_Employment +
                          `estimate_Firm count`) / 3,
    mean_se          = (se_Consumption + se_Employment + `se_Firm count`) / 3,
    divergence_to_se = range_elasticity / mean_se,
    sign_consistent  = (sign(estimate_Consumption) == sign(estimate_Employment)) &
      (sign(estimate_Employment) == sign(`estimate_Firm count`))
  ) %>%
  arrange(desc(range_elasticity))

cat("\n--- Elasticity Divergence Across Weights ---\n")
print(elasticity_divergence %>%
        select(sector, estimate_Consumption, estimate_Employment,
               `estimate_Firm count`, range_elasticity, divergence_to_se,
               sign_consistent), n = 35)

cat("\nSign consistency:", sum(elasticity_divergence$sign_consistent, na.rm = TRUE),
    "/", nrow(elasticity_divergence), "\n")
cat("Mean elasticity range:", round(mean(elasticity_divergence$range_elasticity,
                                         na.rm = TRUE), 6), "\n")
cat("Max elasticity range:", round(max(elasticity_divergence$range_elasticity,
                                       na.rm = TRUE), 6), "\n")
cat("Mean divergence-to-SE ratio:", round(mean(elasticity_divergence$divergence_to_se,
                                               na.rm = TRUE), 4), "\n")
cat("Max divergence-to-SE ratio:", round(max(elasticity_divergence$divergence_to_se,
                                             na.rm = TRUE), 4), "\n")


# ── EI.4 Spearman test: weight divergence vs elasticity divergence ─────────####
# Does geographic disagreement in weights predict disagreement in estimates?

eco_merged <- weight_dispersion %>%
  select(DK36_en, w_range) %>%
  inner_join(
    elasticity_divergence %>%
      select(sector, range_elasticity, divergence_to_se, sign_consistent),
    by = c("DK36_en" = "sector")
  )

cat("\nMatched industries:", nrow(eco_merged), "\n")

if (nrow(eco_merged) >= 5) {
  eco_cor <- cor.test(eco_merged$w_range, eco_merged$range_elasticity,
                      method = "spearman", exact = FALSE)
  
  cat("\n--- Ecological Sensitivity Test (Spearman) ---\n")
  cat("  rho:", round(eco_cor$estimate, 4), "\n")
  cat("  p-value:", format.pval(eco_cor$p.value, digits = 4), "\n")
  cat("  Interpretation:",
      ifelse(eco_cor$p.value < 0.05,
             "Significant — geographic weight choice affects estimates.",
             "Not significant — estimates robust to geographic aggregation."), "\n")
  
  # Supplementary: even if significant, is the magnitude economically relevant?
  if (eco_cor$p.value < 0.05) {
    cat("  Note: Check whether the predicted elasticity divergence at the\n")
    cat("  maximum weight range is large relative to the SEs.\n")
  }
} else {
  cat("WARNING: Too few matched industries for Spearman test.\n")
  eco_cor <- NULL
}


# ── EI.5 Scatter plot: weight divergence vs elasticity divergence ──────────####

if (nrow(eco_merged) >= 5 && !is.null(eco_cor)) {
  
  p_eco <- ggplot(eco_merged, aes(x = w_range, y = range_elasticity)) +
    geom_point(size = 2.5, alpha = 0.7, colour = "#2166AC") +
    geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
                colour = "#B2182B", alpha = 0.15, linewidth = 0.7) +
    ggrepel::geom_text_repel(
      aes(label = DK36_en), size = 2.2, max.overlaps = 15,
      segment.size = 0.3, segment.alpha = 0.5
    ) +
    labs(
      x = "Weight Divergence (DK1 share range across 3 schemes)",
      y = "Elasticity Divergence (absolute range across 3 schemes)",
      title = "Ecological Sensitivity: Weight vs Elasticity Divergence",
      subtitle = paste0("Spearman \u03C1 = ", round(eco_cor$estimate, 3),
                        ", p = ", format.pval(eco_cor$p.value, digits = 3)),
      caption = "Each point is one DK36 industry. Weights averaged across years."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "grey35"),
      plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 0)
    )
  
  print(p_eco)
}


# ── EI.6 SE-anchored ecological classification ─────────────────────────────####
# Threshold logic: classify based on whether cross-weight divergence
# is economically meaningful relative to sampling uncertainty.
#   Low:      divergence < 1× mean SE AND signs agree
#   Moderate: divergence < 2× mean SE AND signs agree
#   High:     divergence >= 2× mean SE OR signs disagree

eco_summary <- eco_merged %>%
  left_join(
    elasticity_divergence %>%
      select(sector, estimate_Consumption, estimate_Employment,
             `estimate_Firm count`, mean_se),
    by = c("DK36_en" = "sector")
  ) %>%
  mutate(
    ecological_class = case_when(
      divergence_to_se < 1 & sign_consistent  ~ "Low (< 1x SE)",
      divergence_to_se < 2 & sign_consistent  ~ "Moderate (1-2x SE)",
      TRUE                                     ~ "High (>= 2x SE or sign flip)"
    )
  ) %>%
  arrange(desc(divergence_to_se))

cat("\n--- SE-Anchored Ecological Classification ---\n")
print(eco_summary %>%
        select(DK36_en, w_range, range_elasticity, mean_se,
               divergence_to_se, sign_consistent, ecological_class), n = 35)

cat("\nClassification summary:\n")
cat("  Low (< 1x SE):     ", sum(eco_summary$ecological_class == "Low (< 1x SE)"), "\n")
cat("  Moderate (1-2x SE):", sum(eco_summary$ecological_class == "Moderate (1-2x SE)"), "\n")
cat("  High (>= 2x SE):   ", sum(eco_summary$ecological_class ==
                                   "High (>= 2x SE or sign flip)"), "\n")


# ── EI.7 Weight stability over time ────────────────────────────────────────####
# Check whether the geographic weights themselves are shifting over the sample

weight_stability <- w_tbl %>%
  group_by(DK36_en) %>%
  summarise(
    sd_w_c    = sd(w_DK1_c,    na.rm = TRUE),
    sd_w_emp  = sd(w_DK1_emp,  na.rm = TRUE),
    sd_w_firm = sd(w_DK1_firm, na.rm = TRUE),
    range_w_c = max(w_DK1_c, na.rm = TRUE) - min(w_DK1_c, na.rm = TRUE),
    n_years   = n_distinct(Year),
    .groups   = "drop"
  ) %>%
  mutate(max_sd = pmax(sd_w_c, sd_w_emp, sd_w_firm)) %>%
  arrange(desc(max_sd))

cat("\n--- Weight Stability Over Time ---\n")
print(weight_stability %>%
        select(DK36_en, sd_w_c, sd_w_emp, sd_w_firm, range_w_c, max_sd), n = 35)
cat("\nMax within-industry SD:", round(max(weight_stability$max_sd, na.rm = TRUE), 4),
    "(", weight_stability$DK36_en[1], ")\n")
cat("Mean within-industry SD:", round(mean(weight_stability$max_sd, na.rm = TRUE), 4), "\n")


# ── EI.8 King (1997) Inflation Factor F ────────────────────────────────────####
# Measures how much information is lost by aggregating firms into DK36 industries.
#
# Theory (King 1997, §3.2–3.3; Palmquist 1993):
#   X_i  = DK1 share for industry i (the composition variable)
#   X_ij = 1 if firm j in industry i is in DK1, 0 if DK2 (individual-level)
#   
#   Individual-level variance:  Var(X_ij) = X_bar * (1 - X_bar)
#   Aggregate-level variance:   sigma2_x  = Var(X_i) across industries
#   
#   eta2 = sigma2_x / [X_bar * (1 - X_bar)]
#   F    = (1 / eta2) - 1
#
# Interpretation:
#   F = 0:  Industries are geographically homogeneous (all-DK1 or all-DK2).
#           No information lost through aggregation.
#   F > 0:  Information lost. Larger F = more potential for aggregation bias.
#           Even small specification shifts get inflated by F.
#
# We compute F separately for each weighting scheme.

compute_king_F <- function(x) {
  # x = vector of DK1 shares across industries (one per industry)
  x <- x[!is.na(x)]
  x_bar    <- mean(x)
  sigma2_x <- var(x)                     # aggregate variance
  var_ind  <- x_bar * (1 - x_bar)        # individual-level (binary) variance
  
  if (var_ind == 0 || sigma2_x == 0) return(NA_real_)
  
  eta2 <- sigma2_x / var_ind
  F_king <- (1 / eta2) - 1
  
  tibble(
    x_bar        = x_bar,
    sigma2_x     = sigma2_x,
    var_ind      = var_ind,
    eta2         = eta2,
    F_inflation  = F_king
  )
}

# Use year-averaged weights from weight_dispersion
king_F <- bind_rows(
  compute_king_F(weight_dispersion$w_c_mean)    %>% mutate(scheme = "Consumption"),
  compute_king_F(weight_dispersion$w_emp_mean)   %>% mutate(scheme = "Employment"),
  compute_king_F(weight_dispersion$w_firm_mean)  %>% mutate(scheme = "Firm count")
) %>%
  select(scheme, x_bar, sigma2_x, var_ind, eta2, F_inflation)

cat("\n--- King (1997) Inflation Factor F ---\n")
cat("F = 0: no information loss from aggregation (perfect geographic segregation)\n")
cat("F > 0: aggregation amplifies any specification shift by factor F\n\n")
print(king_F, n = 5)

cat("\nInterpretation:\n")
for (i in seq_len(nrow(king_F))) {
  row <- king_F[i, ]
  cat(sprintf("  %-12s: F = %.2f — ", row$scheme, row$F_inflation))
  if (row$F_inflation < 1) {
    cat("Low inflation. Industries are geographically concentrated.\n")
    cat(sprintf("               eta2 = %.3f (aggregate variance is %.0f%% of individual).\n",
                row$eta2, row$eta2 * 100))
  } else if (row$F_inflation < 5) {
    cat("Moderate inflation. Specification shifts amplified ", 
        round(row$F_inflation, 1), "x.\n", sep = "")
  } else {
    cat("High inflation. Specification shifts amplified ",
        round(row$F_inflation, 1), "x.\n", sep = "")
    cat("               Geographic mixing within industries is substantial.\n")
  }
}

# Per-industry F contribution: how concentrated is each industry?
# Industries with X_i close to 0 or 1 contribute less to aggregation risk
king_F_industry <- weight_dispersion %>%
  mutate(
    # Distance from homogeneity (0 or 1) for each scheme
    hetero_c    = w_c_mean    * (1 - w_c_mean),
    hetero_emp  = w_emp_mean  * (1 - w_emp_mean),
    hetero_firm = w_firm_mean * (1 - w_firm_mean),
    # Max heterogeneity across schemes
    max_hetero  = pmax(hetero_c, hetero_emp, hetero_firm)
  ) %>%
  arrange(desc(max_hetero))

cat("\n--- Per-Industry Geographic Heterogeneity ---\n")
cat("X*(1-X) = 0 means fully in one zone; 0.25 = perfectly split.\n\n")
print(king_F_industry %>%
        select(DK36_en, w_c_mean, w_emp_mean, w_firm_mean,
               hetero_c, hetero_emp, hetero_firm), n = 35)

cat("\nIndustries with highest within-industry geographic mixing:\n")
print(king_F_industry %>%
        filter(max_hetero >= 0.20) %>%
        select(DK36_en, max_hetero), n = 10)

cat("\nIndustries with lowest within-industry geographic mixing (most concentrated):\n")
print(king_F_industry %>%
        filter(max_hetero < 0.10) %>%
        select(DK36_en, max_hetero), n = 10)


# ── EI.9 Consolidated ecological output ────────────────────────────────────####
# Store for use in the master robustness summary

eco_output <- list(
  weight_correlations   = weight_cors,
  weight_cors_pooled    = weight_cors_pooled,
  weight_dispersion     = weight_dispersion,
  elasticity_divergence = elasticity_divergence,
  spearman_test         = if (!is.null(eco_cor)) {
    tibble(
      rho     = eco_cor$estimate,
      p_value = eco_cor$p.value,
      n       = nrow(eco_merged)
    )
  } else { NULL },
  classification        = eco_summary,
  weight_stability      = weight_stability,
  king_F                = king_F,
  king_F_industry       = king_F_industry
)

cat("\n--- Ecological Inference Summary ---\n")
cat("  Industries matched:", nrow(eco_merged), "\n")
cat("  Sign-consistent:", sum(eco_summary$sign_consistent, na.rm = TRUE),
    "/", nrow(eco_summary), "\n")
cat("  Mean divergence:", round(mean(eco_summary$range_elasticity, na.rm = TRUE), 6), "\n")
cat("  Mean divergence/SE:", round(mean(eco_summary$divergence_to_se, na.rm = TRUE), 4), "\n")
if (!is.null(eco_cor)) {
  cat("  Spearman rho:", round(eco_cor$estimate, 4),
      "  p:", format.pval(eco_cor$p.value, digits = 4), "\n")
}
cat("  Classification: ",
    sum(eco_summary$ecological_class == "Low (< 1x SE)"), " Low, ",
    sum(eco_summary$ecological_class == "Moderate (1-2x SE)"), " Moderate, ",
    sum(eco_summary$ecological_class == "High (>= 2x SE or sign flip)"), " High\n")
cat("  King F (Consumption):",
    round(king_F$F_inflation[king_F$scheme == "Consumption"], 2), "\n")
cat("  King F (Employment):",
    round(king_F$F_inflation[king_F$scheme == "Employment"], 2), "\n")
cat("  King F (Firm count):",
    round(king_F$F_inflation[king_F$scheme == "Firm count"], 2), "\n")

cat("\n============================================================\n")




# ============================================================================
# = PART EIGHT: POWER ANALYSIS  ==============================================
# ===========================================================================
# ── Helper functions ───────────────────────────────────────────────────────####
power_sim_iv <- function(df,
                         iv_effect, true_effect,
                         endog_var, instrument_var, outcome_var,
                         temp_var,
                         sector_var,
                         fe_vars     = c("fe_hour", "fe_month", "fe_dow"),
                         controls    = c("log_gas", "log_coal", "log_carbon"),
                         cluster_var = "fe_week",
                         n_sims = 1,
                         seed = 123)  {
  
  set.seed(seed)
  
  controls_full <- c(temp_var, controls)
  
  needed <- c(sector_var, endog_var, instrument_var, outcome_var,
              fe_vars, controls_full, cluster_var)
  
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))
  }
  
  sectors <- sort(unique(df[[sector_var]]))
  
  # ── Pre-compute residual SD per sector ────────────────────────────────
  resid_formula <- stats::as.formula(
    paste0(
      outcome_var, " ~ ", paste(controls_full, collapse = " + "),
      " | ", paste(fe_vars, collapse = " + ")
    )
  )
  
  sector_resid_sd <- stats::setNames(
    vapply(sectors, function(sec) {
      df_s <- df[df[[sector_var]] == sec, , drop = FALSE]
      
      m <- suppressMessages(suppressWarnings(
        fixest::feols(
          resid_formula,
          data = df_s,
          warn = FALSE,
          notes = FALSE
        )
      ))
      
      stats::sd(stats::residuals(m), na.rm = TRUE)
    }, numeric(1)),
    sectors
  )
  
  permute_block_series <- function(data, value_var, cluster_var) {
    clusters <- unique(data[[cluster_var]])
    
    cluster_map <- tibble::tibble(
      !!cluster_var := clusters,
      source_cluster = sample(clusters)
    )
    
    value_lookup <- data %>%
      dplyr::arrange(.data[[cluster_var]]) %>%
      dplyr::group_by(.data[[cluster_var]]) %>%
      dplyr::mutate(.pos = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(cluster_var, value_var)), .pos) %>%
      dplyr::rename(
        source_cluster = !!rlang::sym(cluster_var),
        permuted_value = !!rlang::sym(value_var)
      )
    
    data %>%
      dplyr::arrange(.data[[cluster_var]]) %>%
      dplyr::group_by(.data[[cluster_var]]) %>%
      dplyr::mutate(.pos = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(cluster_map,  by = cluster_var) %>%
      dplyr::left_join(value_lookup, by = c("source_cluster", ".pos")) %>%
      dplyr::pull(permuted_value)
  }
  
  sims <- vector("list", n_sims)
  
  for (i in seq_len(n_sims)) {
    
    res_i <- lapply(sectors, function(sec) {
      
      df_sec <- df[df[[sector_var]] == sec, , drop = FALSE]
      resid_sd <- sector_resid_sd[[sec]]
      if (!is.finite(resid_sd)) resid_sd <- 0
      
      # ── 1) Block-permute instrument, endogenous variable, and outcome ──
      df_sec <- df_sec %>%
        dplyr::mutate(
          perm_instrument = permute_block_series(., instrument_var, cluster_var),
          perm_endog      = permute_block_series(., endog_var, cluster_var),
          perm_outcome    = permute_block_series(., outcome_var, cluster_var)
        )
      
      # ── 2) Impose first stage on permuted endogenous baseline ───────────
      df_sec <- df_sec %>%
        dplyr::mutate(
          sim_instrument = perm_instrument,
          sim_endog      = perm_endog + iv_effect * sim_instrument
        )
      
      # ── 3) Impose structural effect on permuted outcome baseline ────────
      df_sec <- df_sec %>%
        dplyr::mutate(
          sim_outcome = perm_outcome +
            true_effect * sim_endog +
            rnorm(dplyr::n(), mean = 0, sd = resid_sd)
        )
      
      # ── 4) Re-estimate FE-IV model ──────────────────────────────────────
      fml <- stats::as.formula(
        paste0(
          "sim_outcome ~ ", paste(controls_full, collapse = " + "),
          " | ", paste(fe_vars, collapse = " + "),
          " | sim_endog ~ sim_instrument"
        )
      )
      
      m <- fixest::feols(
        fml,
        data = df_sec,
        cluster = stats::as.formula(paste0("~", cluster_var))
      )
      
      # ── 5) Extract IV coefficient ───────────────────────────────────────
      ct        <- fixest::coeftable(m)
      coef_name <- "fit_sim_endog"
      
      if (!coef_name %in% rownames(ct)) {
        return(tibble::tibble(
          sim       = i,
          sector    = sec,
          estimate  = NA_real_,
          std_error = NA_real_,
          p_value   = NA_real_,
          sig       = NA_integer_
        ))
      }
      
      tibble::tibble(
        sim       = i,
        sector    = sec,
        estimate  = as.numeric(ct[coef_name, "Estimate"]),
        std_error = as.numeric(ct[coef_name, "Std. Error"]),
        p_value   = as.numeric(ct[coef_name, "Pr(>|t|)"]),
        sig       = as.integer(as.numeric(ct[coef_name, "Pr(>|t|)"]) < 0.05)
      )
    })
    
    sims[[i]] <- dplyr::bind_rows(res_i)
  }
  
  dplyr::bind_rows(sims) %>%
    dplyr::mutate(
      iv_effect = iv_effect,
      true_effect = true_effect
    )
}


analyze_iv_power_simulation_sector <- function(results) {
  if (!is.data.frame(results)) results <- dplyr::bind_rows(results)
  
  results <- results %>%
    dplyr::mutate(
      wrong_sign = dplyr::case_when(
        true_effect < 0 & estimate >  0 ~ 1L,
        true_effect < 0 & estimate <= 0 ~ 0L,
        true_effect > 0 & estimate <  0 ~ 1L,
        true_effect > 0 & estimate >= 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      est_ratio = dplyr::if_else(
        true_effect != 0,
        estimate / true_effect,
        NA_real_
      )
    )
  
  power <- results %>%
    dplyr::group_by(granularity, sector, iv_effect, true_effect) %>%
    dplyr::summarise(
      power = mean(sig, na.rm = TRUE),
      .groups = "drop"
    )
  
  aux <- results %>%
    dplyr::filter(sig == 1) %>%
    dplyr::group_by(granularity, sector, iv_effect, true_effect) %>%
    dplyr::summarise(
      wrong_sign = mean(wrong_sign, na.rm = TRUE),
      est_ratio  = mean(est_ratio, na.rm = TRUE),
      .groups    = "drop"
    )
  
  dplyr::left_join(
    power, aux,
    by = c("granularity", "sector", "iv_effect", "true_effect")
  )
}

# ── Power analysis summary table by aggregation level ──────────────────────####

compute_level_summary <- function(df, sector_var, level_label) {
  
  # Number of sectors and cluster counts
  sector_summary <- df %>%
    group_by(.data[[sector_var]]) %>%
    summarise(
      n_obs      = n(),
      n_clusters = n_distinct(fe_week),
      .groups    = "drop"
    )
  
  # Residual SD per sector (same regression as power analysis calibration)
  resid_sd <- df %>%
    group_by(.data[[sector_var]]) %>%
    group_map(~ {
      m <- fixest::feols(
        log_consumption ~ Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_dow + fe_month,
        data = .x
      )
      tibble::tibble(
        sector   = .y[[1]],
        resid_sd = sd(residuals(m), na.rm = TRUE)
      )
    }) %>%
    bind_rows()
  
  tibble::tibble(
    Granularity       = level_label,
    N_sectors         = nrow(sector_summary),
    Mean_clusters     = round(mean(sector_summary$n_clusters), 0),
    Min_clusters      = min(sector_summary$n_clusters),
    Max_clusters      = max(sector_summary$n_clusters),
    Mean_obs          = round(mean(sector_summary$n_obs), 0),
    Mean_residual_SD  = round(mean(resid_sd$resid_sd, na.rm = TRUE), 4),
    Median_residual_SD = round(median(resid_sd$resid_sd, na.rm = TRUE), 4),
    Max_residual_SD   = round(max(resid_sd$resid_sd, na.rm = TRUE), 4),
    Min_residual_SD   = round(min(resid_sd$resid_sd, na.rm = TRUE), 4)
  )
}

level_summary <- bind_rows(
  compute_level_summary(consumption_panel, "DK36Title", "DK36"),
  compute_level_summary(consumption_panel, "DK19Title", "DK19"),
  compute_level_summary(consumption_panel, "DK10Title", "DK10")
)


print(knitr::kable(
  level_summary,
  format = "simple",
  col.names = c("Level", "Sectors", "Mean clusters", "Min clusters", 
                "Max clusters", "Mean obs", "Mean σ̂", "Median σ̂", 
                "Max σ̂", "Min σ̂")
))


# ── Aggregation-level models (DK10 + DK19 splits) ──────────────────────────####
IV_DK10_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)

IV_DK10_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)

IV_DK10_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow| log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)


# ── Shared definitions  ────────────────────────────────────────────────────####
gran_to_var <- c(DK36 = "DK36_en", DK19 = "DK19Title", DK10 = "DK10Title")


run_power_by_gran <- function(power_inputs, df_panel,
                              endog_var, instrument_var, temp_var, weight_name,
                              n_sims = 500, seed = 123,
                              batch_size = 25,
                              save_dir = "power_sim_checkpoints") {
  
  save_dir <- file.path(path.expand(save_dir), weight_name)
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  
  power_inputs_indexed <- power_inputs %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  purrr::pmap_dfr(
    dplyr::select(power_inputs_indexed, row_id, sector, granularity, true_effect, iv_effect),
    function(row_id, sector, granularity, true_effect, iv_effect) {
      
      sv  <- gran_to_var[[granularity]]
      dat <- dplyr::filter(df_panel, .data[[sv]] == sector)
      
      if (nrow(dat) < 100) return(NULL)
      
      sector_safe <- gsub("[^[:alnum:]_\\-]", "_", as.character(sector))
      n_batches <- ceiling(n_sims / batch_size)
      batch_results <- vector("list", n_batches)
      
      for (b in seq_len(n_batches)) {
        
        sims_this_batch <- min(batch_size, n_sims - (b - 1) * batch_size)
        
        batch_file <- file.path(
          save_dir,
          paste0(
            "power_",
            weight_name, "_",
            granularity, "_",
            sector_safe, "_te_", round(true_effect, 6),
            "_iv_", round(iv_effect, 6),
            "_batch_", b, ".rds"
          )
        )
        
        if (file.exists(batch_file)) {
          message("Loading existing batch ", b, "/", n_batches,
                  " for ", sector, " (", granularity, ", ", weight_name, ")")
          batch_results[[b]] <- readRDS(batch_file)
          next
        }
        
        message("Running batch ", b, "/", n_batches,
                " for ", sector, " (", granularity, ", ", weight_name, ")")
        
        batch_seed <- seed + row_id * 10000 + b
        
        res <- tryCatch(
          {
            power_sim_iv(
              df = dat,
              iv_effect = iv_effect,
              true_effect = true_effect,
              endog_var = endog_var,
              instrument_var = instrument_var,
              temp_var = temp_var,
              outcome_var = "log_consumption",
              sector_var = sv,
              fe_vars = c("fe_hour", "fe_month", "fe_dow"),
              controls = c( "log_gas", "log_coal", "log_carbon"),
              cluster_var = "fe_week",
              n_sims = sims_this_batch,
              seed = batch_seed
            ) %>%
              dplyr::mutate(
                sim = sim + (b - 1) * batch_size,
                granularity = granularity,
                sector = sector,
                batch = b,
                weight_name = weight_name
              )
          },
          error = function(e) {
            message("Batch ", b, " failed for ", sector, " (", granularity, "): ", e$message)
            return(NULL)
          }
        )
        
        if (!is.null(res)) saveRDS(res, batch_file)
        batch_results[[b]] <- res
        
        gc()
      }
      
      dplyr::bind_rows(batch_results)
    },
    .progress = "Power simulation"
  )
}

# ── Compute artificial effects  ────────────────────────────────────────────####
# ── Extract observed first-stage coefficients per sector and granularity 

extract_first_stage <- function(model, granularity) {
  purrr::map_dfr(seq_along(names(model)), function(i) {
    m      <- model[[i]]
    sector <- names(model)[i]
    
    fs_ct    <- fixest::coeftable(m$iv_first_stage[[1]])
    inst_row <- rownames(fs_ct)[stringr::str_detect(rownames(fs_ct), "Wind")]
    
    tibble::tibble(
      sector      = stringr::str_remove(sector, "^sample\\.var:.*sample: "),
      granularity = granularity,
      iv_effect   = as.numeric(fs_ct[inst_row, "Estimate"])
    )
  })
}

# ── Consumption weights ───────────────────────────────────────────────────────
fs_inputs_c <- bind_rows(
  extract_first_stage(IV_model_het_DK10_W, "DK36"),
  extract_first_stage(IV_DK19_agg_c,       "DK19"),
  extract_first_stage(IV_DK10_agg_c,       "DK10")
)

# ── Employment weights ────────────────────────────────────────────────────────
fs_inputs_emp <- bind_rows(
  extract_first_stage(IV_model_het_Emp_w,  "DK36"),
  extract_first_stage(IV_DK19_agg_emp,     "DK19"),
  extract_first_stage(IV_DK10_agg_emp,     "DK10")
)

# ── Firm-count weights ────────────────────────────────────────────────────────
fs_inputs_firm <- bind_rows(
  extract_first_stage(IV_model_het_firm_w, "DK36"),
  extract_first_stage(IV_DK19_agg_firm,    "DK19"),
  extract_first_stage(IV_DK10_agg_firm,    "DK10")
)

# ── Literature-anchored effect grid ───────────────────────────────────────────
# Base elasticity from Hirth et al. (2024)
base_elasticity <- -0.045

# 5-point grid: 0.5×, 0.75×, 1.0×, 1.25×, 1.5× the literature benchmark
effect_grid <- base_elasticity * c(0.25, 0.5, 0.75, 1.00, 1.5)

# ── Build simulation inputs: observed first stage × literature effect grid ────

build_power_inputs <- function(fs_inputs, effect_grid) {
  tidyr::crossing(
    fs_inputs,
    true_effect = effect_grid
  )
}

power_inputs_c    <- build_power_inputs(fs_inputs_c,    effect_grid)
power_inputs_emp  <- build_power_inputs(fs_inputs_emp,  effect_grid)
power_inputs_firm <- build_power_inputs(fs_inputs_firm, effect_grid)


# ── Simulation  ────────────────────────────────────────────────────────────####
power_results_c <- run_power_by_gran(
  power_inputs = power_inputs_c,
  df_panel = consumption_panel,
  endog_var = "log_P_c",
  instrument_var = "Wind_c",
  temp_var  = "Temp_c",
  weight_name = "consumption_weight"
)

saveRDS(power_results_c, "power_results_c_1.rds")

power_results_emp <- run_power_by_gran(
  power_inputs = power_inputs_emp,
  df_panel = consumption_panel,
  endog_var = "log_P_emp",
  instrument_var = "Wind_emp",
  temp_var = "Temp_emp",
  weight_name = "employment_weight"
)

saveRDS(power_results_emp, "power_results_emp_1.rds")

power_results_firm <- run_power_by_gran(
  power_inputs = power_inputs_firm,
  df_panel = consumption_panel,
  endog_var = "log_P_firm",
  instrument_var = "Wind_firm",
  temp_var = "Temp_firm",
  weight_name = "firm_weight"
)

saveRDS(power_results_firm, "power_results_firm_1.rds")


# ── Load pre-computed simulations ──────────────────────────────────────────####
power_results_c    <- readRDS("power_results_c_1.rds")
power_results_emp  <- readRDS("power_results_emp_1.rds")
power_results_firm <- readRDS("power_results_firm_1.rds")
# ── Power analysis summaries ───────────────────────────────────────────────####
power_analysis_c    <- analyze_iv_power_simulation_sector(power_results_c)
power_analysis_emp  <- analyze_iv_power_simulation_sector(power_results_emp)
power_analysis_firm <- analyze_iv_power_simulation_sector(power_results_firm)

power_all <- bind_rows(
  power_analysis_c    %>% mutate(weight = "Consumption"),
  power_analysis_emp  %>% mutate(weight = "Employment"),
  power_analysis_firm %>% mutate(weight = "Firm count")
) %>%
  mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    weight      = factor(weight, levels = c("Consumption", "Employment", "Firm count"))
  )




