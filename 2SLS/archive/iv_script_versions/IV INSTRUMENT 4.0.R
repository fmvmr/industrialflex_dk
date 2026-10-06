# IV INSTRUMENT 4.0 VERSION:
# Combined script merging v3.0 (data + IV + power) and v2.1 (Anderson-Rubin + Sensemakr)
setwd("~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")
fixest::setFixest_notes(FALSE)
# =============================================================================
# = PART ONE: DATA PREPARATION  ==============================================
# =============================================================================
# ── Packages ───────────────────────────────────────────────────────────────####


invisible(lapply(
  c("arrow", "dplyr", "lubridate", "stringr", "purrr", "glue", "readxl",
    "tidyr", "fixest", "readr", "ggplot2", "rlang", "tibble", "patchwork",
    "progress", "plm", "AER", "ivreg", "lmtest", "sandwich", "stargazer","kableExtra",
    "scales", "sensemakr"),
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
      bind_nn(lapply(yms, function(ym) safe_read(glue("data/{p}_{ym}.parquet"))))
    ),
    list(
      industry_annual = bind_nn(lapply(years, function(y)
        safe_read(glue("data/consumption_dk10_region_year/consumption_dk10_region_{y}.parquet"))
      )),
      generation = bind_nn(lapply(years, function(y)
        bind_nn(lapply(c("DK1", "DK2"), function(area)
          safe_read(glue("data/generation_prod_type_exchange/generation_{area}_{y}.parquet"))
        ))
      )),
      gas    = arrow::read_parquet("data/controls/gas_daily_2020_2025.parquet"),
      carbon = arrow::read_parquet("data/controls/carbon_daily_2020_2025.parquet"),
      coal   = arrow::read_parquet("data/controls/coal_daily_2020_2025.parquet")
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

IV_model_het_DK10 <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_model <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week
)

summary(IV_model)

IV_model_het_DK10_W <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_model_het_Emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

IV_model_het_Emp_w <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_model_het_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
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
  extract_iv(IV_model_het_DK10,   "Consumption", "Month"),
  extract_iv(IV_model_het_DK10_W, "Consumption", "Week"),
  extract_iv(IV_model_het_Emp,    "Employment",  "Month"),
  extract_iv(IV_model_het_Emp_w,  "Employment",  "Week"),
  extract_iv(IV_model_het_firm,   "Firm count",  "Month"),
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
    cluster = factor(cluster, levels = c("Month", "Week"))
  )

# ── Sector name cleaning ───────────────────────────────────────────────────####
results <- results %>%
  mutate(sector = stringr::str_remove(sector,
                                      "^sample\\.var: DK36_en; sample: "))

sector_order <- results %>%
  filter(weight == "Consumption", cluster == "Month") %>%
  arrange(estimate) %>%
  pull(sector)

results <- results %>%
  mutate(sector = factor(sector, levels = sector_order))

# ── First-stage diagnostics ────────────────────────────────────────────────####
results %>%
  filter(cluster == "Month") %>%
  group_by(weight) %>%
  summarise(
    n_weak     = sum(fs_f < 10,  na.rm = TRUE),
    n_moderate = sum(fs_f >= 10 & fs_f < 20, na.rm = TRUE),
    n_strong   = sum(fs_f >= 20, na.rm = TRUE)
  )

# ── Estimate tables ────────────────────────────────────────────────────────####
est_wide <- results %>%
  group_by(sector) %>%
  mutate(
    mean_estimate_sector = mean(estimate, na.rm = TRUE),
    cell = sprintf("%.3f%s (%.3f)", estimate, sig, se),
    col  = paste0(weight, " / ", cluster)
  ) %>%
  ungroup() %>%
  select(sector, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell)

print(knitr::kable(est_wide, format = "simple",
                   col.names = c("Sector",
                                 "Cons/Month", "Cons/Week",
                                 "Emp/Month",  "Emp/Week",
                                 "Firm/Month", "Firm/Week")))


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
    
    tibble::tibble(
      sector      = stringr::str_remove(s, "^sample\\.var: DK36_en; sample: "),
      granularity = granularity,
      weight      = weight,
      cluster     = cluster,
      instrument  = inst_row[1],
      fs_estimate = as.numeric(fs_ct[inst_row[1], "Estimate"]),
      fs_se       = as.numeric(fs_ct[inst_row[1], "Std. Error"]),
      fs_p_value  = as.numeric(fs_ct[inst_row[1], "Pr(>|t|)"]),
      fs_t_value  = as.numeric(fs_ct[inst_row[1], "t value"]),
      fs_f        = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                             error = function(e) NA_real_)
    )
  })
}

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
  extract_first_stage(IV_model_het_DK10,   "Consumption", "Month", "DK36"),
  extract_first_stage(IV_model_het_DK10_W, "Consumption", "Week",  "DK36"),
  extract_first_stage(IV_model_het_Emp,    "Employment",  "Month", "DK36"),
  extract_first_stage(IV_model_het_Emp_w,  "Employment",  "Week",  "DK36"),
  extract_first_stage(IV_model_het_firm,   "Firm count",  "Month", "DK36"),
  extract_first_stage(IV_model_het_firm_w, "Firm count",  "Week",  "DK36")
) %>%
  dplyr::group_by(sector) %>%
  dplyr::mutate(
    mean_first_stage_sector = mean(fs_estimate, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

names(first_stage_results)
summary(first_stage_results$mean_first_stage_sector)

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
# = PART FOUR: ANDERSON-RUBIN & SENSITIVITY  ================================
# ============================================================================
# ── Anderson-Rubin confidence sets ─────────────────────────────────────────####

# Grid-search inversion of the AR test:
# For each candidate beta0, test H0: beta = beta0
# by regressing (Y - beta0 * D) on Z and controls.
# If the coefficient on Z is insignificant, beta0 is in the AR confidence set.

compute_AR_cs <- function(data, log_price_var, wind_var, temp_var,
                          beta_grid = seq(-0.5, 0.1, by = 0.001),
                          alpha = 0.05) {

  hour_dummies  <- model.matrix(~ factor(fe_hour)  - 1, data = data)
  month_dummies <- model.matrix(~ factor(fe_month) - 1, data = data)

  Y      <- data$log_consumption
  D      <- data[[log_price_var]]
  Z      <- data[[wind_var]]
  Temp   <- data[[temp_var]]
  Gas    <- data$log_gas
  Coal   <- data$log_coal
  Carbon <- data$log_carbon

  ar_results <- map_dfr(beta_grid, function(b0) {
    Y_adj <- Y - b0 * D

    fit <- lm(Y_adj ~ Z + Temp + Gas + Coal + Carbon +
                hour_dummies + month_dummies)

    cf <- summary(fit)$coefficients
    if (!"Z" %in% rownames(cf)) return(NULL)

    t_stat <- cf["Z", "t value"]
    p_val  <- cf["Z", "Pr(>|t|)"]

    tibble(beta0 = b0, t_stat = t_stat, p_value = p_val,
           in_CS = p_val >= alpha)
  })

  ar_results
}

# ── AR: run for 3 weights ──────────────────────────────────────────────────####
ar_consumption <- compute_AR_cs(
  data = consumption_panel, log_price_var = "log_P_c",
  wind_var = "Wind_c", temp_var = "Temp_c",
  beta_grid = seq(-0.35, -0.05, by = 0.001), alpha = 0.05
)

ar_employment <- compute_AR_cs(
  consumption_panel, "log_P_emp", "Wind_emp", "Temp_emp",
  beta_grid = seq(-0.35, -0.05, by = 0.001)
)

ar_firm <- compute_AR_cs(
  consumption_panel, "log_P_firm", "Wind_firm", "Temp_firm",
  beta_grid = seq(-0.35, -0.05, by = 0.001)
)

# ── AR: confidence set extraction ──────────────────────────────────────────####
ar_cs_c <- ar_consumption %>% filter(in_CS == TRUE)
cat("\nAnderson-Rubin 95% Confidence Set (Consumption weights):\n")
cat("  Lower bound:", min(ar_cs_c$beta0), "\n")
cat("  Upper bound:", max(ar_cs_c$beta0), "\n")
cat("  AR CS is connected:", all(diff(which(ar_consumption$in_CS)) == 1), "\n")

# Extract pooled 2SLS point estimates dynamically from results
point_est_c    <- results %>% filter(weight == "Consumption", cluster == "Month") %>%
  summarise(m = weighted.mean(estimate, w = 1/se^2)) %>% pull(m)
point_est_emp  <- results %>% filter(weight == "Employment",  cluster == "Month") %>%
  summarise(m = weighted.mean(estimate, w = 1/se^2)) %>% pull(m)
point_est_firm <- results %>% filter(weight == "Firm count",  cluster == "Month") %>%
  summarise(m = weighted.mean(estimate, w = 1/se^2)) %>% pull(m)

# ── AR: summary table ──────────────────────────────────────────────────────####
ar_summary <- tibble(
  Weight   = c("Consumption", "Employment", "Firm count"),
  Point_Est = c(point_est_c, point_est_emp, point_est_firm),
  AR_Lower = c(
    min(ar_consumption$beta0[ar_consumption$in_CS]),
    min(ar_employment$beta0[ar_employment$in_CS]),
    min(ar_firm$beta0[ar_firm$in_CS])
  ),
  AR_Upper = c(
    max(ar_consumption$beta0[ar_consumption$in_CS]),
    max(ar_employment$beta0[ar_employment$in_CS]),
    max(ar_firm$beta0[ar_firm$in_CS])
  )
)

print(ar_summary)
# ── Sensemakr: extract function ────────────────────────────────────────────####

extract_sens_stats <- function(data, industry_col, industry_val,
                               wind_var, temp_var, weight_label,
                               kd_max = 5, alpha = 0.05) {

  sub <- data %>% filter(.data[[industry_col]] == industry_val)

  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon"
  ))

  itt_lm <- tryCatch(lm(fml, data = sub), error = function(e) NULL)
  if (is.null(itt_lm)) return(NULL)

  sens <- tryCatch(
    sensemakr(model = itt_lm, treatment = wind_var,
              benchmark_covariates = temp_var, kd = 1:kd_max, alpha = alpha),
    error = function(e) NULL
  )
  if (is.null(sens)) return(NULL)

  rv_q1       <- sens$sensitivity_stats$rv_q
  rv_q1_alpha <- sens$sensitivity_stats$rv_qa
  partial_r2  <- sens$sensitivity_stats$r2yd.x
  t_original  <- sens$sensitivity_stats$t_statistic

  bounds <- sens$bounds
  if (!is.null(bounds) && nrow(bounds) > 0) {
    t_crit     <- qt(1 - alpha / 2, df = itt_lm$df.residual)
    bounds_df  <- as.data.frame(bounds)
    bounds_df$kd <- as.numeric(str_extract(bounds_df$bound_label, "^[0-9]+"))

    sig_lost_row <- bounds_df %>%
      filter(abs(adjusted_t) < t_crit) %>%
      slice_min(kd, n = 1)

    critical_kd <- if (nrow(sig_lost_row) > 0) sig_lost_row$kd[1] else paste0(">", kd_max)
  } else {
    critical_kd <- NA
  }

  tibble(
    Industry    = industry_val,
    Weight      = weight_label,
    t_original  = round(t_original, 2),
    Partial_R2  = round(partial_r2, 4),
    RV_q1       = round(rv_q1, 4),
    RV_q1_alpha = round(rv_q1_alpha, 4),
    Critical_kd = as.character(critical_kd),
    N           = nrow(sub)
  )
}


# ── Sensemakr: run across industries ───────────────────────────────────────####
industries <- unique(consumption_panel$DK36_en)

sens_consumption <- map_dfr(industries, function(ind) {
  extract_sens_stats(consumption_panel, "DK36_en", ind,
                     "Wind_c", "Temp_c", "Consumption", kd_max = 5)
})

sens_employment <- map_dfr(industries, function(ind) {
  extract_sens_stats(consumption_panel, "DK36_en", ind,
                     "Wind_emp", "Temp_emp", "Employment", kd_max = 5)
})

sens_firm <- map_dfr(industries, function(ind) {
  extract_sens_stats(consumption_panel, "DK36_en", ind,
                     "Wind_firm", "Temp_firm", "Firm count", kd_max = 5)
})

# ── Sensemakr: robustness tiers ────────────────────────────────────────────####
sens_consumption <- sens_consumption %>%
  mutate(Robustness_Tier = case_when(
    RV_q1_alpha >= 0.10  ~ "High",
    RV_q1_alpha >= 0.03  ~ "Moderate",
    TRUE                 ~ "Fragile"
  )) %>%
  arrange(desc(RV_q1))

sens_all <- bind_rows(sens_consumption, sens_employment, sens_firm) %>%
  arrange(Industry, Weight)

print(sens_all, n = 200)

# ── Sensemakr: cross-weight consistency ────────────────────────────────────####
consistency_check <- sens_all %>%
  select(Industry, Weight, RV_q1, RV_q1_alpha) %>%
  pivot_wider(names_from = Weight, values_from = c(RV_q1, RV_q1_alpha), names_sep = "_") %>%
  mutate(
    RV_range = round(
      pmax(RV_q1_Consumption, RV_q1_Employment, `RV_q1_Firm count`, na.rm = TRUE) -
        pmin(RV_q1_Consumption, RV_q1_Employment, `RV_q1_Firm count`, na.rm = TRUE), 4
    )
  ) %>%
  arrange(desc(RV_q1_Consumption))

print(consistency_check, n = 50)
# ── Sensemakr: summary statistics ──────────────────────────────────────────####
cat("\nNumber of industries analysed:", nrow(sens_consumption), "\n")
cat("  High robustness (RV_alpha >= 0.10):",
    sum(sens_consumption$Robustness_Tier == "High", na.rm = TRUE), "\n")
cat("  Moderate robustness (0.03 <= RV_alpha < 0.10):",
    sum(sens_consumption$Robustness_Tier == "Moderate", na.rm = TRUE), "\n")
cat("  Fragile (RV_alpha < 0.03):",
    sum(sens_consumption$Robustness_Tier == "Fragile", na.rm = TRUE), "\n\n")
cat("RV (q=1) range:",
    round(min(sens_consumption$RV_q1, na.rm = TRUE), 4), "to",
    round(max(sens_consumption$RV_q1, na.rm = TRUE), 4), "\n")
cat("RV (q=1, alpha=0.05) range:",
    round(min(sens_consumption$RV_q1_alpha, na.rm = TRUE), 4), "to",
    round(max(sens_consumption$RV_q1_alpha, na.rm = TRUE), 4), "\n\n")

rv_diffs <- consistency_check$RV_range
cat("Cross-weight RV consistency:\n")
cat("  Mean absolute RV difference:", round(mean(rv_diffs, na.rm = TRUE), 4), "\n")
cat("  Max absolute RV difference:", round(max(rv_diffs, na.rm = TRUE), 4), "\n")


# ===========================================================================
# = PART FIVE: POWER ANALYSIS  ==============================================
# ===========================================================================
# ── Source helpers ─────────────────────────────────────────────────────────####
source("sim_power_iv_clustering.R")
source("iv_sim_prep.R")

# ── difference tables ──────────────────────────────────────────────────────#####

consumption_panel %>%
  group_by(DK36Title) %>%
  summarise(n = n(), .groups = "drop") %>%
  summarise(level = "DK36", n_sectors = n(), mean_obs = mean(n), 
            min_obs = min(n), median_obs = median(n))

consumption_panel %>%
  group_by(DK19Title) %>%
  summarise(n = n(), .groups = "drop") %>%
  summarise(level = "DK19", n_sectors = n(), mean_obs = mean(n), 
            min_obs = min(n), median_obs = median(n))

consumption_panel %>%
  group_by(DK10Title) %>%
  summarise(n = n(), .groups = "drop") %>%
  summarise(level = "DK10", n_sectors = n(), mean_obs = mean(n), 
            min_obs = min(n), median_obs = median(n))


# ── Power analysis summary table by aggregation level ─────────────────────────

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
                              save_dir = "~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis//Data/power_sim_checkpoints") {
  
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
# ── First stage effect  
# ── Extract observed first-stage coefficients per sector and granularity 
# These are fixed (not gridded) — one value per sector, used as-is in simulation

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
# Base elasticity from Hirth et al. (2024) — adjust this value as needed
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

# ──  APPENDIX TABLES: Sector-level Type M / power diagnostics ──────────────####
# ── A.1 Full Type M table at nearest simulated true effect (consumption weight, DK36) ────

power_dk36_c <- power_analysis_c %>% filter(granularity == "DK36")

sig_results_c <- results %>%
  filter(weight == "Consumption", cluster == "Week", p_value < 0.05) %>%
  select(sector, estimate, se, p_value, fs_f)

appendix_typeM_nearest_c <- sig_results_c %>%
  left_join(power_dk36_c, by = "sector") %>%
  group_by(sector) %>%
  slice_min(abs(true_effect - estimate), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    estimate    = round(estimate, 4),
    se          = round(se, 4),
    p_value     = signif(p_value, 3),
    true_effect = round(true_effect, 4),
    power       = round(power, 3),
    wrong_sign  = round(wrong_sign, 3),
    est_ratio   = round(est_ratio, 2),
    fs_f        = round(fs_f, 1)
  ) %>%
  select(
    Sector       = sector,
    Estimate     = estimate,
    SE           = se,
    `p-value`    = p_value,
    `First-stage F` = fs_f,
    `Nearest β₀` = true_effect,
    Power        = power,
    `Wrong-sign rate` = wrong_sign,
    `Type M ratio`    = est_ratio
  ) %>%
  arrange(Power)

print(appendix_typeM_nearest_c, n = Inf)

# ── A.2 Full power surface across the effect grid (consumption weight, DK36) ──

appendix_power_surface_c <- power_analysis_c %>%
  filter(granularity == "DK36") %>%
  inner_join(sig_results_c %>% select(sector), by = "sector") %>%
  mutate(
    power     = round(power, 3),
    est_ratio = round(est_ratio, 2)
  ) %>%
  select(sector, true_effect, power, est_ratio) %>%
  pivot_wider(
    names_from  = true_effect,
    values_from = c(power, est_ratio),
    names_glue  = "{.value}_β={true_effect}"
  ) %>%
  arrange(sector)

print(appendix_power_surface_c, n = Inf, width = Inf)








# =============================================================================
# = PART SIX: VISUALISATIONS ================================================
# =============================================================================
# ── Weight distribution comparison ─────────────────────────────────────────####
industry %>%
  left_join(distinct(consumption_panel, DK36Title, DK36_en), by = "DK36Title") %>%
  group_by(DK36_en) %>%
  summarise(
    Consumption  = mean(w_DK1_c,       na.rm = TRUE),
    Employment   = mean(w_DK1_emp,     na.rm = TRUE),
    `Firm count` = mean(w_DK1_firm,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na() %>% 
  pivot_longer(-DK36_en, names_to = "scheme", values_to = "w_DK1") %>%
  mutate(scheme = factor(scheme, levels = c("Consumption", "Employment", "Firm count", "Geo. mean"))) %>%
  # order sectors by consumption weight descending (same as original plot)
  ggplot(aes(x = scheme, y = DK36_en, fill = w_DK1)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = round(w_DK1, 2)), size = 3, colour = "grey20") +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "#ffffbf",
    high     = "#4575b4",
    midpoint = 0.5,
    limits   = c(0, 1),
    name     = "DK1 weight"
  ) +
  labs(
    title    = "DK1 zone weight by sector and weighting scheme",
    subtitle = "> 0.5 = majority in DK1 (West Denmark)",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x  = element_text(face = "bold"),
    axis.text.y  = element_text(size = 9),
    panel.grid   = element_blank(),
    legend.position = "right"
  )
# ── Weighted price comparison ──────────────────────────────────────────────####
pal_methods <- c(
  "Consumption" = "#2166AC",
  "Employment"  = "#B2182B",
  "Firm count"  = "#1B7837",
  "Geomean"     = "#F4A736"
)

th <- theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 11, margin = margin(b = 4)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4),
    axis.text          = element_text(size = 9, colour = "grey30"),
    axis.title         = element_text(size = 9.5)
  )

price_long <- weighted_prices %>%
  filter(!is.na(P_weighted_c), P_weighted_c > 0) %>%
  select(TimeUTC, P_weighted_c, P_weighted_emp, P_weighted_firm, P_weighted_firmxemp) %>%
  pivot_longer(-TimeUTC, names_to = "method", values_to = "price") %>%
  mutate(method = dplyr::case_match(method,
                                    "P_weighted_c"    ~ "Consumption",
                                    "P_weighted_emp"  ~ "Employment",
                                    "P_weighted_firm" ~ "Firm count",
                                    "P_weighted_firmxemp"  ~ "Geomean"
  ))

price_diff <- weighted_prices %>%
  filter(!is.na(P_weighted_c), P_weighted_c > 0) %>%
  mutate(
    Employment   = P_weighted_emp  - P_weighted_c,
    `Firm count` = P_weighted_firm - P_weighted_c,
    Geomean = P_weighted_firmxemp - P_weighted_c
  ) %>%
  select(TimeUTC, Employment, `Firm count`, Geomean) %>%
  pivot_longer(-TimeUTC, names_to = "method", values_to = "diff") %>%
  filter(!is.na(diff))

p1 <- weighted_prices %>%
  filter(!is.na(P_weighted_c), P_weighted_c > 0) %>%
  slice_sample(n = 5000) %>%   # thin for overplotting
  pivot_longer(c(P_weighted_emp, P_weighted_firm, P_weighted_firmxemp),
               names_to = "method", values_to = "p_alt") %>%
  mutate(method = dplyr::case_match(method,
                                    "P_weighted_emp"      ~ "Employment",
                                    "P_weighted_firm"     ~ "Firm count",
                                    "P_weighted_firmxemp" ~ "Geomean"
  )) %>%
  ggplot(aes(x = P_weighted_c, y = p_alt, colour = method)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.05, size = 0.6) +
  facet_wrap(~ method, ncol = 3) +
  scale_colour_manual(values = pal_methods) +
  scale_x_continuous(labels = scales::label_number(suffix = " €")) +
  scale_y_continuous(labels = scales::label_number(suffix = " €")) +
  labs(title = "A. Alternative weights vs. consumption weight",
       x = "Consumption price (EUR/MWh)", y = "Alternative price (EUR/MWh)") +
  th + theme(legend.position = "none")

p2 <- ggplot(price_diff, aes(x = diff, fill = method, colour = method)) +
  geom_density(alpha = 0.15, linewidth = 0.75) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.45) +
  annotate("text", x = 0.3, y = Inf, label = "No difference",
           hjust = 0, vjust = 1.6, size = 2.8, colour = "grey45", fontface = "italic") +
  scale_x_continuous(labels = scales::label_number(suffix = " \u20AC")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = pal_methods[c("Employment", "Firm count", "Geomean")]) +
  scale_colour_manual(values = pal_methods[c("Employment", "Firm count", "Geomean")]) +
  labs(title = "B. Price deviation from consumption-based weight",
       x = "\u0394 Price (EUR/MWh)", y = "Density", fill = NULL, colour = NULL) +
  th +
  theme(legend.position      = c(0.97, 0.97),
        legend.justification = c(1, 1),
        legend.background    = element_rect(fill = "white", colour = NA),
        legend.key.size      = unit(0.4, "cm"),
        legend.text          = element_text(size = 9))

p1 + p2 +
  plot_annotation(
    title    = "Sensitivity of Sector-Level Electricity Prices to Weighting Approach",
    subtitle = "Panel B uses consumption-based weight as baseline; all sectors and years pooled",
    caption  = "P = w1 x P_DK1 + (1 - w1) x P_DK2, where w1 is the DK1 share under each approach.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, colour = "grey35", margin = margin(b = 8)),
      plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 0, margin = margin(t = 8))
    )
  )

# ── Diverging bar: price deviation from consumption weights ────────────────####
spread_dev <- weighted_prices %>%
  left_join(distinct(consumption_panel, DK36Title, DK36_en), by = "DK36Title") %>%
  mutate(spread = abs(DK1_P - DK2_P)) %>%
  filter(spread > quantile(spread, 0.9, na.rm = TRUE)) %>%  # top 10% spread hours
  mutate(diff_emp  = P_weighted_emp      - P_weighted_c,
         diff_firm = P_weighted_firm     - P_weighted_c,
         diff_geo  = P_weighted_firmxemp - P_weighted_c) %>%
  group_by(DK36_en) %>%
  summarise(across(starts_with("diff_"), mean, na.rm = TRUE)) %>%
  print (n =32)



spread_dev %>%
  filter(!is.na(diff_geo), DK36_en != "Uoplyst aktivitet") %>%
  mutate(DK36_en = factor(DK36_en, levels = spread_dev %>%
                                     filter(!is.na(diff_geo), DK36_en != "Uoplyst aktivitet") %>%
                                     arrange(diff_geo) %>%
                                     pull(DK36_en))) %>%   # ← before pivot
  pivot_longer(c(diff_emp, diff_firm, diff_geo),
               names_to = "method", values_to = "diff") %>%
  mutate(
    method = dplyr::case_match(method,
                               "diff_emp"  ~ "Employment",
                               "diff_firm" ~ "Firm count",
                               "diff_geo"  ~ "Geomean"
    ),
    method = factor(method, levels = c("Employment", "Firm count", "Geomean"))
  ) %>%
  ggplot(aes(x = diff, y = DK36_en, fill = method, colour = method)) +
  geom_col(
    data     = ~ filter(.x, method != "Geomean"),
    position = position_dodge(width = 0.7),
    width    = 0.6, alpha = 0.35
  ) +
  geom_point(
    data  = ~ filter(.x, method == "Geomean"),
    shape = 18, size = 3
  ) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey30") +
  scale_fill_manual(values   = pal_methods[c("Employment", "Firm count", "Geomean")]) +
  scale_colour_manual(values = pal_methods[c("Employment", "Firm count", "Geomean")]) +
  scale_x_continuous(labels = scales::label_number(suffix = " €")) +
  labs(
    title    = "Price deviation from consumption weight during high price-spread hours",
    subtitle = "Top 10% of |P_DK1 − P_DK2| hours; bars = Employment & Firm count, diamond = Geomean",
    x        = "Δ Price vs. consumption weight (EUR/MWh)",
    y        = NULL,
    fill     = NULL, colour = NULL
  ) +
  th +
  theme(
    legend.position = "top",
    axis.text.y     = element_text(size = 8)
  )
monthly_spread <- weighted_prices %>%
  distinct(TimeUTC, DK1_P, DK2_P) %>%
  filter(!is.na(DK1_P), !is.na(DK2_P)) %>%
  mutate(spread = DK1_P - DK2_P,
         month  = lubridate::floor_date(TimeUTC, "month")) %>%
  group_by(month) %>%
  summarise(mean_spread   = mean(spread,      na.rm = TRUE),
            share_nonzero = mean(spread != 0, na.rm = TRUE),
            .groups = "drop")

scale_factor <- max(abs(monthly_spread$mean_spread), na.rm = TRUE)

ggplot(monthly_spread, aes(x = month)) +
  geom_col(aes(y = share_nonzero), fill = "#2166AC", alpha = 0.3,
           width = 25 * 24 * 3600) +
  geom_line(aes(y = mean_spread / scale_factor),
            colour = "#B2182B", linewidth = 0.7) +
  scale_y_continuous(
    name     = "Share of hours with non-zero spread",
    labels   = scales::label_percent(),
    sec.axis = sec_axis(~ . * scale_factor,
                        name = "Mean spread DK1 - DK2 (EUR/MWh)")
  ) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(title    = "DK1 vs DK2 Price Spread Over Time",
       subtitle = "Bars: share of hours with non-zero spread  |  Line: monthly mean spread (right axis)",
       x = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(size = 9, colour = "grey35"),
        panel.grid.minor   = element_blank(),
        axis.title.y.right = element_text(colour = "#B2182B"),
        axis.text.y.right  = element_text(colour = "#B2182B"))
# ── Panel coverage heatmap ─────────────────────────────────────────────────####
consumption_panel %>%
  mutate(month = lubridate::floor_date(TimeUTC, "month")) %>%
  group_by(DK36_en, month) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  ggplot(aes(x = month, y = reorder(DK36_en, n_obs), fill = n_obs)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#deebf7", high = "#2166AC",
                      labels = scales::label_comma()) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(title = "Panel Coverage by Sector and Month",
       subtitle = "Cell colour = number of hourly observations",
       x = NULL, y = NULL, fill = "Hours") +
  theme_minimal(base_size = 10) +
  theme(plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 9, colour = "grey35"),
        axis.text.y      = element_text(size = 7),
        axis.text.x      = element_text(angle = 30, hjust = 1, size = 8),
        panel.grid       = element_blank(),
        legend.position  = "bottom",
        legend.key.width = unit(1.5, "cm"))
# ── Shared theme ───────────────────────────────────────────────────────────####
th_coef <- theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0,
                                      margin = margin(t = 8)),
    axis.text.y        = element_text(size = 7.5),
    axis.text.x        = element_text(size = 9),
    axis.title.x       = element_text(size = 9.5),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9)
  )

# ── Colour / shape schemes ────────────────────────────────────────────────────
pal_gran   <- c(DK10 = "#2166ac", DK19 = "#f4a582", DK36 = "#ca0020")
pal_weight <- c(Consumption = "#2166ac", Employment = "#b2182b", `Firm count` = "#1b7837")
shape_gran <- c(DK10 = 16, DK19 = 17, DK36 = 15)
size_gran  <- c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)

shape_weight <- c(Consumption = 16, Employment = 17, `Firm count` = 15)

# ── Common theme ──────────────────────────────────────────────────────────────
th_pow <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1.25, "lines")
  )




# ── Residualised first stage: Wind_c → log_P_c (FE-partialled) ─────────────####

vars_needed_fs <- c("log_P_c", "Wind_c", "fe_hour", "fe_month", "fe_dow")

panel_fs <- consumption_panel %>%
  select(all_of(vars_needed_fs)) %>%
  tidyr::drop_na(all_of(vars_needed_fs))

fe_logP <- feols(log_P_c ~ 1 | fe_hour + fe_month + fe_dow, data = panel_fs)
fe_wind <- feols(Wind_c   ~ 1 | fe_hour + fe_month + fe_dow, data = panel_fs)

panel_fs <- panel_fs %>%
  mutate(
    resid_logP = as.numeric(residuals(fe_logP)),
    resid_wind = as.numeric(residuals(fe_wind))
  )

fs_plot_data <- panel_fs %>%
  mutate(wind_bin = ntile(resid_wind, n_bins)) %>%
  group_by(wind_bin) %>%
  summarise(
    mean_wind = mean(resid_wind, na.rm = TRUE),
    mean_logP = mean(resid_logP, na.rm = TRUE),
    .groups   = "drop"
  )

ggplot(fs_plot_data, aes(x = mean_wind, y = mean_logP)) +
  geom_line(colour = "black", linewidth = 0.5) +
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.7) +
  labs(
    title    = "First Stage: Wind Forecast → Log Electricity Price",
    subtitle = "Residualised on hour-of-day, year-month, day-of-week fixed effects",
    x        = "Wind forecast (residualised)",
    y        = "Log electricity price (residualised)",
    caption  = "Binned scatter (20 equal-count bins). Consumption-weighted price."
  ) + th_coef



# ── Reduced-form binned scatter: Wind_c → log_consumption (AK style) ───────####
# ── Step 1: Clean sample ──────────────────────────────────────────────────
vars_needed <- c("log_consumption", "Wind_c", "fe_hour", "fe_month", "fe_dow")

panel_rf <- consumption_panel %>%
  select(all_of(vars_needed)) %>%
  tidyr::drop_na(all_of(vars_needed))

# ── Step 2: Partial out FEs ───────────────────────────────────────────────
fe_consump <- feols(log_consumption ~ 1 | fe_hour + fe_month + fe_dow, data = panel_rf)
fe_wind    <- feols(Wind_c          ~ 1 | fe_hour + fe_month + fe_dow, data = panel_rf)

panel_rf <- panel_rf %>%
  mutate(
    resid_consump = as.numeric(residuals(fe_consump)),
    resid_wind    = as.numeric(residuals(fe_wind))
  )

# ── Step 3: Bin on residualised wind ─────────────────────────────────────
n_bins <- 20

rf_plot_data <- panel_rf %>%
  mutate(wind_bin = ntile(resid_wind, n_bins)) %>%
  group_by(wind_bin) %>%
  summarise(
    mean_wind    = mean(resid_wind,    na.rm = TRUE),
    mean_consump = mean(resid_consump, na.rm = TRUE),
    .groups      = "drop"
  )

# ── Step 4: Plot ──────────────────────────────────────────────────────────
ggplot(rf_plot_data, aes(x = mean_wind, y = mean_consump)) +
  geom_line(colour = "black", linewidth = 0.5) +
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.7) +
  labs(
    title    = "Reduced Form: Wind Forecast → Log Electricity Consumption",
    subtitle = "Residualised on hour-of-day, year-month, day-of-week fixed effects",
    x        = "Wind forecast (residualised)",
    y        = "Log electricity consumption (residualised)",
    caption  = "Binned scatter (20 equal-count bins). Consumption-weighted price."
  ) + th_coef

ggsave("reduced_form_residualised.pdf", width = 7, height = 4.5)


# ── Main coefficient plot (consumption weight, month cluster) ──────────────####
p_main <- results %>%
  filter(weight == "Consumption", cluster == "Month") %>%
  ggplot(aes(x = estimate, y = sector, colour = p_value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25, linewidth = 0.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c("TRUE" = "#2166AC", "FALSE" = "grey65"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
    name   = NULL
  ) +
  labs(title    = "Price Elasticity of Electricity Demand by Sector",
       subtitle = "IV estimates - consumption-based weights, SE clustered by month",
       x = "Price elasticity", y = NULL,
       caption  = "95% confidence intervals. Instrument: sector-weighted wind power forecast.") +
  th_coef

p_main

# ── Robustness plot (all 3 weights x 2 clusters) ───────────────────────────####
p_robustness <- results %>%
  ggplot(aes(x = estimate, y = sector, colour = cluster, shape = cluster)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0, linewidth = 0.4, alpha = 0.5,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.6)) +
  scale_colour_manual(values = c("Month" = "#2166AC", "Week" = "#B2182B"),
                      name = "SE cluster") +
  scale_shape_manual(values = c("Month" = 16, "Week" = 17),
                     name = "SE cluster") +
  facet_wrap(~ weight, ncol = 3) +
  labs(title    = "Robustness: Price Elasticity Across Weighting Methods",
       subtitle = "Each panel shows a different weighting approach; clustering varies within panel",
       x = "Price elasticity", y = NULL,
       caption  = "95% confidence intervals. Sector order fixed by consumption-weight estimate.") +
  th_coef +
  theme(axis.text.y = element_text(size = 6.5),
        strip.text  = element_text(face = "bold", size = 10))

p_robustness
# ── Consumption shares ─────────────────────────────────────────────────────####
sector_shares <- consumption_panel %>%
  group_by(DK36_en) %>%
  summarise(total_MWh = sum(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(share_pct = 100 * total_MWh / sum(total_MWh))

sig_data <- results %>%
  filter(weight == "Consumption", cluster == "Week", p_value < 0.05) %>%
  left_join(sector_shares, by = c("sector" = "DK36_en")) %>%
  arrange(desc(share_pct))

sig_data_p <- sig_data %>% 
  filter(estimate > 0)

sig_data_1 <- sig_data %>% 
  filter( p_value < 0.01)

sig_data_5 <- sig_data %>% 
  filter( p_value < 0.05)
  
sum(sig_data_5$share_pct) - sum(sig_data_1$share_pct)
sum(sig_data$total_MWh)/1000

sum(sig_data$share_pct)
sum(sig_data$share_pct)

cat(sprintf("Significant sectors: %d of %d (%.1f%% of total consumption)\n",
            nrow(sig_data),
            n_distinct(results$sector),
            sum(sig_data$share_pct, na.rm = TRUE)))

coverage <- round(sum(sig_data$share_pct, na.rm = TRUE), 1)

ggplot(sig_data,
       aes(x = reorder(sector, share_pct), y = share_pct, fill = estimate)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f%s", estimate, sig)),
            hjust = -0.1, size = 3, colour = "grey20") +
  scale_fill_gradient2(low = "#B2182B", mid = "#f7f7f7", high = "#2166AC",
                       midpoint = 0, name = "Elasticity") +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.18))) +
  coord_flip() +
  labs(title    = "Consumption Share of Sectors with Significant Price Elasticity",
       subtitle = sprintf(
         "%d sectors | %.1f%% of total consumption covered | p < 0.05, IV consumption weights, SE by week",
         nrow(sig_data), coverage),
       x = NULL, y = "Share of total consumption (%)",
       caption = "Bar colour indicates direction and magnitude of price elasticity.\nEstimate and significance stars shown on bars.") +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 4)),
        plot.subtitle      = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
        plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0, margin = margin(t = 8)),
        axis.text.y        = element_text(size = 9),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position    = "right",
        legend.key.height  = unit(1.2, "cm"))
# ── SUTVA diagnostic: sector shares of total consumption ───────────────────####

sector_shares <- consumption_panel %>%
  group_by(DK36_en) %>%
  summarise(total_MWh = sum(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    share_panel = 100 * total_MWh / sum(total_MWh)
  ) %>%
  arrange(desc(share_panel))

plot_data <- sector_shares %>%
  arrange(share_panel) %>%
  mutate(rank = row_number())

ggplot(sector_shares, aes(x = share_panel)) +
  geom_density(
    fill = pal_gran["DK10"],
    alpha = 0.35,
    colour = pal_gran["DK10"],
    linewidth = 1
  ) +
  geom_vline(
    xintercept = max(sector_shares$share_panel),
    linetype = "dashed",
    linewidth = 1,
    colour = pal_weight["Consumption"]
  ) +
  labs(
    title = "Distribution of sector shares in industrial electricity consumption",
    x = "Sector share of total consumption (%)",
    y = "Density"
  ) +
  th_coef +
  theme(
    legend.position = "none"
  )


# ── AR: combined visualisation ─────────────────────────────────────────────####
ar_all <- bind_rows(
  ar_consumption %>% mutate(weight = "Consumption"),
  ar_employment  %>% mutate(weight = "Employment"),
  ar_firm        %>% mutate(weight = "Firm count")
) %>%
  mutate(weight = factor(weight, levels = c("Consumption", "Employment", "Firm count")))

point_ests <- tibble(
  weight    = factor(c("Consumption", "Employment", "Firm count"),
                     levels = c("Consumption", "Employment", "Firm count")),
  point_est = c(point_est_c, point_est_emp, point_est_firm)
)

p_ar <- ggplot(ar_all, aes(x = beta0, y = p_value, colour = weight)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "grey40") +
  geom_vline(data = point_ests, aes(xintercept = point_est, colour = weight),
             linetype = "dotted", linewidth = 0.5) +
  annotate("text", x = -0.35, y = 0.06, label = "alpha = 0.05",
           hjust = 0, colour = "grey40", size = 3) +
  scale_colour_manual(values = pal_methods, name = "Weight") +
  labs(x     = expression("Candidate elasticity " * beta[0]),
       y     = "AR test p-value",
       title = "Anderson-Rubin Confidence Sets: Pooled Specification",
       subtitle = "Dotted verticals = precision-weighted 2SLS point estimates") +
  th_coef +
  theme(panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4))

p_ar
# ── Sensemakr: RV comparison plot ──────────────────────────────────────────####
plot_rv <- sens_all %>%
  mutate(Industry = str_wrap(Industry, width = 30)) %>%
  ggplot(aes(x = RV_q1, y = reorder(Industry, RV_q1), colour = Weight)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 0.052, y = 1, label = "RV = 5%",
           hjust = 0, size = 3, colour = "grey40") +
  scale_colour_manual(values = pal_methods, name = "Weighting Scheme") +
  labs(x = "Robustness Value (q = 1)", y = NULL,
       title = "Sensitivity to Unobserved Confounding: Reduced-Form") +
  th_coef +
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))

plot_rv

# ── Sensemakr: critical kd plot ────────────────────────────────────────────####
pal_tier <- c("High" = "#2E86AB", "Moderate" = "#F6AE2D", "Fragile" = "#F26157")

plot_kd <- sens_consumption %>%
  mutate(
    kd_numeric = as.numeric(ifelse(str_detect(Critical_kd, ">"),
                                   str_extract(Critical_kd, "[0-9]+"),
                                   Critical_kd)),
    exceeded = str_detect(Critical_kd, ">"),
    Industry = str_wrap(Industry, width = 30)
  ) %>%
  ggplot(aes(x = kd_numeric, y = reorder(Industry, kd_numeric))) +
  geom_col(aes(fill = Robustness_Tier), width = 0.6) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
  annotate("text", x = 1.05, y = 1, label = "1x benchmark",
           hjust = 0, size = 3, colour = "red") +
  scale_fill_manual(values = pal_tier) +
  labs(x = "Critical Benchmark Multiplier (kd) at Which Significance is Lost",
       y = NULL, fill = "Robustness Tier",
       title = "How Many Multiples of Temperature Confounding Nullify Results?") +
  th_coef +
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))

plot_kd

# ── Sensemakr: R2 vs RV scatter ────────────────────────────────────────────####
plot_r2_rv <- sens_consumption %>%
  mutate(Industry = str_wrap(Industry, width = 20)) %>%
  ggplot(aes(x = Partial_R2, y = RV_q1, label = Industry)) +
  geom_point(aes(colour = Robustness_Tier), size = 3) +
  geom_text(size = 2.5, vjust = -0.8, check_overlap = TRUE) +
  scale_colour_manual(values = pal_tier) +
  labs(x     = expression("Partial" ~ R^2 ~ "of Wind with Consumption"),
       y     = "Robustness Value (q = 1)",
       colour = "Tier",
       title  = "Instrument Strength vs. Sensitivity to Confounding") +
  th_coef +
  theme(panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3))

plot_r2_rv
# ── plot_power_suite(): reusable per-weight plot function ──────────────────####
plot_power_suite <- function(power_data, weight_label) {
  
  pd <- power_data %>%
    dplyr::mutate(granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")))
  
  th_pow <- theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  
  # ── A: Power curves by sector (faceted by granularity) ──────────────────────
  p_curves <- pd %>%
    arrange(granularity, sector, true_effect) %>%   # ← sort before plotting
    ggplot(aes(x = true_effect, y = power, group = sector)) +
    geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_line(alpha = 0.4, linewidth = 0.5, colour = "grey50") +
    geom_point(aes(colour = granularity, shape = granularity), size = 1.5, alpha = 0.7) +
    scale_x_continuous(
      labels = scales::label_number(accuracy = 0.01)
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_colour_manual(values = pal_gran, guide = "none") +
    scale_shape_manual(values = shape_gran, guide = "none") +
    facet_wrap(~ granularity, ncol = 3) +
    labs(
      x        = "Imposed effect size",
      y        = "Statistical power",
      title    = paste0("Power curves by sector and aggregation level (", weight_label, " weight)"),
      subtitle = "Each line is one sector; dashed line = 80% threshold"
    ) +
    th_pow +
    theme(strip.text = element_text(face = "bold", size = 10))
  
  # ── B: Median power curve by granularity ────────────────────────────────────
  p_median <- pd %>%
    group_by(granularity, true_effect) %>%
    summarise(
      median_power = median(power, na.rm = TRUE),
      q25          = quantile(power, 0.25, na.rm = TRUE),
      q75          = quantile(power, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = true_effect, y = median_power,
               colour = granularity, fill = granularity)) +
    geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_colour_manual(values = pal_gran) +
    scale_fill_manual(values = pal_gran) +
    labs(
      x        = "Imposed effect size",
      y        = "Statistical power",
      colour   = NULL, fill = NULL,
      title    = paste0("Median power curve by aggregation level (", weight_label, " weight)"),
      subtitle = "Ribbon = interquartile range across sectors"
    ) +
    th_pow
  
  # ── C: Type S scatter ───────────────────────────────────────────────────────
  p_type_s <- pd %>%
    filter(!is.na(wrong_sign)) %>%
    ggplot(aes(x = power, y = wrong_sign,
               colour = granularity, shape = granularity, size = granularity)) +
    geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey40", linewidth = 0.4) +
    geom_point(alpha = 0.85) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_colour_manual(values = pal_gran) +
    scale_shape_manual(values = shape_gran) +
    scale_size_manual(values = size_gran) +
    labs(x = "Statistical power", y = "Type S error rate",
         colour = NULL, shape = NULL, size = NULL,
         title    = paste0("Type S errors (", weight_label, " weight)"),
         subtitle = "Conditional on significance; sectors with zero rejections excluded") +
    th_pow
  
  # ── D: Type M scatter ───────────────────────────────────────────────────────
  # ── D: Type M scatter ───────────────────────────────────────────────────────
  type_m_d <- pd %>%
    mutate(type_m = abs(est_ratio)) %>%
    filter(!is.na(est_ratio))
  n_clip <- sum(type_m_d$type_m > 5, na.rm = TRUE)
  
  p_type_m <- type_m_d %>%
    ggplot(aes(x = power, y = type_m,
               colour = granularity, shape = granularity, size = granularity)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
    geom_line(aes(group = interaction(sector, granularity)),
              linewidth = 0.4, alpha = 0.4) +
    geom_point(alpha = 0.85) +
    coord_cartesian(ylim = c(0, 5)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_y_continuous(breaks = 1:5, labels = paste0(1:5, "\u00d7")) +
    scale_colour_manual(values = pal_gran) +
    scale_shape_manual(values = shape_gran) +
    scale_size_manual(values = size_gran) +
    labs(x = "Statistical power", y = "Exaggeration ratio",
         colour = NULL, shape = NULL, size = NULL,
         title    = paste0("Type M errors (", weight_label, " weight)"),
         subtitle = glue::glue("{n_clip} sector(s) with |ratio| > 5x not displayed")) +
    th_pow
  
  list(
    curves   = p_curves,
    median   = p_median,
    type_s   = p_type_s,
    type_m   = p_type_m
  )
}
# ── Per-weight power plots ─────────────────────────────────────────────────####
plots_c    <- plot_power_suite(power_analysis_c,    "Consumption")
plots_emp  <- plot_power_suite(power_analysis_emp,  "Employment")
plots_firm <- plot_power_suite(power_analysis_firm, "Firm count")

plots_c$curves;    plots_c$median;    plots_c$type_s;    plots_c$type_m
plots_emp$curves;    plots_emp$median;    plots_emp$type_s;    plots_emp$type_m
plots_firm$curves;    plots_firm$median;    plots_firm$type_s;    plots_firm$type_m















# ── Combined power analysis across all weights ─────────────────────────────####
power_all <- bind_rows(
  power_analysis_c    %>% mutate(weight = "Consumption"),
  power_analysis_emp  %>% mutate(weight = "Employment"),
  power_analysis_firm %>% mutate(weight = "Firm count")
) %>%
  mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    weight      = factor(weight, levels = c("Consumption", "Employment", "Firm count"))
  )

# ── A: Power curves by sector (faceted by weight × granularity) ──────────────
p_curves_all <- power_all %>%
  arrange(weight, granularity, sector, true_effect) %>%
  ggplot(aes(x = true_effect, y = power, group = interaction(sector, weight))) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_line(alpha = 0.3, linewidth = 0.4, colour = "grey50") +
  geom_point(aes(colour = granularity, shape = granularity), size = 1.2, alpha = 0.6) +
  scale_x_continuous(
    breaks = seq(-0.07, -0.01, by = 0.03),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_colour_manual(values = pal_gran, guide = "none") +
  scale_shape_manual(values = shape_gran, guide = "none") +
  facet_grid(weight ~ granularity) +
  labs(
    x = "Imposed effect size",
    y = "Statistical power",
    title = "Power curves by sector, aggregation level, and weighting method",
    subtitle = "Each line is one sector; dashed line = 80% threshold"
  ) +
  th_pow +
  theme(panel.spacing = unit(1, "cm"))

# ── B: Median power curve (all weights overlaid, faceted by granularity) ─────
p_median_all <- power_all %>%
  group_by(weight, granularity, true_effect) %>%
  summarise(
    median_power = median(power, na.rm = TRUE),
    q25          = quantile(power, 0.25, na.rm = TRUE),
    q75          = quantile(power, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = true_effect, y = median_power, colour = weight, fill = weight)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(-0.07, -0.01, by = 0.03),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_colour_manual(values = pal_weight) +
  scale_fill_manual(values = pal_weight) +
  facet_wrap(~ granularity, ncol = 3) +
  labs(
    x = "Imposed effect size",
    y = "Statistical power",
    colour = NULL,
    fill = NULL,
    title = "Median power curve by aggregation level and weighting method",
    subtitle = "Ribbon = interquartile range across sectors"
  ) +
  th_pow +
  theme(panel.spacing.x = unit(1.5, "cm"))

# ── C: Type S (binned average by power level) ────────────────────────────────
power_all_binned <- power_all %>%
  mutate(
    power_bin = cut(
      power,
      breaks = seq(0, 1, by = 0.2),
      include.lowest = TRUE,
      labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%")
    ),
    power_bin = as.character(power_bin),
    power_mid = case_when(
      power_bin == "0–20%"   ~ 0.1,
      power_bin == "20–40%"  ~ 0.3,
      power_bin == "40–60%"  ~ 0.5,
      power_bin == "60–80%"  ~ 0.7,
      power_bin == "80–100%" ~ 0.9,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(granularity, weight, power_bin, power_mid) %>%
  summarise(
    mean_type_s = mean(wrong_sign, na.rm = TRUE),
    .groups = "drop"
  )

p_type_s_clean <- ggplot(
  power_all_binned,
  aes(x = power_mid, y = mean_type_s, colour = weight, group = weight)
) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ granularity, ncol = 3) +
  scale_x_continuous(
    breaks = c(0.1, 0.3, 0.5, 0.7, 0.9),
    labels = c("0–20", "20–40", "40–60", "60–80", "80–100"),
    limits = c(0.05, 0.95)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_colour_manual(values = pal_weight) +
  labs(
    x = "Statistical power (%)",
    y = "Average Type S error rate",
    colour = NULL,
    title = "Type S error rates by power level",
    subtitle = "Averaged across sectors within power bins"
  ) +
  th_pow +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ── D1: Raw Type M plot (legacy / appendix style) ────────────────────────────
type_m_all <- power_all %>%
  mutate(type_m = abs(est_ratio)) %>%
  filter(!is.na(est_ratio))

n_clip_all <- sum(type_m_all$type_m > 5, na.rm = TRUE)

p_type_m_all <- type_m_all %>%
  ggplot(aes(x = power, y = type_m, colour = weight, shape = weight)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_line(
    aes(group = interaction(sector, granularity, weight)),
    linewidth = 0.3,
    alpha = 0.3
  ) +
  geom_point(size = 2, alpha = 0.7) +
  coord_cartesian(ylim = c(0, 5)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_y_continuous(
    breaks = 1:5,
    labels = paste0(1:5, "\u00d7")
  ) +
  scale_colour_manual(values = pal_weight) +
  scale_shape_manual(values = shape_weight) +
  facet_wrap(~ granularity, ncol = 3) +
  labs(
    x = "Statistical power",
    y = "Exaggeration ratio",
    colour = NULL,
    shape = NULL,
    title = "Type M errors by aggregation level and weighting method",
    subtitle = glue::glue("{n_clip_all} sector(s) with |ratio| > 5x not displayed")
  ) +
  th_pow

# ── D2: Clean Type M plot (all observations + binned medians) ────────────────
type_m_all_plot <- power_all %>%
  mutate(
    type_m = abs(est_ratio),
    power_bin = cut(
      power,
      breaks = seq(0, 1, by = 0.2),
      include.lowest = TRUE,
      labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%")
    ),
    power_bin = as.character(power_bin),
    power_mid = case_when(
      power_bin == "0–20%"   ~ 0.1,
      power_bin == "20–40%"  ~ 0.3,
      power_bin == "40–60%"  ~ 0.5,
      power_bin == "60–80%"  ~ 0.7,
      power_bin == "80–100%" ~ 0.9,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(type_m), !is.na(power), !is.na(power_mid))

type_m_binned <- type_m_all_plot %>%
  group_by(granularity, weight, power_bin, power_mid) %>%
  summarise(
    med_type_m = median(type_m, na.rm = TRUE),
    q25        = quantile(type_m, 0.25, na.rm = TRUE),
    q75        = quantile(type_m, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_type_m_clean <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_point(
    data = type_m_all_plot,
    aes(x = power, y = type_m, colour = weight, shape = weight),
    alpha = 0.25,
    size = 1.8,
    position = position_jitter(width = 0.01, height = 0)
  ) +
  geom_ribbon(
    data = type_m_binned,
    aes(x = power_mid, ymin = q25, ymax = q75, fill = weight, group = weight),
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(
    data = type_m_binned,
    aes(x = power_mid, y = med_type_m, colour = weight, group = weight),
    linewidth = 1.2,
    alpha = 0.95
  ) +
  geom_point(
    data = type_m_binned,
    aes(x = power_mid, y = med_type_m, colour = weight),
    size = 2.5
  ) +
  facet_wrap(~ granularity, ncol = 3) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_y_continuous(
    breaks = 1:5,
    labels = paste0(1:5, "\u00d7")
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 5)) +
  scale_colour_manual(values = pal_weight) +
  scale_fill_manual(values = pal_weight, guide = "none") +
  scale_shape_manual(values = shape_weight) +
  labs(
    x = "Statistical power",
    y = "Exaggeration ratio",
    colour = NULL,
    shape = NULL,
    title = "Type M errors by power level",
    subtitle = "Points show all sector-level observations; lines show median values within power bins; shaded areas show the interquartile range"
  ) +
  th_pow +
  theme(panel.spacing = unit(1.5, "lines"))

# ── Print plots ───────────────────────────────────────────────────────────────
p_curves_all
p_median_all
p_type_s_clean
p_type_m_clean







