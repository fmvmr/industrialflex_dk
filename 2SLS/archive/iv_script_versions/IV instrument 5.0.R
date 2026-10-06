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
# ── Geometric mean weights (emp × firm) ────────────────────────────────────####
geomean_weights <- total_employees %>%
  select(year, DK36_group, w_DK1_emp) %>%
  left_join(firms_long, by = c("year", "DK36_group")) %>%
  mutate(w_DK1_geomean = sqrt(w_DK1_emp * w_DK1_firm)) %>%
  select(year, DK36_group, w_DK1_geomean)
# ── Attach all weights to industry ─────────────────────────────────────────####
industry <- industry %>%
  mutate(Year = as.integer(format(TimeUTC, "%Y"))) %>%
  left_join(select(total_employees, year, DK36_group, w_DK1_emp),
            by = c("Year" = "year", "DK36Code" = "DK36_group")) %>%
  left_join(firms_long,
            by = c("Year" = "year", "DK36Code" = "DK36_group")) %>%
  left_join(geomean_weights,
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
  distinct(TimeUTC, DK10Title, DK19Title, DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm,w_DK1_geomean) %>%
  left_join(prices_wide, by = c("TimeUTC" = "HourUTC")) %>%
  mutate(
    P_weighted_c    = w_DK1_c    * DK1 + (1 - w_DK1_c)    * DK2,
    P_weighted_emp  = w_DK1_emp  * DK1 + (1 - w_DK1_emp)  * DK2,
    P_weighted_firm = w_DK1_firm * DK1 + (1 - w_DK1_firm) * DK2,
    P_weighted_firmxemp =w_DK1_geomean * DK1 + (1-w_DK1_geomean)*DK2
  ) %>%
  rename(DK1_P = DK1, DK2_P = DK2)

weighted_prices_low <- weighted_prices %>% 
  filter(DK1_P < 0 & DK2_P < 0 )


# ── Weight similarity diagnostics ──────not important───────────────────────####
weighted_prices %>%
  distinct(DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  mutate(diff_emp_c  = abs(w_DK1_emp  - w_DK1_c),
         diff_firm_c = abs(w_DK1_firm - w_DK1_c)) %>%
  summarise(mean_diff_emp_c    = mean(diff_emp_c,  na.rm = TRUE),
            median_diff_emp_c  = median(diff_emp_c, na.rm = TRUE),
            max_diff_emp_c     = max(diff_emp_c,   na.rm = TRUE),
            mean_diff_firm_c   = mean(diff_firm_c, na.rm = TRUE),
            median_diff_firm_c = median(diff_firm_c, na.rm = TRUE),
            max_diff_firm_c    = max(diff_firm_c,  na.rm = TRUE))

weighted_prices %>%
  distinct(DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  mutate(diff_emp_c = abs(w_DK1_emp - w_DK1_c)) %>%
  arrange(desc(diff_emp_c)) %>%
  head(20)

# ── Correlation matrix for weighted price series ───────────────────────────####
price_correlation_matrix <- weighted_prices %>%
  select(P_weighted_c, P_weighted_emp, P_weighted_firm, P_weighted_firmxemp) %>%
  cor(use = "complete.obs")

print(round(price_correlation_matrix, 6))

# ── Price spread diagnostic ────────────────────────────────────────────────####
weighted_prices %>%
  filter(!is.na(DK1_P), !is.na(DK2_P)) %>%
  distinct(TimeUTC, DK1_P, DK2_P) %>%
  mutate(spread = DK1_P - DK2_P) %>%
  summarise(mean_spread      = mean(spread,          na.rm = TRUE),
            sd_spread        = sd(spread,            na.rm = TRUE),
            share_identical  = mean(spread == 0,     na.rm = TRUE),
            share_within_1eu = mean(abs(spread) < 1, na.rm = TRUE),
            share_within_5eu = mean(abs(spread) < 5, na.rm = TRUE),
            max_spread       = max(abs(spread),      na.rm = TRUE))

weighted_prices %>%
  filter(!is.na(P_weighted_c), !is.na(P_weighted_emp), !is.na(P_weighted_firm)) %>%
  summarise(cor_c_emp  = cor(P_weighted_c, P_weighted_emp,  use = "complete.obs"),
            cor_c_firm = cor(P_weighted_c, P_weighted_firm, use = "complete.obs"))

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

industry %>%
  summarise(n = n(), n_na = sum(is.na(Wind_DK1)), share_na = mean(is.na(Wind_DK1)))

# ── Fuel controls (pre-joined) ─────────────────────────────────────────────####
fuel <- gas %>%
  mutate(Date = as.Date(Date)) %>% select(Date, Gas_EUR_MWh) %>%
  left_join(carbon %>% mutate(Date = as.Date(Date)) %>% select(Date, EUA_EUR_ton),  by = "Date") %>%
  left_join(coal   %>% mutate(Date = as.Date(Date)) %>% select(Date, Coal_USD_ton), by = "Date")

# ── Main consumption panel ─────────────────────────────────────────────────####
consumption_panel <- industry %>%
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
# ── Panel dimensions ───────────────────────────────────────────────────────####
list(
  N_sectors  = n_distinct(consumption_panel$DK36Title),
  T_hours    = n_distinct(consumption_panel$TimeUTC),
  N_obs      = nrow(consumption_panel),
  start      = format(min(consumption_panel$TimeUTC), "%B %Y"),
  end        = format(max(consumption_panel$TimeUTC), "%B %Y"),
  balanced   = n_distinct(consumption_panel$DK36Title) *
    n_distinct(consumption_panel$TimeUTC) == nrow(consumption_panel)
)
# Check which hours have missing wind data
consumption_panel %>%
  filter(is.na(Wind_DK1) | is.na(Wind_DK2)) %>%
  summarise(
    n_missing = n(),
    first_missing = min(TimeUTC),
    last_missing  = max(TimeUTC)
  )


missing_times <- consumption_panel %>%
  filter(is.na(Wind_DK1) | is.na(Wind_DK2)) %>%
  distinct(TimeUTC, fe_hour)

missing_by_month <- missing_times %>%
  mutate(month = as.Date(format(TimeUTC, "%Y-%m-01"))) %>%
  count(month) %>%
  arrange(month)

missing_by_hour <- missing_times %>%
  count(fe_hour) %>%
  arrange(fe_hour)

missing_by_day <- missing_times %>%
  mutate(date = as.Date(TimeUTC)) %>%
  count(date) %>%
  arrange(date)


# ── Summary statistics ─────────────────────────────────────────────────────####

names(consumption_panel)

consumption_panel %>%
  select(
    # Outcome
    Consumption_MWh,
    # Prices — three weighting schemes
    P_weighted_c, P_weighted_emp, P_weighted_firm,
    # Raw spot prices
    DK1_P, DK2_P,
    # Instruments — wind forecasts
    Wind_c, Wind_emp, Wind_firm,
    Wind_DK1, Wind_DK2,
    # Temperature controls
    Temp_c, Temp_emp, Temp_firm,
    Temp_DK1, Temp_DK2,
    # Fuel prices
    gas, carbon, coal
  ) %>%
  as.data.frame() %>%
  stargazer(
    type  = "text",
    title = "Descriptive Statistics - Consumption Panel",
    covariate.labels = c(
      # Outcome
      "Consumption (MWh)",
      # Prices
      "Electricity price, cons. weight (EUR/MWh)",
      "Electricity price, emp. weight (EUR/MWh)",
      "Electricity price, firm weight (EUR/MWh)",
      "Spot price DK1 (EUR/MWh)",
      "Spot price DK2 (EUR/MWh)",
      # Instruments
      "Wind forecast, cons. weight (MWh)",
      "Wind forecast, emp. weight (MWh)",
      "Wind forecast, firm weight (MWh)",
      "Wind forecast DK1 (MWh)",
      "Wind forecast DK2 (MWh)",
      # Temperature
      "Temperature, cons. weight (°C)",
      "Temperature, emp. weight (°C)",
      "Temperature, firm weight (°C)",
      "Temperature DK1 (°C)",
      "Temperature DK2 (°C)",
      # Fuel prices
      "Gas price (EUR/MWh)",
      "Carbon price (EUR/ton)",
      "Coal price (USD/ton)"
    ),
    summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
    digits       = 2
  )

# ── Sector breakdown ───────────────────────────────────────────────────────####
consumption_panel %>%
  group_by(DK36_en) %>%
  summarise(N_obs = n(), Total_MWh = sum(Consumption_MWh, na.rm = TRUE),
            Mean_MWh = mean(Consumption_MWh, na.rm = TRUE),
            SD_MWh   = sd(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(Share_pct = round(100 * Total_MWh / sum(Total_MWh), 2)) %>%
  arrange(desc(Total_MWh)) %>%
  print(n = Inf)
# ── Panel balance & missing hours diagnostics ──────────────────────────────####

# Full expected hourly grid
full_grid <- expand.grid(
  TimeUTC  = seq(min(consumption_panel$TimeUTC),
                 max(consumption_panel$TimeUTC),
                 by = "hour"),
  DK36Title = unique(consumption_panel$DK36Title)
)

panel_balance <- consumption_panel %>%
  group_by(DK36Title, DK36_en) %>%
  summarise(
    N_obs        = n(),
    N_hours      = n_distinct(TimeUTC),
    Mean_MWh     = mean(Consumption_MWh, na.rm = TRUE),
    First_obs    = min(TimeUTC),
    Last_obs     = max(TimeUTC),
    .groups = "drop"
  ) %>%
  mutate(
    Expected_hours = as.integer(difftime(Last_obs, First_obs, units = "hours")) + 1L,
    Missing_hours  = Expected_hours - N_hours,
    Missing_pct    = round(100 * Missing_hours / Expected_hours, 2)
  ) %>%
  arrange(desc(Missing_pct))

cat("=== Panel Dimensions ===\n")
cat("Period:          ", format(min(consumption_panel$TimeUTC), "%B %Y"),
    "–", format(max(consumption_panel$TimeUTC), "%B %Y"), "\n")
cat("Sectors (DK36):  ", n_distinct(consumption_panel$DK36Title), "\n")
cat("Total obs:       ", format(nrow(consumption_panel), big.mark = ","), "\n")
cat("Unique hours:    ", format(n_distinct(consumption_panel$TimeUTC), big.mark = ","), "\n")
cat("Balanced panel:  ", n_distinct(consumption_panel$DK36Title) *
      n_distinct(consumption_panel$TimeUTC) == nrow(consumption_panel), "\n\n")

cat("=== Missing Hours by Sector ===\n")
panel_balance %>%
  select(DK36_en, N_obs, N_hours, Expected_hours, Missing_hours, Missing_pct, Mean_MWh) %>%
  print(n = Inf)

cat("\n=== Aggregate Missing Summary ===\n")
panel_balance %>%
  summarise(
    Total_expected = sum(Expected_hours),
    Total_observed = sum(N_hours),
    Total_missing  = sum(Missing_hours),
    Missing_pct    = round(100 * sum(Missing_hours) / sum(Expected_hours), 2),
    Sectors_any_missing    = sum(Missing_hours > 0),
    Sectors_gt5pct_missing = sum(Missing_pct > 5)
  ) %>%
  print()
# ============================================================================
# = PART TWO: IV ESTIMATION robu  =================================================
# ============================================================================
# ── OLS baselines ──────────────────────────────────────────────────────────####

OLS_lin_c_w <- feols(
  Consumption_MWh ~ P_weighted_c + Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

OLS_lin_emp_w <- feols(
  Consumption_MWh ~ P_weighted_emp + Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

OLS_lin_firm_w <- feols(
  Consumption_MWh ~ P_weighted_firm + Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

OLS_log_c_w <- feols(
  log_consumption ~ P_weighted_c + Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

OLS_log_emp_w <- feols(
  log_consumption ~ P_weighted_emp + Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

OLS_log_firm_w <- feols(
  log_consumption ~ P_weighted_firm + Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

# ── IV specifications ──────────────────────────────────────────────────────####

IV_lin_DK10 <- feols(
  Consumption_MWh ~ Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

IV_lin_DK10_W <- feols(
  Consumption_MWh ~ Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_lin_Emp <- feols(
  Consumption_MWh ~ Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

IV_lin_Emp_w <- feols(
  Consumption_MWh ~ Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_lin_firm <- feols(
  Consumption_MWh ~ Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

IV_lin_firm_w <- feols(
  Consumption_MWh ~ Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_log_DK10 <- feols(
  log_consumption ~ Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

IV_log_DK10_W <- feols(
  log_consumption ~ Temp_c + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_log_Emp <- feols(
  log_consumption ~ Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

IV_log_Emp_w <- feols(
  log_consumption ~ Temp_emp + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK36_en
)

IV_log_firm <- feols(
  log_consumption ~ Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

IV_log_firm_w <- feols(
  log_consumption ~ Temp_firm + gas + coal + carbon |
    fe_hour + fe_month + fe_dow |
    P_weighted_firm ~ Wind_firm,
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

# ── Estimate tables ────────────────────────────────────────────────────────####
est_wide <- results %>%
  mutate(
    cell = sprintf("%.4f%s (%.4f)", estimate, sig, se),
    col  = paste0(spec, " / ", weight, " / ", cluster = "week")
  ) %>%
  select(sector, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell)

print(knitr::kable(est_wide, format = "simple",
                   col.names = c("Sector",
                                 # Linear
                                  "Lin/Cons/W",
                                  "Lin/Emp/W",
                                  "Lin/Firm/W",
                                 # Log-linear
                                 "Log/Cons/W",
                                 "Log/Emp/W",
                                 "Log/Firm/W")))

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

first_stage_results %>%
  filter(cluster == "Week") %>%
  mutate(weight = factor(weight, levels = c("Consumption", "Employment", "Firm count"))) %>%
  ggplot(aes(x = fs_f_robust, y = weight, fill = weight, color = weight)) +
  geom_boxplot(alpha = 0.15, width = 0.4) +
  geom_vline(xintercept = 23.11, linetype = "dashed", color = "#A32D2D", linewidth = 0.7) +
  annotate("text", x = 23.11, y = 0.55, label = "F = 23.11", color = "#A32D2D",
           hjust = -0.1, vjust = 0, size = 3, fontface = "italic") +
  scale_fill_manual(values = c(
    "Consumption" = "#B5D4F4",
    "Employment"  = "#F4C0D1",
    "Firm count"  = "#C0DD97"
  )) +
  scale_color_manual(values = c(
    "Consumption" = "#185FA5",
    "Employment"  = "#993556",
    "Firm count"  = "#3B6D11"
  )) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title    = "First-stage F-statistics by weighting method",
    subtitle = "Distribution across all sectors, week-clustered standard errors",
    x        = "Cluster-robust partial F-statistic (t²)",
    y        = NULL,
    caption  = "Dashed line = Olea-Pflueger (2013) weak instrument threshold (F = 23.11).\nRobust partial F computed as the square of the week-clustered first-stage t-statistic."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(color = "grey50", size = 10, margin = margin(b = 8)),
    plot.caption     = element_text(color = "grey60", size = 8, margin = margin(t = 8)),
    axis.text.y      = element_text(size = 11),
    axis.text.x      = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

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
  left_join(fstat_c  |> rename(F_cons = fstat), by = "DK36_en") |>
  left_join(fstat_emp  |> rename(F_emp  = fstat), by = "DK36_en") |>
  left_join(fstat_firm |> rename(F_firm = fstat), by = "DK36_en") |>
  mutate(across(starts_with("F_"), ~ formatC(.x, digits = 1, format = "f"))) |>
  arrange(DK36_en)

# ──table ────────────────────────────────────────────────────────
results_table |>
  kbl(
    format    = "latex",
    col.names = c(
      "Sector",
      "Consumption", "Employment", "Firm count",
      "Consumption",
      "F (cons.)", "F (emp.)", "F (firm)"
    ),
    caption  = "Table X. Sector-level Price Elasticities of Electricity Demand",
    booktabs = TRUE,
    align    = c("l", rep("c", 7))
  ) |>
  add_header_above(c(
    " "              = 1,
    "2SLS Estimates" = 3,
    "OLS"            = 1,
    "First-stage F"  = 3
  )) |>
  footnote(
    general = paste(
      "Clustered standard errors (week level) in parentheses.",
      "*** p<0.01, ** p<0.05, * p<0.10.",
      "All specifications include hour-of-day, day-of-week, and year-month fixed effects.",
      "Sample: June 2021 – September 2025."
    ),
    threeparttable = TRUE
  ) |>
  cat()

