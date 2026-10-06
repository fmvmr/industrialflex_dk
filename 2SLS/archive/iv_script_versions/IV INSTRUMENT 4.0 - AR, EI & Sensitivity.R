# IV INSTRUMENT 4.0 VERSION:
# Combined script merging v3.0 (data + IV + power) and v2.1 (Anderson-Rubin + Sensemakr)
setwd("~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")
setwd("C:/Users/JesBrobyTinghuusPete/OneDrive - GetWhy/Dokumenter/GMA/Thesis/Data/Data")
setwd("C:/Users/jespe/OneDrive - CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")
fixest::setFixest_notes(FALSE)
# =============================================================================
# = PART ONE: DATA PREPARATION  ==============================================
# =============================================================================
# ── Packages ───────────────────────────────────────────────────────────────####

install.packages("arrow")
install.packages("dplyr")
install.packages("lubridate")
install.packages("stringr")
install.packages("tidyverse")
install.packages("glue")
install.packages("readxl")
install.packages("tidyr")
install.packages("fixest")
install.packages("readr")
install.packages("ggplot2")
install.packages("rlang")
install.packages("tibble")
install.packages("patchwork")
install.packages("progress")
install.packages("plm")
install.packages("AER")
install.packages("ivreg")
install.packages("lmtest")
install.packages("sandwich")
install.packages("stargazer")
install.packages("scales")
install.packages("sensemakr")

library(arrow)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)
library(glue)
library(readxl)
library(tidyr)
library(fixest)
library(readr)
library(ggplot2)
library(rlang)
library(tibble)
library(patchwork)
library(progress)
library(plm)
library(AER)
library(ivreg)
library(lmtest)
library(sandwich)
library(stargazer)
library(scales)
library(sensemakr)

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
      gas    = arrow::read_parquet("data/controls/gas_daily_2020_2025.parquet"),
      carbon = arrow::read_parquet("data/controls/carbon_daily_2020_2025.parquet"),
      coal   = arrow::read_parquet("data/controls/coal_daily_2020_2025.parquet")
    )
  )
}

# ── Pull into environment ──────────────────────────────────────────────────####
d <- load_data(2021:2025)
list2env(d[c("prices","forecast","temp","industry","industry_annual",
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
  "Kultur og fritid",                                       "Offentlig administration, undervisning og sundhed",
  "Andre serviceydelser mv",                                "Offentlig administration, undervisning og sundhed",
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
    Gas_EUR_MWh, EUA_EUR_ton, Coal_USD_ton
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
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36_en
)

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
-0.005059*29
summary(coef(IV_model_het_DK10_W))
# ── Build master table ────────────────────────────────────────────────────────
results_table <- fmt(iv_c,   "IV_cons") |>
  left_join(fmt(iv_emp,  "IV_emp"),  by = "DK36_en") |>
  left_join(fmt(iv_firm, "IV_firm"), by = "DK36_en") |>
  left_join(fmt(ols_c,   "OLS"),     by = "DK36_en") |>
  mutate(across(starts_with("F_"), ~ formatC(.x, digits = 1, format = "f"))) |>
  arrange(DK36_en)

results_table


# ============================================================================
# = PART THREE: ANDERSON-RUBIN & SENSITIVITY  ================================
# ============================================================================
# ── 1.0 AR function — matches v4.0 feols: fe_hour + fe_month + fe_dow ─────####

compute_AR_cs <- function(data, log_price_var, wind_var, temp_var,
                          beta_grid = seq(-0.5, 0.1, by = 0.001),
                          alpha = 0.05) {
  
  # Pre-compute dummy matrices once (faster than factor() inside loop)
  hour_dummies  <- model.matrix(~ factor(fe_hour)  - 1, data = data)
  month_dummies <- model.matrix(~ factor(fe_month) - 1, data = data)
  dow_dummies   <- model.matrix(~ factor(fe_dow)   - 1, data = data)
  
  Y      <- data$log_consumption
  D      <- data[[log_price_var]]
  Z      <- data[[wind_var]]
  Temp   <- data[[temp_var]]
  Gas    <- data$log_gas
  Coal   <- data$log_coal
  Carbon <- data$log_carbon
  
  ar_results <- purrr::map(beta_grid, function(b0) {
    Y_adj <- Y - b0 * D
    
    fit <- lm(Y_adj ~ Z + Temp + Gas + Coal + Carbon +
                hour_dummies + month_dummies + dow_dummies)
    
    cf <- summary(fit)$coefficients
    if (!"Z" %in% rownames(cf)) return(NULL)
    
    tibble(
      beta0   = b0,
      t_stat  = cf["Z", "t value"],
      p_value = cf["Z", "Pr(>|t|)"],
      in_CS   = cf["Z", "Pr(>|t|)"] >= alpha
    )
  }) %>% dplyr::bind_rows()
}

# Helper to extract AR bounds from grid search results
extract_ar_bounds <- function(ar_results, label) {
  cs <- ar_results %>% filter(in_CS == TRUE)
  if (nrow(cs) == 0) {
    tibble(Weight = label, AR_Lower = NA_real_, AR_Upper = NA_real_,
           AR_Width = NA_real_, AR_Empty = TRUE)
  } else {
    tibble(Weight = label, AR_Lower = min(cs$beta0), AR_Upper = max(cs$beta0),
           AR_Width = max(cs$beta0) - min(cs$beta0), AR_Empty = FALSE)
  }
}


# ── 1.1 POOLED Anderson-Rubin ────────────────────────────────────────────####

# Pooled ivreg — must include fe_dow to match feols
iv_pooled_c <- ivreg(
  log_consumption ~ log_P_c + Temp_c + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) + factor(fe_dow) |
    Wind_c + Temp_c + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) + factor(fe_dow),
  data = consumption_panel
)

iv_pooled_emp <- ivreg(
  log_consumption ~ log_P_emp + Temp_emp + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) + factor(fe_dow) |
    Wind_emp + Temp_emp + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) + factor(fe_dow),
  data = consumption_panel
)

iv_pooled_firm <- ivreg(
  log_consumption ~ log_P_firm + Temp_firm + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) + factor(fe_dow) |
    Wind_firm + Temp_firm + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) + factor(fe_dow),
  data = consumption_panel
)

cat("Pooled ivreg point estimates:\n")
cat("  Consumption:", round(coef(iv_pooled_c)["log_P_c"], 4), "\n")
cat("  Employment: ", round(coef(iv_pooled_emp)["log_P_emp"], 4), "\n")
cat("  Firm count: ", round(coef(iv_pooled_firm)["log_P_firm"], 4), "\n")

# Pooled AR grid search
ar_pooled_c <- compute_AR_cs(
  consumption_panel, "log_P_c", "Wind_c", "Temp_c",
  beta_grid = seq(-0.35, 0.05, by = 0.001)
)

cat("Running pooled AR grid search (employment)...\n")
ar_pooled_emp <- compute_AR_cs(
  consumption_panel, "log_P_emp", "Wind_emp", "Temp_emp",
  beta_grid = seq(-0.35, 0.05, by = 0.001)
)

cat("Running pooled AR grid search (firm count)...\n")
ar_pooled_firm <- compute_AR_cs(
  consumption_panel, "log_P_firm", "Wind_firm", "Temp_firm",
  beta_grid = seq(-0.35, 0.05, by = 0.001)
)

ar_pooled_summary <- bind_rows(
  extract_ar_bounds(ar_pooled_c,    "Consumption"),
  extract_ar_bounds(ar_pooled_emp,  "Employment"),
  extract_ar_bounds(ar_pooled_firm, "Firm count")
) %>%
  mutate(
    Point_Est = c(
      coef(iv_pooled_c)["log_P_c"],
      coef(iv_pooled_emp)["log_P_emp"],
      coef(iv_pooled_firm)["log_P_firm"]
    ),
    Inside_CS = Point_Est >= AR_Lower & Point_Est <= AR_Upper
  )

print(ar_pooled_summary)


# ── 1.2 INDUSTRY-SPLIT Anderson-Rubin ───────────────────────────────────####
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
results <- results %>%
  filter(cluster == "week") %>%
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
print(ar_with_estimates, n = 87)

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
# Save as .rds (preserves all R object structure)
saveRDS(ar_with_estimates, "ar_with_estimates.rds")

# Reload later
ar_with_estimates <- readRDS("ar_with_estimates.rds")
# ============================================================================
# AR FUNCTION — CLUSTER-ROBUST (matches v4.0 feols specification)
# Uses feols with clustering instead of lm with OLS SEs
# ============================================================================

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

# Merge with feols point estimates from results (already cleaned in v4.0)
results_clean <- results %>%
  filter(cluster == "Month") %>%
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
print(ar_with_estimates, n = 87)

# ── 1.3 AR Reporting ────────────────────────────────────────────────────####
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

cat("  All sets bounded:", all(!ar_pooled_summary$AR_Empty), "\n")
cat("  All point estimates inside CS:", all(ar_pooled_summary$Inside_CS), "\n")

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


# ===========================================================================
# = PART THREE: SENSITIVITY  ================================================
# ===========================================================================
# ── Placebo Test ─────────────────────────────────────────────────────────── ####
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

cat("\n============================================================\n")
cat("PLACEBO TEST RESULTS\n")
cat("============================================================\n")
cat("24h lead - significant at 5%:", sum(plac_24$placebo_sig), "/", nrow(plac_24), "\n")
cat("48h lead - significant at 5%:", sum(plac_48$placebo_sig), "/", nrow(plac_48), "\n")
print(plac_all, n = 70)

# Compare magnitudes: actual RF vs placebo
if (exists("rf_c_res")) {
  comparison <- rf_c_res %>%
    select(industry, actual_rf = rf_coef) %>%
    left_join(
      plac_24 %>% select(industry, placebo_24h = placebo_coef),
      by = "industry"
    ) %>%
    left_join(
      plac_48 %>% select(industry, placebo_48h = placebo_coef),
      by = "industry"
    ) %>%
    mutate(
      ratio_24 = abs(placebo_24h / actual_rf),
      ratio_48 = abs(placebo_48h / actual_rf)
    )
  
  print(comparison, n = 35)
  
  cat("\nMean |placebo/actual| ratio:\n")
  cat("  24h:", round(mean(comparison$ratio_24, na.rm = TRUE), 4), "\n")
  cat("  48h:", round(mean(comparison$ratio_48, na.rm = TRUE), 4), "\n")
}


# ── Sensemakr: extract function ────────────────────────────────────────────####

# NOTE: sensemakr operates on lm objects. The reduced-form lm should include
# the same controls as the feols specification. However, sensemakr does not
# natively support high-dimensional FE via demeaning. Including fe_hour and
# fe_month as factor() dummies in lm is computationally expensive on the
# full panel. We therefore include fe_dow (7 levels) explicitly, and note
# that fe_hour (24 levels) and fe_month (~48 levels) are absorbed via the
# partial R2 computation within sensemakr's framework. For computational
# tractability on single-industry subsamples, we include all three FE sets.


# This will show the actual error instead of catching it silently
extract_sens_stats <- function(data, industry_col, industry_val,
                               wind_var, temp_var, weight_label,
                               kd_max = 5, alpha = 0.05) {
  
  sub <- data %>% filter(.data[[industry_col]] == industry_val)
  
  # sensemakr lm: controls + fe_dow only (hour/month FE excluded for numerical stability)
  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon + factor(fe_dow)"
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

# Run across all industries and weighting schemes
industries_sens <- unique(consumption_panel$DK36_en)

cat("Running sensitivity analysis (consumption weights)...\n")
sens_consumption <- map_dfr(industries_sens, function(ind) {
  extract_sens_stats(consumption_panel, "DK36_en", ind,
                     "Wind_c", "Temp_c", "Consumption", kd_max = 5)
})

cat("Running sensitivity analysis (employment weights)...\n")
sens_employment <- map_dfr(industries_sens, function(ind) {
  extract_sens_stats(consumption_panel, "DK36_en", ind,
                     "Wind_emp", "Temp_emp", "Employment", kd_max = 5)
})

cat("Running sensitivity analysis (firm count weights)...\n")
sens_firm <- map_dfr(industries_sens, function(ind) {
  extract_sens_stats(consumption_panel, "DK36_en", ind,
                     "Wind_firm", "Temp_firm", "Firm count", kd_max = 5)
})

# Robustness tiers
assign_tiers <- function(df) {
  df %>% mutate(
    Robustness_Tier = case_when(
      RV_q1_alpha >= 0.10 ~ "High",
      RV_q1_alpha >= 0.03 ~ "Moderate",
      TRUE                ~ "Fragile"
    )
  )
}

sens_consumption <- assign_tiers(sens_consumption) %>% arrange(desc(RV_q1))
sens_employment  <- assign_tiers(sens_employment)
sens_firm        <- assign_tiers(sens_firm)

sens_all <- bind_rows(sens_consumption, sens_employment, sens_firm) %>%
  arrange(Industry, Weight)

cat("\n--- Sensitivity Results (All Weights) ---\n")
print(sens_all, n = 100)

# Cross-weight consistency
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

cat("\n--- Cross-Weight Consistency ---\n")
print(consistency_check, n = 35)

# Compare: sensemakr t-stat (OLS SE) vs feols t-stat (cluster-robust SE)
# Pick a well-identified industry
test_ind <- "Real estate"

# sensemakr t-stat (from your output)
sens_t <- sens_consumption %>% 
  filter(Industry == test_ind) %>% 
  pull(t_original)

# feols reduced-form t-stat with cluster-robust SE
rf_test <- feols(
  log_consumption ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel %>% filter(DK36_en == test_ind),
  cluster = ~ fe_month
)

feols_t <- coeftable(rf_test)["Wind_c", "t value"]

cat("sensemakr t-stat (OLS SE):", sens_t, "\n")
cat("feols t-stat (cluster SE):", feols_t, "\n")
cat("Inflation ratio:", abs(sens_t / feols_t), "\n")


# Summary
cat("\n--- Sensitivity Summary (Consumption Weights) ---\n")
cat("Industries analysed:", nrow(sens_consumption), "\n")
cat("  High (RV_alpha >= 0.10):",
    sum(sens_consumption$Robustness_Tier == "High", na.rm = TRUE), "\n")
cat("  Moderate (0.03 <= RV_alpha < 0.10):",
    sum(sens_consumption$Robustness_Tier == "Moderate", na.rm = TRUE), "\n")
cat("  Fragile (RV_alpha < 0.03):",
    sum(sens_consumption$Robustness_Tier == "Fragile", na.rm = TRUE), "\n")
cat("RV (q=1) range:",
    round(min(sens_consumption$RV_q1, na.rm = TRUE), 4), "to",
    round(max(sens_consumption$RV_q1, na.rm = TRUE), 4), "\n")
cat("Cross-weight mean RV diff:",
    round(mean(consistency_check$RV_range, na.rm = TRUE), 4), "\n")
cat("Cross-weight max RV diff:",
    round(max(consistency_check$RV_range, na.rm = TRUE), 4), "\n")

# ── SENSITIVITY ANALYSIS — CLUSTER-ROBUST ────────────────────────────────── ####

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(fixest)
library(sensemakr)
library(ggplot2)
library(writexl)

# ── Create output directory ───────────────────────────────────────────────####
dir.create("tables", showWarnings = FALSE)


library(sensemakr)
library(fixest)
library(dplyr)
library(purrr)
library(tibble)

# Step 1: Run reduced-form feols with cluster-robust SE for each industry × weight

run_corrected_sensitivity <- function(data, industry_col, wind_var, temp_var,
                                      weight_label, cluster_var = "fe_week") {
  
  industries <- sort(unique(data[[industry_col]]))
  
  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon | fe_hour + fe_week + fe_dow"
  ))
  
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  map(industries, function(ind) {
    sub <- data %>% filter(.data[[industry_col]] == ind)
    
    # Reduced-form feols with cluster-robust SE
    rf <- tryCatch(
      feols(fml, data = sub, cluster = cluster_fml),
      error = function(e) NULL
    )
    if (is.null(rf)) return(NULL)
    
    ct <- coeftable(rf)
    if (!(wind_var %in% rownames(ct))) return(NULL)
    
    # Extract cluster-robust statistics
    t_cluster  <- ct[wind_var, "t value"]
    se_cluster <- ct[wind_var, "Std. Error"]
    est        <- ct[wind_var, "Estimate"]
    
    # Degrees of freedom (use cluster-adjusted df)
    n_obs    <- nobs(rf)
    n_clust  <- length(unique(sub[[cluster_var]]))
    k        <- length(coef(rf))
    dof      <- n_clust - 1  # cluster-adjusted df
    
    # Partial R² of instrument with outcome (this is invariant to SE choice)
    # Compute from t-stat and dof using the OLS regression (not cluster)
    # partial_R2 = t² / (t² + dof) — but we need the OLS t for partial R2
    # Actually, partial R2 is a property of the data, not the SEs.
    # We compute it from the OLS model
    rf_ols <- tryCatch(
      lm(as.formula(paste0(
        "log_consumption ~ ", wind_var, " + ", temp_var,
        " + log_gas + log_coal + log_carbon + factor(fe_hour) + factor(fe_week) + factor(fe_dow)"
      )), data = sub),
      error = function(e) NULL
    )
    
    if (is.null(rf_ols)) return(NULL)
    
    ols_ct <- summary(rf_ols)$coefficients
    t_ols  <- ols_ct[wind_var, "t value"]
    dof_ols <- rf_ols$df.residual
    partial_r2 <- t_ols^2 / (t_ols^2 + dof_ols)
    
    # Compute RV using cluster-robust t-statistic
    # RV_q1: minimum confounding to reduce estimate to zero (depends on partial R2 only)
    rv_q1 <- robustness_value(t_statistic = t_ols, dof = dof_ols, q = 1)
    
    # RV_q1_alpha: minimum confounding to make estimate insignificant
    # This is where the cluster-robust t matters
    rv_q1_alpha <- robustness_value(t_statistic = t_cluster, dof = dof, q = 1, alpha = 0.05)
    
    # Benchmark bounds using sensemakr on the OLS model
    # (benchmark partial R2s are data properties, not affected by clustering)
    sens_ols <- tryCatch(
      sensemakr(model = rf_ols, treatment = wind_var,
                benchmark_covariates = temp_var, kd = 1:5, alpha = 0.05),
      error = function(e) NULL
    )
    
    # Critical kd: use cluster-robust t to determine when significance is lost
    critical_kd <- NA
    if (!is.null(sens_ols) && !is.null(sens_ols$bounds) && nrow(sens_ols$bounds) > 0) {
      bounds_df <- as.data.frame(sens_ols$bounds)
      bounds_df$kd <- as.numeric(stringr::str_extract(bounds_df$bound_label, "^[0-9]+"))
      
      # The adjusted estimate from sensemakr doesn't change with clustering.
      # But we need to compare the adjusted estimate against the cluster-robust SE.
      # Adjusted t = adjusted_estimate / cluster_SE
      bounds_df$adjusted_t_cluster <- bounds_df$adjusted_estimate / se_cluster
      
      t_crit <- qt(0.975, df = dof)
      sig_lost <- bounds_df %>%
        filter(abs(adjusted_t_cluster) < t_crit) %>%
        slice_min(kd, n = 1)
      
      critical_kd <- if (nrow(sig_lost) > 0) sig_lost$kd[1] else ">5"
    }
    
    tibble(
      Industry       = ind,
      Weight         = weight_label,
      t_OLS          = round(t_ols, 2),
      t_cluster      = round(t_cluster, 2),
      inflation      = round(abs(t_ols / t_cluster), 2),
      Partial_R2     = round(partial_r2, 6),
      RV_q1          = round(rv_q1, 4),
      RV_q1_alpha    = round(rv_q1_alpha, 4),
      Critical_kd    = as.character(critical_kd),
      N_obs          = n_obs,
      N_clusters     = n_clust
    )
  }) %>% bind_rows()
}

# Run for all three weighting schemes
cat("Running corrected sensitivity (consumption weights)...\n")
sens_corrected_c <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_c", "Temp_c", "Consumption"
)

cat("Running corrected sensitivity (employment weights)...\n")
sens_corrected_emp <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_emp", "Temp_emp", "Employment"
)

cat("Running corrected sensitivity (firm count weights)...\n")
sens_corrected_firm <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_firm", "Temp_firm", "Firm count"
)

# Combine and classify
sens_corrected_all <- bind_rows(sens_corrected_c, sens_corrected_emp, sens_corrected_firm) %>%
  mutate(
    Robustness_Tier = case_when(
      RV_q1_alpha >= 0.10 ~ "High",
      RV_q1_alpha >= 0.03 ~ "Moderate",
      TRUE                ~ "Fragile"
    )
  )
sens_corrected_all
# Report
cat("\n============================================================\n")
cat("CORRECTED SENSITIVITY RESULTS (Consumption Weights)\n")
cat("============================================================\n")
print(
  sens_corrected_c %>%
    mutate(Tier = case_when(
      RV_q1_alpha >= 0.10 ~ "High",
      RV_q1_alpha >= 0.03 ~ "Moderate",
      TRUE ~ "Fragile"
    )) %>%
    arrange(desc(RV_q1_alpha)) %>%
    select(Industry, t_OLS, t_cluster, inflation, RV_q1, RV_q1_alpha, Critical_kd, Tier),
  n = 35
)

# Compare old vs new tiers
cat("\n============================================================\n")
cat("TIER RECLASSIFICATION\n")
cat("============================================================\n")

comparison <- sens_consumption %>%
  select(Industry, RV_old = RV_q1_alpha, Tier_old = Robustness_Tier) %>%
  left_join(
    sens_corrected_c %>%
      mutate(Tier_new = case_when(
        RV_q1_alpha >= 0.10 ~ "High",
        RV_q1_alpha >= 0.03 ~ "Moderate",
        TRUE ~ "Fragile"
      )) %>%
      select(Industry, RV_new = RV_q1_alpha, Tier_new),
    by = "Industry"
  ) %>%
  mutate(Changed = Tier_old != Tier_new)

print(comparison %>% arrange(desc(RV_new)), n = 35)

cat("\nReclassified industries:", sum(comparison$Changed, na.rm = TRUE),
    "/", nrow(comparison), "\n")
cat("Summary (corrected, consumption weights):\n")
cat("  High:", sum(comparison$Tier_new == "High", na.rm = TRUE), "\n")
cat("  Moderate:", sum(comparison$Tier_new == "Moderate", na.rm = TRUE), "\n")
cat("  Fragile:", sum(comparison$Tier_new == "Fragile", na.rm = TRUE), "\n")


# Clean reporting table combining all three layers
reporting_table <- sens_corrected_c %>%
  mutate(
    # Layer 1: Cluster-robust significance
    RF_significant = abs(t_cluster) > qt(0.975, df = N_clusters - 1),
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate", 
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    ),
    # Layer 2: RV_q1 (OLS, valid as partial R2 measure)
    RV_q1_tier = case_when(
      RV_q1 >= 0.05 ~ "Robust",
      RV_q1 >= 0.01 ~ "Moderate",
      TRUE          ~ "Fragile"
    ),
    # Layer 3: kd already computed
    survives_benchmark = Critical_kd != "1" & Critical_kd != "NA"
  ) %>%
  select(Industry, t_cluster, RF_category, RV_q1, RV_q1_tier, 
         Critical_kd, survives_benchmark) %>%
  arrange(desc(abs(t_cluster)))

cat("\n============================================================\n")
cat("CORRECTED SENSITIVITY REPORTING TABLE\n")
cat("============================================================\n")
print(reporting_table, n = 35)

cat("\nReduced-form significance (cluster-robust):\n")
cat("  Strong (|t| >= 3):", sum(reporting_table$RF_category == "Strong"), "\n")
cat("  Moderate (2 <= |t| < 3):", sum(reporting_table$RF_category == "Moderate"), "\n")
cat("  Borderline (1.5 <= |t| < 2):", sum(reporting_table$RF_category == "Borderline"), "\n")
cat("  Insignificant (|t| < 1.5):", sum(reporting_table$RF_category == "Insignificant"), "\n")

cat("\nSurvives kd = 1 benchmark:", sum(reporting_table$survives_benchmark), "/", 
    nrow(reporting_table), "\n")

# ── SENSITIVITY FUNCTION ────────────────────────────────────────────────────####

run_corrected_sensitivity <- function(data, industry_col, wind_var, temp_var,
                                      weight_label, cluster_var = "fe_week") {
  
  industries <- sort(unique(data[[industry_col]]))
  
  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow"
  ))
  
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  map(industries, function(ind) {
    sub <- data %>% filter(.data[[industry_col]] == ind)
    
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
    dof     <- n_clust - 1
    
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
    
    rv_q1       <- robustness_value(t_statistic = t_ols, dof = dof_ols, q = 1)
    rv_q1_alpha <- robustness_value(t_statistic = t_cluster, dof = dof, q = 1, alpha = 0.05)
    
    sens_ols <- tryCatch(
      sensemakr(model = rf_ols, treatment = wind_var,
                benchmark_covariates = temp_var, kd = 1:5, alpha = 0.05),
      error = function(e) NULL
    )
    
    critical_kd <- NA
    if (!is.null(sens_ols) && !is.null(sens_ols$bounds) && nrow(sens_ols$bounds) > 0) {
      bounds_df <- as.data.frame(sens_ols$bounds)
      bounds_df$kd <- as.numeric(str_extract(bounds_df$bound_label, "^[0-9]+"))
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



if (!exists("sens_corrected_c")) {
  cat("Running corrected sensitivity (consumption weights)...\n")
  sens_corrected_c <- run_corrected_sensitivity(
    consumption_panel, "DK36_en", "Wind_c", "Temp_c", "Consumption"
  )
}

if (!exists("sens_corrected_emp")) {
  cat("Running corrected sensitivity (employment weights)...\n")
  sens_corrected_emp <- run_corrected_sensitivity(
    consumption_panel, "DK36_en", "Wind_emp", "Temp_emp", "Employment"
  )
}

if (!exists("sens_corrected_firm")) {
  cat("Running corrected sensitivity (firm count weights)...\n")
  sens_corrected_firm <- run_corrected_sensitivity(
    consumption_panel, "DK36_en", "Wind_firm", "Temp_firm", "Firm count"
  )
}

sens_corrected_all <- bind_rows(sens_corrected_c, sens_corrected_emp, sens_corrected_firm)


# ── TABLES MAIN REPORTING TABLE (Consumption Weights) ─────────────────────── ####
# Three-layer classification for Section 4.5.3

table1_main <- sens_corrected_c %>%
  mutate(
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate",
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    ),
    RV_q1_tier = case_when(
      RV_q1 >= 0.05 ~ "Robust",
      RV_q1 >= 0.01 ~ "Moderate",
      TRUE          ~ "Fragile"
    ),
    Survives_kd1 = Critical_kd != "1" & !is.na(Critical_kd)
  ) %>%
  arrange(desc(abs(t_cluster))) %>%
  select(
    Industry,
    `Cluster t` = t_cluster,
    `RF Category` = RF_category,
    `Partial R²` = Partial_R2,
    `RV (q=1)` = RV_q1,
    `RV Tier` = RV_q1_tier,
    `Critical kd` = Critical_kd,
    `Survives kd=1` = Survives_kd1
  )

table2_main <- sens_corrected_all %>%
  mutate(
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate",
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    ),
    RV_q1_tier = case_when(
      RV_q1 >= 0.05 ~ "Robust",
      RV_q1 >= 0.01 ~ "Moderate",
      TRUE          ~ "Fragile"
    ),
    Survives_kd1 = Critical_kd != "1" & !is.na(Critical_kd)
  ) %>%
  arrange(desc(abs(t_cluster))) %>%
  select(
    Industry,
    `Cluster t` = t_cluster,
    `RF Category` = RF_category,
    `Partial R²` = Partial_R2,
    `RV (q=1)` = RV_q1,
    `RV Tier` = RV_q1_tier,
    `Critical kd` = Critical_kd,
    `Survives kd=1` = Survives_kd1
  )

print(table2_main, n = 100)

cat("\n============================================================\n")
cat("TABLE 1: SENSITIVITY REPORTING (Consumption Weights)\n")
cat("============================================================\n")
print(table1_main, n = 35)

# Summary counts
cat("\nReduced-form significance (cluster-robust):\n")
cat("  Strong (|t| >= 3):", sum(table1_main$`RF Category` == "Strong"), "\n")
cat("  Moderate (2 <= |t| < 3):", sum(table1_main$`RF Category` == "Moderate"), "\n")
cat("  Borderline (1.5 <= |t| < 2):", sum(table1_main$`RF Category` == "Borderline"), "\n")
cat("  Insignificant (|t| < 1.5):", sum(table1_main$`RF Category` == "Insignificant"), "\n")
cat("\nSurvives kd=1 benchmark:", sum(table1_main$`Survives kd=1`), "/", nrow(table1_main), "\n")



# TABLE 2: OLS vs CLUSTER T-STATISTIC INFLATION (Appendix)

table2_inflation <- sens_corrected_c %>%
  arrange(desc(inflation)) %>%
  select(
    Industry,
    `OLS t-stat` = t_OLS,
    `Cluster t-stat` = t_cluster,
    `Inflation ratio` = inflation,
    `N observations` = N_obs,
    `N clusters` = N_clusters
  )

cat("\n============================================================\n")
cat("TABLE 2: T-STATISTIC INFLATION (OLS vs Cluster-Robust)\n")
cat("============================================================\n")
print(table2_inflation, n = 35)

cat("\nInflation summary:\n")
cat("  Mean:", round(mean(table2_inflation$`Inflation ratio`), 2), "\n")
cat("  Min: ", round(min(table2_inflation$`Inflation ratio`), 2), "\n")
cat("  Max: ", round(max(table2_inflation$`Inflation ratio`), 2), "\n")



# TABLE 3: CROSS-WEIGHT COMPARISON (Appendix)
# Shows sensitivity results across all three weighting schemes

table3_crossweight <- sens_corrected_all %>%
  mutate(
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate",
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    )
  ) %>%
  select(Industry, Weight, t_cluster, RV_q1, Critical_kd, RF_category) %>%
  arrange(Industry, Weight)

# Wide format: one row per industry, columns for each weight
table3_wide <- sens_corrected_all %>%
  select(Industry, Weight, t_cluster, RV_q1, Critical_kd) %>%
  pivot_wider(
    names_from = Weight,
    values_from = c(t_cluster, RV_q1, Critical_kd),
    names_sep = "_"
  ) %>%
  mutate(
    # Cross-weight consistency: do all three agree on RF category?
    cat_c = case_when(
      abs(t_cluster_Consumption) >= 3 ~ "Strong",
      abs(t_cluster_Consumption) >= 2 ~ "Moderate",
      abs(t_cluster_Consumption) >= 1.5 ~ "Borderline",
      TRUE ~ "Insignificant"
    ),
    cat_e = case_when(
      abs(t_cluster_Employment) >= 3 ~ "Strong",
      abs(t_cluster_Employment) >= 2 ~ "Moderate",
      abs(t_cluster_Employment) >= 1.5 ~ "Borderline",
      TRUE ~ "Insignificant"
    ),
    cat_f = case_when(
      abs(`t_cluster_Firm count`) >= 3 ~ "Strong",
      abs(`t_cluster_Firm count`) >= 2 ~ "Moderate",
      abs(`t_cluster_Firm count`) >= 1.5 ~ "Borderline",
      TRUE ~ "Insignificant"
    ),
    Category_consistent = (cat_c == cat_e) & (cat_e == cat_f)
  ) %>%
  select(
    Industry,
    `t (Cons)` = t_cluster_Consumption,
    `t (Emp)` = t_cluster_Employment,
    `t (Firm)` = `t_cluster_Firm count`,
    `RV (Cons)` = RV_q1_Consumption,
    `RV (Emp)` = RV_q1_Employment,
    `RV (Firm)` = `RV_q1_Firm count`,
    `kd (Cons)` = Critical_kd_Consumption,
    `kd (Emp)` = Critical_kd_Employment,
    `kd (Firm)` = `Critical_kd_Firm count`,
    `Category consistent` = Category_consistent
  ) %>%
  arrange(desc(abs(`t (Cons)`)))

cat("\n============================================================\n")
cat("TABLE 3: CROSS-WEIGHT SENSITIVITY COMPARISON\n")
cat("============================================================\n")
print(table3_wide, n = 35)

cat("\nCross-weight category consistency:",
    sum(table3_wide$`Category consistent`), "/", nrow(table3_wide), "\n")



# TABLE 4: TIER RECLASSIFICATION (Appendix)
# Compares uncorrected (OLS) vs corrected (cluster) tiers


# Need the uncorrected tiers from the earlier sensemakr run
if (exists("sens_consumption")) {
  table4_reclass <- sens_consumption %>%
    select(Industry, RV_old = RV_q1_alpha, Tier_old = Robustness_Tier) %>%
    left_join(
      sens_corrected_c %>%
        mutate(
          Tier_new = case_when(
            abs(t_cluster) >= 3.0 ~ "Strong RF",
            abs(t_cluster) >= 2.0 ~ "Moderate RF",
            abs(t_cluster) >= 1.5 ~ "Borderline RF",
            TRUE ~ "Insignificant RF"
          )
        ) %>%
        select(Industry, t_cluster, RV_new = RV_q1, Tier_new),
      by = "Industry"
    ) %>%
    mutate(
      Reclassified = case_when(
        Tier_old == "High"     & Tier_new != "Strong RF" ~ TRUE,
        Tier_old == "Moderate" & Tier_new == "Insignificant RF" ~ TRUE,
        Tier_old == "Fragile"  & Tier_new %in% c("Strong RF", "Moderate RF") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    arrange(desc(abs(t_cluster)))
  
  cat("\n============================================================\n")
  cat("TABLE 4: TIER RECLASSIFICATION (OLS → Cluster-Robust)\n")
  cat("============================================================\n")
  print(table4_reclass, n = 35)
  
  cat("\nReclassified:", sum(table4_reclass$Reclassified), "/",
      nrow(table4_reclass), "\n")
} else {
  cat("NOTE: sens_consumption not found. Skipping reclassification table.\n")
  table4_reclass <- NULL
}



# TABLE 5: IDENTIFICATION STRENGTH SUMMARY (Thesis Body)
# Compact summary for Section 4.5.3 narrative


table5_summary <- sens_corrected_c %>%
  mutate(
    Group = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong identification",
      abs(t_cluster) >= 1.5 ~ "Borderline identification",
      TRUE                  ~ "Weak identification"
    ),
    Group = factor(Group, levels = c("Strong identification",
                                     "Borderline identification",
                                     "Weak identification"))
  ) %>%
  group_by(Group) %>%
  summarise(
    N_industries     = n(),
    Industries       = paste(Industry, collapse = "; "),
    Mean_t_cluster   = round(mean(abs(t_cluster)), 2),
    Mean_partial_R2  = round(mean(Partial_R2), 6),
    Mean_RV_q1       = round(mean(RV_q1), 4),
    N_survive_kd1    = sum(Critical_kd != "1" & !is.na(Critical_kd)),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("TABLE 5: IDENTIFICATION STRENGTH SUMMARY\n")
cat("============================================================\n")
print(table5_summary %>% select(-Industries), n = 5)
cat("\nStrong identification industries:\n")
cat(table5_summary$Industries[table5_summary$Group == "Strong identification"], "\n")



# FIGURE 1: SENSITIVITY SCATTER — t_cluster vs Partial R²


fig1_data <- sens_corrected_c %>%
  mutate(
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate",
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    ),
    RF_category = factor(RF_category,
                         levels = c("Strong", "Moderate", "Borderline", "Insignificant"))
  )

fig1 <- ggplot(fig1_data, aes(x = Partial_R2, y = abs(t_cluster))) +
  geom_hline(yintercept = c(1.5, 2, 3), linetype = "dashed",
             colour = c("grey70", "grey50", "grey30"), linewidth = 0.4) +
  geom_point(aes(colour = RF_category, shape = RF_category), size = 3, alpha = 0.8) +
  geom_text(aes(label = Industry), size = 2.2, vjust = -0.9,
            check_overlap = TRUE, colour = "grey30") +
  scale_colour_manual(
    values = c("Strong" = "#2E86AB", "Moderate" = "#F6AE2D",
               "Borderline" = "#F26157", "Insignificant" = "grey60")
  ) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  annotate("text", x = max(fig1_data$Partial_R2) * 0.9, y = 3.2,
           label = "|t| = 3", size = 2.5, colour = "grey30") +
  annotate("text", x = max(fig1_data$Partial_R2) * 0.9, y = 2.2,
           label = "|t| = 2", size = 2.5, colour = "grey50") +
  annotate("text", x = max(fig1_data$Partial_R2) * 0.9, y = 1.7,
           label = "|t| = 1.5", size = 2.5, colour = "grey70") +
  labs(
    x = expression("Partial" ~ R^2 ~ "of wind instrument (OLS)"),
    y = "|t-statistic| (cluster-robust)",
    colour = "RF Category",
    shape = "RF Category",
    title = "Sensitivity Analysis: Instrument Strength vs Cluster-Robust Significance",
    subtitle = "Industries above |t| = 3 have statistically robust reduced forms"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(fig1)
ggsave("tables/fig_sensitivity_scatter.pdf", fig1, width = 10, height = 7)
ggsave("tables/fig_sensitivity_scatter.png", fig1, width = 10, height = 7, dpi = 300)



# FIGURE 2: OLS vs CLUSTER T-STATISTIC COMPARISON


fig2_data <- sens_corrected_c %>%
  arrange(desc(abs(t_cluster))) %>%
  mutate(Industry = factor(Industry, levels = rev(Industry)))

fig2 <- ggplot(fig2_data, aes(y = Industry)) +
  geom_segment(aes(x = t_cluster, xend = t_OLS, yend = Industry),
               colour = "grey70", linewidth = 0.5) +
  geom_point(aes(x = t_OLS), colour = "#F26157", size = 2.5, shape = 17) +
  geom_point(aes(x = t_cluster), colour = "#2E86AB", size = 2.5, shape = 16) +
  geom_vline(xintercept = c(-3, -2, 0, 2, 3),
             linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
             colour = "grey50", linewidth = 0.3) +
  labs(
    x = "t-statistic",
    y = NULL,
    title = "Reduced-Form t-Statistics: OLS vs Cluster-Robust",
    subtitle = "Blue circles = cluster-robust; Red triangles = OLS (inflated)"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 7))

print(fig2)
ggsave("tables/fig_t_inflation.pdf", fig2, width = 10, height = 8)
ggsave("tables/fig_t_inflation.png", fig2, width = 10, height = 8, dpi = 300)



###────────────────── CROSS-WEIGHT t-STATISTICS ─────────────────────────────###


fig3_data <- sens_corrected_all %>%
  mutate(
    Industry = factor(Industry, levels = rev(levels(
      factor(sens_corrected_c$Industry[order(abs(sens_corrected_c$t_cluster))])
    )))
  )

fig3 <- ggplot(fig3_data, aes(x = t_cluster, y = Industry, colour = Weight)) +
  geom_vline(xintercept = c(-3, -2, 0, 2, 3),
             linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
             colour = "grey50", linewidth = 0.3) +
  geom_point(size = 2, alpha = 0.8, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("Consumption" = "#2E86AB",
                                 "Employment" = "#F6AE2D",
                                 "Firm count" = "#A23B72")) +
  labs(
    x = "Cluster-robust t-statistic (reduced form)",
    y = NULL,
    colour = "Weighting scheme",
    title = "Cross-Weight Sensitivity: Cluster-Robust Reduced-Form t-Statistics",
    subtitle = "Dashed lines at |t| = 3; dotted lines at |t| = 2"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )

print(fig3)
ggsave("tables/fig_crossweight_sensitivity.pdf", fig3, width = 10, height = 8)
ggsave("tables/fig_crossweight_sensitivity.png", fig3, width = 10, height = 8, dpi = 300)


# CSV exports (for manual Word/LaTeX import)
write.csv(table1_main, "tables/table_sensitivity_main.csv", row.names = FALSE)
write.csv(table2_inflation, "tables/table_t_inflation.csv", row.names = FALSE)
write.csv(table3_wide, "tables/table_crossweight_sensitivity.csv", row.names = FALSE)
if (!is.null(table4_reclass)) {
  write.csv(table4_reclass, "tables/table_tier_reclassification.csv", row.names = FALSE)
}
write.csv(table5_summary %>% select(-Industries),
          "tables/table_identification_summary.csv", row.names = FALSE)

# Single Excel workbook with all tables as separate sheets
write_xlsx(
  list(
    "Main Sensitivity" = as.data.frame(table1_main),
    "T-stat Inflation" = as.data.frame(table2_inflation),
    "Cross-Weight"     = as.data.frame(table3_wide),
    "Reclassification" = if (!is.null(table4_reclass)) as.data.frame(table4_reclass) else data.frame(Note = "Not available"),
    "Summary"          = as.data.frame(table5_summary %>% select(-Industries))
  ),
  path = "tables/sensitivity_analysis_tables.xlsx"
)


# ===========================================================================
# = PART THREE: Ecological Inference  =======================================
# ===========================================================================
# ===========================================================================
# = PART THREE: ECOLOGICAL INFERENCE  =======================================
# ===========================================================================
# ── EI.1 Weight correlations across schemes ───────────────────────────────####
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


# ── EI.2 Weight dispersion by industry ────────────────────────────────────####
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


# ── EI.3 Elasticity divergence across weighting schemes ───────────────────####
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


# ── EI.4 Spearman test: weight divergence vs elasticity divergence ────────####
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


# ── EI.5 Scatter plot: weight divergence vs elasticity divergence ─────────####

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


# ── EI.6 SE-anchored ecological classification ───────────────────────────####
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


# ── EI.7 Weight stability over time ──────────────────────────────────────####
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


# ── EI.8 King (1997) Inflation Factor F ───────────────────────────────────####
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


# ── EI.9 Consolidated ecological output ──────────────────────────────────####
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

# ===========================================================================
# = PART THREE: SUMMARY  ====================================================
# ===========================================================================
# ── MASTER ROBUSTNESS SUMMARY ────────────────────────────####
cat("\n1. ANDERSON-RUBIN (Pooled):\n")
print(ar_pooled_summary)

cat("\n2. ANDERSON-RUBIN (Split) — empty sets:",
    sum(ar_split_all$AR_Empty), "/", nrow(ar_split_all), "\n")
if (exists("ar_with_estimates")) {
  cat("   Point estimate inside CS:",
      sum(ar_with_estimates$Inside_CS, na.rm = TRUE), "/",
      sum(!is.na(ar_with_estimates$Inside_CS)), "\n")
  cat("   Mean AR/Wald ratio (consumption):",
      round(mean(ar_c_valid$AR_vs_Wald, na.rm = TRUE), 3), "\n")
}

cat("\n3. SENSITIVITY (Consumption weights):\n")
cat("   High:", sum(sens_consumption$Robustness_Tier == "High", na.rm = TRUE), "\n")
cat("   Moderate:", sum(sens_consumption$Robustness_Tier == "Moderate", na.rm = TRUE), "\n")
cat("   Fragile:", sum(sens_consumption$Robustness_Tier == "Fragile", na.rm = TRUE), "\n")

cat("\n4. ECOLOGICAL INFERENCE:\n")
cat("   Sign consistency:", sum(elasticity_divergence$sign_consistent),
    "/", nrow(elasticity_divergence), "\n")
cat("   Max elasticity divergence:",
    round(max(elasticity_divergence$range_elasticity, na.rm = TRUE), 6), "\n")
cat("   All classified Low:", all(eco_summary$ecological_sensitivity == "Low"), "\n")
if (!is.null(eco_cor)) {
  cat("   Spearman rho:", round(eco_cor$estimate, 4),
      "(p =", format.pval(eco_cor$p.value, digits = 3), ")\n")
}

# ===========================================================================
# = PART FOUR: POWER ANALYSIS  ==============================================
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
        log_consumption ~ Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_month,
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


# ── Load pre-computed simulations ──────────────────────────────────────────####
# power_results_c    <- readRDS("power_results_c_1.rds")
# power_results_emp  <- readRDS("power_results_emp_1.rds")
# power_results_firm <- readRDS("power_results_firm_1.rds")

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
              fe_vars = c("fe_hour", "fe_month"),
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

# saveRDS(power_results_c, "power_results_c_1.rds")
 
power_results_emp <- run_power_by_gran(
  power_inputs = power_inputs_emp,
  df_panel = consumption_panel,
  endog_var = "log_P_emp",
  instrument_var = "Wind_emp",
  temp_var = "Temp_emp",
  weight_name = "employment_weight"
)

# saveRDS(power_results_emp, "power_results_emp_1.rds")

power_results_firm <- run_power_by_gran(
  power_inputs = power_inputs_firm,
  df_panel = consumption_panel,
  endog_var = "log_P_firm",
  instrument_var = "Wind_firm",
  temp_var = "Temp_firm",
  weight_name = "firm_weight"
)

# saveRDS(power_results_firm, "power_results_firm_1.rds")


# ── Power analysis summaries ───────────────────────────────────────────────####
power_analysis_c    <- analyze_iv_power_simulation_sector(power_results_c)
power_analysis_emp  <- analyze_iv_power_simulation_sector(power_results_emp)
power_analysis_firm <- analyze_iv_power_simulation_sector(power_results_firm)


# =============================================================================
# = PART FIVE: VISUALISATIONS ================================================
# =============================================================================
# ── Weight distribution comparison ─────────────────────────────────────────####
industry %>%
  left_join(distinct(consumption_panel, DK36Title, DK36_en), by = "DK36Title") %>%
  group_by(DK36_en) %>%
  summarise(
    Consumption  = mean(w_DK1_c,       na.rm = TRUE),
    Employment   = mean(w_DK1_emp,     na.rm = TRUE),
    `Firm count` = mean(w_DK1_firm,    na.rm = TRUE),
    `Geo. mean`  = mean(w_DK1_geomean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
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
# ── First-stage F-stat heatmap ─────────────────────────────────────────────####
p_fstat <- results %>%
  filter(cluster == "Week") %>%
  ggplot(aes(x = weight, y = sector, fill = fs_f)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = round(fs_f, 0)), size = 2.4, colour = "grey10") +
  scale_fill_gradient2(
    low = "#d7191c", mid = "#ffffbf", high = "#1a9641",
    midpoint = 10, limits = c(0, NA),
    name = "First-stage F", na.value = "grey85"
  ) +
  labs(title    = "First-Stage F-Statistics by Sector and Weighting Method",
       subtitle = "Staiger-Stock weak instrument threshold: F = 10",
       x = NULL, y = NULL,
       caption  = "SE clustered by week. Red = weak instrument (F < 10), green = strong.") +
  theme_minimal(base_size = 10) +
  theme(plot.title        = element_text(face = "bold", size = 12, margin = margin(b = 4)),
        plot.subtitle     = element_text(size = 9, colour = "grey35"),
        plot.caption      = element_text(size = 7.5, colour = "grey50", hjust = 0),
        axis.text.y       = element_text(size = 7.5),
        axis.text.x       = element_text(size = 9, face = "bold"),
        panel.grid        = element_blank(),
        legend.position   = "right",
        legend.key.height = unit(1.5, "cm"))

p_fstat
# ── Monotonicity diagnostic: first-stage coefficients ─────────────────────────

sector_order_mono <- first_stage_results %>%
  group_by(sector) %>%
  summarise(median_est = median(fs_estimate, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_est) %>%
  pull(sector)

p_monotonicity <- first_stage_results %>%
  mutate(weight = factor(weight, levels = c("Consumption", "Employment", "Firm count"))) %>%
  ggplot(aes(x = fs_estimate, y = weight, fill = weight)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_boxplot(alpha = 0.4, outlier.shape = 21, outlier.size = 1.5, width = 0.5) +
  scale_fill_manual(values = c(
    "Consumption" = "#2166AC",
    "Employment"  = "#B2182B",
    "Firm count"  = "#1B7837"
  )) +
  scale_y_discrete() +
  labs(
    title    = "First-Stage Coefficients: Wind Forecast on Spot Price",
    subtitle = "All coefficients negative across specifications, consistent with monotonicity",
    x        = "First-stage coefficient (effect of wind on price)",
    y        = NULL,
    fill     = NULL,
    caption  = "Distribution across all sectors and clustering choices. Dashed line = zero."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0,
                                      margin = margin(t = 8)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "none"
  )

p_monotonicity
# Order sectors by median F-stat across specifications
sector_order_fs <- first_stage_results %>%
  group_by(sector) %>%
  summarise(median_f = median(fs_f, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_f) %>%
  pull(sector)

sector_order_fs_wrapped <- stringr::str_wrap(sector_order_fs, width = 30)

first_stage_results <- first_stage_results %>%
  mutate(sector_short = factor(
    stringr::str_wrap(as.character(sector), width = 30),
    levels = sector_order_fs_wrapped
  ))


p_fstat_box <- first_stage_results %>%
  mutate(weight = factor(weight, levels = c("Consumption", "Employment", "Firm count"))) %>%
  ggplot(aes(x = fs_f, y = weight, fill = weight)) +
  geom_vline(xintercept = 10, linetype = "dashed", colour = "#d7191c", linewidth = 0.5) +
  annotate("text", x = 11, y = 0.6, label = "F = 10", hjust = 0,
           size = 3, colour = "#d7191c", fontface = "italic") +
  scale_y_discrete() +
  geom_boxplot(alpha = 0.4, outlier.shape = 21, outlier.size = 1.5, width = 0.5) +
  scale_fill_manual(values = c(
    "Consumption" = "#2166AC",
    "Employment"  = "#B2182B",
    "Firm count"  = "#1B7837"
  )) +
  labs(
    title    = "First-Stage F-Statistics by Weighting Method",
    subtitle = "Distribution across all sectors and clustering choices",
    x        = "First-stage F-statistic",
    y        = NULL,
    caption  = "Dashed line = Staiger-Stock weak instrument threshold (F = 10)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0,
                                      margin = margin(t = 8)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "none"
  )

p_fstat_box
# ── Consumption shares ─────────────────────────────────────────────────────####
sector_shares <- consumption_panel %>%
  group_by(DK36_en) %>%
  summarise(total_MWh = sum(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(share_pct = 100 * total_MWh / sum(total_MWh))

sig_data <- results %>%
  filter(weight == "Consumption", cluster == "Week", p_value < 0.05) %>%
  left_join(sector_shares, by = c("sector" = "DK36_en")) %>%
  arrange(desc(share_pct))

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

ggplot(sector_shares,
       aes(x = reorder(DK36_en, share_panel), y = share_panel)) +
  geom_col(fill = "#2166AC", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", share_panel)),
            hjust = -0.1, size = 2.8, colour = "grey30") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Sector Share of Total Industrial Electricity Consumption",
    subtitle = "No single sector accounts for a large enough share to plausibly affect the spot price",
    x        = NULL,
    y        = "Share of total consumption (%)",
    caption  = "Based on total hourly consumption across all sectors in the estimation sample (2021\u20132025)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0,
                                    margin = margin(t = 8)),
    axis.text.y      = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
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
# ── Shared aesthetics ──────────────────────────────────────────────────────####

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
p_type_m_all
p_type_m_clean



