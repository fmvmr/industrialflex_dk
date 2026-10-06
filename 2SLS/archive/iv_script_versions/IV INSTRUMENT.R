### IV instrument ### 

setwd("~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")

################################ LOAD PACKAGES ###############################################
library(arrow)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(glue)
library(readxl)

library(tidyr)

################################ LOAD DATA #######################################################
load_data <- function(years, months = 1:12) {
  
  ym_grid <- expand.grid(year = years, month = months) %>%
    dplyr::arrange(year, month) %>%
    dplyr::mutate(ym = sprintf("%d_%02d", year, month))
  
  # helper: read parquet if exists, else NULL
  safe_read <- function(path){
    if (file.exists(path)) arrow::read_parquet(path) else NULL
  }
  
  data_list <- lapply(seq_len(nrow(ym_grid)), function(i) {
    ym <- ym_grid$ym[i]
    
    list(
      prices      = safe_read(glue("data/elspotprices_monthly/elspot_{ym}.parquet")),
      forecast    = safe_read(glue("data/forecast_hourly_monthly_compact/forecast_compact_{ym}.parquet")),
      temp        = safe_read(glue("data/temp_zone_hourly_monthly/temp_zone_{ym}.parquet")),
      industry    = safe_read(glue("data/consumption_industry_hourly_monthly/consumption_industry_hour_{ym}.parquet")),
      consumption = safe_read(glue("data/consumption_category_hourly_monthly/consumption_cat_hour_{ym}.parquet"))
    )
  })
  
  bind_nonnull <- function(x) dplyr::bind_rows(Filter(Negate(is.null), x))
  
  list(
    prices      = bind_nonnull(lapply(data_list, `[[`, "prices")),
    forecast    = bind_nonnull(lapply(data_list, `[[`, "forecast")),
    temp        = bind_nonnull(lapply(data_list, `[[`, "temp")),
    industry    = bind_nonnull(lapply(data_list, `[[`, "industry")),
    consumption = bind_nonnull(lapply(data_list, `[[`, "consumption")),
    
    industry_annual = dplyr::bind_rows(
      Filter(
        Negate(is.null),
        lapply(years, function(y) {
          path <- glue("data/consumption_dk10_region_year/consumption_dk10_region_{y}.parquet")
          if (file.exists(path)) arrow::read_parquet(path) else NULL
        })
      )
    ),
    
    gas    = arrow::read_parquet("data/controls/gas_daily_2020_2025.parquet"),
    carbon = arrow::read_parquet("data/controls/carbon_daily_2020_2025.parquet"),
    coal   = arrow::read_parquet("data/controls/coal_daily_2020_2025.parquet")
  )
}

### Select month ###
d <- load_data(2021:2025, 1:12)
### Load data for selected month)
prices   <- d$prices
forecast <- d$forecast
temp     <- d$temp
industry <- d$industry
cons     <- d$consumption
gas      <- d$gas
carbon   <- d$carbon
coal     <- d$coal
industry_annual <- d$industry_annual

############################# Combine DK10 AND DK19 ################################
dk19title_to_dk10title <- tibble::tribble(
  ~DK19Title, ~DK10Title,
  "Landbrug, skovbrug og fiskeri", "Landbrug, skovbrug og fiskeri",
  "Råstofindvinding & Vandforsyning og renovation", "Industri, råstofindvinding og forsyningsvirksomhed",
  "Industri",                                       "Industri, råstofindvinding og forsyningsvirksomhed",
  "Energiforsyning",                                "Industri, råstofindvinding og forsyningsvirksomhed",
  "Bygge og anlæg", "Bygge og anlæg",
  "Handel",                    "Handel og transport",
  "Transport",                 "Handel og transport",
  "Hoteller og restauranter",  "Handel og transport",
  "Information og kommunikation", "Information og kommunikation",
  "Finansiering og forsikring", "Finansiering og forsikring",
  "Ejendomshandel og udlejning", "Ejendomshandel og udlejning",
  "Videnservice",                                          "Erhvervsservice",
  "Rejsebureauer, rengøring og anden operationel service",  "Erhvervsservice",
  "Offentlig administration, forsvar og politi", "Offentlig administration, undervisning og sundhed",
  "Undervisning",                                "Offentlig administration, undervisning og sundhed",
  "Sundhed og socialvæsen",                      "Offentlig administration, undervisning og sundhed",
  "Kultur og fritid",                            "Offentlig administration, undervisning og sundhed",
  "Andre serviceydelser mv",                     "Offentlig administration, undervisning og sundhed",
  "Privat",           NA_character_,
  "Uoplyst aktivitet",NA_character_
)

############################# Compute distributions #### 

industry <- industry %>%
  left_join(dk19title_to_dk10title, by = "DK19Title")


DK2_REGIONS <- c("Region Hovedstaden", "Region Sjælland")
DK1_REGIONS <- c("Region Syddanmark", "Region Midtjylland", "Region Nordjylland")


annual_zone_share <- industry_annual %>%
  filter(DK10Title != "Privat") %>%
  mutate(
    Year = as.integer(format(Year, "%Y")),   # <- force integer year
    Zone = case_when(
      RegionName %in% DK2_REGIONS ~ "DK2",
      RegionName %in% DK1_REGIONS ~ "DK1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Zone)) %>%
  group_by(Year, DK10Title, Zone) %>%
  summarise(
    Cons = sum(ConsumptionkWh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = Zone,
    values_from = Cons,
    values_fill = 0
  ) %>%
  mutate(
    w_DK1 = if_else(DK1 + DK2 > 0, DK1 / (DK1 + DK2), NA_real_)
  ) %>%
  select(Year, DK10Title, w_DK1)


##### Add weights to industry consumption
industry <- industry %>%
  mutate(Year = as.integer(format(TimeDK, "%Y"))) %>%
  left_join(annual_zone_share, by = c("Year", "DK10Title"))



#############################Compute weighted prices ####################
prices <- prices %>% arrange(HourDK, PriceArea)

prices_wide <- prices %>%
  select(HourDK, PriceArea, SpotPriceEUR) %>%
  tidyr::pivot_wider(
    names_from  = PriceArea,
    values_from = SpotPriceEUR,
    values_fn   = dplyr::first
  )


weighted_prices <- industry %>% select(TimeDK, DK19Title,DK36Title, w_DK1) %>% 
  distinct() %>% left_join(prices_wide, by = c("TimeDK" = "HourDK")) %>% 
  mutate(P_weighted = w_DK1 * DK1 + (1 - w_DK1) * DK2)


####################### Robustness checks for weights ( employees) #########################

employees2024 <- read_delim("employees2024.csv", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

employees2021 <- read_delim("employees2021.csv", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
employees2022 <- read_delim("Employees2022.csv", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
employees2023 <- read_delim("employees2023.csv", 
                            delim = ";", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)

total_employees <- bind_rows(
  employees2021,
  employees2022,
  employees2023,
  employees2024
)
  


total_employees <- total_employees %>%
  select(-X1) %>%   # remove X1
  rename(
    year = X2,
    status = X3,
    age = X4,
    industry = X5,
    region_hovedstaden = X6,
    region_sjaelland = X7,
    region_syddanmark = X8,
    region_midtjylland = X9,
    region_nordjylland = X10
  )


total_employees<- total_employees %>%
group_by(year, industry) %>%
  summarise(
    region_hovedstaden  = sum(region_hovedstaden, na.rm = TRUE),
    region_sjaelland    = sum(region_sjaelland, na.rm = TRUE),
    region_syddanmark   = sum(region_syddanmark, na.rm = TRUE),
    region_midtjylland  = sum(region_midtjylland, na.rm = TRUE),
    region_nordjylland  = sum(region_nordjylland, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    DK2 = region_hovedstaden + region_sjaelland,
    DK1 = region_syddanmark + region_midtjylland + region_nordjylland
  ) %>% 
  select(DK1,DK2,year,industry) %>% 
  mutate(
    w_DK1_emp = DK1 / (DK1 + DK2)
  )


####################### Robustness checks for weights (firm numbers) #############
DK36_Regional_Split <- read_excel("DK36 Regional Split.xlsx")

firms_long <- DK36_Regional_Split %>%
  pivot_longer(
    cols = c(`2021`, `2022`, `2023`),
    names_to = "year",
    values_to = "n_firms"
  ) %>%
  mutate(year = as.integer(year))

firms_long <- firms_long %>%
  mutate(
    zone = case_when(
      Region %in% c("Region Hovedstaden", "Region Sjælland") ~ "DK2",
      Region %in% c("Region Syddanmark",
                    "Region Midtjylland",
                    "Region Nordjylland") ~ "DK1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(zone)) %>% 
  group_by(DK36_Code, DK36_Label, year, zone) %>%
  summarise(
    firms = sum(n_firms, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = zone,
    values_from = firms,
    values_fill = 0
  ) %>% 
  mutate(
    w_DK1_firm = DK1 / (DK1 + DK2)
  )







###########################################################################
##### PART TWO                            #################################
###########################################################################



# ============================================================================
# PANEL 2SLS: TOTAL WIND POWER INSTRUMENT WITH TIME & TEMPERATURE FE
# ============================================================================

required_pkgs <- c("dplyr","plm","AER","ivreg","lmtest","sandwich","stargazer","fixest")

invisible(lapply(required_pkgs, library, character.only = TRUE))

# ====================================================================
# 1. DATA PREPARATION
# ====================================================================

# 1.1 Clean and Merge Supply Data (Total Wind Instrument)
# Aggregating wind power by hour and PriceArea
wind_supply <- forecast %>%
  mutate(TimeCET = as.POSIXct(HourUTC, tz = "CET")) %>%
  group_by(TimeCET, PriceArea) %>% 
  select(PriceArea,TimeCET,Wind_DayAhead)

wind_supply <- wind_supply %>%
  tidyr::pivot_wider(
    names_from = PriceArea,
    values_from = Wind_DayAhead,   # <- change to your wind column name if different
    values_fn = dplyr::first
  ) %>%
  rename(Wind_DK1 = DK1, Wind_DK2 = DK2)


# 1.2 Prepare Temperature Data
# Combine DK1 and DK2 temperature data
temp_combined <- temp %>% 
  mutate(TimeCET = HourUTC) %>%
  group_by(TimeCET, PriceArea) %>% 
  select(TimeCET,PriceArea,TempC)

temp_combined <- temp_combined %>%
  tidyr::pivot_wider(
    names_from = PriceArea,
    values_from = TempC,   # change if your column name differs
    values_fn = dplyr::first
  ) %>%
  rename(Temp_DK1 = DK1, Temp_DK2 = DK2)




# 1.3 Prepare Main Consumption Panel

consumption_panel <- industry %>%
  left_join(
    weighted_prices %>% select(TimeDK, DK36Title, P_weighted,DK1,DK2),
    by = c("TimeDK", "DK36Title")
  ) %>%
  left_join(
    wind_supply,
    by = c("TimeDK" = "TimeCET")
  ) %>%
  left_join(
    temp_combined,
    by = c("TimeDK" = "TimeCET")
  ) %>%
  mutate(
    Wind_weighted = w_DK1 * Wind_DK1 + (1 - w_DK1) * Wind_DK2,
    Temp_weighted = w_DK1 * Temp_DK1 + (1 - w_DK1) * Temp_DK2,
    fe_hour = factor(lubridate::hour(TimeDK)),
    fe_month = factor(format(TimeDK, "%Y-%m")),
    fe_year = factor(format(TimeDK, "%Y"))
  ) %>% 
  filter(
    P_weighted > 0,
    Consumption_MWh > 0) %>% 
  mutate(log_cons = log(Consumption_MWh)) %>%
  mutate(log_price = log(P_weighted)) %>% 
  mutate(Date = as.Date(TimeDK)) %>%
  mutate(Date = as.Date(TimeDK)) %>%
  left_join(
    gas %>% mutate(Date = as.Date(Date)),
    by = "Date"
  ) %>% 
  left_join(
    carbon %>% mutate(Date = as.Date(Date)),
    by = "Date"
  ) %>% 
left_join(
  coal %>% mutate(Date = as.Date(Date)),
  by = "Date"
)

na_rows <- consumption_panel %>%
  filter(if_any(everything(), is.na))

nrow(na_rows)



# ====================================================================
# 2. PANEL 2SLS ESTIMATION (FIXED EFFECTS)
# ====================================================================

consumption_panel %>%
  filter(abs(DK1 - DK2) > 0) %>%
  summarise(share_obs = n() / nrow(consumption_panel))

# Specification:
# Dependent Var: ConsumptionkWh
# Endogenous Var: SpotPriceDKK
# Instrument: TotalWind
# Exogenous Controls: temperature
# Fixed Effects: Municipality (Panel ID), Year, Month, Hour

IV_model_het <- feols(
  log_cons ~ Temp_weighted + Gas_EUR_MWh + EUA_EUR_ton + Coal_USD_ton |
    fe_hour + fe_month + fe_year |
    log_price ~ Wind_weighted,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

summary(IV_model_het)



# ====================================================================
# 3. DIAGNOSTICS & RESULTS
# ====================================================================


summary(IV_model_het, diagnostics = TRUE)

etable(
  IV_model_het,
  vcov = ~ fe_month,
  keep = "%fit_log_price"
)









