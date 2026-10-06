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
library(fixest)
library(readr)
library(ggplot2)
library(rlang)
library(tibble)
library(patchwork)
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

############################# Compute weighted distributions based on annual consumption #### 

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
    w_DK1_c = if_else(DK1 + DK2 > 0, DK1 / (DK1 + DK2), NA_real_)
  ) %>%
  select(Year, DK10Title, w_DK1_c)


##### Add weights to industry consumption
industry <- industry %>%
  mutate(Year = as.integer(format(TimeDK, "%Y"))) %>%
  left_join(annual_zone_share, by = c("Year", "DK10Title"))



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
  ) %>% 
  mutate(
    IndustryCode  = str_extract(industry, "^[A-Z]+"),
    IndustryTitle = str_remove(industry, "^[A-Z]+\\s+")
  )


length(unique(industry$DK36Code))
length(unique(total_employees$IndustryCode))


dk36_mapping <- tibble(
  DK36_group = unique(industry$DK36Code)
) %>%
  filter(DK36_group != "-") %>%
  mutate(
    IndustryCode = str_split(DK36_group, "_")
  ) %>%
  unnest(IndustryCode)

total_employees <- total_employees %>%
  left_join(dk36_mapping, by = "IndustryCode") %>%
  group_by(DK36_group, year) %>%
  summarise(
    DK1 = sum(DK1, na.rm = TRUE),
    DK2 = sum(DK2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    w_DK1_emp = DK1 / (DK1 + DK2)
  )


total_employees <- total_employees %>%
  bind_rows(
    total_employees %>%
      filter(year == 2024) %>%
      mutate(year = 2025)
  )
  
employment_weights <- total_employees %>%
  group_by(year, DK36_group) %>%
  summarise(
    DK1 = sum(DK1, na.rm = TRUE),
    DK2 = sum(DK2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    w_DK1_emp = DK1 / (DK1 + DK2)
  )
  
industry <- industry %>%
  mutate(Year = as.integer(format(TimeUTC, "%Y"))) %>%
  left_join(
    employment_weights,
    by = c("Year" = "year", "DK36Code" = "DK36_group")
  ) %>%
  select(-DK1, -DK2)

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


firms_long <- firms_long %>%
  left_join(dk36_mapping, by = c("DK36_Code" = "IndustryCode")) %>%
  filter(!is.na(DK36_group)) %>%
  group_by(year, DK36_group) %>%
  summarise(
    DK1 = sum(DK1, na.rm = TRUE),
    DK2 = sum(DK2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    w_DK1_firm = DK1 / (DK1 + DK2)
  )


firms_long <- firms_long %>%
  bind_rows(
    firms_long %>% filter(year == 2023) %>% mutate(year = 2024),
    firms_long %>% filter(year == 2023) %>% mutate(year = 2025)
  ) %>% 
  select(w_DK1_firm,year,DK36_group)



industry <- industry %>%
  mutate(Year = as.integer(format(TimeUTC, "%Y"))) %>%
  left_join(
    firms_long,
    by = c("Year" = "year", "DK36Code" = "DK36_group")
  )


#############################Compute weighted prices ####################
industry <- industry %>%
  filter(!DK36Code %in% c("-", "PR"))

prices <- prices %>% arrange(HourDK, PriceArea)

prices_wide <- prices %>%
  select(HourDK,HourUTC, PriceArea, SpotPriceEUR) %>%
  tidyr::pivot_wider(
    names_from  = PriceArea,
    values_from = SpotPriceEUR,
    values_fn   = dplyr::first
  )


weighted_prices <- industry %>%
  select(TimeUTC,DK10Title, DK19Title, DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  distinct() %>%
  left_join(prices_wide, by = c("TimeUTC" = "HourUTC")) %>%
  mutate(
    P_weighted_c    = w_DK1_c      * DK1 + (1 - w_DK1_c)      * DK2,
    P_weighted_emp  = w_DK1_emp  * DK1 + (1 - w_DK1_emp)  * DK2,
    P_weighted_firm = w_DK1_firm * DK1 + (1 - w_DK1_firm) * DK2
  ) %>%
  select(TimeUTC, DK10Title, DK19Title, DK36Title,DK1,DK2,
         P_weighted_c, P_weighted_emp, P_weighted_firm, w_DK1_c, w_DK1_emp, w_DK1_firm) %>% 
  rename(DK1_P = DK1, DK2_P = DK2 )

weighted_prices %>%
  summarise(
    miss_c    = sum(is.na(w_DK1_c)),
    miss_emp  = sum(is.na(w_DK1_emp)),
    miss_firm = sum(is.na(w_DK1_firm))
  )

weighted_prices %>%
  summarise(
    share_neg = mean(P_weighted_c <= 0, na.rm = TRUE)
  )

############################ Visualise the difference in the weights ##########

w_tbl <- industry %>%
  filter(!DK36Code %in% c("-", "PR")) %>%
  distinct(Year, DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm)

w_long <- w_tbl %>%
  pivot_longer(
    cols = c(w_DK1_c, w_DK1_emp, w_DK1_firm),
    names_to = "weight_type",
    values_to = "w_DK1"
  ) %>%
  mutate(weight_type = recode(weight_type,
                              w_DK1_c = "Consumption",
                              w_DK1_emp = "Employment",
                              w_DK1_firm = "Firms"))

ggplot(w_tbl, aes(x = Year, colour = DK36Title)) +
  geom_density() +
  labs(x = "DK1 exposure weight (w_DK1)", y = "Density", colour = "Weight proxy") +
  theme_minimal()

w_diff <- w_tbl %>%
  filter(Year == 2022) %>% 
  mutate(
    d_emp  = w_DK1_emp  - w_DK1_c,
    d_firm = w_DK1_firm - w_DK1_c
  ) %>%
  pivot_longer(c(d_emp, d_firm), names_to = "comparison", values_to = "diff") %>%
  mutate(comparison = recode(comparison,
                             d_emp = "Employment − Consumption",
                             d_firm = "Firms − Consumption"))

ggplot(w_diff, aes(x = diff, y = reorder(DK36Title, diff), colour = comparison)) +
  geom_point(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference in DK1 exposure weight", y = NULL, colour = NULL) +
  theme_minimal()

w_scatter <- w_tbl %>% filter(Year == max(Year, na.rm = TRUE))  # latest year

p1 <- ggplot(w_scatter, aes(x = w_DK1_c, y = w_DK1_emp)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal() +
  labs(x = "w_DK1 (Consumption)", y = "w_DK1 (Employment)") +
  theme_minimal()

p2 <- ggplot(w_scatter, aes(x = w_DK1_c, y = w_DK1_firm)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal() +
  labs(x = "w_DK1 (Consumption)", y = "w_DK1 (Firms)") +
  theme_minimal()

p3 <- ggplot(w_scatter, aes(x = w_DK1_emp, y = w_DK1_firm)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal() +
  labs(x = "w_DK1 (Employment)", y = "w_DK1 (Firms)") +
  theme_minimal()


p1
p2
p3

w_year <- w_tbl %>%
  group_by(Year) %>%
  summarise(
    mean_abs_emp  = mean(abs(w_DK1_emp  - w_DK1_c), na.rm = TRUE),
    mean_abs_firm = mean(abs(w_DK1_firm - w_DK1_c), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-Year, names_to = "comparison", values_to = "mean_abs_diff") %>%
  mutate(comparison = recode(comparison,
                             mean_abs_emp  = "Employment vs Consumption",
                             mean_abs_firm = "Firms vs Consumption"))

ggplot(w_year, aes(x = Year, y = mean_abs_diff, colour = comparison)) +
  geom_line() +
  geom_point() +
  labs(x = NULL, y = "Mean absolute difference in w_DK1", colour = NULL) +
  theme_minimal()

w_diff_year <- w_tbl %>%
  mutate(
    d_emp  = abs(w_DK1_emp  - w_DK1_c),
    d_firm = abs(w_DK1_firm - w_DK1_c)
  ) %>%
  pivot_longer(c(d_emp, d_firm),
               names_to = "comparison",
               values_to = "abs_diff")

ggplot(w_diff_year,
       aes(x = factor(Year),
           y = abs_diff,
           fill = comparison)) +
  geom_boxplot(position = position_dodge(0.8)) +
  labs(x = NULL,
       y = "Absolute difference in DK1 weight",
       fill = NULL) +
  theme_minimal()

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
  group_by(HourUTC, PriceArea) %>% 
  select(PriceArea,HourUTC,Wind_DayAhead)

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
  group_by(HourUTC, PriceArea) %>% 
  select(HourUTC,PriceArea,TempC)

temp_combined <- temp_combined %>%
  tidyr::pivot_wider(
    names_from = PriceArea,
    values_from = TempC,   # change if your column name differs
    values_fn = dplyr::first
  ) %>%
  rename(Temp_DK1 = DK1, Temp_DK2 = DK2)


# 1.2.1 combine temp and wind into one dataframe. 

wind_temp <- temp_combined %>%
  select(HourUTC, Temp_DK1, Temp_DK2) %>%
  left_join(
    wind_supply %>%
      select(HourUTC, Wind_DK1, Wind_DK2),
    by = "HourUTC"
  )

industry <- industry %>%
  left_join(
    wind_temp,
    by = c("TimeUTC" = "HourUTC")
  )


industry %>%
  summarise(
    n = n(),
    n_na_wind1 = sum(is.na(Wind_DK1)),
    share_na_wind1 = mean(is.na(Wind_DK1))
  )


industry <- industry %>%
  mutate(
    # Wind
    Wind_c    = w_DK1_c    * Wind_DK1 + (1 - w_DK1_c)    * Wind_DK2,
    Wind_emp  = w_DK1_emp  * Wind_DK1 + (1 - w_DK1_emp)  * Wind_DK2,
    Wind_firm = w_DK1_firm * Wind_DK1 + (1 - w_DK1_firm) * Wind_DK2,
    
    # Temperature
    Temp_c    = w_DK1_c    * Temp_DK1 + (1 - w_DK1_c)    * Temp_DK2,
    Temp_emp  = w_DK1_emp  * Temp_DK1 + (1 - w_DK1_emp)  * Temp_DK2,
    Temp_firm = w_DK1_firm * Temp_DK1 + (1 - w_DK1_firm) * Temp_DK2
  )



# 1.3 Prepare Main Consumption Panel

consumption_panel <- industry %>%
  left_join(
    weighted_prices %>%
      select(TimeUTC, DK36Title,
             P_weighted_c, P_weighted_emp, P_weighted_firm,
             DK1_P, DK2_P),
    by = c("TimeUTC", "DK36Title")
  ) %>% mutate(
    fe_hour = factor(lubridate::hour(TimeUTC)),
    fe_month = factor(format(TimeUTC, "%Y-%m")),
    fe_year = factor(format(TimeUTC, "%Y"))
  )%>% 
  filter(
    P_weighted_c > 0,
    P_weighted_emp > 0,
    P_weighted_firm > 0,
    Consumption_MWh > 0)%>% 
  mutate(Date = as.Date(TimeUTC)) %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  left_join(
    gas %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, Gas_EUR_MWh),
    by = "Date"
  ) %>%
  left_join(
    carbon %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, EUA_EUR_ton),
    by = "Date"
  ) %>%
  left_join(
    coal %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, Coal_USD_ton),
    by = "Date"
  ) %>%
  mutate(
    log_gas    = log(Gas_EUR_MWh),
    log_carbon = log(EUA_EUR_ton),
    log_coal   = log(Coal_USD_ton)
  ) %>% 
  mutate(
    log_consumption = log(Consumption_MWh),
    log_P_c = log(P_weighted_c),
    log_P_emp = log(P_weighted_emp),
    log_P_firm = log(P_weighted_firm)
  )




consumption_panel <- industry %>%
  left_join(
    weighted_prices %>%
      select(TimeUTC, DK36Title,
             P_weighted_c, P_weighted_emp, P_weighted_firm,
             DK1_P, DK2_P),
    by = c("TimeUTC", "DK36Title")
  ) %>% mutate(
    fe_hour = factor(lubridate::hour(TimeUTC)),
    fe_month = factor(format(TimeUTC, "%Y-%m")),
    fe_year = factor(format(TimeUTC, "%Y")),
    fe_week = factor(format(TimeUTC, "%Y-%U"))
  )%>% 
  filter(
    P_weighted_c > 0,
    P_weighted_emp > 0,
    P_weighted_firm > 0,
    Consumption_MWh > 0)%>% 
  mutate(Date = as.Date(TimeUTC)) %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  left_join(
    gas %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, Gas_EUR_MWh),
    by = "Date"
  ) %>%
  left_join(
    carbon %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, EUA_EUR_ton),
    by = "Date"
  ) %>%
  left_join(
    coal %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, Coal_USD_ton),
    by = "Date"
  ) %>%
  mutate(
    log_gas    = log(Gas_EUR_MWh),
    log_carbon = log(EUA_EUR_ton),
    log_coal   = log(Coal_USD_ton)
  ) %>% 
  mutate(
    log_consumption = log(Consumption_MWh),
    log_P_c = log(P_weighted_c),
    log_P_emp = log(P_weighted_emp),
    log_P_firm = log(P_weighted_firm)
  )


# ====================================================================
# 2. PANEL 2SLS ESTIMATION (FIXED EFFECTS) #################
r

IV_model_het_DK10 <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

IV_model_het_DK10_W <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36Title
)


IV_model_het_Emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_emp ~ Wind_emp,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

IV_model_het_Emp_w <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_emp ~ Wind_emp,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36Title
)


IV_model_het_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_firm ~ Wind_firm,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

IV_model_het_firm_w <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_firm ~ Wind_firm,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36Title
)



# ====================================================================
# 3. DIAGNOSTICS & RESULTS
# ====================================================================
######### Estimates ###############
summary(IV_model_het_DK10)
# ====================================================================
######### F stats ###################################################

F_stats <- sapply(as.list(IV_model_het_DK10), function(m) {
  as.numeric(fitstat(m, "ivf.stat"))
})

summary(F_stats)
min(F_stats)


ivwald_by_ind <- sapply(as.list(IV_model_het_DK10), function(m) {
  as.numeric(fitstat(m, "ivwald.stat"))
})

summary(ivwald_by_ind)
min(ivwald_by_ind, na.rm = TRUE)

# ===================================================================
########## VISUALS FOR DIFFERENCE IN ESTIMATES #######################

extract_split_iv <- function(fx_multi, term, weight_label) {
  # fx_multi is fixest_multi from split=
  # term is the endogenous regressor name in stage 2, e.g. "fit_log_P_c"
  # (fixest names it fit_<endogenous>)
  
  models <- as.list(fx_multi)
  ids    <- names(models)  # split labels, e.g. DK36Title categories
  
  map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    ct <- fixest::coeftable(m)  # matrix: Estimate, Std. Error, t value, Pr(>|t|)
    
    if (!(term %in% rownames(ct))) return(NULL)
    
    est <- ct[term, "Estimate"]
    se  <- ct[term, "Std. Error"]
    
    tibble(
      industry = ids[i],
      estimate = est,
      se = se,
      conf.low  = est - 1.96 * se,
      conf.high = est + 1.96 * se,
      weight = weight_label
    )
  })
}

res_c    <- extract_split_iv(IV_model_het_DK10, term = "fit_log_P_c",    weight_label = "Consumption")
res_emp  <- extract_split_iv(IV_model_het_Emp,  term = "fit_log_P_emp",  weight_label = "Employment")
res_firm <- extract_split_iv(IV_model_het_firm, term = "fit_log_P_firm", weight_label = "Firm")

results_all <- bind_rows(res_c, res_emp, res_firm)


ggplot(results_all,
       aes(x = estimate,
           y = reorder(industry, estimate),
           color = weight)) +
  geom_point(position = position_dodge(width = 0.6)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.6),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "IV price elasticity (2SLS coefficient on log price)",
       y = NULL,
       color = "Weights") +
  theme_minimal()

diff_df <- results_all %>%
  select(industry, weight, estimate) %>%
  tidyr::pivot_wider(names_from = weight, values_from = estimate) %>%
  mutate(
    d_emp  = Employment - Consumption,
    d_firm = Firm - Consumption
  ) %>%
  tidyr::pivot_longer(c(d_emp, d_firm), names_to = "comparison", values_to = "diff")

ggplot(diff_df, aes(x = diff, y = reorder(industry, diff), color = comparison)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference in elasticity vs consumption weights", y = NULL, color = NULL) +
  theme_minimal()

extract_split_iv <- function(fx_multi, term, weight_label) {
  
  models <- as.list(fx_multi)
  ids    <- names(models)
  
  map_dfr(seq_along(models), function(i) {
    m  <- models[[i]]
    ct <- fixest::coeftable(m)
    
    if (!(term %in% rownames(ct))) return(NULL)
    
    est <- ct[term, "Estimate"]
    se  <- ct[term, "Std. Error"]
    t   <- ct[term, "t value"]
    p   <- ct[term, "Pr(>|t|)"]
    
    tibble(
      industry = ids[i],
      estimate = est,
      se = se,
      conf.low  = est - 1.96 * se,
      conf.high = est + 1.96 * se,
      p_value = p,
      weight = weight_label
    )
  })
}

res_c    <- extract_split_iv(IV_model_het_DK10, "fit_log_P_c",    "Consumption")
res_emp  <- extract_split_iv(IV_model_het_Emp,  "fit_log_P_emp",  "Employment")
res_firm <- extract_split_iv(IV_model_het_firm, "fit_log_P_firm", "Firm")

results_all <- bind_rows(res_c, res_emp, res_firm)

results_sig <- results_all %>%
  filter(p_value < 0.05)

ggplot(results_sig,
       aes(x = estimate,
           y = reorder(industry, estimate),
           color = weight)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.6),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Significant IV price elasticities",
       y = NULL,
       color = "Weights") +
  theme_minimal()

diff_df <- results_sig %>%
  select(industry, weight, estimate) %>%
  tidyr::pivot_wider(names_from = weight, values_from = estimate) %>%
  mutate(
    d_emp  = Employment - Consumption,
    d_firm = Firm - Consumption
  ) %>%
  tidyr::pivot_longer(c(d_emp, d_firm), names_to = "comparison", values_to = "diff")

ggplot(diff_df, aes(x = diff, y = reorder(industry, diff), color = comparison)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Difference in elasticity vs consumption weights", y = NULL, color = NULL) +
  theme_minimal()


diff_df %>%
  summarise(
    mean_abs_diff = mean(abs(diff), na.rm = TRUE),
    max_abs_diff  = max(abs(diff),  na.rm = TRUE)
  )





# ====================================================================
########## Descriptive statistics.  ####################

vars_for_table <- consumption_panel %>%
  select(
    log_consumption,
    log_P_c, log_P_emp, log_P_firm,
    Wind_c, Wind_emp, Wind_firm,
    Temp_c, Temp_emp, Temp_firm,
    log_gas, log_coal, log_carbon
  )

desc_table <- vars_for_table %>%
  summarise(across(
    everything(),
    list(
      N    = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD   = ~sd(., na.rm = TRUE),
      Min  = ~min(., na.rm = TRUE),
      P25  = ~quantile(., 0.25, na.rm = TRUE),
      Med  = ~median(., na.rm = TRUE),
      P75  = ~quantile(., 0.75, na.rm = TRUE),
      Max  = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  ))


desc_table_clean <- desc_table %>%
  pivot_longer(everything()) %>%
  separate(name, into = c("Variable", "Statistic"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Statistic, values_from = value)



# ====================================================================
######### F-stat and estimates #######################################

get_iv_F <- function(m){
  val <- tryCatch(as.numeric(fitstat(m, "ivf.stat")), error = function(e) NA_real_)
  if (length(val) == 0 || !is.finite(val)) NA_real_ else val
}

extract_iv_table <- function(fx_multi, term, weight_label = NULL) {
  models <- as.list(fx_multi)
  ids    <- names(models)
  
  out <- map_dfr(seq_along(models), function(i) {
    m  <- models[[i]]
    ct <- fixest::coeftable(m)
    
    if (!(term %in% rownames(ct))) return(NULL)
    
    est <- as.numeric(ct[term, "Estimate"])
    se  <- as.numeric(ct[term, "Std. Error"])
    p   <- as.numeric(ct[term, "Pr(>|t|)"])
    
    stars <- case_when(
      p < 0.01 ~ "***",
      p < 0.05 ~ "**",
      p < 0.10 ~ "*",
      TRUE ~ ""
    )
    
    Fstat <- get_iv_F(m)
    
    tibble(
      Industry   = ids[i],
      Elasticity = round(est, 4),
      SE         = round(se, 4),
      Stars      = stars,
      F_stat     = ifelse(is.na(Fstat), NA_real_, round(Fstat, 2))
    )
  })
  
  out %>%
    mutate(Industry = str_remove(Industry, "^sample\\.var:.*?sample:\\s*")) %>%
    { if (!is.null(weight_label)) mutate(., Weight = weight_label) else . }
}

table_c    <- extract_iv_table(IV_model_het_DK10, "fit_log_P_c",    "Consumption")
table_emp  <- extract_iv_table(IV_model_het_Emp,  "fit_log_P_emp",  "Employment")
table_firm <- extract_iv_table(IV_model_het_firm, "fit_log_P_firm", "Firm")

table_all <- bind_rows(table_c, table_emp, table_firm) %>%
  arrange(Industry, Weight)

print(table_all, n = 200)

table_all %>%
  summarise(
    minF = min(F_stat, na.rm = TRUE),
    medF = median(F_stat, na.rm = TRUE),
    maxF = max(F_stat, na.rm = TRUE)
  )

ggplot(table_all,
       aes(x = F_stat,
           y = reorder(Industry, F_stat),
           colour = Weight)) +
  geom_point(size = 2) +
  labs(
    x = "First-Stage F-Statistic",
    y = NULL,
    colour = "Weighting"
  ) +
  theme_minimal()





#=====================================================================
######### Monotonicity ###############################################

mono_hour <- consumption_panel %>%
  arrange(TimeUTC) %>%
  distinct(TimeUTC, Wind_DK1, Wind_DK2, P_weighted_c, DK1_P, DK2_P) %>%
  mutate(
    wind_total = Wind_DK1 + Wind_DK2,
    d_wind  = wind_total - dplyr::lag(wind_total),
    d_pw    = P_weighted_c - dplyr::lag(P_weighted_c),
    d_p1    = DK1_P - dplyr::lag(DK1_P),
    d_p2    = DK2_P - dplyr::lag(DK2_P)
  ) %>%
  filter(!is.na(d_wind), !is.na(d_pw), !is.na(d_p1), !is.na(d_p2))

eps_w <- 1    # 1 MW (adjust if your wind unit is different)
eps_p <- 0.1  # 0.1 EUR/MWh

mono_hour %>%
  summarise(
    share_violation_weighted = mean(d_wind > eps_w & d_pw > eps_p)
  )
mono_hour %>%
  summarise(
    share_violation_DK1 = mean((Wind_DK1 - lag(Wind_DK1)) > eps_w & d_p1 > eps_p, na.rm = TRUE),
    share_violation_DK2 = mean((Wind_DK2 - lag(Wind_DK2)) > eps_w & d_p2 > eps_p, na.rm = TRUE)
  )

mono_hour %>%
  summarise(
    share_congestion_pattern = mean(
      ((Wind_DK1 - dplyr::lag(Wind_DK1)) > eps_w) &
        (d_p1 < -eps_p) &
        (d_p2 > eps_p),
      na.rm = TRUE
    )
  )

ggplot(mono_hour, aes(x = wind_total, y = P_weighted_c)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal()

bin_scatter <- mono_hour %>%
  mutate(bin = ntile(wind_total, 40)) %>%
  group_by(bin) %>%
  summarise(
    wind = mean(wind_total),
    price = mean(P_weighted_c),
    .groups = "drop"
  )

ggplot(bin_scatter, aes(wind, price)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = "Total wind generation (MW)",
    y = "Weighted electricity price (EUR/MWh)"
  ) +
  theme_minimal()

# ====================================================================
######## PLacebo regression ##########################################
consumption_panel <- consumption_panel %>%
  mutate(Wind_c_lead24 = dplyr::lead(Wind_c, 24))

lead_test24 <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c_lead24,
  data = consumption_panel,
  cluster = ~ fe_month
)


lead_test24_het <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c_lead24,
  data = consumption_panel,
  cluster = ~ fe_month, 
  split = ~ DK36Title
)




# =====================================================================
######## Time aggregregated consumption( Not very relevant) ###############################
daily_panel <- consumption_panel %>%
  mutate(date = as.Date(TimeUTC)) %>%
  group_by(DK36Title, date) %>%
  summarise(
    consumption = sum(Consumption_MWh, na.rm = TRUE),
    price = mean(P_weighted_c, na.rm = TRUE),
    wind = mean(Wind_c, na.rm = TRUE),
    temp = mean(Temp_c, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_consumption = log(consumption),
    log_price = log(price)
  )


iv_daily <- feols(
  log_consumption ~ temp + log(gas) + log(coal) + log(carbon) |
    DK36Title + date |
    log_price ~ wind,
  data = daily_panel,
  cluster = ~ date
)

summary(iv_daily)



# ====================================================================
######## Rebound effect #################################################
### Build lagged regressors: 
consumption_panel <- consumption_panel %>%
  arrange(DK36Title, TimeUTC) %>%
  group_by(DK36Title) %>%
  mutate(
    cons_lead1  = dplyr::lead(log_consumption, 1),
    cons_lead2  = dplyr::lead(log_consumption, 2),
    cons_lead3  = dplyr::lead(log_consumption, 3),
    cons_lead6  = dplyr::lead(log_consumption, 6),
    cons_lead12 = dplyr::lead(log_consumption, 12)
  ) %>%
  ungroup()




irf0 <- feols(
  log_consumption ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month
)

irf1 <- feols(
  cons_lead1 ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month
)

irf2 <- feols(
  cons_lead2 ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month
)

irf3 <- feols(
  cons_lead3 ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month
)

irf6 <- feols(
  cons_lead6 ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month
)

irf12 <- feols(
  cons_lead12 ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month
)

irf_coefs <- c(
  h0  = coef(irf0)["Wind_c"],
  h1  = coef(irf1)["Wind_c"],
  h2  = coef(irf2)["Wind_c"],
  h3  = coef(irf3)["Wind_c"],
  h6  = coef(irf6)["Wind_c"],
  h12 = coef(irf12)["Wind_c"]
)

irf_coefs


# ======================================================================
########### POWER #####################################################
source("sim_power_iv_clustering.R")
source("iv_sim_prep.R")

sector_test <- "Føde-, drikke- og tobaksvareindustri"
consumption_panel_sec <- consumption_panel %>%
  filter(DK36Title == sector_test)

first_stage_sec <- feols(
  log_P_c ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel_sec,
  cluster = ~ fe_week
)

iv_sec <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel_sec,
  cluster = ~ fe_week
)


coef(iv_sec)["fit_log_P_c"]
coef(first_stage_sec)["Wind_c"]

