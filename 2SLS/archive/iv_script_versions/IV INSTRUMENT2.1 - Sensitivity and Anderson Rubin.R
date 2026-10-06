### IV instrument ### 
setwd("~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")
setwd("C:/Users/jespe/OneDrive - CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")

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

library(sandwich)
library(dplyr)
library(plm)
library(AER)
library(ivreg)
library(fixest)
library(lmtest)
library(stargazer)

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

# ====================================================================
# 2. PANEL 2SLS ESTIMATION (FIXED EFFECTS)
# ====================================================================

consumption_panel %>%
  filter(abs(DK1_P - DK2_P) > 0) %>%
  summarise(share_obs = n() / nrow(consumption_panel))

# Specification:
# Dependent Var: ConsumptionkWh
# Endogenous Var: SpotPriceDKK
# Instrument: TotalWind
# Exogenous Controls: temperature
# Fixed Effects: Municipality (Panel ID), Year, Month, Hour

IV_model_het_DK10 <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_month,
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

IV_model_het_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_firm ~ Wind_firm,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)


# Wu Hausman Test

sapply(as.list(IV_model_het_DK10), function(m) {
  fitstat(m, ~ivwald + wh)  # Wu-Hausman test
})

sapply(as.list(IV_model_het_Emp), function(m) {
  fitstat(m, ~ivwald + wh)  # Wu-Hausman test
})


sapply(as.list(IV_model_het_firm), function(m) {
  fitstat(m, ~ivwald + wh)  # Wu-Hausman test
})

# ====================================================================
# 3. DIAGNOSTICS & RESULTS
# ====================================================================
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

desc_table_clean

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


##### Comparisons OLS, ITTT ####

# OLS benchmark (same spec minus the instrument)
OLS_benchmark_c <- feols(
  log_consumption ~ log_P_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~fe_month,
  split = ~DK36Title
)

OLS_benchmark_emp <- feols(
  log_consumption ~ log_P_emp + Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~fe_month,
  split = ~DK36Title
)

OLS_benchmark_firm <- feols(
  log_consumption ~ log_P_firm + Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~fe_month,
  split = ~DK36Title
)


# Reduced-form / ITT: log_consumption on the instrument directly
ITT_model_c <- feols(
  log_consumption ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~fe_month,
  split = ~DK36Title
)

ITT_model_emp <- feols(
  log_consumption ~ Wind_emp + Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~fe_month,
  split = ~DK36Title
)

ITT_model_firm <- feols(
  log_consumption ~ Wind_firm + Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~fe_month,
  split = ~DK36Title
)



##### Report Weak-Instrument-Robust Confidence Intervals ####
# Extract and verify robust F-statistics
# fixest::fitstat gives the Wald F on excluded instruments
# With cluster, this should already be cluster-robust
sapply(as.list(IV_model_het_DK10), function(m) {
  fitstat(m, ~ivwald)  # cluster-robust Wald stat
})

sapply(as.list(IV_model_het_Emp), function(m) {
  fitstat(m, ~ivwald)  # cluster-robust Wald stat
})

sapply(as.list(IV_model_het_firm), function(m) {
  fitstat(m, ~ivwald)  # cluster-robust Wald stat
})



# ================ ANDERSON-RUBIN CONFIDENCE SETS =====================


install.packages("ivDiag")
library(ivDiag)

# For a single industry subset (example for one split):
subset_data <- consumption_panel %>% 
  filter(DK36Title == "Industri, råstofindvinding og forsyningsvirksomhed")

# AR test using AER::ivreg for compatibility
library(AER)
# CORRECT pooled specification — NO industry FE
iv_aer_c <- ivreg(
  log_consumption ~ log_P_c + Temp_c + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) |
    Wind_c + Temp_c + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month),
  data = consumption_panel
)

iv_aer_emp <- ivreg(
  log_consumption ~ log_P_emp + Temp_emp + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) |
    Wind_emp + Temp_emp + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month),
  data = consumption_panel
)

iv_aer_firm <- ivreg(
  log_consumption ~ log_P_firm + Temp_firm + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month) |
    Wind_firm + Temp_firm + log_gas + log_coal + log_carbon +
    factor(fe_hour) + factor(fe_month),
  data = consumption_panel
)

library(AER)
library(dplyr)
library(purrr)

# --- Method: Grid-search inversion of the AR test ---
# For each candidate beta0, test H0: beta = beta0
# by regressing (Y - beta0 * D) on Z and controls.
# If the coefficient on Z is insignificant, beta0 is in the AR confidence set.

# 1. CORRECTED AR function — NO industry FE (matches ivreg spec)
compute_AR_cs <- function(data, log_price_var, wind_var, temp_var,
                          beta_grid = seq(-0.5, 0.1, by = 0.001),
                          alpha = 0.05) {
  
  Y <- data$log_consumption
  D <- data[[log_price_var]]
  Z <- data[[wind_var]]
  Temp <- data[[temp_var]]
  Gas <- data$log_gas
  Coal <- data$log_coal
  Carbon <- data$log_carbon
  fe_h <- factor(data$fe_hour)
  fe_m <- factor(data$fe_month)
  
  ar_results <- map_dfr(beta_grid, function(b0) {
    Y_adj <- Y - b0 * D
    
    fit <- lm(Y_adj ~ Z + Temp + Gas + Coal + Carbon + fe_h + fe_m)
    
    cf <- summary(fit)$coefficients
    if (!"Z" %in% rownames(cf)) return(NULL)
    
    tibble(
      beta0   = b0,
      t_stat  = cf["Z", "t value"],
      p_value = cf["Z", "Pr(>|t|)"],
      in_CS   = cf["Z", "Pr(>|t|)"] >= alpha
    )
  })
  
  ar_results
}

# 2. RUN the grid searches
ar_consumption <- compute_AR_cs(
  consumption_panel, "log_P_c", "Wind_c", "Temp_c",
  beta_grid = seq(-0.35, -0.05, by = 0.001)
)

ar_employment <- compute_AR_cs(
  consumption_panel, "log_P_emp", "Wind_emp", "Temp_emp",
  beta_grid = seq(-0.35, -0.05, by = 0.001)
)

ar_firm <- compute_AR_cs(
  consumption_panel, "log_P_firm", "Wind_firm", "Temp_firm",
  beta_grid = seq(-0.35, -0.05, by = 0.001)
)

# 3. EXTRACT results
ar_summary <- tibble(
  Weight = c("Consumption", "Employment", "Firm"),
  Point_Est = c(
    coef(iv_aer_c)["log_P_c"],
    coef(iv_aer_emp)["log_P_emp"],
    coef(iv_aer_firm)["log_P_firm"]
  ),
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

# 4. VERIFY point estimates inside AR sets
cat("\nConsistency checks:\n")
cat("  Consumption:", coef(iv_aer_c)["log_P_c"] >= ar_summary$AR_Lower[1] & 
      coef(iv_aer_c)["log_P_c"] <= ar_summary$AR_Upper[1], "\n")
cat("  Employment:", coef(iv_aer_emp)["log_P_emp"] >= ar_summary$AR_Lower[2] & 
      coef(iv_aer_emp)["log_P_emp"] <= ar_summary$AR_Upper[2], "\n")
cat("  Firm:", coef(iv_aer_firm)["log_P_firm"] >= ar_summary$AR_Lower[3] & 
      coef(iv_aer_firm)["log_P_firm"] <= ar_summary$AR_Upper[3], "\n")
# --- Extract the confidence set bounds ---
ar_cs <- ar_consumption %>% filter(in_CS == TRUE)

cat("Anderson-Rubin 95% Confidence Set (Consumption weights):\n")
cat("  Lower bound:", min(ar_cs$beta0), "\n")
cat("  Upper bound:", max(ar_cs$beta0), "\n")
cat("  2SLS point estimate: -0.2019\n")
cat("  AR CS is connected:", 
    all(diff(which(ar_consumption$in_CS)) == 1), "\n")

# --- Visualise ---
library(ggplot2)

ggplot(ar_consumption, aes(x = beta0, y = p_value)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = -0.2019, linetype = "dotted", colour = "blue") +
  annotate("text", x = -0.2019, y = 0.8, label = "2SLS estimate",
           hjust = -0.1, colour = "blue", size = 3) +
  annotate("text", x = -0.35, y = 0.06, label = "α = 0.05",
           hjust = 0, colour = "red", size = 3) +
  labs(
    x = expression("Candidate elasticity " * beta[0]),
    y = "AR test p-value",
    title = "Anderson-Rubin Confidence Set: Pooled Specification"
  ) +
  theme_minimal()




#====== Bias ====== ####
install.packages("sensemakr")
library(sensemakr)


for (ind in unique(consumption_panel$DK36Title)) {
  sub <- consumption_panel %>% filter(DK36Title == ind)
  itt_lm <- lm(log_consumption ~ Wind_c + Temp_c + log_gas + 
                 log_coal + log_carbon, data = sub)
  
  sens <- sensemakr(
    model = itt_lm,
    treatment = "Wind_c",
    benchmark_covariates = "Temp_c",  # benchmark against temperature
    kd = 1:3  # multiples of benchmark strength
  )
  
  summary(sens)
  plot(sens)
}

for (ind in unique(consumption_panel$DK36Title)) {
  sub <- consumption_panel %>% filter(DK36Title == ind)
  itt_lm <- lm(log_consumption ~ Wind_emp + Temp_emp + log_gas + 
                 log_coal + log_carbon, data = sub)
  
  sens <- sensemakr(
    model = itt_lm,
    treatment = "Wind_emp",
    benchmark_covariates = "Temp_emp",  # benchmark against temperature
    kd = 1:3  # multiples of benchmark strength
  )
  
  summary(sens)
  plot(sens)
}

for (ind in unique(consumption_panel$DK36Title)) {
  sub <- consumption_panel %>% filter(DK36Title == ind)
  itt_lm <- lm(log_consumption ~ Wind_firm + Temp_firm + log_gas + 
                 log_coal + log_carbon, data = sub)
  
  sens <- sensemakr(
    model = itt_lm,
    treatment = "Wind_firm",
    benchmark_covariates = "Temp_firm",  # benchmark against temperature
    kd = 1:3  # multiples of benchmark strength
  )
  
  summary(sens)
  plot(sens)
}

# ====== SENSEMAKR SUMMARY TABLE: Sensitivity Analysis for IV Reduced-Form =====
# Felton & Stewart (2026) Checklist Item 4a


library(sensemakr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)


extract_sens_stats <- function(data, industry_col, industry_val,
                               wind_var, temp_var, weight_label,
                               kd_max = 5, alpha = 0.05) {
  
  sub <- data %>% filter(.data[[industry_col]] == industry_val)
  
  # Fit reduced-form OLS: outcome on instrument + controls
  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon"
  ))
  
  itt_lm <- tryCatch(lm(fml, data = sub), error = function(e) NULL)
  if (is.null(itt_lm)) return(NULL)
  
  # Run sensemakr
  sens <- tryCatch(
    sensemakr(
      model = itt_lm,
      treatment = wind_var,
      benchmark_covariates = temp_var,
      kd = 1:kd_max,
      alpha = alpha
    ),
    error = function(e) NULL
  )
  if (is.null(sens)) return(NULL)
  
  # --- Extract core sensitivity statistics ---
  rv_q1       <- sens$sensitivity_stats$rv_q
  rv_q1_alpha <- sens$sensitivity_stats$rv_qa
  partial_r2  <- sens$sensitivity_stats$r2yd.x
  t_original  <- sens$sensitivity_stats$t_statistic
  
  # --- Find critical benchmark multiplier ---
  # The multiplier k at which the adjusted t-stat crosses the significance threshold
  bounds <- sens$bounds
  
  if (!is.null(bounds) && nrow(bounds) > 0) {
    # Extract adjusted t-values for each kd multiplier
    # sensemakr stores bounds with columns: bound_label, r2dz.x, r2yz.dx,
    # treatment, adjusted_estimate, adjusted_se, adjusted_t, adjusted_lower_CI, adjusted_upper_CI
    
    t_crit <- qt(1 - alpha / 2, df = itt_lm$df.residual)
    
    # Find the first kd where |adjusted_t| < t_crit
    bounds_df <- as.data.frame(bounds)
    
    # The bound_label contains the multiplier, e.g. "1x Temp_c"
    bounds_df$kd <- as.numeric(str_extract(bounds_df$bound_label, "^[0-9]+"))
    
    sig_lost_row <- bounds_df %>%
      filter(abs(adjusted_t) < t_crit) %>%
      slice_min(kd, n = 1)
    
    if (nrow(sig_lost_row) > 0) {
      critical_kd <- sig_lost_row$kd[1]
    } else {
      # Significance not lost within kd_max range
      critical_kd <- paste0(">", kd_max)
    }
  } else {
    critical_kd <- NA
  }
  
  # --- Return summary row ---
  tibble(
    Industry     = industry_val,
    Weight       = weight_label,
    t_original   = round(t_original, 2),
    Partial_R2   = round(partial_r2, 4),
    RV_q1        = round(rv_q1, 4),
    RV_q1_alpha  = round(rv_q1_alpha, 4),
    Critical_kd  = as.character(critical_kd),
    N            = nrow(sub)
  )
}


# 2. RUN ACROSS ALL INDUSTRIES AND WEIGHTING SCHEMES

industries <- unique(consumption_panel$DK36Title)

# --- Consumption weights (main specification) ---
sens_firm <- map_dfr(industries, function(ind) {
  extract_sens_stats(
    data          = consumption_panel,
    industry_col  = "DK36Title",
    industry_val  = ind,
    wind_var      = "Wind_firm",
    temp_var      = "Temp_firm",
    weight_label  = "Consumption",
    kd_max        = 5
  )
})

# --- Employment weights (robustness) ---
sens_employment <- map_dfr(industries, function(ind) {
  extract_sens_stats(
    data          = consumption_panel,
    industry_col  = "DK36Title",
    industry_val  = ind,
    wind_var      = "Wind_emp",
    temp_var      = "Temp_emp",
    weight_label  = "Employment",
    kd_max        = 5
  )
})

# --- Firm weights (robustness) ---
sens_consumption <- map_dfr(industries, function(ind) {
  extract_sens_stats(
    data          = consumption_panel,
    industry_col  = "DK36Title",
    industry_val  = ind,
    wind_var      = "Wind_c",
    temp_var      = "Temp_c",
    weight_label  = "Firm",
    kd_max        = 5
  )
})


# 3. MAIN TABLE: Consumption weights (for thesis body)


# Assign robustness tier
sens_firm <- sens_firm %>%
  mutate(
    Robustness_Tier = case_when(
      RV_q1_alpha >= 0.10  ~ "High",
      RV_q1_alpha >= 0.03  ~ "Moderate",
      TRUE                 ~ "Fragile"
    )
  ) %>%
  arrange(desc(RV_q1))

cat("\n============================================================\n")
cat("TABLE: Sensitivity Analysis — Consumption Weights (Main Spec)\n")
cat("============================================================\n\n")
print(sens_consumption, n = 50)


# 4. COMBINED TABLE: All three weighting schemes (for appendix)


sens_all <- bind_rows(sens_firm, sens_employment, sens_consumption) %>%
  arrange(Industry, Weight)

cat("\n============================================================\n")
cat("TABLE: Sensitivity Analysis — All Weighting Schemes\n")
cat("============================================================\n\n")
print(sens_all, n = 200)

# 5. CROSS-WEIGHT CONSISTENCY CHECK

# Compare RV values across weighting schemes for each industry
consistency_check <- sens_all %>%
  select(Industry, Weight, RV_q1, RV_q1_alpha) %>%
  pivot_wider(
    names_from  = Weight,
    values_from = c(RV_q1, RV_q1_alpha),
    names_sep   = "_"
  ) %>%
  mutate(
    RV_range = round(
      pmax(RV_q1_Consumption, RV_q1_Employment, RV_q1_Firm, na.rm = TRUE) -
        pmin(RV_q1_Consumption, RV_q1_Employment, RV_q1_Firm, na.rm = TRUE), 4
    )
  ) %>%
  arrange(desc(RV_q1_Consumption))

cat("\n============================================================\n")
cat("TABLE: Cross-Weight Consistency of Robustness Values\n")
cat("============================================================\n\n")
print(consistency_check, n = 50)

# 6. VISUALISATION: Robustness Values by Industry

# --- Plot 1: RV comparison across weights ---
plot_rv <- sens_all %>%
  mutate(Industry = str_wrap(Industry, width = 30)) %>%
  ggplot(aes(x = RV_q1, y = reorder(Industry, RV_q1), colour = Weight)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", colour = "grey50") +
  annotate("text", x = 0.052, y = 1, label = "RV = 5%",
           hjust = 0, size = 3, colour = "grey40") +
  labs(
    x = "Robustness Value (q = 1)",
    y = NULL,
    colour = "Weighting Scheme",
    title = "Sensitivity to Unobserved Confounding: Reduced-Form"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

print(plot_rv)

# --- Plot 2: Critical kd multiplier ---
plot_kd <- sens_firm %>%
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
  annotate("text", x = 1.05, y = 1, label = "1× benchmark",
           hjust = 0, size = 3, colour = "red") +
  scale_fill_manual(
    values = c("High" = "#2E86AB", "Moderate" = "#F6AE2D", "Fragile" = "#F26157")
  ) +
  labs(
    x = "Critical Benchmark Multiplier (kd) at Which Significance is Lost",
    y = NULL,
    fill = "Robustness Tier",
    title = "How Many Multiples of Temperature Confounding Nullify Results?"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

print(plot_kd)

# --- Plot 3: Partial R² vs Robustness Value ---
plot_r2_rv <- sens_firm %>%
  mutate(Industry = str_wrap(Industry, width = 20)) %>%
  ggplot(aes(x = Partial_R2, y = RV_q1, label = Industry)) +
  geom_point(aes(colour = Robustness_Tier), size = 3) +
  geom_text(size = 2.5, vjust = -0.8, check_overlap = TRUE) +
  scale_colour_manual(
    values = c("High" = "#2E86AB", "Moderate" = "#F6AE2D", "Fragile" = "#F26157")
  ) +
  labs(
    x = expression("Partial" ~ R^2 ~ "of Wind with Consumption"),
    y = "Robustness Value (q = 1)",
    colour = "Tier",
    title = "Instrument Strength vs. Sensitivity to Confounding"
  ) +
  theme_minimal(base_size = 11)

print(plot_r2_rv)


# 7. SUMMARY STATISTICS FOR THESIS TEXT


cat("\n============================================================\n")
cat("SUMMARY FOR THESIS TEXT\n")
cat("============================================================\n\n")

cat("Number of industries analysed:", nrow(sens_firm), "\n")
cat("  High robustness (RV_alpha >= 0.10):", 
    sum(sens_firm$Robustness_Tier == "High", na.rm = TRUE), "\n")
cat("  Moderate robustness (0.03 <= RV_alpha < 0.10):", 
    sum(sens_firm$Robustness_Tier == "Moderate", na.rm = TRUE), "\n")
cat("  Fragile (RV_alpha < 0.03):", 
    sum(sens_firm$Robustness_Tier == "Fragile", na.rm = TRUE), "\n\n")

cat("RV (q=1) range:", 
    round(min(sens_firm$RV_q1, na.rm = TRUE), 4), "to",
    round(max(sens_firm$RV_q1, na.rm = TRUE), 4), "\n")
cat("RV (q=1, alpha=0.05) range:", 
    round(min(sens_firm$RV_q1_alpha, na.rm = TRUE), 4), "to",
    round(max(sens_firm$RV_q1_alpha, na.rm = TRUE), 4), "\n\n")

# Cross-weight consistency
rv_diffs <- consistency_check$RV_range
cat("Cross-weight RV consistency:\n")
cat("  Mean absolute RV difference:", round(mean(rv_diffs, na.rm = TRUE), 4), "\n")
cat("  Max absolute RV difference:", round(max(rv_diffs, na.rm = TRUE), 4), "\n")

# ======= ECOLOGICAL INFERENCE ============
# 1. WEIGHT DIVERGENCE ANALYSIS
# ============================================================================
# The three weighting schemes embed different assumptions about how firms
# within each DK36 industry are distributed across DK1 and DK2.
# Large divergence between weights signals high within-industry heterogeneity
# in the spatial distribution of economic activity — exactly the kind of
# compositional variation that drives ecological bias.

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# 1. How many rows survived the join?
cat("Rows in eco_merged:", nrow(eco_merged), "\n")

# 2. Check what the industry names look like in each source
cat("\n--- Names in weight_dispersion ---\n")
sort(unique(weight_dispersion$DK36Title))

cat("\n--- Names in elasticity_divergence ---\n")
sort(unique(elasticity_divergence$industry))

# 3. Check for NA/Inf in the join columns
cat("\n--- NAs in eco_merged ---\n")
eco_merged %>% summarise(
  n = n(),
  na_w = sum(is.na(w_range)),
  na_e = sum(is.na(range_elasticity)),
  inf_w = sum(is.infinite(w_range)),
  inf_e = sum(is.infinite(range_elasticity))
)

elasticity_divergence <- elasticity_divergence %>%
  mutate(industry = str_remove(industry, "^.*:\\s*"))


# Check
sort(unique(elasticity_divergence$industry))

# Re-run the join
eco_merged <- weight_dispersion %>%
  select(DK36Title, w_range) %>%
  inner_join(
    elasticity_divergence %>% select(industry, range_elasticity),
    by = c("DK36Title" = "industry")
  )

cat("Rows in eco_merged:", nrow(eco_merged), "\n")

# Now the correlation test should work
eco_cor <- cor.test(eco_merged$w_range, eco_merged$range_elasticity,
                    method = "spearman")
print(eco_cor)


results_all <- results_all %>%
  mutate(industry = str_remove(industry, "^.*sample:\\s*"))

# Extract unique industry-year weight combinations
w_tbl <- consumption_panel %>%
  filter(!DK36Code %in% c("-", "PR")) %>%
  distinct(Year, DK36Title, DK36Code, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  filter(!is.na(w_DK1_c), !is.na(w_DK1_emp), !is.na(w_DK1_firm))

# --- 1a. Pairwise weight correlations ---
# High correlation => spatial distribution of consumption, employment, and firms
# is similar => less scope for ecological bias from weight choice
weight_cors <- w_tbl %>%
  group_by(Year) %>%
  summarise(
    cor_c_emp  = cor(w_DK1_c, w_DK1_emp, use = "complete.obs"),
    cor_c_firm = cor(w_DK1_c, w_DK1_firm, use = "complete.obs"),
    cor_emp_firm = cor(w_DK1_emp, w_DK1_firm, use = "complete.obs"),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("TABLE: Pairwise Correlations Between DK1 Weights (by Year)\n")
cat("============================================================\n")
print(weight_cors)

# --- 1b. Industry-level weight dispersion ---
# For each industry, compute the range across the three weighting schemes.
# Industries with large range are most sensitive to ecological assumptions.
weight_dispersion <- w_tbl %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  mutate(
    w_range = pmax(w_DK1_c, w_DK1_emp, w_DK1_firm) -
      pmin(w_DK1_c, w_DK1_emp, w_DK1_firm),
    w_mean = (w_DK1_c + w_DK1_emp + w_DK1_firm) / 3,
    w_cv   = w_range / (w_mean + 1e-10)  # coefficient of variation (range-based)
  ) %>%
  arrange(desc(w_range))

cat("\n============================================================\n")
cat("TABLE: Weight Dispersion Across Schemes (Latest Year)\n")
cat("============================================================\n")
print(weight_dispersion %>% select(DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm, w_range), n = 35)

cat("\nSummary statistics for weight range across industries:\n")
cat("  Mean range:", round(mean(weight_dispersion$w_range, na.rm = TRUE), 4), "\n")
cat("  Median range:", round(median(weight_dispersion$w_range, na.rm = TRUE), 4), "\n")
cat("  Max range:", round(max(weight_dispersion$w_range, na.rm = TRUE), 4),
    "  (", weight_dispersion$DK36Title[1], ")\n")



# 2. ECOLOGICAL SENSITIVITY: ELASTICITY DIVERGENCE ACROSS WEIGHTS

# The key ecological diagnostic: if the aggregate elasticity were robust to
# within-industry compositional assumptions, estimates should be similar
# across weighting schemes. Large divergence signals ecological sensitivity.

# Requires results_all from IV_INSTRUMENT2_1 (bind_rows of res_c, res_emp, res_firm)
# If not available, reconstruct from the split models:
# res_c    <- extract_split_iv(IV_model_het_DK10, "fit_log_P_c",    "Consumption")
# res_emp  <- extract_split_iv(IV_model_het_Emp,  "fit_log_P_emp",  "Employment")
# res_firm <- extract_split_iv(IV_model_het_firm, "fit_log_P_firm", "Firm")
# results_all <- bind_rows(res_c, res_emp, res_firm)

elasticity_divergence <- results_all %>%
  select(industry, weight, estimate) %>%
  pivot_wider(names_from = weight, values_from = estimate) %>%
  mutate(
    range_elasticity = pmax(Consumption, Employment, Firm, na.rm = TRUE) -
      pmin(Consumption, Employment, Firm, na.rm = TRUE),
    mean_elasticity = (Consumption + Employment + Firm) / 3,
    # Sign consistency: do all three schemes agree on the sign?
    sign_consistent = (sign(Consumption) == sign(Employment)) &
      (sign(Employment) == sign(Firm))
  ) %>%
  arrange(desc(range_elasticity))

cat("\n============================================================\n")
cat("TABLE: Elasticity Divergence Across Weighting Schemes\n")
cat("============================================================\n")
print(elasticity_divergence %>%
        select(industry, Consumption, Employment, Firm,
               range_elasticity, sign_consistent), n = 35)

cat("\nSign consistency across all industries:",
    sum(elasticity_divergence$sign_consistent, na.rm = TRUE), "/",
    nrow(elasticity_divergence), "\n")
cat("Mean elasticity range:", round(mean(elasticity_divergence$range_elasticity, na.rm = TRUE), 4), "\n")
cat("Median elasticity range:", round(median(elasticity_divergence$range_elasticity, na.rm = TRUE), 4), "\n")



# 3. ECOLOGICAL DECOMPOSITION: WEIGHT DIVERGENCE vs ELASTICITY DIVERGENCE

# If ecological bias is operative, industries with larger weight divergence
# should also show larger elasticity divergence. A positive correlation here
# would be direct evidence that aggregation assumptions matter.

eco_merged <- weight_dispersion %>%
  select(DK36Title, w_range) %>%
  inner_join(
    elasticity_divergence %>% select(industry, range_elasticity),
    by = c("DK36Title" = "industry")
  )

eco_cor <- cor.test(eco_merged$w_range, eco_merged$range_elasticity,
                    method = "spearman")

cat("\n============================================================\n")
cat("ECOLOGICAL SENSITIVITY TEST\n")
cat("============================================================\n")
cat("Spearman correlation between weight range and elasticity range:\n")
cat("  rho =", round(eco_cor$estimate, 4), "\n")
cat("  p-value =", format.pval(eco_cor$p.value, digits = 4), "\n")
cat("  Interpretation: ",
    ifelse(eco_cor$p.value < 0.05,
           "Significant — aggregation assumptions materially affect estimates.",
           "Not significant — estimates are robust to aggregation assumptions."),
    "\n")



# 4. VISUALISATION: ECOLOGICAL SENSITIVITY SCATTER


p_eco <- ggplot(eco_merged, aes(x = w_range, y = range_elasticity)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
              colour = "steelblue", alpha = 0.2) +
  geom_text(aes(label = str_wrap(DK36Title, 20)),
            size = 2.3, vjust = -0.8, check_overlap = TRUE) +
  labs(
    x = "Weight Divergence Across Schemes (DK1 share range)",
    y = "Elasticity Divergence Across Schemes (absolute range)",
    title = "Ecological Sensitivity: Weight Divergence vs Elasticity Divergence",
    subtitle = paste0("Spearman rho = ", round(eco_cor$estimate, 3),
                      ", p = ", format.pval(eco_cor$p.value, digits = 3))
  ) +
  theme_minimal(base_size = 11)

print(p_eco)



# 5. SUMMARY TABLE FOR THESIS


eco_summary <- eco_merged %>%
  left_join(
    elasticity_divergence %>% select(industry, Consumption, Employment, Firm, sign_consistent),
    by = c("DK36Title" = "industry")
  ) %>%
  mutate(
    ecological_sensitivity = case_when(
      range_elasticity < 0.05 & sign_consistent ~ "Low",
      range_elasticity < 0.15 & sign_consistent ~ "Moderate",
      TRUE ~ "High"
    )
  ) %>%
  arrange(desc(range_elasticity))

cat("\n============================================================\n")
cat("TABLE: Ecological Sensitivity Classification\n")
cat("============================================================\n")
print(eco_summary %>%
        select(DK36Title, w_range, range_elasticity,
               sign_consistent, ecological_sensitivity), n = 35)

cat("\nClassification summary:\n")
cat("  Low ecological sensitivity:", sum(eco_summary$ecological_sensitivity == "Low"), "\n")
cat("  Moderate ecological sensitivity:", sum(eco_summary$ecological_sensitivity == "Moderate"), "\n")
cat("  High ecological sensitivity:", sum(eco_summary$ecological_sensitivity == "High"), "\n")
