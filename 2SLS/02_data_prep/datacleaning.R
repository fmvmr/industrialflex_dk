# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
### Data cleaning for Exam project. 
# Read in data #####


library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl)

load_data <- function(years, months = 1:12) {
  safe_read <- function(path) if (file.exists(path)) arrow::read_parquet(path)
  bind_nn   <- function(lst) dplyr::bind_rows(Filter(Negate(is.null), lst))
  
  yms <- unlist(lapply(years, function(y) {
    yy <- substr(as.character(y), 3, 4)
    sprintf("%s_%02d", yy, months)
  }))
  
  list(
    DKcons = bind_nn(lapply(yms, function(ym)
      safe_read(glue::glue("DK_consumption_2022/DKconsumption{ym}.parquet"))
    ))
  )
}

d <- load_data(2022)
list2env(d["DKcons"], .GlobalEnv)




Data_hourly <- DKcons %>%
  group_by(TimeDK) %>%
  summarise(
    Total_kWh = sum(ConsumptionkWh, na.rm = TRUE),
    Privat_kWh = sum(ConsumptionkWh[ConsumerCategory3 == "Privat"], na.rm = TRUE),
    Erhverv_kWh = sum(ConsumptionkWh[ConsumerCategory3 == "Erhverv"], na.rm = TRUE),
    Offentlig_kWh = sum(ConsumptionkWh[ConsumerCategory3 == "Offentlige foretagender"], na.rm = TRUE),
    ErhOff_kWh = Erhverv_kWh + Offentlig_kWh,
    ratio_Privat_Total = if_else(Total_kWh > 0, Privat_kWh / Total_kWh, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(TimeDK)


Data_hourly <- Data_hourly %>%
  mutate(TimeDK = as.character(TimeDK)) %>%
  mutate(
    TimeDK = if_else(
      !is.na(TimeDK) & str_detect(TimeDK, "\\d{1,2}:\\d{2}(:\\d{2})?$"),
      TimeDK,
      paste0(TimeDK, " 00:00:00")
    )
  ) %>%
  mutate(
    TimeDK = parse_date_time(
      TimeDK,
      orders = c("Y-m-d H:M:S", "Y/m/d H:M:S", "d/m/Y H:M:S", "Y-m-d H:M"),
      tz = "Europe/Copenhagen"
    ),
    Date = as_date(TimeDK),
    Week = isoweek(Date),
    Hour = hour(TimeDK),
    Doy = yday(TimeDK)
  )

# Consumption data
Data_hourly <- Data_hourly %>%
  mutate(Weekday = wday(TimeDK, label = TRUE, abbr = FALSE, week_start = 1))




ninja_pv_2024 <- ninja_pv_2024 %>%
  mutate(
    Date = as_date(time),
    Week = isoweek(Date),
    Hour = hour(time),
    Doy  = yday(Date)
  )

ninja_wind_2024 <- ninja_wind_2024 %>%
  mutate(
    Date = as_date(time),
    Week = isoweek(Date),
    Hour = hour(time),
    Doy  = yday(Date)
  )


## 1) Read and clean Energinet capacity per municipality ----

cap <- read_delim(
  "CapacityPerMunicipality.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
) %>%
  mutate(
    MunicipalityNo       = as.integer(MunicipalityNo),
    OffshoreWindCapacity = as.numeric(OffshoreWindCapacity),
    OnshoreWindCapacity  = as.numeric(OnshoreWindCapacity),
    SolarPowerCapacity   = as.numeric(SolarPowerCapacity),
    Total_RES_MW         = OffshoreWindCapacity + OnshoreWindCapacity + SolarPowerCapacity
  ) %>%
  group_by(MunicipalityNo) %>%
  filter(Month == max(Month)) %>%       # keep latest snapshot per municipality
  ungroup() %>%
  filter(MunicipalityNo != 1)           # drop weird code = 1


## 2) Read and collapse mapping: municipality -> NUTS2 ----

nuts_map <- read_excel(
  "Korrespondancetabel-mellem-kommuner-foer-og-efter-kommunalreformen-i-2007.xlsx",
  sheet = "Nøgle mellem AMT_KOM og NUTS"
) %>%
  select(MunicipalityNo = NUTS_KODE, Region = REGION_TXT) %>%
  mutate(MunicipalityNo = as.integer(MunicipalityNo)) %>%
  distinct(MunicipalityNo, .keep_all = TRUE) %>%
  mutate(
    NUTS2 = case_when(
      Region == "Region Hovedstaden"   ~ "DK01",
      Region == "Region Sjælland"      ~ "DK02",
      Region == "Region Syddanmark"    ~ "DK03",
      Region == "Region Midtjylland"   ~ "DK04",
      Region == "Region Nordjylland"   ~ "DK05",
      TRUE                             ~ NA_character_
    )
  )

## 3) Attach NUTS2 info directly onto `cap` ----

cap <- cap %>%
  left_join(nuts_map, by = "MunicipalityNo")

## 4) Summarise capacity by NUTS2 ----

capacity_by_NUTS2 <- cap %>%
  group_by(NUTS2) %>%
  summarise(
    Total_RES_MW    = sum(Total_RES_MW, na.rm = TRUE),
    OffshoreWind_MW = sum(OffshoreWindCapacity, na.rm = TRUE),
    OnshoreWind_MW  = sum(OnshoreWindCapacity, na.rm = TRUE),
    Solar_MW        = sum(SolarPowerCapacity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Total_RES_GW    = Total_RES_MW    / 1000,
    OffshoreWind_GW = OffshoreWind_MW / 1000,
    OnshoreWind_GW  = OnshoreWind_MW  / 1000,
    Solar_GW        = Solar_MW        / 1000
  ) %>%
  mutate(
    TotalWind_MW = OffshoreWind_MW + OnshoreWind_MW
  ) %>%
  mutate(
    Share_OffshoreWind = OffshoreWind_MW / sum(OffshoreWind_MW, na.rm = TRUE),
    Share_OnshoreWind  = OnshoreWind_MW  / sum(OnshoreWind_MW,  na.rm = TRUE),
    Share_TotalWind    = TotalWind_MW    / sum(TotalWind_MW,    na.rm = TRUE)
  ) %>%
  arrange(NUTS2)

capacity_by_NUTS2


##### Import prices neighbouring countries #####


Sweden <- read_csv("Sweden.csv")
Norway <- read_csv ("Norway.csv")
Germany <- read_csv("Germany.csv")
Finland <- read_csv("Finland.csv")
UK <- read_csv("United Kingdom.csv")
Netherlands <- read_csv("Netherlands.csv")

Sweden <- Sweden %>% 
  filter(`Datetime (Local)` >= "2024-01-01 01:00:00")
Norway <- Norway %>% 
  filter(`Datetime (Local)` >= "2024-01-01 01:00:00")
Germany <- Germany %>% 
  filter(`Datetime (Local)` >= "2024-01-01 01:00:00")
Finland <- Finland %>% 
  filter(`Datetime (Local)` >= "2024-01-01 01:00:00")
UK <- UK %>% 
  filter(`Datetime (Local)` >= "2024-01-01 01:00:00")
Netherlands <- Netherlands %>% 
  filter(`Datetime (Local)` >= "2024-01-01 01:00:00")

Sweden <- Sweden %>% 
  filter(`Datetime (Local)` <= "2025-01-01 00:00:00")
Norway <- Norway %>% 
  filter(`Datetime (Local)` <= "2025-01-01 00:00:00")
Germany <- Germany %>% 
  filter(`Datetime (Local)` <= "2025-01-01 00:00:00")
Finland <- Finland %>% 
  filter(`Datetime (Local)` <= "2025-01-01 00:00:00")
UK <- UK %>% 
  filter(`Datetime (Local)` <= "2025-01-01 00:00:00")
Netherlands <- Netherlands %>% 
  filter(`Datetime (Local)` <= "2025-01-01 00:00:00")

Sweden <- Sweden %>% 
  filter(!as.Date(`Datetime (Local)`) == as.Date("2024-02-29"))
Germany <- Germany %>% 
  filter(!as.Date(`Datetime (Local)`) == as.Date("2024-02-29"))
Norway <- Norway %>% 
  filter(!as.Date(`Datetime (Local)`) == as.Date("2024-02-29"))
UK <- UK %>%
  filter(!as.Date(`Datetime (Local)`) == as.Date("2024-02-29"))
Finland <- Finland %>%
  filter(!as.Date(`Datetime (Local)`) == as.Date("2024-02-29"))
Netherlands <- Netherlands %>% 
  filter(!as.Date(`Datetime (Local)`) == as.Date("2024-02-29"))



# Export CSVs #####

# Define the output path
out_path <- "Data_for_modelling"

# Write CSVs
write_csv(ninja_pv_2024,   file.path(out_path, "ninja_pv_2024.csv"))
write_csv(ninja_wind_2024, file.path(out_path, "ninja_wind_2024.csv"))
write_csv(Data_hourly,     file.path(out_path, "Data_hourly.csv"))
write_csv(Germany,     file.path(out_path, "Germany.csv"))
write_csv(Sweden,     file.path(out_path, "Sweden.csv"))
write_csv(Norway,     file.path(out_path, "Norway.csv"))
write_csv(UK,     file.path(out_path, "UK.csv"))
write_csv(Finland,     file.path(out_path, "Finland.csv"))
write_csv(Netherlands,     file.path(out_path, "Netherlands.csv"))

