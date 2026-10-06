install.packages(c("httr2", "jsonlite"))
library(httr2)
library(jsonlite)

library(httr2)
library(jsonlite)
library(dplyr)

# Function to download data by month
download_by_month <- function(start_year, end_year, output_dir = ".") {
  
  dir.create(output_dir, showWarnings = FALSE)
  
  for (year in start_year:end_year) {
    for (month in 1:12) {
      
      # Create date range for each month
      start_date <- sprintf("%d-%02d-01", year, month)
      
      # Calculate end date (first day of next month)
      if (month == 12) {
        end_date <- sprintf("%d-01-01", year + 1)
      } else {
        end_date <- sprintf("%d-%02d-01", year, month + 1)
      }
      
      filename <- file.path(output_dir, sprintf("consumption_%d_%02d.csv", year, month))
      
      # Skip if file already exists
      if (file.exists(filename)) {
        message(paste("Skipping", filename, "- already exists"))
        next
      }
      
      url <- paste0(
        "https://api.energidataservice.dk/dataset/PrivateConsumptionHeatingHour/download",
        "?format=csv",
        "&start=", start_date,
        "&end=", end_date,
        "&limit=0"
      )
      
      message(paste("Downloading:", start_date, "to", end_date))
      
      tryCatch({
        download.file(url, filename, mode = "wb", quiet = TRUE)
        message(paste("  Saved:", filename))
        
        # Wait to respect rate limits
        Sys.sleep(5)
        
      }, error = function(e) {
        message(paste("  Error:", e$message))
      })
    }
  }
  
  message("Download complete!")
}

# Download data from 2020 to 2025
download_by_month(2020, 2025, output_dir = "energy_data")

library(data.table)

# Simple one-liner consolidation
all_data <- rbindlist(
  lapply(list.files("energy_data", pattern = "\\.csv$", full.names = TRUE), fread),
  fill = TRUE
)

# Remove duplicates
all_data <- unique(all_data)

# Sort by date
setorder(all_data, TimeDK)

# Save
fwrite(all_data, "private_consumption_full.csv")

all_data[, TimeDK := as.POSIXct(TimeDK)]  # Ensure proper datetime format

filtered_data <- all_data[
  TimeDK >= as.POSIXct("2022-09-01 00:00:00") & 
    TimeDK < as.POSIXct("2025-09-01 00:00:00")
]

# Check the result
message(paste("Rows after filtering:", nrow(filtered_data)))
message(paste("Date range:", min(filtered_data$TimeDK), "to", max(filtered_data$TimeDK)))
setwd("C:/Users/jespe/OneDrive/Dokumenter/cand.merc.GMA/Minor/Econometrics of Firm Data/Exam")
library(readr)
Elspotprices <- read_delim("Raw Data/Elspotprices.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
GenerationProdTypeExchange <- read_delim("Raw Data/GenerationProdTypeExchange.csv", delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
private_consumption_full <- read_csv("Raw Data/private_consumption_full.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))


karup_temp <- read_csv("Raw Data/karup_temp.csv")

roskilde_temp <- read_csv("Raw Data/roskilde_temp.csv")


consumption <- private_consumption_full
supply <- GenerationProdTypeExchange
spotprice <- Elspotprices

DK1_temp <- karup_temp
DK2_temp <- roskilde_temp

str(consumption)
str(supply)
str(spotprice)
str(DK1_temp)
str(DK2_temp)

head(consumption)
head(supply)
tail(supply)
head(spotprice)
tail(spotprice)
head(production)
head(DK1_temp)
tail(DK1_temp)
head(DK2_temp)
tail(DK1_temp)

supply <- supply[
  supply$HourUTC >= as.POSIXct("2022-09-01 00:00:00", tz = "UTC") & 
    supply$HourUTC < as.POSIXct("2025-09-01 00:00:00", tz = "UTC"),
]

# Convert observed to datetime first for temperature data
DK1_temp$observed <- as.POSIXct(DK1_temp$observed, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
DK2_temp$observed <- as.POSIXct(DK2_temp$observed, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Filter DK1_temp (Karup/Herning - 06060)
DK1_temp <- DK1_temp[
  DK1_temp$observed >= as.POSIXct("2022-09-01 00:00:00", tz = "UTC") & 
    DK1_temp$observed < as.POSIXct("2025-09-01 00:00:00", tz = "UTC"),
]

# Filter DK2_temp (Roskilde - 06170)
DK2_temp <- DK2_temp[
  DK2_temp$observed >= as.POSIXct("2022-09-01 00:00:00", tz = "UTC") & 
    DK2_temp$observed < as.POSIXct("2025-09-01 00:00:00", tz = "UTC"),
]

message(paste("Supply rows:", nrow(supply)))
message(paste("DK1_temp rows:", nrow(DK1_temp)))
message(paste("DK2_temp rows:", nrow(DK2_temp)))


# ============================================================
# Convert DK1_temp to hourly
# ============================================================

DK1_temp <- DK1_temp %>%
  mutate(
    observed = as.POSIXct(round(as.numeric(observed) / 3600) * 3600, origin = "1970-01-01", tz = "UTC")
  ) %>%
  distinct(observed, station_id, .keep_all = TRUE) %>%
  arrange(desc(observed))

message(paste("DK1_temp rows after hourly conversion:", nrow(DK1_temp)))
print(head(DK1_temp))

# ============================================================
# Convert DK2_temp to hourly
# ============================================================

DK2_temp <- DK2_temp %>%
  mutate(
    observed = as.POSIXct(round(as.numeric(observed) / 3600) * 3600, origin = "1970-01-01", tz = "UTC")
  ) %>%
  distinct(observed, station_id, .keep_all = TRUE) %>%
  arrange(desc(observed))

message(paste("DK2_temp rows after hourly conversion:", nrow(DK2_temp)))
print(head(DK2_temp))

head(DK2_temp)
head(DK1_temp)
tail(DK1_temp)
tail(DK2_temp)


# ============================================================
# Summaries
# ============================================================


summary(consumption)
summary(supply)
summary(spotprice)
summary(DK1_temp)
summary(DK2_temp)


# ============================
# Plots
# ============================

# Histogram of spot prices (all areas)
library(ggplot2)
ggplot(spotprice, aes(x = SpotPriceDKK)) +
  geom_line() +
  labs(title = "Distribution of Spot Prices (DKK)",
       x = "Spot Price [DKK/MWh]",
       y = "Count") +
  theme_minimal()

prod_long <- supply %>%
  filter(PriceArea == "DK1") %>%
  select(TimeDK, Biomass, FossilGas, Waste, OnshoreWindPower, OffshoreWindPower, SolarPower) %>%
  tidyr::pivot_longer(-TimeDK, names_to = "Source", values_to = "MW")

ggplot(prod_long,
       aes(x = TimeDK, y = MW, color = Source)) +
  geom_line(alpha = 0.7) +
  labs(title = "Production by Source - DK1",
       x = "Time (DK)",
       y = "Power [MW]",
       color = "Source") +
  theme_minimal()

library(dplyr)
library(ggplot2)
library(lubridate)

# Daily aggregate of total production (GrossCon) in DK1
prod_daily_DK1 <- supply %>%
  filter(PriceArea == "DK1") %>%
  mutate(Date = as.Date(HourDK)) %>%
  group_by(Date) %>%
  summarise(
    GrossCon_MWh = sum(GrossCon, na.rm = TRUE)
  )

ggplot(prod_daily_DK1, aes(x = Date, y = GrossCon_MWh)) +
  geom_line(color = "black") +
  labs(
    title = "Daily total production (GrossCon) - DK1",
    x = "",
    y = "Production (MWh)"
  ) +
  theme_minimal()


str(spotprice)

spot_daily <- spotprice %>%
  mutate(Date = as.Date(HourDK)) %>%
  group_by(Date, PriceArea) %>%
  summarise(
    SpotPrice_DKK = mean(SpotPriceDKK, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(spot_daily, aes(x = Date, y = SpotPrice_DKK, color = PriceArea)) +
  geom_line() +
  labs(
    title = "Daily average spot price",
    x = "",
    y = "Price (DKK/kWh)",
    color = ""
  ) +
  theme_minimal()

ggplot(spot_daily, aes(x = Date, y = SpotPrice_DKK, group = PriceArea)) +
  geom_line(data = subset(spot_daily, PriceArea == "DK1"),
            aes(color = "DK1"), size = 0.7, alpha = 1) +
  geom_line(data = subset(spot_daily, PriceArea == "DK2"),
            aes(color = "DK2"), size = 0.7, alpha = 0.4) +
  scale_color_manual(values = c("DK1" = "black", "DK2" = "red")) +
  labs(
    title = "Daily average spot price",
    x = "",
    y = "Price (DKK/kWh)",
    color = "") +
  theme_minimal()


# DK1_temp and DK2_temp already hourly numeric temperature
DK1_daily <- DK1_temp %>%
  mutate(Date = as.Date(observed)) %>%
  group_by(Date) %>%
  summarise(Temp = mean(temperature, na.rm = TRUE), .groups = "drop") %>%
  mutate(Area = "DK1")

DK2_daily <- DK2_temp %>%
  mutate(Date = as.Date(observed)) %>%
  group_by(Date) %>%
  summarise(Temp = mean(temperature, na.rm = TRUE), .groups = "drop") %>%
  mutate(Area = "DK2")

temp_daily <- bind_rows(DK1_daily, DK2_daily)

ggplot(temp_daily, aes(x = Date, y = Temp, color = Area)) +
  geom_line() +
  labs(
    title = "Daily average temperature",
    x = "",
    y = "Temperature (°C)",
    color = ""
  ) +
  theme_minimal()


# Example assuming the column is TimeDK (change if needed)
prod_daily_DK1 <- production %>%
  filter(PriceArea == "DK1") %>%
  mutate(Date = as.Date(TimeDK)) %>%   # <- change TimeDK to your actual time column
  group_by(Date) %>%
  summarise(
    GrossCon_MWh = sum(GrossCon, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(prod_daily_DK1, aes(x = Date, y = GrossCon_MWh)) +
  geom_line(color = "black") +
  labs(
    title = "Daily total production (GrossCon) - DK1",
    x = "",
    y = "Production (MWh)"
  ) +
  theme_minimal()


library(dplyr)
library(tidyr)
library(ggplot2)

# 1) Prepare daily aggregated supply for DK1
# 2. Aggregate by day and stack sources
supply_daily_long <- production %>%
  filter(PriceArea == "DK2") %>%
  mutate(Date = as.Date(TimeDK)) %>%
  group_by(Date) %>%
  summarise(
    OnshoreWind    = sum(OnshoreWindPower, na.rm = TRUE),
    OffshoreWind   = sum(OffshoreWindPower, na.rm = TRUE),
    Waste          = sum(Waste, na.rm = TRUE),
    HydroPower           = sum(HydroPower, na.rm = TRUE),
    SolarPower           = sum(SolarPower, na.rm = TRUE),
    SolarPowerSelfCon    = sum(SolarPowerSelfCon, na.rm = TRUE),
    Biomass              = sum(Biomass, na.rm = TRUE),
    Biogas               = sum(Biogas, na.rm = TRUE),
    Waste                = sum(Waste, na.rm = TRUE),
    FossilGas            = sum(FossilGas, na.rm = TRUE),
    FossilOil            = sum(FossilOil, na.rm = TRUE),
    FossilHardCoal       = sum(FossilHardCoal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Source",
    values_to = "MWh"
  )

ggplot(supply_daily_long, aes(x = Date, y = MWh, fill = Source)) +
  geom_area(alpha = 0.8, colour = NA) +
  labs(
    title = "Daily aggregated supply by source (Zoomed Example, DK1)",
    x = "",
    y = "MWh",
    fill = "Source"
  ) +
  theme_minimal()

View(supply)




library(dplyr)



# Verify
str(production)
summary(supply)
summary(DK1_temp)
summary(DK2_temp)
summary(spotprice)
summary(consumption)


# Add Grid (Region to Consumption)

# Add grid column: DK2 for these municipalities, DK1 for all others
consumption$Grid <- ifelse(
  consumption$RegionName == "Region Hovedstaden", "DK2",
  ifelse(consumption$RegionName == "Region Sjælland", "DK2","DK1"))



# ============================================================================
# PANEL 2SLS: TOTAL WIND POWER INSTRUMENT WITH TIME & TEMPERATURE FE
# ============================================================================

library(dplyr)
library(plm)
library(AER)
library(ivreg)
library(lmtest)
library(sandwich)
library(stargazer)
library(fixest)

# ====================================================================
# 1. DATA PREPARATION
# ====================================================================

# 1.1 Clean and Merge Supply Data (Total Wind Instrument)
# Aggregating wind power by hour and PriceArea
wind_supply <- supply %>%
  mutate(TimeUTC = as.POSIXct(TimeUTC, tz = "UTC")) %>%
  group_by(TimeUTC, PriceArea) %>%
  summarise(
    # Combine Onshore and Offshore into Total Wind
    TotalWind = mean(OnshoreWindPower + OffshoreWindPower, na.rm = TRUE),
    .groups = "drop"
  )

# 1.2 Prepare Temperature Data
# Combine DK1 and DK2 temperature data
temp_combined <- bind_rows(
  DK1_temp %>% mutate(Grid = "DK1"),
  DK2_temp %>% mutate(Grid = "DK2")
) %>%
  mutate(TimeUTC = as.POSIXct(observed, tz = "UTC")) %>%
  group_by(TimeUTC, Grid) %>%
  summarise(temperature = mean(temperature, na.rm = TRUE), .groups = "drop")

# 1.3 Prepare Main Consumption Panel
consumption_panel <- consumption %>%
  mutate(TimeUTC = as.POSIXct(TimeUTC, tz = "UTC"),
         year = as.integer(format(TimeUTC, "%Y")),
         month = as.integer(format(TimeUTC, "%m")),
         hour = as.integer(format(TimeUTC, "%H"))) %>%
  # Merge Spot Prices (using existing Grid column)
  left_join(spotprice %>% 
              mutate(HourUTC = as.POSIXct(HourUTC, tz = "UTC")) %>%
              select(HourUTC, PriceArea, SpotPriceDKK),
            by = c("TimeUTC" = "HourUTC", "Grid" = "PriceArea")) %>%
  # Merge Total Wind (Instrument)
  left_join(wind_supply, by = c("TimeUTC", "Grid" = "PriceArea")) %>%
  # Merge Temperature (Control)
  left_join(temp_combined, by = c("TimeUTC", "Grid")) %>%
  # Create Fixed Effect Factors
  mutate(
    fe_municipality = factor(MunicipalityCode),
    fe_year = factor(year),
    fe_month = factor(month),
    fe_hour = factor(hour)
  ) %>%
  # Filter out missing data
  filter(!is.na(ConsumptionkWh), !is.na(SpotPriceDKK), 
         !is.na(TotalWind), !is.na(temperature))

# ====================================================================
# 2. PANEL 2SLS ESTIMATION (FIXED EFFECTS)
# ====================================================================

# Specification:
# Dependent Var: ConsumptionkWh
# Endogenous Var: SpotPriceDKK
# Instrument: TotalWind
# Exogenous Controls: temperature
# Fixed Effects: Municipality (Panel ID), Year, Month, Hour

iv_fe_model <- plm(ConsumptionkWh ~ SpotPriceDKK + temperature + 
                     fe_year + fe_month + fe_hour |
                     TotalWind + temperature + 
                     fe_year + fe_month + fe_hour,
                   data = consumption_panel,
                   index = c("MunicipalityCode", "TimeUTC"),
                   model = "within") # "within" = Fixed Effects


iv_fast <- feols(ConsumptionkWh ~ temperature | 
                   # Fixed Effects (Projected out efficiently)
                   MunicipalityCode + year + month + hour | 
                   # IV Part: Endogenous ~ Instrument
                   SpotPriceDKK ~ TotalWind,
                 data = consumption_panel,
                 # Automatically clusters SEs by the first Fixed Effect (Municipality)
                 cluster = "MunicipalityCode")

# ====================================================================
# 3. DIAGNOSTICS & RESULTS
# ====================================================================

# 3.1 Robust Standard Errors (Clustered by Municipality)
robust_se <- vcovHC(iv_fe_model, type = "HC1", cluster = "group")
coeftest(iv_fe_model, vcov = robust_se)

# 3.2 First-Stage Diagnostics (Instrument Strength)
# Run first stage manually to check F-statistic for TotalWind
first_stage <- plm(SpotPriceDKK ~ TotalWind + temperature + 
                     fe_year + fe_month + fe_hour,
                   data = consumption_panel,
                   index = c("MunicipalityCode", "TimeUTC"),
                   model = "within")

f_stat <- summary(first_stage)$fstatistic
cat("\nFirst-Stage F-statistic (Instrument Strength):", f_stat$statistic, "\n")
if(!is.null(f_stat$statistic) && f_stat$statistic < 10) warning("Instrument may be weak (F < 10).")

# 3.3 Output Table
stargazer(iv_fast, 
          type = "text",
          title = "Panel 2SLS Estimation: Electricity Demand (Total Wind IV)",
          dep.var.labels = "Consumption (kWh)",
          covariate.labels = c("Spot Price (DKK)", "Temperature"),
          add.lines = list(c("Municipality FE", "Yes"),
                           c("Time FE (Year, Month, Hour)", "Yes"),
                           c("Instrument", "Total Wind Power")),
          omit = c("fe_year", "fe_month", "fe_hour"), 
          header = FALSE)


# View Summary (Includes 1st Stage F-stat and Robust SEs)
summary(iv_fast)

# Export to Stargazer (fixest fits standard stargazer workflow)
install.packages("modelsummary")
library(modelsummary) # Better than stargazer for fixest, but stargazer works too
etable(iv_fast) # fixest's built-in table generator (very clean)

iv_fstat <- fitstat(iv_fast, type = "ivf", simplify = TRUE)

cat("\nFirst-Stage F-statistic:", as.numeric(iv_fstat), "\n")



# ============================================================================
# PANEL 2SLS: LOG-LINEAR MODEL WITH DAY-OF-WEEK FE
# ============================================================================

library(dplyr)
library(fixest)

# ====================================================================
# 1. DATA PREPARATION
# ====================================================================

# 1.1 Clean and Merge Supply Data (Total Wind Instrument)
wind_supply <- supply %>%
  mutate(TimeUTC = as.POSIXct(TimeUTC, tz = "UTC")) %>%
  group_by(TimeUTC, PriceArea) %>%
  summarise(
    # Total Wind Power (Instrument)
    TotalWind = mean(OnshoreWindPower + OffshoreWindPower, na.rm = TRUE),
    .groups = "drop"
  )

# 1.2 Prepare Temperature Data
temp_combined <- bind_rows(
  DK1_temp %>% mutate(Grid = "DK1"),
  DK2_temp %>% mutate(Grid = "DK2")
) %>%
  mutate(TimeUTC = as.POSIXct(observed, tz = "UTC")) %>%
  group_by(TimeUTC, Grid) %>%
  summarise(temperature = mean(temperature, na.rm = TRUE), .groups = "drop")

# 1.3 Prepare Main Consumption Panel
consumption_panel <- consumption %>%
  mutate(TimeUTC = as.POSIXct(TimeUTC, tz = "UTC"),
         year = as.integer(format(TimeUTC, "%Y")),
         month = as.integer(format(TimeUTC, "%m")),
         hour = as.integer(format(TimeUTC, "%H")),
         # Day of Week (1=Monday, 7=Sunday)
         dow = as.integer(format(TimeUTC, "%u"))) %>%
  # Merge Spot Prices (using existing Grid column)
  left_join(spotprice %>% 
              mutate(HourUTC = as.POSIXct(HourUTC, tz = "UTC")) %>%
              select(HourUTC, PriceArea, SpotPriceDKK),
            by = c("TimeUTC" = "HourUTC", "Grid" = "PriceArea")) %>%
  # Merge Total Wind (Instrument)
  left_join(wind_supply, by = c("TimeUTC", "Grid" = "PriceArea")) %>%
  # Merge Temperature (Control)
  left_join(temp_combined, by = c("TimeUTC", "Grid")) %>%
  # Filter out missing data
  filter(!is.na(ConsumptionkWh), !is.na(SpotPriceDKK), 
         !is.na(TotalWind), !is.na(temperature),
         ConsumptionkWh > 0) # Required for log transformation

# ====================================================================
# 2. PANEL 2SLS ESTIMATION (LOG-LINEAR SPECIFICATION)
# ====================================================================

# Model: log(Consumption) ~ SpotPrice + Temp | TotalWind + Temp
# Fixed Effects: Municipality, Year, Month, Hour, Day-of-Week

iv_fast <- feols(log(ConsumptionkWh) ~ temperature | 
                   # Fixed Effects
                   MunicipalityCode + year + month + hour + dow | 
                   # IV Part: Endogenous ~ Instrument
                   SpotPriceDKK ~ TotalWind,
                 data = consumption_panel,
                 # Cluster SEs by Municipality
                 cluster = "MunicipalityCode")

# ====================================================================
# 3. DIAGNOSTICS & RESULTS
# ====================================================================

# 3.1 Display Summary
summary(iv_fast)
etable(iv_fast)

# 3.2 Extract First-Stage F-statistic
f_stat_val <- fitstat(iv_fast, type = "ivf")$ivf1$stat

cat("\nFirst-Stage F-statistic:", f_stat_val, "\n")

# 3.3 Extract Coefficients and SE for Stargazer
real_coefs <- coef(iv_fast)
real_se    <- se(iv_fast)
n_obs      <- nobs(iv_fast)

# 3.4 Stargazer Table (Dummy Object Method)
dummy_df <- data.frame(y = 1:10, x1 = 1:10, x2 = 1:10)
dummy_model <- lm(y ~ x1 + x2 - 1, data = dummy_df)

names(dummy_model$coefficients) <- c("Spot Price (DKK/MWh)", "Temperature (°C)")

stargazer(dummy_model, 
          type = "text",
          title = "Panel 2SLS: Log-Linear Model",
          dep.var.labels = "log(Consumption kWh)",
          # REMOVE covariate.labels (let the names above do the work)
          coef = list(real_coefs),
          se   = list(real_se),
          omit.stat = "all",
          add.lines = list(
            c("Municipality FE", "Yes"),
            c("Year FE", "Yes"),
            c("Month FE", "Yes"),
            c("Hour FE", "Yes"),
            c("Day-of-Week FE", "Yes"),
            c("1st Stage F-stat", round(f_stat_val, 2)),
            c("Observations", format(n_obs, big.mark = ","))
          ),
          header = FALSE)

# ====================================================================
# 4. EXPORT RESULTS
# ====================================================================

# Export the cleaned consumption panel
library(data.table)
fwrite(consumption_panel, "consumption_panel_cleaned.csv")

# Display coefficient names for verification
cat("\nCoefficient Order (verify for Stargazer labels):\n")
print(names(real_coefs))



# ============================================================================
# R CODE: DESCRIPTIVE ANALYSIS AND CRISIS HYPOTHESIS TESTING
# Aggregate Price Elasticity of Residential Electricity Demand
# ============================================================================

library(dplyr)
library(ggplot2)
library(fixest)
library(data.table)

# ============================================================================
# 1. DESCRIPTIVE ANALYSIS: FIGURE 1 - MERIT ORDER EFFECT
# ============================================================================
# This code creates a scatter plot showing the negative relationship between
# wind production and spot prices (the Merit Order Effect)

# Assuming your cleaned consumption_panel is loaded (from previous code)
# Create summary statistics at daily level for cleaner visualization

daily_summary <- consumption_panel %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  group_by(Date, Grid) %>%
  summarise(
    AvgPrice = mean(SpotPriceDKK, na.rm = TRUE),
    TotalWind = mean(TotalWind, na.rm = TRUE),
    AvgTemp = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  )

# Create Figure 1: Scatter plot of Price vs Wind Production
figure_1 <- ggplot(daily_summary, aes(x = TotalWind, y = AvgPrice)) +
  geom_point(alpha = 0.4, size = 2, color = "#1f77b4") +
  geom_smooth(method = "lm", se = TRUE, color = "#d62728", linetype = "solid", size = 1) +
  labs(
    title = "Merit Order Effect — Spot Price vs. Total Wind Production",
    subtitle = "Daily Averages, September 2022 - August 2025",
    x = "Total Wind Power Production (MWh)",
    y = "Average Spot Price (DKK/MWh)",
    caption = "Data source: Energinet."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    axis.title = element_text(size = 11),
    panel.grid.major = element_line(color = "lightgray", size = 0.3),
    panel.grid.minor = element_blank()
  )

figure_1
# Display and save
print(figure_1)
ggsave("Figure_1_Merit_Order_Effect.png", figure_1, width = 10, height = 6, dpi = 300)

# Summary statistics for the first stage
first_stage_corr <- cor(daily_summary$TotalWind, daily_summary$AvgPrice, use = "complete.obs")
cat("\nCorrelation between Wind and Spot Price:", round(first_stage_corr, 4), "\n")

# ============================================================================
# 2. TEMPORAL VISUALIZATION: PRICE AND WIND OVER TIME
# ============================================================================
# Optional: Create a time series showing how price and wind co-move over the sample period

time_series_data <- consumption_panel %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  group_by(Date) %>%
  summarise(
    AvgPrice = mean(SpotPriceDKK, na.rm = TRUE),
    TotalWind = mean(TotalWind, na.rm = TRUE),
    .groups = "drop"
  )

figure_2 <- ggplot(time_series_data, aes(x = Date)) +
  geom_line(aes(y = AvgPrice, color = "Spot Price"), size = 0.8) +
  geom_line(aes(y = TotalWind * 0.5, color = "Wind Production (scaled)"), size = 0.8) +
  labs(
    title = "Spot Price and Wind Production Over Time",
    subtitle = "Daily Averages, September 2022 - August 2025",
    x = "Date",
    y = "Spot Price (DKK/MWh) / Wind",
    color = "Series"
  ) +
  scale_color_manual(values = c("Spot Price" = "#d62728", "Wind Production (scaled)" = "#2ca02c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    legend.position = "bottom"
  )

figure_2
print(figure_2)
ggsave("Figure_2_TimeSeries_Price_Wind.png", figure_2, width = 12, height = 6, dpi = 300)

# ============================================================================
# 3. CRISIS HYPOTHESIS TESTING: INTERACTION MODEL
# ============================================================================
# Define crisis period: September 2022 - December 2023
# Post-crisis: January 2024 - August 2025

consumption_panel <- consumption_panel %>%
  mutate(
    # Ensure TimeUTC is proper POSIXct format
    TimeUTC = as.POSIXct(TimeUTC, tz = "UTC"),
    
    # Create Time Fixed Effects variables
    year  = as.integer(format(TimeUTC, "%Y")),
    month = as.integer(format(TimeUTC, "%m")),
    hour  = as.integer(format(TimeUTC, "%H")),
    
    # Create Day of Week (1=Monday, 7=Sunday)
    dow   = as.integer(format(TimeUTC, "%u"))
  )

consumption_panel <- consumption_panel %>%
  mutate(
    Date = as.Date(TimeUTC),
    Year = year(TimeUTC),
    Month = month(TimeUTC),
    Crisis = ifelse(Date <= as.Date("2023-12-31"), 1, 0),
    Crisis_Label = ifelse(Crisis == 1, "Crisis (Sep 2022 - Dec 2023)", "Post-Crisis (Jan 2024 - Aug 2025)")
  )

# ----
# Baseline Model (for reference)
# ----
cat("\n\n==== BASELINE MODEL (No Crisis Interaction) ====\n")

model_baseline <- feols(
  log(ConsumptionkWh) ~ temperature | 
    MunicipalityCode + year + month + hour + dow | 
    SpotPriceDKK ~ TotalWind,
  data = consumption_panel,
  cluster = "MunicipalityCode"
)

summary(model_baseline)
cat("\nBaseline Model Completed.\n")

# Extract baseline results for reporting
baseline_coef_price <- coef(model_baseline)["fit_SpotPriceDKK"]
baseline_se_price   <- se(model_baseline)["fit_SpotPriceDKK"]

# ----
# CRISIS HYPOTHESIS MODEL: Interaction with Crisis Dummy
# ----
cat("\n\n==== CRISIS HYPOTHESIS MODEL (with Interaction) ====\n")

model_crisis <- feols(
  log(ConsumptionkWh) ~ temperature | 
    MunicipalityCode + year + month + hour + dow | 
    SpotPriceDKK + SpotPriceDKK:Crisis ~ TotalWind + TotalWind:Crisis,
  data = consumption_panel,
  cluster = "MunicipalityCode"
)

summary(model_crisis)
cat("\nCrisis Model Completed.\n")

# Extract interaction results
crisis_coef_price <- coef(model_crisis)["fit_SpotPriceDKK"]
crisis_coef_interaction <- coef(model_crisis)["fit_SpotPriceDKK:Crisis"]
crisis_se_price <- se(model_crisis)["fit_SpotPriceDKK"]
crisis_se_interaction <- se(model_crisis)["fit_SpotPriceDKK:Crisis"]

# ----
# Interpretation of Crisis Results
# ----
cat("\n\n==== CRISIS HYPOTHESIS INTERPRETATION ====\n")
cat("\nBaseline Semi-Elasticity (Post-Crisis):  ", round(crisis_coef_price, 8), "\n")
cat("Standard Error:                          ", round(crisis_se_price, 8), "\n")

cat("\nInteraction Coefficient (Crisis Effect): ", round(crisis_coef_interaction, 8), "\n")
cat("Standard Error:                          ", round(crisis_se_interaction, 8), "\n")

crisis_elasticity <- crisis_coef_price + crisis_coef_interaction
cat("\nCombined Semi-Elasticity (During Crisis):", round(crisis_elasticity, 8), "\n")
cat("This is", round(abs(crisis_elasticity) / abs(crisis_coef_price), 2), 
    "times more elastic than post-crisis period.\n")

# Statistical significance of interaction
t_stat_interaction <- crisis_coef_interaction / crisis_se_interaction
p_value_interaction <- 2 * (1 - pnorm(abs(t_stat_interaction)))

cat("\nInteraction t-statistic:", round(t_stat_interaction, 4), "\n")
cat("Interaction p-value:    ", round(p_value_interaction, 6), "\n")

if (p_value_interaction < 0.05) {
  cat("*** Interaction is statistically significant at 5% level ***\n")
  cat("Evidence: Elasticity WAS higher (more responsive) during the crisis period.\n")
} else {
  cat("Interaction is NOT statistically significant.\n")
  cat("Evidence: No significant difference in elasticity across periods.\n")
}

# ============================================================================
# 4. COMPARISON TABLE: BASELINE vs CRISIS MODEL
# ============================================================================

comparison_table <- data.frame(
  Model = c("Baseline (Pooled)", "Crisis Model - Post-Crisis", "Crisis Model - During Crisis"),
  Semi_Elasticity = c(
    round(baseline_coef_price, 8),
    round(crisis_coef_price, 8),
    round(crisis_elasticity, 8)
  ),
  Std_Error = c(
    round(baseline_se_price, 8),
    round(crisis_se_price, 8),
    round(sqrt(crisis_se_price^2 + crisis_se_interaction^2 + 
                 2 * cov(c(crisis_coef_price, crisis_coef_interaction))[1,2]), 8)
  ),
  Real_World_Impact = c(
    paste0(round(baseline_coef_price * 100, 4), "% per 100 DKK spike"),
    paste0(round(crisis_coef_price * 100, 4), "% per 100 DKK spike"),
    paste0(round(crisis_elasticity * 100, 4), "% per 100 DKK spike")
  )
)

cat("\n\n==== TABLE: CRISIS HYPOTHESIS RESULTS ====\n")
print(comparison_table)

# ============================================================================
# 5. FIRST-STAGE F-STATISTIC FOR CRISIS MODEL
# ============================================================================

f_stat_crisis <- fitstat(model_crisis, type = "ivf")
cat("\n\nFirst-Stage F-statistic (Excluded Instruments):\n")
print(f_stat_crisis)

# ============================================================================
# 6. ROBUSTNESS: SUMMARY BY CRISIS PERIOD
# ============================================================================
# Descriptive comparison of price and consumption across periods

period_summary <- consumption_panel %>%
  group_by(Crisis_Label) %>%
  summarise(
    Mean_Price = mean(SpotPriceDKK, na.rm = TRUE),
    SD_Price = sd(SpotPriceDKK, na.rm = TRUE),
    Mean_Consumption = mean(ConsumptionkWh, na.rm = TRUE),
    SD_Consumption = sd(ConsumptionkWh, na.rm = TRUE),
    Mean_Wind = mean(TotalWind, na.rm = TRUE),
    Mean_Temp = mean(temperature, na.rm = TRUE),
    N_Obs = n(),
    .groups = "drop"
  )

cat("\n\n==== DESCRIPTIVE STATISTICS BY PERIOD ====\n")
print(period_summary)




# ============================================================================
# HYPOTHESIS 2: MUNICIPAL HETEROGENEITY (Distribution of Elasticities)
# ============================================================================

library(dplyr)
library(fixest)
library(ggplot2)

# 1. Initialize a list to store results
uni_results <- list()
muni_codes <- unique(consumption_panel$MunicipalityCode)

cat("Estimating elasticities for", length(muni_codes), "municipalities...\n")

# 2. Loop through each municipality
for(code in muni_codes) {
  
  # Subset data for this municipality
  sub_data <- consumption_panel %>% filter(MunicipalityCode == code)
  
  # Run FE-2SLS for this specific municipality
  # Note: Municipality FE removed (constant within subset), keeping Time FEs
  tryCatch({
    model_muni <- feols(
      log(ConsumptionkWh) ~ temperature | 
        year + month + hour + dow | 
        SpotPriceDKK ~ TotalWind,
      data = sub_data
    )
    
    # Store result
    uni_results[[as.character(code)]] <- data.frame(
      Municipality = code,
      Elasticity = coef(model_muni)["fit_SpotPriceDKK"],
      SE = se(model_muni)["fit_SpotPriceDKK"],
      N_Obs = nobs(model_muni)
    )
  }, error = function(e) {
    cat("Error for Municipality", code, ":", conditionMessage(e), "\n")
  })
}

# 3. Combine results into one dataframe
heterogeneity_df <- do.call(rbind, uni_results)

# 4. Summary Statistics of the Distribution
summary(heterogeneity_df$Elasticity)

# 5. Visualizing Hypothesis 2
# Create a density plot/histogram of the elasticities
plot_heterogeneity <- ggplot(heterogeneity_df, aes(x = Elasticity)) +
  geom_histogram(bins = 30, fill = "#1f77b4", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(heterogeneity_df$Elasticity), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0, color = "black", size = 0.8) +
  labs(
    title = "Figure 3: Heterogeneity in Price Elasticity Across Municipalities",
    subtitle = "Distribution of Municipality-Specific Semi-Elasticities (N=98)",
    x = "Semi-Elasticity (Log Consumption / Price)",
    y = "Count of Municipalities",
    caption = "Red line: Mean Elasticity. Black line: Zero."
  ) +
  theme_minimal()

plot_heterogeneity
print(plot_heterogeneity)
ggsave("Figure_3_Municipal_Heterogeneity.png", plot_heterogeneity, width = 8, height = 6)

# 6. Extract Top/Bottom Responders for Discussion
most_elastic <- heterogeneity_df %>% arrange(Elasticity) %>% head(5)
least_elastic <- heterogeneity_df %>% arrange(desc(Elasticity)) %>% head(5)

cat("\nMost Responsive Municipalities (Most Negative):\n")
print(most_elastic)

cat("\nLeast Responsive Municipalities (Closest to Zero/Positive):\n")
print(least_elastic)
