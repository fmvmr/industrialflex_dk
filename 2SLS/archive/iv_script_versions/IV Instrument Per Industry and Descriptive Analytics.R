### IV instrument ### 

setwd("~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")

################################ LOAD PACKAGES ###############################################
library(arrow)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(glue)
################################ LOAD DATA #######################################################
load_data <- function(years, months = 1:12) {
  
  ym_grid <- expand.grid(year = years, month = months) %>%
    dplyr::arrange(year, month)
  
  data_list <- lapply(seq_len(nrow(ym_grid)), function(i) {
    y  <- ym_grid$year[i]
    m  <- ym_grid$month[i]
    ym <- sprintf("%d_%02d", y, m)
    
    list(
      prices = read_parquet(glue("data/elspotprices_monthly/elspot_{ym}.parquet")),
      forecast = read_parquet(glue("data/forecast_hourly_monthly_compact/forecast_compact_{ym}.parquet")),
      temp = read_parquet(glue("data/temp_zone_hourly_monthly/temp_zone_{ym}.parquet")),
      industry = read_parquet(glue("data/consumption_industry_hourly_monthly/consumption_industry_hour_{ym}.parquet")),
      consumption = read_parquet(glue("data/consumption_category_hourly_monthly/consumption_cat_hour_{ym}.parquet"))
    )
  })
  
  list(
    prices = dplyr::bind_rows(lapply(data_list, `[[`, "prices")),
    forecast = dplyr::bind_rows(lapply(data_list, `[[`, "forecast")),
    temp = dplyr::bind_rows(lapply(data_list, `[[`, "temp")),
    industry = dplyr::bind_rows(lapply(data_list, `[[`, "industry")),
    consumption = dplyr::bind_rows(lapply(data_list, `[[`, "consumption")),
    industry_annual = dplyr::bind_rows(lapply(years, function(y) {
      read_parquet(glue("data/consumption_dk10_region_year/consumption_dk10_region_{y}.parquet"))
    })),
    gas = read_parquet("data/controls/gas_daily_2020_2025.parquet"),
    carbon = read_parquet("data/controls/carbon_daily_2020_2025.parquet")
  )
}
### Select month ###
d <- load_data(2022:2024, 1:12)
### Load data for selected month)
prices   <- d$prices
forecast <- d$forecast
temp     <- d$temp
industry <- d$industry
cons     <- d$consumption
gas      <- d$gas
carbon   <- d$carbon
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

names(industry)

industry <- industry %>%
  left_join(dk19title_to_dk10title, by = "DK19Title")

industry <- industry %>%
  select(DK10Title,Consumption_MWh,TimeDK)

DK2_REGIONS <- c("Region Hovedstaden", "Region Sjælland")
DK1_REGIONS <- c("Region Syddanmark", "Region Midtjylland", "Region Nordjylland")

annual_zone_share <- industry_annual %>%
  filter(DK10Title != "Privat") %>%   # remove aggregate
  mutate(
    Zone = case_when(
      RegionName %in% DK2_REGIONS ~ "DK2",
      RegionName %in% DK1_REGIONS ~ "DK1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Zone)) %>%
  group_by(DK10Title, Zone) %>%
  summarise(Cons = sum(ConsumptionkWh, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Zone, values_from = Cons, values_fill = 0) %>%
  mutate(
    w_DK1 = DK1 / (DK1 + DK2)
  ) %>%
  select(DK10Title, w_DK1)


##### Add weights to industry consumption
industry <- industry %>%
  left_join(annual_zone_share, by = "DK10Title")





#############################Compute weighted prices ####################
prices <- prices %>% arrange(HourDK, PriceArea)

prices_wide <- prices %>%
  select(HourDK, PriceArea, SpotPriceEUR) %>%
  tidyr::pivot_wider(
    names_from  = PriceArea,
    values_from = SpotPriceEUR,
    values_fn   = dplyr::first
  )


weighted_prices <- industry %>% select(TimeDK, DK10Title, w_DK1) %>% 
  distinct() %>% left_join(prices_wide, by = c("TimeDK" = "HourDK")) %>% 
  mutate(P_weighted = w_DK1 * DK1 + (1 - w_DK1) * DK2)




weighted_prices %>%
  summarise(
    equal_hours = sum(DK1 == DK2, na.rm = TRUE),
    total_hours = sum(!is.na(DK1) & !is.na(DK2)),
    share_equal = equal_hours / total_hours
  )


weighted_prices %>%
  summarise(
    mean_spread = mean(abs(DK1 - DK2), na.rm = TRUE),
    p95_spread  = quantile(abs(DK1 - DK2), 0.95, na.rm = TRUE),
    max_spread  = max(abs(DK1 - DK2), na.rm = TRUE)
  )


###########################################################################
##### PART TWO                            #################################
###########################################################################



# ============================================================================
# PANEL 2SLS: TOTAL WIND POWER INSTRUMENT WITH TIME & TEMPERATURE FE
# ============================================================================

install.packages(c("dplyr","plm","AER","ivreg","lmtest","sandwich","stargazer","fixest"))

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


# 1.2.2 Prepare price data, aggregate on DK10TITLE.
industry <- industry %>%
  group_by(TimeDK, DK10Title) %>%
  summarise(
    Consumption_MWh = sum(Consumption_MWh, na.rm = TRUE),
    w_DK1 = first(w_DK1),
    .groups = "drop"
  )


# 1.3 controls: Gas



# 1.5 Prepare Main Consumption Panel


consumption_panel <- industry %>%
  left_join(
    weighted_prices %>% select(TimeDK, DK10Title, P_weighted,DK1,DK2),
    by = c("TimeDK", "DK10Title")
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
    !is.na(Consumption_MWh),
    !is.na(P_weighted),
    !is.na(Wind_weighted),
    !is.na(Temp_weighted)
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
  )

# =============================================================================
# INDUSTRY-SPECIFIC 2SLS: Price Elasticity per DK10 Industry
# =============================================================================
# Estimates separate IV regressions for each DK10 industry, allowing all
# coefficients (price, temperature, gas, carbon) to vary by industry.
#
# This avoids the pooled-control bias in the interaction specification
# where shared control coefficients distort industry-specific price effects.
#
# References:
#   Alberini, A., & Filippini, M. (2011). Response of residential electricity
#     demand to price. Energy Economics, 33(5), 889–895.
#   Labandeira, X., Labeaga, J. M., & López-Otero, X. (2017). A meta-analysis
#     on the price elasticity of energy demand. Energy Policy, 102, 549–568.
# =============================================================================

library(fixest)
library(ggplot2)
library(dplyr)
library(AER)
library(lmtest)
library(sandwich)


cat("  INDUSTRY-SPECIFIC 2SLS ESTIMATION\n")



# =============================================================================
# 1. PREPARE PANEL (from consumption_panel)
# =============================================================================

# Ensure log wind instrument exists
if (!"ln_wind_fc" %in% names(consumption_panel)) {
  consumption_panel <- consumption_panel %>%
    mutate(
      ln_wind_fc = log(Wind_weighted),
      temp_sq    = Temp_weighted^2
    )
}

# Filter valid observations
panel_ind <- consumption_panel %>%
  filter(
    !is.na(log_cons), !is.na(log_price), !is.na(Wind_weighted),
    !is.na(Temp_weighted), !is.na(Gas_EUR_MWh), !is.na(EUA_EUR_ton),
    P_weighted > 0, Consumption_MWh > 0, Wind_weighted > 0,
    Gas_EUR_MWh > 0, EUA_EUR_ton > 0
  ) %>%
  mutate(
    ln_wind_fc = log(Wind_weighted),
    ln_gas     = log(Gas_EUR_MWh),
    ln_carbon  = log(EUA_EUR_ton),
    temp_sq    = Temp_weighted^2
  )

cat("  Panel observations:", nrow(panel_ind), "\n")
cat("  Industries:", n_distinct(panel_ind$DK10Title), "\n\n")

# Observation count per industry
cat("── Observations per industry ──\n")
panel_ind %>%
  count(DK10Title, name = "n_obs") %>%
  arrange(desc(n_obs)) %>%
  print(n = 20)


# =============================================================================
# 2. SPLIT 2SLS: SEPARATE REGRESSION PER INDUSTRY
# =============================================================================
# fixest::feols with split = ~DK10Title runs a fully separate IV regression
# for each industry. Each industry gets its own price elasticity AND its own
# temperature, gas, and carbon coefficients.


cat("SPLIT 2SLS: PER-INDUSTRY REGRESSIONS\n")


# Specification: log(Q) ~ controls | time FE | log(P) ~ log(WindFC)
# Each industry estimated separately with its own coefficients

iv_split <- feols(
  log_cons ~ Temp_weighted + temp_sq + ln_gas + ln_carbon |
    fe_hour + fe_month + fe_year |
    log_price ~ ln_wind_fc,
  data    = panel_ind,
  split   = ~DK10Title,
  cluster = ~fe_month
)

# Print all industry results
summary(iv_split)

# Extract elasticities into a data frame
cat("\n── Extracting industry elasticities ──\n")

industry_names <- names(iv_split)
n_industries   <- length(industry_names)

results_list <- lapply(seq_len(n_industries), function(i) {
  mod <- iv_split[[i]]
  cf  <- coef(mod)
  se_vals <- se(mod)
  
  # Price elasticity
  price_coef <- cf["fit_log_price"]
  price_se   <- se_vals["fit_log_price"]
  
  # First-stage F — extract numeric value safely
  fs <- fitstat(mod, type = "ivf")
  # fitstat returns a list; extract the stat value
  f_val <- tryCatch({
    fs_inner <- fs[[1]]
    if (is.list(fs_inner)) fs_inner$stat else as.numeric(fs_inner)
  }, error = function(e) NA_real_)
  
  # Within R2
  wr2 <- tryCatch({
    w <- fitstat(mod, type = "wr2")
    if (is.list(w[[1]])) w[[1]]$wr2 else as.numeric(w[[1]])
  }, error = function(e) NA_real_)
  
  data.frame(
    Industry   = industry_names[i],
    Elasticity = price_coef,
    SE         = price_se,
    CI_lower   = price_coef - 1.96 * price_se,
    CI_upper   = price_coef + 1.96 * price_se,
    F_stat     = f_val,
    N          = nobs(mod),
    R2_within  = wr2,
    stringsAsFactors = FALSE
  )
})

elasticity_df <- do.call(rbind, results_list)
rownames(elasticity_df) <- NULL
elasticity_df <- elasticity_df %>% arrange(Elasticity)

cat("\n── Industry-Specific Price Elasticities (2SLS) ──\n\n")
print(
  elasticity_df %>%
    mutate(across(c(Elasticity, SE, CI_lower, CI_upper), ~round(., 4)),
           F_stat = round(F_stat, 1)) %>%
    select(Industry, Elasticity, SE, CI_lower, CI_upper, F_stat, N),
  right = FALSE, row.names = FALSE
)

# Flag weak instruments
cat("\n── Instrument strength check (Stock & Yogo threshold: F > 16.38) ──\n")
weak <- elasticity_df %>% filter(F_stat < 16.38)
if (nrow(weak) > 0) {
  cat("  WARNING: Weak instrument for:\n")
  print(weak %>% select(Industry, F_stat))
} else {
  cat("  ✓ All industries pass the weak instrument test.\n")
}

# Flag positive (wrong-sign) elasticities
positive <- elasticity_df %>% filter(Elasticity > 0)
if (nrow(positive) > 0) {
  cat("\n  ⚠ Positive elasticities (unexpected for demand):\n")
  print(positive %>% select(Industry, Elasticity, SE, CI_lower, CI_upper))
  cat("  Consider: measurement error, inelastic demand, or omitted variables.\n")
}


# =============================================================================
# 3. COMPARISON: SPLIT vs POOLED vs INTERACTION
# =============================================================================


cat("MODEL COMPARISON\n")


# Pooled model (single elasticity for all industries)
iv_pooled <- feols(
  log_cons ~ Temp_weighted + temp_sq + ln_gas + ln_carbon |
    DK10Title + fe_hour + fe_month + fe_year |
    log_price ~ ln_wind_fc,
  data    = panel_ind,
  cluster = ~fe_month
)

cat("── Pooled model (single elasticity) ──\n")
summary(iv_pooled)

# Interaction model (heterogeneous price, pooled controls)
iv_interaction <- feols(
  log_cons ~ Temp_weighted + ln_gas + ln_carbon |
    fe_hour + fe_month + fe_year |
    log_price:DK10Title ~ Wind_weighted:DK10Title,
  data    = panel_ind,
  cluster = ~fe_month
)

cat("\n── Interaction model (heterogeneous price, pooled controls) ──\n")
summary(iv_interaction)


# =============================================================================
# 4. ETABLE: SPLIT MODELS SIDE-BY-SIDE
# =============================================================================
cat("\n── Split models comparison table ──\n")

etable(
  iv_split,
  keep    = "log_price",
  se.below = TRUE,
  fitstat = c("n", "ivf", "wr2")
)


# =============================================================================
# 5. PUBLICATION-QUALITY FIGURES
# =============================================================================

cat("FIGURES\n")


# 5a. Forest plot: Industry-specific elasticities
# Shorten long industry names for readability
elasticity_plot <- elasticity_df %>%
  mutate(
    Industry_short = case_when(
      grepl("Industri, råstof", Industry) ~ "Manufacturing & Utilities",
      grepl("Handel og transport", Industry) ~ "Trade & Transport",
      grepl("Offentlig", Industry) ~ "Public Admin & Health",
      grepl("Landbrug", Industry) ~ "Agriculture & Fishing",
      grepl("Information", Industry) ~ "ICT",
      grepl("Ejendomshandel", Industry) ~ "Real Estate",
      grepl("Erhvervsservice", Industry) ~ "Business Services",
      grepl("Bygge", Industry) ~ "Construction",
      grepl("Finansiering", Industry) ~ "Finance & Insurance",
      TRUE ~ Industry
    ),
    # Significance markers
    sig = case_when(
      abs(Elasticity / SE) > 2.576 ~ "***",
      abs(Elasticity / SE) > 1.960 ~ "**",
      abs(Elasticity / SE) > 1.645 ~ "*",
      TRUE ~ ""
    )
  )

# Order by elasticity for clean visual
elasticity_plot$Industry_short <- factor(
  elasticity_plot$Industry_short,
  levels = elasticity_plot$Industry_short[order(elasticity_plot$Elasticity)]
)

fig_forest <- ggplot(elasticity_plot,
                     aes(x = Elasticity, y = Industry_short)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                 height = 0.25, colour = "steelblue", linewidth = 0.7) +
  geom_point(size = 3, colour = "firebrick") +
  geom_text(aes(label = sprintf("%.3f%s", Elasticity, sig)),
            hjust = -0.3, size = 3.2) +
  labs(
    title    = "Industry-Specific Price Elasticity of Electricity Demand",
    subtitle = "2SLS estimates with 95% CI — Denmark, 2022–2024",
    x        = "Price Elasticity (ln–ln)",
    y        = NULL,
    caption  = paste0("Instrument: ln(Day-ahead wind forecast). ",
                      "Clustered SE by month. ",
                      "FE: hour, month, year.\n",
                      "*** p<0.01, ** p<0.05, * p<0.1")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    axis.text.y   = element_text(size = 10)
  )

print(fig_forest)


# 5b. Add pooled estimate as reference line
pooled_elast <- coef(iv_pooled)["fit_log_price"]

fig_forest_ref <- fig_forest +
  geom_vline(xintercept = pooled_elast, linetype = "dotted",
             colour = "darkgreen", linewidth = 0.8) +
  annotate("text", x = pooled_elast, y = 0.5,
           label = sprintf("Pooled: %.3f", pooled_elast),
           colour = "darkgreen", hjust = -0.1, size = 3.5)

print(fig_forest_ref)


# 5c. First-stage scatter per industry (faceted)
fig_fs_facet <- panel_ind %>%
  group_by(DK10Title, Date) %>%
  summarise(
    avg_wind  = mean(ln_wind_fc, na.rm = TRUE),
    avg_price = mean(log_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DK10_short = case_when(
      grepl("Industri, råstof", DK10Title) ~ "Manuf. & Utilities",
      grepl("Handel", DK10Title) ~ "Trade & Transport",
      grepl("Offentlig", DK10Title) ~ "Public Admin",
      grepl("Landbrug", DK10Title) ~ "Agriculture",
      grepl("Information", DK10Title) ~ "ICT",
      grepl("Ejendom", DK10Title) ~ "Real Estate",
      grepl("Erhverv", DK10Title) ~ "Business Serv.",
      grepl("Bygge", DK10Title) ~ "Construction",
      grepl("Finans", DK10Title) ~ "Finance",
      TRUE ~ DK10Title
    )
  ) %>%
  ggplot(aes(x = avg_wind, y = avg_price)) +
  geom_point(alpha = 0.15, size = 0.5, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "firebrick", se = FALSE, linewidth = 0.8) +
  facet_wrap(~DK10_short, scales = "free_y", ncol = 3) +
  labs(
    title    = "First Stage: Wind Forecast vs. Price by Industry",
    subtitle = "Daily averages, 2022–2024",
    x        = "ln(Wind Forecast)",
    y        = "ln(Price, EUR/MWh)",
    caption  = "Each panel shows the first-stage relationship for one DK10 industry."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 9)
  )

print(fig_fs_facet)


# 5d. Consumption vs. price scatter per industry (reduced form)
fig_reduced_form <- panel_ind %>%
  group_by(DK10Title, Date) %>%
  summarise(
    avg_cons  = mean(log_cons, na.rm = TRUE),
    avg_price = mean(log_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    DK10_short = case_when(
      grepl("Industri, råstof", DK10Title) ~ "Manuf. & Utilities",
      grepl("Handel", DK10Title) ~ "Trade & Transport",
      grepl("Offentlig", DK10Title) ~ "Public Admin",
      grepl("Landbrug", DK10Title) ~ "Agriculture",
      grepl("Information", DK10Title) ~ "ICT",
      grepl("Ejendom", DK10Title) ~ "Real Estate",
      grepl("Erhverv", DK10Title) ~ "Business Serv.",
      grepl("Bygge", DK10Title) ~ "Construction",
      grepl("Finans", DK10Title) ~ "Finance",
      TRUE ~ DK10Title
    )
  ) %>%
  ggplot(aes(x = avg_price, y = avg_cons)) +
  geom_point(alpha = 0.15, size = 0.5, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "firebrick", se = FALSE, linewidth = 0.8) +
  facet_wrap(~DK10_short, scales = "free", ncol = 3) +
  labs(
    title    = "Reduced Form: Consumption vs. Price by Industry",
    subtitle = "Daily averages, 2022–2024",
    x        = "ln(Price, EUR/MWh)",
    y        = "ln(Consumption, MWh)",
    caption  = "Negative slopes indicate price-responsive demand."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 9)
  )

print(fig_reduced_form)


# =============================================================================
# 6. SUMMARY TABLE
# =============================================================================

cat("SUMMARY: INDUSTRY-SPECIFIC PRICE ELASTICITIES\n")


summary_table <- elasticity_plot %>%
  select(Industry_short, Elasticity, SE, CI_lower, CI_upper,
         F_stat, N, sig) %>%
  mutate(
    Elasticity = sprintf("%.4f%s", Elasticity, sig),
    SE         = sprintf("(%.4f)", SE),
    CI         = sprintf("[%.4f, %.4f]", CI_lower, CI_upper),
    F_stat     = sprintf("%.0f", F_stat),
    N          = format(N, big.mark = ",")
  ) %>%
  select(Industry = Industry_short, Elasticity, SE, CI, F_stat, N)

cat("  Industry-Specific Price Elasticities of Industrial Electricity Demand\n")
cat("  Denmark, 2022–2024 | 2SLS with Day-Ahead Wind Forecast Instrument\n\n")
print(as.data.frame(summary_table), row.names = FALSE, right = FALSE)

cat("\n  Pooled elasticity:", round(pooled_elast, 4), "\n")
cat("  Instrument: ln(Wind Forecast). FE: hour, month, year.\n")
cat("  Clustered SE by month (36 clusters).\n")
cat("  Stock & Yogo (2005) 10% critical value: 16.38\n")


cat("  ESTIMATION COMPLETE\n")
cat("================================================================\n")
