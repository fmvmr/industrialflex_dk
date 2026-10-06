# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(fixest)
library(purrr)
library(tidyr)
library(lubridate)

# ── Step 1: Compute first-stage fitted values per sector ──────────────────────
# The "wind-induced" component of the log price for each observation

compute_wind_induced_price <- function(df, sector_var, sector_name) {
  
  df_sec <- df %>% dplyr::filter(.data[[sector_var]] == sector_name)
  
  # First-stage regression: log_P_c on Wind_c + controls + FEs
  fs_model <- fixest::feols(
    log_P_c ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
      fe_hour + fe_month + fe_dow,
    data = df_sec,
    cluster = ~ fe_week
  )
  
  # Decompose: total fitted = predict(fs_model)
  # Wind-induced = pi_hat * Wind_c (the marginal contribution of the instrument)
  pi_hat <- coef(fs_model)["Wind_c"]
  
  df_sec %>%
    dplyr::mutate(
      wind_induced_price = pi_hat * Wind_c,
      wind_induced_price_centered = wind_induced_price - mean(wind_induced_price, na.rm = TRUE),
      sector = sector_name
    ) %>%
    dplyr::select(sector, TimeUTC, fe_hour, fe_month, fe_dow, fe_week,
                  wind_induced_price, wind_induced_price_centered, log_P_c, Wind_c)
}

# ── Step 2: Temporal aggregation ─────────────────────────────────────────────
# Aggregate the wind-induced price variation by hour-of-day, month, and DOW

summarise_temporal <- function(wind_induced_df) {
  
  # By hour-of-day
  by_hour <- wind_induced_df %>%
    dplyr::group_by(sector, fe_hour) %>%
    dplyr::summarise(
      mean_wind_price   = mean(wind_induced_price_centered, na.rm = TRUE),
      sd_wind_price     = sd(wind_induced_price_centered, na.rm = TRUE),
      var_share         = var(wind_induced_price_centered, na.rm = TRUE),
      n_obs             = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dimension = "hour_of_day", value = fe_hour) %>%
    dplyr::select(sector, dimension, value, mean_wind_price, sd_wind_price, var_share, n_obs)
  
  # By month
  by_month <- wind_induced_df %>%
    dplyr::group_by(sector, fe_month) %>%
    dplyr::summarise(
      mean_wind_price   = mean(wind_induced_price_centered, na.rm = TRUE),
      sd_wind_price     = sd(wind_induced_price_centered, na.rm = TRUE),
      var_share         = var(wind_induced_price_centered, na.rm = TRUE),
      n_obs             = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dimension = "month", value = as.character(fe_month)) %>%
    dplyr::select(sector, dimension, value, mean_wind_price, sd_wind_price, var_share, n_obs)
  
  # By day of week
  by_dow <- wind_induced_df %>%
    dplyr::group_by(sector, fe_dow) %>%
    dplyr::summarise(
      mean_wind_price   = mean(wind_induced_price_centered, na.rm = TRUE),
      sd_wind_price     = sd(wind_induced_price_centered, na.rm = TRUE),
      var_share         = var(wind_induced_price_centered, na.rm = TRUE),
      n_obs             = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dimension = "day_of_week", value = as.character(fe_dow)) %>%
    dplyr::select(sector, dimension, value, mean_wind_price, sd_wind_price, var_share, n_obs)
  
  dplyr::bind_rows(by_hour, by_month, by_dow)
}


# ── Step 3: Apply to all sectors ──────────────────────────────────

all_sectors <- unique(consumption_panel$DK36_en)

all_temporal <- purrr::map_dfr(all_sectors, function(sec) {
  message("Processing: ", sec)
  
  result <- tryCatch({
    wind_df <- compute_wind_induced_price(consumption_panel, "DK36_en", sec)
    summarise_temporal(wind_df)
  }, error = function(e) {
    message("Failed for ", sec, ": ", e$message)
    NULL
  })
  
  result
})


# ── Step 4: Visual diagnostics  ────────────────────────────

p_hour <- all_temporal%>%
  dplyr::filter(dimension == "hour_of_day") %>%
  dplyr::mutate(hour = as.numeric(as.character(value))) %>%
  ggplot(aes(x = hour, y = sd_wind_price)) +
  geom_col(fill = "#3B7EA1", alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  labs(
    x = "Hour of day",
    y = "SD of wind-induced log-price variation",
    title = "Hourly distribution",
    subtitle = "Higher values = wind moves prices more in this hour"
  ) +
  theme_minimal(base_size = 11)

p_month <- all_temporal %>%
  dplyr::filter(dimension == "month") %>%
  dplyr::mutate(month_date = as.Date(paste0(value, "-01"))) %>%
  ggplot(aes(x = month_date, y = sd_wind_price)) +
  geom_col(fill = "#5A9367", alpha = 0.85) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  labs(
    x = "Year-month",
    y = "SD of wind-induced log-price variation",
    title = "Seasonal distribution",
    subtitle = "Higher values = wind moves prices more in this month"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_dow <- all_temporal %>%
  dplyr::filter(dimension == "day_of_week") %>%
  ggplot(aes(x = value, y = sd_wind_price)) +
  geom_col(fill = "#A65A53", alpha = 0.85) +
  labs(
    x = "Day of week",
    y = "SD of wind-induced log-price variation",
    title = "Day-of-week distribution",
    subtitle = "Higher values = wind moves prices more in this day",
  ) +
  theme_minimal(base_size = 11)

library(patchwork)

# Combine the three plots side by side
combined_plot <- p_hour + p_month + p_dow +
  plot_layout(ncol = 3, widths = c(1, 1.5, 0.7)) +
  plot_annotation(
    title = "Temporal distribution of wind-induced price variation",
    subtitle = "Averaged across the 29 DK36 sectors",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(size = 10, colour = "grey40")
    )
  )

print(combined_plot)

# ── Step 5: Aggregate cross-sector summary ────────────────────────────────────

aggregate_temporal <- all_temporal %>%
  dplyr::group_by(dimension, value) %>%
  dplyr::summarise(
    mean_sd        = mean(sd_wind_price, na.rm = TRUE),
    median_sd      = median(sd_wind_price, na.rm = TRUE),
    n_sectors_above_median = sum(sd_wind_price > median(all_temporal$sd_wind_price, na.rm = TRUE), na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== Aggregate hour-of-day pattern ===\n")
print(aggregate_temporal %>% dplyr::filter(dimension == "hour_of_day"), n = 24)

cat("\n=== Aggregate month pattern ===\n")
print(aggregate_temporal %>% dplyr::filter(dimension == "month"), n = Inf)

cat("\n=== Aggregate day-of-week pattern ===\n")
print(aggregate_temporal %>% dplyr::filter(dimension == "day_of_week"), n = 7)