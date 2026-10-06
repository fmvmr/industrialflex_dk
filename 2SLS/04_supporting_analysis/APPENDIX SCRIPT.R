# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# THESIS SCRIPT FOR Appendixes.  
#### APPENDIX 2- ALTERNATIVE MODEL SPECIFICATIONS ───────────────────────────####
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


#### APPENDIX 3 - NEGATIVE SPOT PRICES ──────────────────────────────────────#####
weighted_prices_low %>%
  summarise(
    N_hours        = n_distinct(TimeUTC),
    N_obs          = n(),
    Pct_of_total   = round(100 * n_distinct(TimeUTC) / n_distinct(weighted_prices$TimeUTC), 2),
    Mean_DK1_P     = round(mean(DK1_P), 2),
    Mean_DK2_P     = round(mean(DK2_P), 2),
    Min_DK1_P      = round(min(DK1_P),  2),
    Min_DK2_P      = round(min(DK2_P),  2)
  ) %>%
  print()

#### APPENDIX 5 - Comparison of HAC and Clustered standard errors ───────────####


get_iv_coef <- function(m) coef(m)["fit_log_P_c"]
get_iv_se   <- function(m) se(m)["fit_log_P_c"]
clean_sector <- function(x) sub(".*sample: ", "", x)

# Week-clustered
IV_model_het_DK10_W <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data    = consumption_panel,
  cluster = ~ fe_week,
  split   = ~ DK36_en
)

# HAC / Newey-West
IV_model_het_DK10_HAC <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data     = consumption_panel,
  panel.id = ~ DK36_en + TimeUTC,
  vcov     = NW ~ TimeUTC,
  split    = ~ DK36_en
)

# Extract bandwidth directly from model vcov attributes

str(IV_model_het_DK10_HAC[[1]], max.level = 1)

nw_type <- attr(summary(IV_model_het_DK10_HAC[[1]])$se, "type")
nw_bw   <- as.integer(regmatches(nw_type, regexpr("(?<=L=)\\d+", nw_type, perl = TRUE)))
nw_bw   # returns 13

# Align sectors
nms_w  <- names(IV_model_het_DK10_W)
nms_h  <- names(IV_model_het_DK10_HAC)
common <- intersect(nms_w, nms_h)
idx_w  <- match(common, nms_w)
idx_h  <- match(common, nms_h)

# Build table with 1% and 5% significance
comp <- data.frame(
  sector     = clean_sector(common),
  beta       = sapply(IV_model_het_DK10_W[idx_w],   get_iv_coef),
  se_cluster = sapply(IV_model_het_DK10_W[idx_w],   get_iv_se),
  se_hac     = sapply(IV_model_het_DK10_HAC[idx_h], get_iv_se),
  row.names  = NULL
) %>% 
  mutate(
    pct_diff    = 100 * (se_hac - se_cluster) / se_cluster,
    t_cluster   = beta / se_cluster,
    t_hac       = beta / se_hac,
    sig_cluster = case_when(
      abs(t_cluster) > 2.576 ~ "**",
      abs(t_cluster) > 1.960 ~ "*",
      TRUE                   ~ ""
    ),
    sig_hac = case_when(
      abs(t_hac) > 2.576 ~ "**",
      abs(t_hac) > 1.960 ~ "*",
      TRUE               ~ ""
    )
  ) %>% 
  select(-t_cluster, -t_hac)


#### APPENDIX 6 - Q-Q diagnostics for simulated errors ──────────────────────####
library(dplyr)
library(ggplot2)
library(fixest)
library(purrr)
library(tibble)
library(moments)

compute_sector_residuals <- function(df, sector_var) {
  sectors <- unique(df[[sector_var]])
  purrr::map_dfr(sectors, function(sec) {
    df_sec <- df %>% dplyr::filter(.data[[sector_var]] == sec)
    m <- tryCatch(
      fixest::feols(
        log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
          fe_hour + fe_month + fe_dow,
        data = df_sec
      ),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)
    res <- residuals(m)
    tibble::tibble(
      sector = as.character(sec),
      residual = res,
      residual_std = scale(res)[, 1]
    )
  })
}

residuals_dk36 <- compute_sector_residuals(consumption_panel, "DK36_en")

moments_dk36 <- residuals_dk36 %>%
  group_by(sector) %>%
  summarise(
    n           = n(),
    sd          = sd(residual),
    skewness    = moments::skewness(residual),
    excess_kurt = moments::kurtosis(residual) - 3,
    .groups = "drop"
  ) %>%
  arrange(desc(excess_kurt))

print(moments_dk36, n = Inf)

qq_plot_dk36 <- ggplot(residuals_dk36, aes(sample = residual_std)) +
  stat_qq(alpha = 0.3, size = 0.5) +
  stat_qq_line(colour = "red", linewidth = 0.5) +
  facet_wrap(~ sector, scales = "free", ncol = 4) +
  labs(
    x = "Theoretical quantiles (N(0,1))",
    y = "Standardised residual quantiles",
    title = "Q-Q plots: auxiliary regression residuals by sector (DK36)"
  ) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(size = 7), panel.grid.minor = element_blank())


#### APPENDIX 7 - Missing observations ──────────────────────────────────────####
library(ggplot2)
library(patchwork)

full_grid <- expand.grid(
  TimeUTC   = seq(min(consumption_panel$TimeUTC),
                  max(consumption_panel$TimeUTC),
                  by = "hour"),
  DK36Title = unique(consumption_panel$DK36Title),
  stringsAsFactors = FALSE
) %>% as_tibble()

observed <- consumption_panel %>%
  select(TimeUTC, DK36Title) %>%
  mutate(observed = TRUE)

missing_hours <- full_grid %>%
  left_join(observed, by = c("TimeUTC", "DK36Title")) %>%
  filter(is.na(observed)) %>%
  mutate(
    hour_of_day = lubridate::hour(TimeUTC),
    day_of_week = lubridate::wday(TimeUTC, label = TRUE, abbr = TRUE),
    month       = lubridate::month(TimeUTC, label = TRUE, abbr = TRUE),
    year        = lubridate::year(TimeUTC),
    date        = as.Date(TimeUTC)
  )

cat("=== Total missing sector-hours:", format(nrow(missing_hours), big.mark = ","), "===\n\n")

cat("--- By hour of day ---\n")
missing_hours %>%
  count(hour_of_day) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  print(n = 24)

cat("\n--- By day of week ---\n")
missing_hours %>%
  count(day_of_week) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  print()

cat("\n--- By month ---\n")
missing_hours %>%
  count(year, month) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  arrange(year, month) %>%
  print(n = Inf)

cat("\n--- Top 20 dates with most missing sector-hours ---\n")
missing_hours %>%
  count(date, sort = TRUE) %>%
  mutate(
    n_sectors_affected = n,
    pct_of_sectors     = round(100 * n / n_distinct(consumption_panel$DK36Title), 1)
  ) %>%
  head(20) %>%
  print()

cat("\n--- Missing hours by sector ---\n")
missing_hours %>%
  count(DK36Title, sort = TRUE) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  left_join(distinct(consumption_panel, DK36Title, DK36_en), by = "DK36Title") %>%
  select(DK36_en, n, pct) %>%
  print(n = Inf)

cat("\n--- Are missings system-wide (all sectors missing same hour)? ---\n")
missing_hours %>%
  count(TimeUTC) %>%
  mutate(share_sectors = round(n / n_distinct(consumption_panel$DK36Title), 2)) %>%
  summarise(
    pct_hours_all_sectors_missing  = round(100 * mean(share_sectors == 1), 2),
    pct_hours_half_sectors_missing = round(100 * mean(share_sectors >= 0.5), 2),
    pct_hours_one_sector_missing   = round(100 * mean(share_sectors < 0.1), 2)
  ) %>%
  print()


theme_thesis <- theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 11),
    plot.subtitle   = element_text(size = 9, color = "grey40"),
    axis.title      = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.background = element_rect(fill = "white", color = NA)
  )

p1 <- missing_hours %>%
  count(hour_of_day) %>%
  ggplot(aes(x = hour_of_day, y = n)) +
  geom_col(fill = "#2C6E9E", width = 0.8) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "By hour of day",
       subtitle = "Peak missings 08:00–14:00",
       x = "Hour (UTC)", y = "Missing sector-hours") +
  theme_thesis

p2 <- missing_hours %>%
  count(day_of_week) %>%
  mutate(weekend = day_of_week %in% c("Sat", "Sun")) %>%
  ggplot(aes(x = day_of_week, y = n, fill = weekend)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("FALSE" = "#2C6E9E", "TRUE" = "#C0392B"),
                    guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "By day of week",
       subtitle = "Weekends (red) account for 60% of gaps",
       x = NULL, y = "Missing sector-hours") +
  theme_thesis

p3 <- missing_hours %>%
  mutate(year_num  = as.integer(year),
         month_num = as.integer(month)) %>%
  count(year_num, month_num, month) %>%
  ggplot(aes(x = month, y = factor(year_num), fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(n > 200, scales::comma(n), "")),
            size = 2.8, color = "white") +
  scale_fill_gradient(low = "#D6E8F5", high = "#1A4971",
                      labels = scales::comma, name = "Missing\nhours") +
  labs(title = "Over time",
       subtitle = "Missing sector-hours by year and month",
       x = NULL, y = NULL) +
  theme_thesis +
  theme(legend.position = "right")

p4 <- missing_hours %>%
  count(TimeUTC) %>%
  mutate(
    share_sectors = n / n_distinct(consumption_panel$DK36Title),
    type = case_when(
      share_sectors == 1   ~ "All sectors (100%)",
      share_sectors >= 0.5 ~ "Majority (50–99%)",
      TRUE                 ~ "Idiosyncratic (<50%)"
    ),
    type = factor(type, levels = c("All sectors (100%)", "Majority (50–99%)", "Idiosyncratic (<50%)"))
  ) %>%
  count(type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ggplot(aes(x = type, y = n, fill = type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("All sectors (100%)" = "#C0392B",
                               "Majority (50–99%)"  = "#E67E22",
                               "Idiosyncratic (<50%)" = "#2C6E9E"),
                    guide = "none") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "System-wide vs. idiosyncratic",
       subtitle = "Each bar is a mutually exclusive bin of missing timestamps by scope",
       x = NULL, y = "N missing timestamps") +
  theme_thesis

combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title    = "Missing Data Patterns in the Consumption Panel",
    subtitle = "28,867 missing sector-hours | July 2021 – September 2025",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )


combined

total_possible <- n_distinct(consumption_panel$DK36Title) *
  as.integer(difftime(max(consumption_panel$TimeUTC),
                      min(consumption_panel$TimeUTC),
                      units = "hours")) + 1L

total_observed <- nrow(consumption_panel)
total_missing  <- nrow(missing_hours)

missing_pct <- round(100 * total_missing / total_possible, 2)

cat("Total possible sector-hours:", format(total_possible, big.mark = ","), "\n")
cat("Total observed sector-hours:", format(total_observed, big.mark = ","), "\n")
cat("Total missing sector-hours: ", format(total_missing,  big.mark = ","), "\n")
cat("Missing share:              ", missing_pct, "%\n")

# Missing wind
p1 <- ggplot(missing_by_month, aes(x = month, y = n)) +
  geom_col(fill = "#2C6E9E", width = 25) +
  labs(
    title = "By month",
    subtitle = "Missing wind observations across the sample period",
    x = NULL,
    y = "Missing observations"
  ) +
  scale_y_continuous(labels = comma)

p2 <- ggplot(missing_by_hour, aes(x = fe_hour, y = n)) +
  geom_col(fill = "#2C6E9E", width = 0.8) +
  labs(
    title = "By hour of day",
    subtitle = "Intraday pattern of missing wind observations",
    x = "Hour",
    y = "Missing observations"
  ) +
  scale_y_continuous(labels = comma)

p3 <- ggplot(missing_by_day, aes(x = factor(date), y = n)) +
  geom_col(fill = "#2C6E9E", width = 0.8) +
  labs(
    title = "By day",
    subtitle = "Daily count of missing wind observations",
    x = NULL,
    y = "Missing observations"
  ) +
  scale_y_continuous(labels = comma) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

(p1 / p2 / p3) +
  plot_annotation(
    title = "Pattern of missing wind data",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13)
    )
  )

#### APPENDIX 8 - QUASI-COMPLIER PROFILING ──────────────────────────────────####
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

#### APPENDIX 10 - ANDERSON RUBIN  ──────────────────────────────────────────####
# ── Appendix Table: AR Confidence Sets Across Weighting Schemes ───────────###
# This code assumes ar_with_estimates is already constructed (see Section 1.3)

ar_appendix <- ar_with_estimates %>%
  filter(!AR_Empty) %>%
  select(industry, weight, AR_Lower, AR_Upper, AR_Width, AR_vs_Wald) %>%
  mutate(
    AR_Lower   = round(AR_Lower, 4),
    AR_Upper   = round(AR_Upper, 4),
    AR_Width   = round(AR_Width, 4),
    AR_vs_Wald = round(AR_vs_Wald, 3)
  ) %>%
  pivot_wider(
    names_from  = weight,
    values_from = c(AR_Lower, AR_Upper, AR_Width, AR_vs_Wald),
    names_glue  = "{weight}_{.value}"
  ) %>%
  # Reorder columns: industry, then consumption, employment, firm count
  select(
    industry,
    Consumption_AR_Lower, Consumption_AR_Upper, Consumption_AR_Width, Consumption_AR_vs_Wald,
    Employment_AR_Lower,  Employment_AR_Upper,  Employment_AR_Width,  Employment_AR_vs_Wald,
    `Firm count_AR_Lower`, `Firm count_AR_Upper`, `Firm count_AR_Width`, `Firm count_AR_vs_Wald`
  ) %>%
  arrange(industry)

# Rename for cleaner table headers
names(ar_appendix) <- c(
  "Industry",
  "Lower", "Upper", "Width", "AR/Wald",
  "Lower", "Upper", "Width", "AR/Wald",
  "Lower", "Upper", "Width", "AR/Wald"
)

# Print table
knitr::kable(
  ar_appendix,
  format   = "simple",
  caption  = "Table X. Anderson-Rubin 95% Confidence Sets by Weighting Scheme",
  digits   = 4,
  align    = c("l", rep("c", 12))
) %>%
  cat()

print(
  ar_appendix, n = 30)

install.packages("kableExtra")
library(kableExtra)
kbl(ar_appendix,
    caption  = "Anderson-Rubin 95\\% Confidence Sets by Weighting Scheme",
    booktabs = TRUE,
    align    = c("l", rep("c", 12))) %>%
  add_header_above(c(" " = 1,
                     "Consumption" = 4,
                     "Employment" = 4,
                     "Firm Count" = 4)) %>%
  footnote(general = paste(
    "AR confidence sets constructed by grid-search inversion over",
    "beta in [-0.35, 0.05] at 0.001 increments, alpha = 0.05.",
    "AR/Wald is the ratio of AR set width to the Wald 95% CI width.",
    "All 87 industry-weight combinations produce bounded, non-empty sets."
  ), threeparttable = TRUE)





#### APPENDIX 11 - ECOLOGICAL INFERENCE  ────────────────────────────────────####

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

#### APPENDIX 11 - ECOLOGICAL INFERENCE - SE-anchored ecological classificatio  ─────────────────────────────────────###
# ── EI SE-anchored ecological classification ───────────────────────────###
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

# ── EI Spearman test: weight divergence vs elasticity divergence ────────###
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


# ── EI Scatter plot: weight divergence vs elasticity divergence ─────────###

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


#### APPENDIX 12 - Dis-aggregated power curves  ─────────────────────────────####
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

p_curves_all



#### APPENDIX 13 - POWER RESULTS  ───────────────────────────────────────────####
# ── A Full Type M table at nearest simulated true effect (consumption weight, DK36)

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

# ── B. Full power surface across the effect grid (consumption weight, DK36) 

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






#### APPENDIX 15 - PACF RESULTS  ────────────────────────────────────────────####
# ── 1. Reduced-form OLS: absorb FEs, get residuals ─────────────────────────###
#    This mirrors the second-stage structure minus the endogenous price.
#    We want the residual autocorrelation of *demand* after controls.

rf_demand <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel,
  split = ~ DK36_en
)

# ── 2. Extract residuals by industry ───────────────────────────────────────###
#    Each element of the split feols list is one industry.

resid_by_industry <- imap_dfr(rf_demand, function(mod, split_label) {
  sector_name <- sub(".*sample:\\s*", "", split_label)
  tibble(
    DK36_en = sector_name,
    resid   = as.numeric(residuals(mod))
  )
})

# ── 3. Compute PACF for each industry (up to 48 hourly lags) ──────────────###
max_lag <- 48

pacf_by_industry <- resid_by_industry %>%
  group_by(DK36_en) %>%
  group_modify(~ {
    r <- .x$resid
    # Only compute if enough observations
    if (length(r) < max_lag + 10) return(tibble())
    p <- pacf(r, lag.max = max_lag, plot = FALSE)
    tibble(
      lag  = p$lag[, 1, 1],
      pacf = p$acf[, 1, 1]
    )
  }) %>%
  ungroup()

# ── 4. Also compute ACF for comparison ────────────────────────────────────###
acf_by_industry <- resid_by_industry %>%
  group_by(DK36_en) %>%
  group_modify(~ {
    r <- .x$resid
    if (length(r) < max_lag + 10) return(tibble())
    a <- acf(r, lag.max = max_lag, plot = FALSE)
    tibble(
      lag = a$lag[-1, 1, 1],   # drop lag 0 (always 1)
      acf = a$acf[-1, 1, 1]
    )
  }) %>%
  ungroup()

# ── 5. Summary table: lag-1 PACF per industry ─────────────────────────────###
#    This is the key diagnostic. The Thams bias factor is approximately
#    1 / (1 - alpha_d * alpha_w), so even alpha_d = 0.3 with alpha_w = 0.9
#    gives a bias factor of ~1.37 (37% overestimation).

lag1_summary <- pacf_by_industry %>%
  filter(lag == 1) %>%
  select(DK36_en, pacf_lag1 = pacf) %>%
  arrange(desc(abs(pacf_lag1)))

cat("\n============================================================\n")
cat("THAMS BIAS DIAGNOSTIC: Residual PACF at lag 1 by industry\n")
cat("(after absorbing fe_hour + fe_month + fe_dow)\n")
cat("============================================================\n\n")
print(lag1_summary, n = 30)

cat("\n── Summary statistics ──\n")
cat(sprintf("  Mean |PACF(1)|:   %.3f\n", mean(abs(lag1_summary$pacf_lag1))))
cat(sprintf("  Median |PACF(1)|: %.3f\n", median(abs(lag1_summary$pacf_lag1))))
cat(sprintf("  Max |PACF(1)|:    %.3f  (%s)\n",
            max(abs(lag1_summary$pacf_lag1)),
            lag1_summary$DK36_en[which.max(abs(lag1_summary$pacf_lag1))]))
cat(sprintf("  Min |PACF(1)|:    %.3f  (%s)\n",
            min(abs(lag1_summary$pacf_lag1)),
            lag1_summary$DK36_en[which.min(abs(lag1_summary$pacf_lag1))]))

# ── 6. Approximate Thams bias factor per industry ─────────────────────────###
#    Using lag-1 PACF of residuals as proxy for alpha_d.
#    For alpha_w, compute PACF of the wind instrument residuals too.

rf_wind <- feols(
  Wind_c ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel
)

wind_resid <- as.numeric(residuals(rf_wind))
wind_pacf1 <- pacf(wind_resid, lag.max = 1, plot = FALSE)$acf[1, 1, 1]

cat(sprintf("\n  Wind instrument residual PACF(1): %.3f\n", wind_pacf1))

thams_factor <- lag1_summary %>%
  mutate(
    alpha_w      = wind_pacf1,
    bias_factor  = 1 / (1 - pacf_lag1 * alpha_w),
    pct_overest  = (bias_factor - 1) * 100
  ) %>%
  arrange(desc(abs(pct_overest)))

cat("\n── Approximate Thams bias factor by industry ──\n")
cat("   (bias_factor > 1 means overestimation in absolute terms)\n\n")
print(thams_factor %>% select(DK36_en, pacf_lag1, bias_factor, pct_overest), n = 30)

# ── 7. PACF plot: all industries, first 24 lags ──────────────────────────###
n_obs_approx <- resid_by_industry %>%
  count(DK36_en) %>%
  summarise(median_n = median(n)) %>%
  pull(median_n)

crit_val <- qnorm(0.975) / sqrt(n_obs_approx)

p_pacf <- pacf_by_industry %>%
  filter(lag <= 24) %>%
  ggplot(aes(x = lag, y = pacf)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = c(-crit_val, crit_val),
             linetype = "dashed", colour = "steelblue", linewidth = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.5, colour = "grey30") +
  geom_point(size = 0.8, colour = "firebrick") +
  facet_wrap(~ DK36_en, ncol = 5, scales = "free_y") +
  labs(
    title = "Partial autocorrelation of demand residuals after FE absorption",
    subtitle = "fe_hour + fe_month + fe_dow | Dashed lines = 95% significance bounds",
    x = "Lag (hours)",
    y = "PACF"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text = element_text(size = 6.5),
    panel.grid.minor = element_blank()
  )

print(p_pacf)

# ── 8. ACF plot for comparison ───────────────────────────────────────────###
p_acf <- acf_by_industry %>%
  filter(lag <= 24) %>%
  ggplot(aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_hline(yintercept = c(-crit_val, crit_val),
             linetype = "dashed", colour = "steelblue", linewidth = 0.3) +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.5, colour = "grey30") +
  geom_point(size = 0.8, colour = "darkblue") +
  facet_wrap(~ DK36_en, ncol = 5, scales = "free_y") +
  labs(
    title = "Autocorrelation of demand residuals after FE absorption",
    subtitle = "fe_hour + fe_month + fe_dow | Dashed lines = 95% significance bounds",
    x = "Lag (hours)",
    y = "ACF"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text = element_text(size = 6.5),
    panel.grid.minor = element_blank()
  )

print(p_acf)

# ── 9. Compact summary for thesis Table / Appendix ───────────────────────###
#    Lag 1, 2, and 24 PACF values per industry

compact_table <- pacf_by_industry %>%
  filter(lag %in% c(1, 2, 24)) %>%
  pivot_wider(names_from = lag, values_from = pacf, names_prefix = "PACF_lag") %>%
  left_join(
    thams_factor %>% select(DK36_en, bias_factor, pct_overest),
    by = "DK36_en"
  ) %>%
  arrange(desc(abs(PACF_lag1)))

cat("\n── Compact diagnostic table (for thesis appendix) ──\n\n")
print(compact_table, n = 30)



# Extract residuals from existing reduced-form feols and compute PACF(1)
rf_models <- feols(
  log_consumption ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel,
  cluster = ~fe_week,
  split = ~DK36_en
)

models_list <- as.list(rf_models)
model_names <- names(models_list)

pacf_table <- map_dfr(seq_along(models_list), function(i) {
  resids <- residuals(models_list[[i]])
  pacf_val <- pacf(resids, lag.max = 2, plot = FALSE)$acf
  tibble(
    Industry = str_remove(model_names[i], "^.*\\."),
    PACF_1 = round(pacf_val[1], 3),
    PACF_2 = round(pacf_val[2], 3)
  )
})

print(pacf_table, n = 30)




