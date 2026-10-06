# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ============================================================================
# CONTRACT STRUCTURE ROBUSTNESS: TEMPORAL STABILITY OF IV ESTIMATES
# Tests whether the shift from fixed to flexible contracts biases results
# ============================================================================
# Requires: consumption_panel, dk36_translate already in environment
# Uses same v4.0 specification: fe_hour + fe_month + fe_dow, cluster ~ fe_week

library(fixest)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)

fixest::setFixest_notes(FALSE)


# ============================================================================
# TASK 1: YEAR-BY-YEAR IV REGRESSIONS
# ============================================================================

# ── 1.1 Create year variable ───────────────────────────────────────────────####
consumption_panel_yoy <- consumption_panel %>%
  mutate(est_year = as.integer(format(TimeUTC, "%Y")))

years <- sort(unique(consumption_panel_yoy$est_year))
cat("Years in panel:", paste(years, collapse = ", "), "\n")

# ── 1.2 Run IV split regressions per year ──────────────────────────────────####

run_iv_by_year <- function(data, price_var, wind_var, temp_var, weight_label) {
  
  map_dfr(years, function(yr) {
    cat("  Year:", yr, "| Weight:", weight_label, "\n")
    
    sub <- data %>% filter(est_year == yr)
    
    # Need year-specific fe_month (already year-month format)
    # fe_dow stays the same
    fml <- as.formula(paste0(
      "log_consumption ~ ", temp_var, " + log_gas + log_coal + log_carbon | ",
      "fe_hour + fe_month + fe_dow | ",
      price_var, " ~ ", wind_var
    ))
    
    model <- tryCatch(
      feols(fml, data = sub, cluster = ~fe_week, split = ~DK36_en),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
    
    if (is.null(model)) return(NULL)
    
    # Extract results per industry
    sector_names <- names(model)
    
    map_dfr(seq_along(sector_names), function(i) {
      m  <- model[[i]]
      s  <- sector_names[i]
      ct <- fixest::coeftable(m)
      
      iv_row <- rownames(ct)[str_detect(rownames(ct), "^fit_")]
      if (length(iv_row) == 0) return(NULL)
      
      fs_f <- tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                        error = function(e) NA_real_)
      
      tibble(
        year     = yr,
        sector   = str_remove(s, "^sample\\.var: DK36_en; sample: "),
        weight   = weight_label,
        estimate = as.numeric(ct[iv_row, "Estimate"]),
        se       = as.numeric(ct[iv_row, "Std. Error"]),
        p_value  = as.numeric(ct[iv_row, "Pr(>|t|)"]),
        fs_f     = fs_f,
        n_obs    = nobs(m)
      )
    })
  })
}

cat("\n=== Running year-by-year IV (Consumption weights) ===\n")
yearly_c <- run_iv_by_year(
  consumption_panel_yoy, "log_P_c", "Wind_c", "Temp_c", "Consumption"
)

cat("\n=== Running year-by-year IV (Employment weights) ===\n")
yearly_emp <- run_iv_by_year(
  consumption_panel_yoy, "log_P_emp", "Wind_emp", "Temp_emp", "Employment"
)

cat("\n=== Running year-by-year IV (Firm count weights) ===\n")
yearly_firm <- run_iv_by_year(
  consumption_panel_yoy, "log_P_firm", "Wind_firm", "Temp_firm", "Firm count"
)

yearly_all <- bind_rows(yearly_c, yearly_emp, yearly_firm) %>%
  mutate(
    sig  = p_value < 0.05,
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se
  )

# ── 1.3 Summary table: year-by-year ───────────────────────────────────────####

yearly_summary <- yearly_all %>%
  filter(weight == "Consumption") %>%
  group_by(year) %>%
  summarise(
    n_sectors     = n(),
    mean_elast    = round(mean(estimate, na.rm = TRUE), 4),
    median_elast  = round(median(estimate, na.rm = TRUE), 4),
    sd_elast      = round(sd(estimate, na.rm = TRUE), 4),
    n_sig         = sum(sig, na.rm = TRUE),
    n_negative    = sum(estimate < 0, na.rm = TRUE),
    mean_F        = round(mean(fs_f, na.rm = TRUE), 1),
    median_F      = round(median(fs_f, na.rm = TRUE), 1),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("YEAR-BY-YEAR IV SUMMARY (Consumption Weights)\n")
cat("============================================================\n")
print(yearly_summary)

# ── 1.4 Trend test: is the mean elasticity trending over time? ────────────####

yearly_means <- yearly_all %>%
  filter(weight == "Consumption") %>%
  group_by(year) %>%
  summarise(mean_elast = mean(estimate, na.rm = TRUE), .groups = "drop")

trend_test <- cor.test(yearly_means$year, yearly_means$mean_elast, method = "pearson")
cat("\nTrend test (Pearson correlation of year vs mean elasticity):\n")
cat("  r =", round(trend_test$estimate, 4), "\n")
cat("  p =", round(trend_test$p.value, 4), "\n")
cat("  Interpretation:",
    ifelse(trend_test$p.value < 0.05,
           "Significant trend detected — contract structure shift may affect estimates.",
           "No significant trend — estimates temporally stable."), "\n")

# ── 1.5 Visualisations: year-by-year ──────────────────────────────────────####

th_custom <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

# --- Figure A: Coefficient evolution by industry (consumption weights) ---
fig_yearly_industry <- yearly_all %>%
  filter(weight == "Consumption") %>%
  ggplot(aes(x = year, y = estimate, group = sector)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  geom_line(alpha = 0.3, linewidth = 0.4, colour = "grey50") +
  geom_point(aes(colour = sig), size = 1.5, alpha = 0.7) +
  scale_colour_manual(
    values = c("TRUE" = "#2166AC", "FALSE" = "grey70"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = years) +
  labs(
    x        = NULL,
    y        = "Price elasticity",
    title    = "Year-by-Year IV Elasticity Estimates by Industry",
    subtitle = "Each line is one DK36 industry; consumption weights, SE clustered by week",
    caption  = "Specification: fe_hour + fe_month + fe_dow. Instrument: day-ahead wind forecast."
  ) +
  th_custom

fig_yearly_industry

# --- Figure B: Mean elasticity with confidence ribbon across years ---
yearly_agg <- yearly_all %>%
  filter(weight == "Consumption") %>%
  group_by(year) %>%
  summarise(
    mean_est = mean(estimate, na.rm = TRUE),
    se_mean  = sd(estimate, na.rm = TRUE) / sqrt(n()),
    median_est = median(estimate, na.rm = TRUE),
    q25 = quantile(estimate, 0.25, na.rm = TRUE),
    q75 = quantile(estimate, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

fig_yearly_mean <- ggplot(yearly_agg, aes(x = year)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#2166AC", alpha = 0.15) +
  geom_line(aes(y = mean_est), colour = "#2166AC", linewidth = 1) +
  geom_point(aes(y = mean_est), colour = "#2166AC", size = 3) +
  geom_line(aes(y = median_est), colour = "#B2182B", linewidth = 0.7, linetype = "dotted") +
  geom_point(aes(y = median_est), colour = "#B2182B", size = 2, shape = 17) +
  scale_x_continuous(breaks = years) +
  labs(
    x        = NULL,
    y        = "Price elasticity",
    title    = "Evolution of Mean and Median Price Elasticity Across Years",
    subtitle = "Blue = mean (ribbon = IQR); red = median | Consumption weights",
    caption  = paste0("Pearson r(year, mean elasticity) = ", round(trend_test$estimate, 3),
                      ", p = ", round(trend_test$p.value, 3))
  ) +
  th_custom
fig_yearly_mean

# --- Figure C: Cross-weight consistency by year ---
yearly_cross_weight <- yearly_all %>%
  group_by(year, weight) %>%
  summarise(mean_est = mean(estimate, na.rm = TRUE), .groups = "drop")

fig_yearly_weights <- ggplot(yearly_cross_weight,
                              aes(x = year, y = mean_est, colour = weight)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("Consumption" = "#2166AC",
                                  "Employment"  = "#B2182B",
                                  "Firm count"  = "#1B7837")) +
  scale_x_continuous(breaks = years) +
  labs(
    x      = NULL,
    y      = "Mean price elasticity",
    colour = NULL,
    title    = "Cross-Weight Stability of Mean Elasticity Over Time",
    subtitle = "Convergence across weights indicates geographic allocation is not driving trends"
  ) +
  th_custom

fig_yearly_weights

# --- Figure D: First-stage F evolution ---
yearly_F <- yearly_all %>%
  filter(weight == "Consumption") %>%
  group_by(year) %>%
  summarise(
    mean_F   = mean(fs_f, na.rm = TRUE),
    median_F = median(fs_f, na.rm = TRUE),
    min_F    = min(fs_f, na.rm = TRUE),
    .groups  = "drop"
  )

fig_yearly_F <- ggplot(yearly_F, aes(x = year)) +
  geom_hline(yintercept = 10, linetype = "dashed", colour = "#d7191c", linewidth = 0.4) +
  geom_line(aes(y = median_F), colour = "#2166AC", linewidth = 0.9) +
  geom_point(aes(y = median_F), colour = "#2166AC", size = 2.5) +
  geom_line(aes(y = min_F), colour = "#B2182B", linewidth = 0.6, linetype = "dotted") +
  annotate("text", x = min(years) + 0.1, y = 11, label = "F = 10",
           hjust = 0, size = 3, colour = "#d7191c") +
  scale_x_continuous(breaks = years) +
  labs(
    x        = NULL,
    y        = "First-stage F-statistic",
    title    = "Instrument Strength Over Time",
    subtitle = "Blue = median F across industries; red = minimum F"
  ) +
  th_custom

fig_yearly_F

# Print all year-by-year figures
fig_yearly_industry
fig_yearly_mean
fig_yearly_weights
fig_yearly_F

# ── 1.6 Detailed year-by-year table (all industries) ──────────────────────####
yearly_detail <- yearly_all %>%
  filter(weight == "Consumption") %>%
  select(year, sector, estimate, se, p_value, fs_f) %>%
  mutate(
    sig = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**",
                    p_value < 0.10 ~ "*", TRUE ~ ""),
    cell = sprintf("%.4f%s", estimate, sig)
  ) %>%
  select(sector, year, cell) %>%
  pivot_wider(names_from = year, values_from = cell) %>%
  arrange(sector)

cat("\n============================================================\n")
cat("YEAR-BY-YEAR ELASTICITIES (Consumption Weights)\n")
cat("============================================================\n")
print(yearly_detail, n = 35)


# ============================================================================
# TASK 2: ENERGY CRISIS vs POST-CRISIS
# ============================================================================

# ── 2.1 Define crisis periods ─────────────────────────────────────────────####
# Energy crisis: July 2021 – December 2022 (onset of gas price surge through
# the peak of wholesale electricity prices in the NordPool DK zones)
# Post-crisis: January 2023 – September 2025

consumption_panel_crisis <- consumption_panel %>%
  mutate(
    crisis_period = case_when(
      TimeUTC >= as.POSIXct("2021-07-01", tz = "UTC") &
        TimeUTC < as.POSIXct("2023-01-01", tz = "UTC") ~ "Crisis (Jul 2021 – Dec 2022)",
      TimeUTC >= as.POSIXct("2023-01-01", tz = "UTC") ~ "Post-crisis (Jan 2023 – Sep 2025)",
      TRUE ~ "Pre-crisis (Jan – Jun 2021)"
    )
  )

cat("\nObservations by period:\n")
consumption_panel_crisis %>%
  count(crisis_period) %>%
  mutate(share = round(100 * n / sum(n), 1)) %>%
  print()

# ── 2.2 Run IV by crisis period ───────────────────────────────────────────####

run_iv_by_period <- function(data, price_var, wind_var, temp_var, weight_label) {
  
  periods <- sort(unique(data$crisis_period))
  
  map_dfr(periods, function(per) {
    cat("  Period:", per, "| Weight:", weight_label, "\n")
    
    sub <- data %>% filter(crisis_period == per)
    
    # Check sufficient variation
    if (n_distinct(sub$fe_month) < 3) {
      cat("    Skipping: too few months\n")
      return(NULL)
    }
    
    fml <- as.formula(paste0(
      "log_consumption ~ ", temp_var, " + log_gas + log_coal + log_carbon | ",
      "fe_hour + fe_month + fe_dow | ",
      price_var, " ~ ", wind_var
    ))
    
    model <- tryCatch(
      feols(fml, data = sub, cluster = ~fe_week, split = ~DK36_en),
      error = function(e) { message("  Failed: ", e$message); NULL }
    )
    
    if (is.null(model)) return(NULL)
    
    sector_names <- names(model)
    
    map_dfr(seq_along(sector_names), function(i) {
      m  <- model[[i]]
      s  <- sector_names[i]
      ct <- fixest::coeftable(m)
      
      iv_row <- rownames(ct)[str_detect(rownames(ct), "^fit_")]
      if (length(iv_row) == 0) return(NULL)
      
      fs_f <- tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                        error = function(e) NA_real_)
      
      tibble(
        period   = per,
        sector   = str_remove(s, "^sample\\.var: DK36_en; sample: "),
        weight   = weight_label,
        estimate = as.numeric(ct[iv_row, "Estimate"]),
        se       = as.numeric(ct[iv_row, "Std. Error"]),
        p_value  = as.numeric(ct[iv_row, "Pr(>|t|)"]),
        fs_f     = fs_f,
        n_obs    = nobs(m)
      )
    })
  })
}

cat("\n=== Running crisis-period IV (Consumption weights) ===\n")
crisis_c <- run_iv_by_period(
  consumption_panel_crisis, "log_P_c", "Wind_c", "Temp_c", "Consumption"
)

cat("\n=== Running crisis-period IV (Employment weights) ===\n")
crisis_emp <- run_iv_by_period(
  consumption_panel_crisis, "log_P_emp", "Wind_emp", "Temp_emp", "Employment"
)

cat("\n=== Running crisis-period IV (Firm count weights) ===\n")
crisis_firm <- run_iv_by_period(
  consumption_panel_crisis, "log_P_firm", "Wind_firm", "Temp_firm", "Firm count"
)

crisis_all <- bind_rows(crisis_c, crisis_emp, crisis_firm) %>%
  mutate(
    sig   = p_value < 0.05,
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se,
    period = factor(period, levels = c(
      "Pre-crisis (Jan – Jun 2021)",
      "Crisis (Jul 2021 – Dec 2022)",
      "Post-crisis (Jan 2023 – Sep 2025)"
    ))
  )

# ── 2.3 Summary table: crisis vs post-crisis ──────────────────────────────####

crisis_summary <- crisis_all %>%
  filter(weight == "Consumption") %>%
  group_by(period) %>%
  summarise(
    n_sectors    = n(),
    mean_elast   = round(mean(estimate, na.rm = TRUE), 4),
    median_elast = round(median(estimate, na.rm = TRUE), 4),
    sd_elast     = round(sd(estimate, na.rm = TRUE), 4),
    n_sig        = sum(sig, na.rm = TRUE),
    n_negative   = sum(estimate < 0, na.rm = TRUE),
    mean_F       = round(mean(fs_f, na.rm = TRUE), 1),
    .groups      = "drop"
  )

cat("\n============================================================\n")
cat("CRISIS vs POST-CRISIS SUMMARY (Consumption Weights)\n")
cat("============================================================\n")
print(crisis_summary)

# ── 2.4 Formal difference test: crisis vs post-crisis ─────────────────────####

crisis_paired <- crisis_all %>%
  filter(weight == "Consumption",
         period %in% c("Crisis (Jul 2021 – Dec 2022)",
                        "Post-crisis (Jan 2023 – Sep 2025)")) %>%
  select(sector, period, estimate) %>%
  pivot_wider(names_from = period, values_from = estimate,
              names_prefix = "est_") %>%
  mutate(diff = .[[3]] - .[[2]])  # post-crisis minus crisis

if (nrow(crisis_paired) >= 5) {
  t_crisis <- t.test(crisis_paired$diff, mu = 0)
  wilcox_crisis <- wilcox.test(crisis_paired$diff, mu = 0)
  
  cat("\nPaired difference test (post-crisis minus crisis):\n")
  cat("  Mean difference:", round(mean(crisis_paired$diff, na.rm = TRUE), 4), "\n")
  cat("  t-test p-value:", round(t_crisis$p.value, 4), "\n")
  cat("  Wilcoxon p-value:", round(wilcox_crisis$p.value, 4), "\n")
  cat("  Interpretation:",
      ifelse(t_crisis$p.value < 0.05,
             "Significant shift — contract structure change may affect estimates.",
             "No significant shift — elasticities stable across crisis regimes."), "\n")
}

# ── 2.5 Visualisations: crisis vs post-crisis ─────────────────────────────####

pal_crisis <- c(
  "Pre-crisis (Jan – Jun 2021)"      = "#F6AE2D",
  "Crisis (Jul 2021 – Dec 2022)"     = "#F26157",
  "Post-crisis (Jan 2023 – Sep 2025)" = "#2E86AB"
)

# --- Figure E: Side-by-side coefficient plot ---
sector_order_crisis <- crisis_all %>%
  filter(weight == "Consumption", period == "Post-crisis (Jan 2023 – Sep 2025)") %>%
  arrange(estimate) %>%
  pull(sector)

fig_crisis_coef <- crisis_all %>%
  filter(weight == "Consumption") %>%
  mutate(sector = factor(sector, levels = sector_order_crisis)) %>%
  ggplot(aes(x = estimate, y = sector, colour = period)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0, linewidth = 0.4, alpha = 0.5,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 2, position = position_dodge(width = 0.6)) +
  scale_colour_manual(values = pal_crisis, name = NULL) +
  labs(
    x        = "Price elasticity",
    y        = NULL,
    title    = "Price Elasticity: Energy Crisis vs Post-Crisis",
    subtitle = "Consumption weights, SE clustered by week",
    caption  = "95% confidence intervals. Industries ordered by post-crisis estimate."
  ) +
  th_custom +
  theme(axis.text.y = element_text(size = 7))

# --- Figure F: Paired difference plot ---
if (nrow(crisis_paired) >= 5) {
  fig_crisis_diff <- crisis_paired %>%
    mutate(
      sector = factor(sector, levels = crisis_paired %>%
                        arrange(diff) %>% pull(sector)),
      direction = ifelse(diff > 0, "More elastic post-crisis", "Less elastic post-crisis")
    ) %>%
    ggplot(aes(x = diff, y = sector, fill = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_col(width = 0.6, alpha = 0.7) +
    scale_fill_manual(values = c("More elastic post-crisis" = "#2E86AB",
                                  "Less elastic post-crisis" = "#F26157"),
                       name = NULL) +
    labs(
      x        = "Change in elasticity (post-crisis minus crisis)",
      y        = NULL,
      title    = "Shift in Price Elasticity: Post-Crisis vs Crisis Period",
      subtitle = paste0("Paired t-test p = ", round(t_crisis$p.value, 3),
                        " | Wilcoxon p = ", round(wilcox_crisis$p.value, 3)),
      caption  = "Positive = more price-responsive after the crisis (consistent with flexible contract adoption)"
    ) +
    th_custom +
    theme(axis.text.y = element_text(size = 7))
}

# --- Figure G: Cross-weight crisis comparison ---
crisis_cross <- crisis_all %>%
  filter(period != "Pre-crisis (Jan – Jun 2021)") %>%
  group_by(period, weight) %>%
  summarise(
    mean_est   = mean(estimate, na.rm = TRUE),
    median_est = median(estimate, na.rm = TRUE),
    .groups    = "drop"
  )

fig_crisis_weights <- ggplot(crisis_cross,
                              aes(x = weight, y = mean_est, fill = period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  scale_fill_manual(values = pal_crisis, name = NULL) +
  labs(
    x     = NULL,
    y     = "Mean price elasticity",
    title = "Mean Elasticity by Weighting Scheme and Crisis Period"
  ) +
  th_custom

# Print crisis figures
fig_crisis_coef
if (exists("fig_crisis_diff")) fig_crisis_diff
fig_crisis_weights


# ============================================================================
# COMBINED SUMMARY
# ============================================================================

cat("\n============================================================\n")
cat("TEMPORAL STABILITY SUMMARY\n")
cat("============================================================\n\n")

cat("1. YEAR-BY-YEAR ANALYSIS:\n")
cat("   Years covered:", paste(years, collapse = ", "), "\n")
cat("   Mean elasticity range:",
    round(min(yearly_summary$mean_elast), 4), "to",
    round(max(yearly_summary$mean_elast), 4), "\n")
cat("   Trend test (r, p):", round(trend_test$estimate, 4), ",",
    round(trend_test$p.value, 4), "\n\n")

cat("2. CRISIS vs POST-CRISIS:\n")
print(crisis_summary)
if (exists("t_crisis")) {
  cat("   Paired t-test p:", round(t_crisis$p.value, 4), "\n")
  cat("   Wilcoxon p:", round(wilcox_crisis$p.value, 4), "\n")
}

