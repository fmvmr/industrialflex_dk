# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# =============================================================================
# THAMS BIAS DIAGNOSTIC: PACF of residuals after fixed-effect absorption
# Purpose: Check how much stochastic autocorrelation survives the FE structure
#          (fe_hour + fe_month + fe_dow) in the demand equation.
#          If residual PACF at lag 1 is near zero, the Thams bias is negligible.
#          If it remains large (e.g., > 0.3), the bias warrants discussion.
#
# Insert after consumption_panel is built and before the IV estimation block.
# =============================================================================
library(f)
library(fixest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# ── 1. Reduced-form OLS: absorb FEs, get residuals ─────────────────────────####
#    This mirrors the second-stage structure minus the endogenous price.
#    We want the residual autocorrelation of *demand* after controls.

rf_demand <- feols(
 log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
   fe_hour + fe_month + fe_dow,
 data = consumption_panel,
 split = ~ DK36_en
)

# ── 2. Extract residuals by industry ───────────────────────────────────────####
#    Each element of the split feols list is one industry.

resid_by_industry <- imap_dfr(rf_demand, function(mod, split_label) {
  sector_name <- sub(".*sample:\\s*", "", split_label)
  tibble(
    DK36_en = sector_name,
    resid   = as.numeric(residuals(mod))
  )
})

# ── 3. Compute PACF for each industry (up to 48 hourly lags) ──────────────####
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

# ── 4. Also compute ACF for comparison ────────────────────────────────────####
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

# ── 5. Summary table: lag-1 PACF per industry ─────────────────────────────####
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

# ── 6. Approximate Thams bias factor per industry ─────────────────────────####
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

# ── 7. PACF plot: all industries, first 24 lags ──────────────────────────####
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

# ── 8. ACF plot for comparison ───────────────────────────────────────────####
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

# ── 9. Compact summary for thesis Table / Appendix ───────────────────────####
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

# ── INTERPRETATION GUIDE ─────────────────────────────────────────────────####
# 
# If median |PACF(1)| < 0.10:
#   → Residual autocorrelation is negligible after FE absorption.
#   → Thams bias is not a material concern for your specification.
#   → State this as evidence that FEs serve as an effective mitigation.
#
# If median |PACF(1)| is 0.10–0.30:
#   → Moderate residual persistence survives.
#   → Thams bias factor is 1.1–1.4× (with typical wind PACF ~0.9).
#   → Acknowledge in robustness section; note direction is overestimation
#     in absolute terms (elasticity appears more negative than truth).
#
# If median |PACF(1)| > 0.30:
#   → Substantial residual autocorrelation.
#   → Consider nuisance IV (adding lagged demand) as robustness check.
#   → Report both specifications and discuss the gap.
#
# Note: The PACF of the *instrument* residuals also matters. If your FEs
# also absorb wind autocorrelation (e.g., seasonal wind patterns), then
# alpha_w is also reduced, further shrinking the bias.
