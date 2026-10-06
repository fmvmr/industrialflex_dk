library(fixest)

# ── Pooled identification chain regressions ───────────────────────────────────
# Using consumption-weighted scheme as baseline

# First stage: price ~ wind + controls + FE
fs_pooled <- feols(
  log_P_c ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel,
  cluster = ~ fe_week
)

# Reduced form: consumption ~ wind + controls + FE
rf_pooled <- feols(
  log_consumption ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel,
  cluster = ~ fe_week
)

# OLS: consumption ~ price + controls + FE
ols_pooled <- feols(
  log_consumption ~ log_P_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data = consumption_panel,
  cluster = ~ fe_week
)

# 2SLS pooled: consumption ~ price + controls + FE | price ~ wind
iv_pooled <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_week
)

# ── Extract and verify Wald ratio ─────────────────────────────────────────────
pi_rf <- coef(rf_pooled)["Wind_c"]
pi_fs <- coef(fs_pooled)["Wind_c"]
beta_wald <- pi_rf / pi_fs

beta_2sls <- coef(iv_pooled)["fit_log_P_c"]

cat("Reduced form (pi_rf):", round(pi_rf, 6), "\n")
cat("First stage  (pi_fs):", round(pi_fs, 6), "\n")
cat("Wald ratio   (beta): ", round(beta_wald, 6), "\n")
cat("2SLS estimate:       ", round(beta_2sls, 6), "\n")
cat("Difference (should be ~0):", round(beta_wald - beta_2sls, 8), "\n")

# ── Quick summary table ───────────────────────────────────────────────────────
etable(fs_pooled, rf_pooled, ols_pooled, iv_pooled,
       keep = c("Wind_c", "log_P_c", "fit_log_P_c"),
       se.below = TRUE,
       digits = 4,
       title = "Identification Chain — Pooled Estimates")