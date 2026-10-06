# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ── Rescale wind to GWh for readable coefficients ─────────────────────────────
consumption_panel <- consumption_panel |>
  mutate(
    Wind_c_GWh    = Wind_c    / 1000,
    Wind_emp_GWh  = Wind_emp  / 1000,
    Wind_firm_GWh = Wind_firm / 1000
  )

# ── Pooled identification chain regressions (GWh-scaled wind) ─────────────────

# First stage
fs_pooled <- feols(
  log_P_c ~ Wind_c_GWh + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data    = consumption_panel,
  cluster = ~ fe_week
)

# Reduced form
rf_pooled <- feols(
  log_consumption ~ Wind_c_GWh + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data    = consumption_panel,
  cluster = ~ fe_week
)

# OLS
ols_pooled <- feols(
  log_consumption ~ log_P_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow,
  data    = consumption_panel,
  cluster = ~ fe_week
)

# 2SLS
iv_pooled <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c_GWh,
  data    = consumption_panel,
  cluster = ~ fe_week
)

# ── Verify Wald ratio ─────────────────────────────────────────────────────────
pi_rf    <- coef(rf_pooled)["Wind_c_GWh"]
pi_fs    <- coef(fs_pooled)["Wind_c_GWh"]
beta_iv  <- coef(iv_pooled)["fit_log_P_c"]
beta_ols <- coef(ols_pooled)["log_P_c"]

cat("First stage  (pi_fs): ", round(pi_fs,   6), "\n")
cat("Reduced form (pi_rf): ", round(pi_rf,   6), "\n")
cat("Wald ratio:           ", round(pi_rf / pi_fs, 6), "\n")
cat("2SLS estimate:        ", round(beta_iv,  6), "\n")
cat("OLS estimate:         ", round(beta_ols, 6), "\n")
cat("Difference (should ~0):", round((pi_rf / pi_fs) - beta_iv, 8), "\n")

# ── Extract for table ─────────────────────────────────────────────────────────
chain_table <- tibble(
  ` `          = c("Coefficient", "Estimate", "SE"),
  `First Stage`  = c(
    "$\\pi_1^{Fs}$",
    formatC(coef(fs_pooled)["Wind_c_GWh"],        digits = 4, format = "f"),
    paste0("(", formatC(se(fs_pooled)["Wind_c_GWh"],  digits = 4, format = "f"), ")")
  ),
  `Reduced Form` = c(
    "$\\pi_1^{Rf}$",
    formatC(coef(rf_pooled)["Wind_c_GWh"],        digits = 4, format = "f"),
    paste0("(", formatC(se(rf_pooled)["Wind_c_GWh"],  digits = 4, format = "f"), ")")
  ),
  OLS = c(
    "$\\hat{\\beta}^{OLS}$",
    formatC(coef(ols_pooled)["log_P_c"],          digits = 4, format = "f"),
    paste0("(", formatC(se(ols_pooled)["log_P_c"],    digits = 4, format = "f"), ")")
  ),
  `2SLS` = c(
    "$\\beta$",
    formatC(coef(iv_pooled)["fit_log_P_c"],       digits = 4, format = "f"),
    paste0("(", formatC(se(iv_pooled)["fit_log_P_c"], digits = 4, format = "f"), ")")
  )
)

print(chain_table)



unique(consumption_panel$DK36_en)
unique(consumption_panel$DK36Title)
