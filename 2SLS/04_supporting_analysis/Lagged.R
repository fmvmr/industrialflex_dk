# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# Robustness lagged variables. 

# Create lag within each sector-panel (must be done in data, not inside feols)
consumption_panel <- consumption_panel %>% arrange(DK36_en, TimeUTC)

consumption_panel$log_cons_lag <- ave(
  consumption_panel$log_consumption,
  consumption_panel$DK36_en,
  FUN = function(x) c(NA, x[-length(x)])
)
consumption_panel %>%
  select(DK36_en, TimeUTC, log_consumption, log_cons_lag) %>%
  head(5)

IV_model_het_DK10_W_lag <- feols(
  log_consumption ~ log_cons_lag + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36_en
)

# Compare
mods_base <- IV_model_het_DK10_W
mods_lag  <- IV_model_het_DK10_W_lag

comp_lag <- data.frame(
  sector    = clean_sector(names(mods_base)),
  beta_base = sapply(mods_base, get_iv_coef),
  beta_lag  = sapply(mods_lag,  get_iv_coef),
  se_base   = sapply(mods_base, get_iv_se),
  se_lag    = sapply(mods_lag,  get_iv_se),
  row.names = NULL
)

comp_lag$beta_change_pct <- 100 * (comp_lag$beta_lag - comp_lag$beta_base) / 
  abs(comp_lag$beta_base)

print(comp_lag[order(abs(comp_lag$beta_change_pct), decreasing = TRUE), ], 
      digits = 3)
