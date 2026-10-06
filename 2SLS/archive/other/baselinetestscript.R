# Baseline test 
# ============================================================================
# = CHECK FOR "BASELINE" SUBSETS WITH NEAR-ZERO FIRST STAGE ==================
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Quantile bins instead of fixed cutoffs
# ----------------------------------------------------------------------------

consumption_panel <- consumption_panel %>%
  filter(Wind_c < 5)

IV_model_het_DK10 <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_month, split = ~ DK36Title
)
