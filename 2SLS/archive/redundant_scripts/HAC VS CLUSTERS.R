# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# Baseline: week-clustered SEs
IV_model_het_DK10_W <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36_en
)

# Alternative: HAC / Newey-West SEs
IV_model_het_DK10_HAC <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  panel.id  = ~ DK36_en + TimeUTC,   # let fixest infer the time step
  vcov      = NW ~ TimeUTC,
  split     = ~ DK36_en
)



# Models are stored directly on the fixest_multi object, not in $models
mods_w <- IV_model_het_DK10_W
mods_h <- IV_model_het_DK10_HAC

# Check names now
nms_w <- names(mods_w)
nms_h <- names(mods_h)

clean_sector <- function(x) sub(".*sample: ", "", x)

comp <- data.frame(
  sector     = clean_sector(nms_w),
  beta       = sapply(mods_w, get_iv_coef),
  se_cluster = sapply(mods_w, get_iv_se),
  se_hac     = sapply(mods_h[nms_h %in% nms_w], get_iv_se),
  row.names  = NULL
)

comp$se_ratio <- comp$se_hac / comp$se_cluster
comp$pct_diff <- 100 * (comp$se_hac - comp$se_cluster) / comp$se_cluster

print(comp[order(comp$se_ratio), ], digits = 3)
