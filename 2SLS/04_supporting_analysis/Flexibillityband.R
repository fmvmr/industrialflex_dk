# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ── Implied price variation with level first stage ────────────────────────────
pi_fs_level   <- -0.026425
price_variation_EUR <- abs(system_wind$wind_var_MWh * pi_fs_level)
cat("System wind variation (P5-P95):", round(system_wind$wind_var_MWh, 1), "MWh\n")
cat("Implied price variation (EUR/MWh):", round(price_variation_EUR, 2), "\n")

# ── Sector-level flexibility bands ───────────────────────────────────────────
flexibility_bands <- iv_c %>%
  mutate(sig_binary = pval < 0.05) %>%
  filter(sig_binary) %>%
  left_join(sector_means, by = "DK36_en") %>%
  mutate(
    price_variation_EUR = price_variation_EUR,
    abs_response_MWh    = abs(coef) * (mean_C_MWh / mean_P) * price_variation_EUR,
    pct_of_avg_demand   = abs_response_MWh / mean_C_MWh * 100
  ) %>%
  arrange(coef) %>%
  select(DK36_en, coef, mean_C_MWh, mean_P,
         price_variation_EUR, abs_response_MWh, pct_of_avg_demand)

cat("\nFlexibility bands:\n")
print(flexibility_bands, n = Inf)

# ── Aggregate across significant negative sectors ─────────────────────────────
agg <- flexibility_bands %>%
  filter(coef < 0) %>%
  summarise(
    total_response_MWh = sum(abs_response_MWh),
    total_mean_C_MWh   = sum(mean_C_MWh)
  )

cat("\nAggregate flexibility (significant negative sectors):\n")
cat("Total absolute response:", round(agg$total_response_MWh, 2), "MWh\n")
cat("Total mean consumption:", round(agg$total_mean_C_MWh, 1), "MWh\n")
cat("Aggregate % of combined consumption:",
    round(agg$total_response_MWh / agg$total_mean_C_MWh * 100, 3), "%\n")








