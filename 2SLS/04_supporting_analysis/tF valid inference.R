# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# =============================================================================
# tF VALID t-RATIO INFERENCE FOR IV (Lee et al., 2022, AER)
# Purpose: Apply the tF standard error adjustment to 2SLS estimates
#          from split feols IV regressions. Compares conventional,
#          tF-adjusted, and Anderson-Rubin confidence intervals.
#
# Reference: Lee, D.S., McCrary, J., Moreira, M.J., & Porter, J. (2022).
#            Valid t-Ratio Inference for IV. American Economic Review, 112(10), 3260-3290.
#
# Insert after IV models are estimated and results are extracted.
# Requires: results data frame with columns (sector, weight, cluster, estimate, se, fs_f)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# ── 1. tF Critical Value Lookup Table (Table 3A, 5% level) ─────────────────####
#    These are selected (F, adjustment_factor) pairs from Lee et al. (2022)
#    Table 3A. The adjustment factor = sqrt(c_0.05(F)) / 1.96.
#    Multiply the conventional 2SLS SE by this factor to get the tF SE.

tF_table_05 <- tibble::tribble(
  ~F_stat, ~adj_factor,
   4.000,   9.519,
   4.079,   7.756,
   4.212,   6.177,
   4.422,   4.920,
   4.759,   3.919,
   5.319,   3.121,
   6.304,   2.486,
   7.482,   1.980,
   8.196,   1.935,
   9.835,   1.767,
  10.253,   1.727,
  11.766,   1.577,
  13.048,   1.542,
  14.631,   1.473,
  16.618,   1.407,
  19.167,   1.345,
  22.516,   1.285,
  27.058,   1.228,
  33.457,   1.173,
  42.930,   1.121,
  57.902,   1.071,
  83.823,   1.024,
 104.670,   1.000
)

# ── 2. tF Critical Value Lookup Table (Table 3B, 1% level) ─────────────────####
tF_table_01 <- tibble::tribble(
  ~F_stat, ~adj_factor,
   6.670,  35.366,
   6.768,  18.034,
   6.901,  12.652,
   7.164,   8.876,
   7.683,   6.227,
   8.721,   4.369,
   9.603,   3.659,
  10.904,   3.065,
  12.889,   2.567,
  16.094,   2.150,
  20.333,   1.866,
  25.399,   1.678,
  33.624,   1.509,
  48.511,   1.357,
  80.502,   1.220,
 128.950,   1.136,
 174.370,   1.097,
 252.342,   1.059
)

# ── 3. Interpolation function ──────────────────────────────────────────────####
#    Given an observed F-statistic, return the tF adjustment factor.
#    Uses linear interpolation (conservative, since the function is convex).
#    Returns Inf if F < minimum table value, 1.0 if F > maximum table value.

get_tF_adjustment <- function(F_obs, level = 0.05) {
  tbl <- if (level == 0.05) tF_table_05 else tF_table_01
  
  if (is.na(F_obs)) return(NA_real_)
  if (F_obs >= max(tbl$F_stat)) return(1.0)
  if (F_obs < min(tbl$F_stat)) return(Inf)
  
  # Find bracketing rows
  idx_upper <- which(tbl$F_stat >= F_obs)[1]
  idx_lower <- idx_upper - 1
  
  if (idx_lower < 1) return(tbl$adj_factor[1])
  
  # Linear interpolation (conservative for convex function)
  F_lo  <- tbl$F_stat[idx_lower]
  F_hi  <- tbl$F_stat[idx_upper]
  adj_lo <- tbl$adj_factor[idx_lower]
  adj_hi <- tbl$adj_factor[idx_upper]
  
  adj_lo + (F_obs - F_lo) / (F_hi - F_lo) * (adj_hi - adj_lo)
}

# ── 4. Apply tF adjustment to all specifications ───────────────────────────####
#    Filter to week-clustered specifications (main specification).

tF_results <- results %>%
  filter(cluster == "Week") %>%
  rowwise() %>%
  mutate(
    # tF adjustment factors
    tF_adj_05 = get_tF_adjustment(fs_f, level = 0.05),
    tF_adj_01 = get_tF_adjustment(fs_f, level = 0.01),
    
    # Adjusted standard errors
    se_tF_05 = se * tF_adj_05,
    se_tF_01 = se * tF_adj_01,
    
    # Conventional 95% CI (using ±1.96)
    ci_conv_lo = estimate - 1.96 * se,
    ci_conv_hi = estimate + 1.96 * se,
    ci_conv_width = ci_conv_hi - ci_conv_lo,
    
    # tF-adjusted 95% CI (using ±1.96 × adjusted SE)
    ci_tF_05_lo = estimate - 1.96 * se_tF_05,
    ci_tF_05_hi = estimate + 1.96 * se_tF_05,
    ci_tF_05_width = ci_tF_05_hi - ci_tF_05_lo,
    
    # tF-adjusted 99% CI (using ±2.576 × adjusted SE)
    ci_tF_01_lo = estimate - 2.576 * se_tF_01,
    ci_tF_01_hi = estimate + 2.576 * se_tF_01,
    ci_tF_01_width = ci_tF_01_hi - ci_tF_01_lo,
    
    # Width inflation ratio
    inflation_05 = ci_tF_05_width / ci_conv_width,
    inflation_01 = ci_tF_01_width / (2 * 2.576 * se)  # ratio vs conventional 99%
  ) %>%
  ungroup()

# ── 5. Summary output ──────────────────────────────────────────────────────####
cat("\n============================================================\n")
cat("tF VALID t-RATIO INFERENCE DIAGNOSTICS\n")
cat("Lee et al. (2022, AER)\n")
cat("============================================================\n\n")

# Summary by weight
tF_summary <- tF_results %>%
  group_by(weight) %>%
  summarise(
    n = n(),
    min_F = min(fs_f, na.rm = TRUE),
    median_F = median(fs_f, na.rm = TRUE),
    max_F = max(fs_f, na.rm = TRUE),
    mean_adj_05 = mean(tF_adj_05, na.rm = TRUE),
    max_adj_05 = max(tF_adj_05, na.rm = TRUE),
    mean_adj_01 = mean(tF_adj_01, na.rm = TRUE),
    max_adj_01 = max(tF_adj_01, na.rm = TRUE),
    pct_adj_equals_1 = mean(tF_adj_05 == 1.0, na.rm = TRUE) * 100,
    .groups = "drop"
  )

cat("── Summary by weighting scheme ──\n")
print(tF_summary)

cat("\n── Interpretation ──\n")
if (all(tF_results$tF_adj_05 == 1.0, na.rm = TRUE)) {
  cat("All first-stage F-statistics exceed 104.7.\n")
  cat("The tF adjustment factor is 1.0 for all specifications.\n")
  cat("Conventional 2SLS standard errors require no correction.\n")
  cat("This confirms that t-ratio inference is valid at the 5% level.\n")
} else {
  n_adjusted <- sum(tF_results$tF_adj_05 > 1.0, na.rm = TRUE)
  cat(sprintf("%d of %d specifications require tF adjustment at the 5%% level.\n",
              n_adjusted, nrow(tF_results)))
}

# ── 6. Industry-level detail table ─────────────────────────────────────────####
cat("\n── Industry-level tF diagnostics (consumption weights, week cluster) ──\n\n")

detail_table <- tF_results %>%
  filter(weight == "Consumption") %>%
  select(sector, estimate, se, fs_f, tF_adj_05, se_tF_05,
         ci_conv_lo, ci_conv_hi, ci_tF_05_lo, ci_tF_05_hi, inflation_05) %>%
  arrange(sector)

print(detail_table, n = 30)


# ── 7. Comparison plot: Conventional vs tF vs AR ───────────────────────────####
#    If AR results exist, merge them for a three-way comparison.

if (exists("ar_split_all")) {
  comparison <- tF_results %>%
    filter(weight == "Consumption") %>%
    select(sector, estimate, ci_conv_lo, ci_conv_hi,
           ci_tF_05_lo, ci_tF_05_hi, fs_f, tF_adj_05) %>%
    left_join(
      ar_split_all %>%
        filter(Weight == "Consumption") %>%
        select(sector = Sector, AR_lo = AR_Lower, AR_hi = AR_Upper),
      by = "sector"
    ) %>%
    arrange(estimate)
  
  # Reshape for plotting
  comp_long <- comparison %>%
    mutate(sector = factor(sector, levels = sector)) %>%
    pivot_longer(
      cols = c(ci_conv_lo, ci_conv_hi, ci_tF_05_lo, ci_tF_05_hi, AR_lo, AR_hi),
      names_to = "bound",
      values_to = "value"
    ) %>%
    mutate(
      method = case_when(
        str_detect(bound, "conv") ~ "Conventional 2SLS",
        str_detect(bound, "tF")   ~ "tF-adjusted",
        str_detect(bound, "AR")   ~ "Anderson-Rubin"
      ),
      side = if_else(str_detect(bound, "lo|Lower"), "lower", "upper")
    ) %>%
    select(sector, estimate, method, side, value) %>%
    pivot_wider(names_from = side, values_from = value)
  
  p_comparison <- ggplot(comp_long, aes(y = sector, colour = method)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = 0.3, position = position_dodge(width = 0.6)) +
    geom_point(aes(x = estimate), data = comparison %>%
                 mutate(sector = factor(sector, levels = sector)),
               colour = "black", size = 1.5) +
    scale_colour_manual(values = c(
      "Conventional 2SLS" = "steelblue",
      "tF-adjusted" = "firebrick",
      "Anderson-Rubin" = "darkgreen"
    )) +
    labs(
      title = "Confidence Interval Comparison: Conventional vs tF vs AR",
      subtitle = "Consumption weights, week clustering",
      x = "Estimated elasticity",
      y = NULL,
      colour = "Method"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  print(p_comparison)
  
  cat("\n── Three-way CI comparison (consumption weights) ──\n")
  print(comparison %>%
          select(sector, estimate, ci_conv_lo, ci_conv_hi,
                 ci_tF_05_lo, ci_tF_05_hi, AR_lo, AR_hi, fs_f) %>%
          mutate(across(where(is.numeric), ~round(., 5))),
        n = 30)
}

# ── 8. Summary for thesis ──────────────────────────────────────────────────####
cat("\n============================================================\n")
cat("tF SUMMARY FOR THESIS\n")
cat("============================================================\n")
cat(sprintf("Specifications examined: %d\n", nrow(tF_results)))
cat(sprintf("F-statistics range: %.0f to %.0f\n",
            min(tF_results$fs_f, na.rm = TRUE),
            max(tF_results$fs_f, na.rm = TRUE)))
cat(sprintf("tF threshold for no adjustment (5%%): F > 104.7\n"))
cat(sprintf("tF threshold for no adjustment (1%%): F > 252.3\n"))
cat(sprintf("Specifications requiring adjustment (5%%): %d / %d\n",
            sum(tF_results$tF_adj_05 > 1.0, na.rm = TRUE), nrow(tF_results)))
cat(sprintf("Specifications requiring adjustment (1%%): %d / %d\n",
            sum(tF_results$tF_adj_01 > 1.0, na.rm = TRUE), nrow(tF_results)))
cat(sprintf("Mean inflation factor (5%%): %.4f\n",
            mean(tF_results$inflation_05, na.rm = TRUE)))
cat(sprintf("Max inflation factor (5%%): %.4f\n",
            max(tF_results$inflation_05, na.rm = TRUE)))

# ── INTERPRETATION GUIDE ─────────────────────────────────────────────────####
#
# If all adjustment factors = 1.0:
#   → First-stage F-statistics are far above 104.7 (5%) and 252.3 (1%).
#   → Conventional 2SLS standard errors deliver correct coverage.
#   → The tF diagnostic confirms that weak-instrument distortion is
#     absent and that the conventional Wald intervals are valid.
#   → Report this as: "The tF adjustment of Lee et al. (2022) produces
#     identical confidence intervals to conventional 2SLS, confirming
#     that instrument strength is sufficient for valid t-ratio inference."
#
# If some adjustment factors > 1.0:
#   → For those industries, conventional SEs are understated.
#   → Report the tF-adjusted CIs alongside conventional and AR CIs.
#   → The tF CI will be wider than conventional but narrower than AR
#     in expected length (Lee et al., 2022, Theorem).
#
# Relationship to AR:
#   → AR confidence sets are valid regardless of instrument strength
#     but have infinite conditional expected length.
#   → tF confidence intervals have finite conditional expected length.
#   → In this setting (F >> 104.7), the three methods converge,
#     providing a complete robustness picture.
