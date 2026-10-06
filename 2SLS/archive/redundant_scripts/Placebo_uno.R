# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ============================================================================
# = PLACEBO TEST: t-24 / t+24 / t-48 / t+48 / t-72 / t+72 ====================
# ============================================================================

# Assumes consumption_panel already exists and includes:
# DK36_en, TimeUTC, log_consumption, log_P_c, Wind_c, Temp_c,
# log_gas, log_coal, log_carbon, fe_hour, fe_month, fe_dow, fe_week

# ── 1. Create placebo instruments ───────────────────────────────────────────####
placebo_panel <- consumption_panel %>%
  arrange(DK36_en, TimeUTC) %>%
  group_by(DK36_en) %>%
  mutate(
    Wind_c_m24 = dplyr::lag(Wind_c, 24),
    Wind_c_p24 = dplyr::lead(Wind_c, 24),
    Wind_c_m48 = dplyr::lag(Wind_c, 48),
    Wind_c_p48 = dplyr::lead(Wind_c, 48),
    Wind_c_m72 = dplyr::lag(Wind_c, 72),
    Wind_c_p72 = dplyr::lead(Wind_c, 72)
  ) %>%
  ungroup()

# ── 2. Helper to run one placebo IV ─────────────────────────────────────────####
run_placebo_iv <- function(data, inst_var, shift_label) {
  
  df_k <- data %>%
    filter(!is.na(.data[[inst_var]]))
  
  model_k <- feols(
    as.formula(
      paste0(
        "log_consumption ~ Temp_c + log_gas + log_coal + log_carbon | ",
        "fe_hour + fe_month + fe_dow | ",
        "log_P_c ~ ", inst_var
      )
    ),
    data    = df_k,
    cluster = ~ fe_week,
    split   = ~ DK36_en
  )
  
  purrr::map_dfr(seq_along(model_k), function(i) {
    m  <- model_k[[i]]
    ct <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    
    if (length(iv_row) == 0) return(NULL)
    
    tibble(
      DK36_en    = sub(".*sample: ", "", names(model_k)[i]),
      shift      = shift_label,
      instrument = inst_var,
      estimate   = as.numeric(ct[iv_row, "Estimate"]),
      se         = as.numeric(ct[iv_row, "Std. Error"]),
      p_value    = as.numeric(ct[iv_row, "Pr(>|t|)"]),
      ci_low     = as.numeric(ct[iv_row, "Estimate"]) - 1.96 * as.numeric(ct[iv_row, "Std. Error"]),
      ci_high    = as.numeric(ct[iv_row, "Estimate"]) + 1.96 * as.numeric(ct[iv_row, "Std. Error"]),
      fs_f       = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat, error = function(e) NA_real_)
    )
  })
}

# ── 3. Run all placebo tests ────────────────────────────────────────────────####
placebo_results_multi <- bind_rows(
  run_placebo_iv(placebo_panel, "Wind_c_m24", "-24"),
  run_placebo_iv(placebo_panel, "Wind_c_p24", "+24"),
  run_placebo_iv(placebo_panel, "Wind_c_m48", "-48"),
  run_placebo_iv(placebo_panel, "Wind_c_p48", "+48"),
  run_placebo_iv(placebo_panel, "Wind_c_m72", "-72"),
  run_placebo_iv(placebo_panel, "Wind_c_p72", "+72")
) %>%
  mutate(
    sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    abs_estimate = abs(estimate),
    cell = paste0(sprintf("%.3f", estimate), sig, " (", sprintf("%.3f", se), ")")
  )

print(placebo_results_multi, n = Inf)

# ── 4. Wide table by sector ─────────────────────────────────────────────────####
placebo_table_multi <- placebo_results_multi %>%
  select(DK36_en, shift, cell, fs_f) %>%
  pivot_wider(names_from = shift, values_from = c(cell, fs_f))

print(placebo_table_multi, n = Inf)

# ── 5. Summary by shift ─────────────────────────────────────────────────────####
placebo_summary_multi <- placebo_results_multi %>%
  group_by(shift) %>%
  summarise(
    mean_estimate   = mean(estimate, na.rm = TRUE),
    median_estimate = median(estimate, na.rm = TRUE),
    mean_abs_est    = mean(abs_estimate, na.rm = TRUE),
    median_abs_est  = median(abs_estimate, na.rm = TRUE),
    n_sig_10pct     = sum(p_value < 0.10, na.rm = TRUE),
    n_sig_5pct      = sum(p_value < 0.05, na.rm = TRUE),
    n_sig_1pct      = sum(p_value < 0.01, na.rm = TRUE),
    share_sig_5pct  = mean(p_value < 0.05, na.rm = TRUE),
    mean_fs_f       = mean(fs_f, na.rm = TRUE),
    median_fs_f     = median(fs_f, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    horizon = dplyr::case_when(
      shift %in% c("-24", "+24") ~ 24L,
      shift %in% c("-48", "+48") ~ 48L,
      shift %in% c("-72", "+72") ~ 72L
    ),
    direction = ifelse(substr(shift, 1, 1) == "-", "lead_back", "lead_forward")
  ) %>%
  arrange(horizon, direction)

print(placebo_summary_multi, n = Inf)

# ── 6. Compact horizon-level summary ────────────────────────────────────────####
placebo_summary_horizon <- placebo_results_multi %>%
  mutate(
    horizon = dplyr::case_when(
      shift %in% c("-24", "+24") ~ 24L,
      shift %in% c("-48", "+48") ~ 48L,
      shift %in% c("-72", "+72") ~ 72L
    )
  ) %>%
  group_by(horizon) %>%
  summarise(
    mean_abs_est   = mean(abs(estimate), na.rm = TRUE),
    median_abs_est = median(abs(estimate), na.rm = TRUE),
    n_sig_5pct     = sum(p_value < 0.05, na.rm = TRUE),
    share_sig_5pct = mean(p_value < 0.05, na.rm = TRUE),
    mean_fs_f      = mean(fs_f, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(horizon)

print(placebo_summary_horizon)

# ── 7. Plot: mean estimate by shift ─────────────────────────────────────────####
ggplot(placebo_summary_multi, aes(x = shift, y = mean_estimate, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Placebo IV estimates using shifted wind instruments",
    subtitle = "Consumption-weighted specification",
    x = "Instrument shift",
    y = "Mean IV elasticity across sectors"
  ) +
  theme_minimal()

# ── 8. Plot: mean absolute estimate by horizon ──────────────────────────────####
ggplot(placebo_summary_horizon, aes(x = factor(horizon), y = mean_abs_est)) +
  geom_col() +
  labs(
    title = "Average absolute placebo estimate by horizon",
    subtitle = "Consumption-weighted specification",
    x = "Hours away from contemporaneous instrument",
    y = "Mean absolute IV estimate"
  ) +
  theme_minimal()

# ── 9. Optional: compare placebo sectors to your actual IV results ──────────####
# Requires iv_c from your main script
if (exists("iv_c")) {
  
  placebo_vs_actual <- placebo_results_multi %>%
    left_join(
      iv_c %>%
        select(DK36_en, actual_estimate = coef, actual_pval = pval),
      by = "DK36_en"
    ) %>%
    mutate(
      actual_sig_5pct = actual_pval < 0.05,
      placebo_sig_5pct = p_value < 0.05
    )
  
  placebo_overlap_summary <- placebo_vs_actual %>%
    group_by(shift) %>%
    summarise(
      placebo_sig_sectors = sum(placebo_sig_5pct, na.rm = TRUE),
      actual_sig_sectors  = sum(actual_sig_5pct, na.rm = TRUE),
      overlap_sig         = sum(placebo_sig_5pct & actual_sig_5pct, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(placebo_overlap_summary)
}