# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
###############################################################################
# CORRECTED SENSITIVITY ANALYSIS — CLUSTER-ROBUST

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(stringr)
library(fixest)
library(sensemakr)
library(ggplot2)
library(writexl)

# ── Create output directory ───────────────────────────────────────────────####
dir.create("tables", showWarnings = FALSE)

# ============================================================================
# CORRECTED SENSITIVITY ANALYSIS — using cluster-robust t-statistics
# ============================================================================
# sensemakr::robustness_value() accepts t-stat and df directly,
# bypassing the lm object entirely.

library(sensemakr)
library(fixest)
library(dplyr)
library(purrr)
library(tibble)

# Step 1: Run reduced-form feols with cluster-robust SE for each industry × weight

run_corrected_sensitivity <- function(data, industry_col, wind_var, temp_var,
                                      weight_label, cluster_var = "fe_month") {
  
  industries <- sort(unique(data[[industry_col]]))
  
  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon | fe_hour + fe_week + fe_dow"
  ))
  
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  map(industries, function(ind) {
    sub <- data %>% filter(.data[[industry_col]] == ind)
    
    # Reduced-form feols with cluster-robust SE
    rf <- tryCatch(
      feols(fml, data = sub, cluster = cluster_fml),
      error = function(e) NULL
    )
    if (is.null(rf)) return(NULL)
    
    ct <- coeftable(rf)
    if (!(wind_var %in% rownames(ct))) return(NULL)
    
    # Extract cluster-robust statistics
    t_cluster  <- ct[wind_var, "t value"]
    se_cluster <- ct[wind_var, "Std. Error"]
    est        <- ct[wind_var, "Estimate"]
    
    # Degrees of freedom (use cluster-adjusted df)
    n_obs    <- nobs(rf)
    n_clust  <- length(unique(sub[[cluster_var]]))
    k        <- length(coef(rf))
    dof      <- n_clust - 1  # cluster-adjusted df
    
    # Partial R² of instrument with outcome (this is invariant to SE choice)
    # Compute from t-stat and dof using the OLS regression (not cluster)
    # partial_R2 = t² / (t² + dof) — but we need the OLS t for partial R2
    # Actually, partial R2 is a property of the data, not the SEs.
    # We compute it from the OLS model
    rf_ols <- tryCatch(
      lm(as.formula(paste0(
        "log_consumption ~ ", wind_var, " + ", temp_var,
        " + log_gas + log_coal + log_carbon + factor(fe_hour) + factor(fe_week) + factor(fe_dow)"
      )), data = sub),
      error = function(e) NULL
    )
    
    if (is.null(rf_ols)) return(NULL)
    
    ols_ct <- summary(rf_ols)$coefficients
    t_ols  <- ols_ct[wind_var, "t value"]
    dof_ols <- rf_ols$df.residual
    partial_r2 <- t_ols^2 / (t_ols^2 + dof_ols)
    
    # Compute RV using cluster-robust t-statistic
    # RV_q1: minimum confounding to reduce estimate to zero (depends on partial R2 only)
    rv_q1 <- robustness_value(t_statistic = t_ols, dof = dof_ols, q = 1)
    
    # RV_q1_alpha: minimum confounding to make estimate insignificant
    # This is where the cluster-robust t matters
    rv_q1_alpha <- robustness_value(t_statistic = t_cluster, dof = dof, q = 1, alpha = 0.05)
    
    # Benchmark bounds using sensemakr on the OLS model
    # (benchmark partial R2s are data properties, not affected by clustering)
    sens_ols <- tryCatch(
      sensemakr(model = rf_ols, treatment = wind_var,
                benchmark_covariates = temp_var, kd = 1:5, alpha = 0.05),
      error = function(e) NULL
    )
    
    # Critical kd: use cluster-robust t to determine when significance is lost
    critical_kd <- NA
    if (!is.null(sens_ols) && !is.null(sens_ols$bounds) && nrow(sens_ols$bounds) > 0) {
      bounds_df <- as.data.frame(sens_ols$bounds)
      bounds_df$kd <- as.numeric(stringr::str_extract(bounds_df$bound_label, "^[0-9]+"))
      
      # The adjusted estimate from sensemakr doesn't change with clustering.
      # But we need to compare the adjusted estimate against the cluster-robust SE.
      # Adjusted t = adjusted_estimate / cluster_SE
      bounds_df$adjusted_t_cluster <- bounds_df$adjusted_estimate / se_cluster
      
      t_crit <- qt(0.975, df = dof)
      sig_lost <- bounds_df %>%
        filter(abs(adjusted_t_cluster) < t_crit) %>%
        slice_min(kd, n = 1)
      
      critical_kd <- if (nrow(sig_lost) > 0) sig_lost$kd[1] else ">5"
    }
    
    tibble(
      Industry       = ind,
      Weight         = weight_label,
      t_OLS          = round(t_ols, 2),
      t_cluster      = round(t_cluster, 2),
      inflation      = round(abs(t_ols / t_cluster), 2),
      Partial_R2     = round(partial_r2, 6),
      RV_q1          = round(rv_q1, 4),
      RV_q1_alpha    = round(rv_q1_alpha, 4),
      Critical_kd    = as.character(critical_kd),
      N_obs          = n_obs,
      N_clusters     = n_clust
    )
  }) %>% bind_rows()
}

# Run for all three weighting schemes
cat("Running corrected sensitivity (consumption weights)...\n")
sens_corrected_c <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_c", "Temp_c", "Consumption"
)

cat("Running corrected sensitivity (employment weights)...\n")
sens_corrected_emp <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_emp", "Temp_emp", "Employment"
)

cat("Running corrected sensitivity (firm count weights)...\n")
sens_corrected_firm <- run_corrected_sensitivity(
  consumption_panel, "DK36_en", "Wind_firm", "Temp_firm", "Firm count"
)

# Combine and classify
sens_corrected_all <- bind_rows(sens_corrected_c, sens_corrected_emp, sens_corrected_firm) %>%
  mutate(
    Robustness_Tier = case_when(
      RV_q1_alpha >= 0.10 ~ "High",
      RV_q1_alpha >= 0.03 ~ "Moderate",
      TRUE                ~ "Fragile"
    )
  )

# Report
cat("\n============================================================\n")
cat("CORRECTED SENSITIVITY RESULTS (Consumption Weights)\n")
cat("============================================================\n")
print(
  sens_corrected_c %>%
    mutate(Tier = case_when(
      RV_q1_alpha >= 0.10 ~ "High",
      RV_q1_alpha >= 0.03 ~ "Moderate",
      TRUE ~ "Fragile"
    )) %>%
    arrange(desc(RV_q1_alpha)) %>%
    select(Industry, t_OLS, t_cluster, inflation, RV_q1, RV_q1_alpha, Critical_kd, Tier),
  n = 35
)

# Compare old vs new tiers
cat("\n============================================================\n")
cat("TIER RECLASSIFICATION\n")
cat("============================================================\n")

comparison <- sens_consumption %>%
  select(Industry, RV_old = RV_q1_alpha, Tier_old = Robustness_Tier) %>%
  left_join(
    sens_corrected_c %>%
      mutate(Tier_new = case_when(
        RV_q1_alpha >= 0.10 ~ "High",
        RV_q1_alpha >= 0.03 ~ "Moderate",
        TRUE ~ "Fragile"
      )) %>%
      select(Industry, RV_new = RV_q1_alpha, Tier_new),
    by = "Industry"
  ) %>%
  mutate(Changed = Tier_old != Tier_new)

print(comparison %>% arrange(desc(RV_new)), n = 35)

cat("\nReclassified industries:", sum(comparison$Changed, na.rm = TRUE),
    "/", nrow(comparison), "\n")
cat("Summary (corrected, consumption weights):\n")
cat("  High:", sum(comparison$Tier_new == "High", na.rm = TRUE), "\n")
cat("  Moderate:", sum(comparison$Tier_new == "Moderate", na.rm = TRUE), "\n")
cat("  Fragile:", sum(comparison$Tier_new == "Fragile", na.rm = TRUE), "\n")


# Clean reporting table combining all three layers
reporting_table <- sens_corrected_c %>%
  mutate(
    # Layer 1: Cluster-robust significance
    RF_significant = abs(t_cluster) > qt(0.975, df = N_clusters - 1),
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate", 
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    ),
    # Layer 2: RV_q1 (OLS, valid as partial R2 measure)
    RV_q1_tier = case_when(
      RV_q1 >= 0.05 ~ "Robust",
      RV_q1 >= 0.01 ~ "Moderate",
      TRUE          ~ "Fragile"
    ),
    # Layer 3: kd already computed
    survives_benchmark = Critical_kd != "1" & Critical_kd != "NA"
  ) %>%
  select(Industry, t_cluster, RF_category, RV_q1, RV_q1_tier, 
         Critical_kd, survives_benchmark) %>%
  arrange(desc(abs(t_cluster)))

cat("\n============================================================\n")
cat("CORRECTED SENSITIVITY REPORTING TABLE\n")
cat("============================================================\n")
print(reporting_table, n = 35)

cat("\nReduced-form significance (cluster-robust):\n")
cat("  Strong (|t| >= 3):", sum(reporting_table$RF_category == "Strong"), "\n")
cat("  Moderate (2 <= |t| < 3):", sum(reporting_table$RF_category == "Moderate"), "\n")
cat("  Borderline (1.5 <= |t| < 2):", sum(reporting_table$RF_category == "Borderline"), "\n")
cat("  Insignificant (|t| < 1.5):", sum(reporting_table$RF_category == "Insignificant"), "\n")

cat("\nSurvives kd = 1 benchmark:", sum(reporting_table$survives_benchmark), "/", 
    nrow(reporting_table), "\n")

# ============================================================================
# 1. CORRECTED SENSITIVITY FUNCTION
# ============================================================================
# (Include here if not already run — otherwise skip to Section 2)

run_corrected_sensitivity <- function(data, industry_col, wind_var, temp_var,
                                      weight_label, cluster_var = "fe_month") {
  
  industries <- sort(unique(data[[industry_col]]))
  
  fml <- as.formula(paste0(
    "log_consumption ~ ", wind_var, " + ", temp_var,
    " + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow"
  ))
  
  cluster_fml <- as.formula(paste0("~", cluster_var))
  
  map(industries, function(ind) {
    sub <- data %>% filter(.data[[industry_col]] == ind)
    
    rf <- tryCatch(
      feols(fml, data = sub, cluster = cluster_fml),
      error = function(e) NULL
    )
    if (is.null(rf)) return(NULL)
    
    ct <- coeftable(rf)
    if (!(wind_var %in% rownames(ct))) return(NULL)
    
    t_cluster  <- ct[wind_var, "t value"]
    se_cluster <- ct[wind_var, "Std. Error"]
    est        <- ct[wind_var, "Estimate"]
    
    n_obs   <- nobs(rf)
    n_clust <- length(unique(sub[[cluster_var]]))
    dof     <- n_clust - 1
    
    rf_ols <- tryCatch(
      lm(as.formula(paste0(
        "log_consumption ~ ", wind_var, " + ", temp_var,
        " + log_gas + log_coal + log_carbon + factor(fe_hour) + factor(fe_month) + factor(fe_dow)"
      )), data = sub),
      error = function(e) NULL
    )
    if (is.null(rf_ols)) return(NULL)
    
    ols_ct  <- summary(rf_ols)$coefficients
    t_ols   <- ols_ct[wind_var, "t value"]
    dof_ols <- rf_ols$df.residual
    partial_r2 <- t_ols^2 / (t_ols^2 + dof_ols)
    
    rv_q1       <- robustness_value(t_statistic = t_ols, dof = dof_ols, q = 1)
    rv_q1_alpha <- robustness_value(t_statistic = t_cluster, dof = dof, q = 1, alpha = 0.05)
    
    sens_ols <- tryCatch(
      sensemakr(model = rf_ols, treatment = wind_var,
                benchmark_covariates = temp_var, kd = 1:5, alpha = 0.05),
      error = function(e) NULL
    )
    
    critical_kd <- NA
    if (!is.null(sens_ols) && !is.null(sens_ols$bounds) && nrow(sens_ols$bounds) > 0) {
      bounds_df <- as.data.frame(sens_ols$bounds)
      bounds_df$kd <- as.numeric(str_extract(bounds_df$bound_label, "^[0-9]+"))
      bounds_df$adjusted_t_cluster <- bounds_df$adjusted_estimate / se_cluster
      t_crit <- qt(0.975, df = dof)
      sig_lost <- bounds_df %>%
        filter(abs(adjusted_t_cluster) < t_crit) %>%
        slice_min(kd, n = 1)
      critical_kd <- if (nrow(sig_lost) > 0) sig_lost$kd[1] else ">5"
    }
    
    tibble(
      Industry    = ind,
      Weight      = weight_label,
      t_OLS       = round(t_ols, 2),
      t_cluster   = round(t_cluster, 2),
      inflation   = round(abs(t_ols / t_cluster), 2),
      Partial_R2  = round(partial_r2, 6),
      RV_q1       = round(rv_q1, 4),
      RV_q1_alpha = round(rv_q1_alpha, 4),
      Critical_kd = as.character(critical_kd),
      N_obs       = n_obs,
      N_clusters  = n_clust
    )
  }) %>% bind_rows()
}


# ============================================================================
# 2. RUN CORRECTED SENSITIVITY (if not already in environment)
# ============================================================================

if (!exists("sens_corrected_c")) {
  cat("Running corrected sensitivity (consumption weights)...\n")
  sens_corrected_c <- run_corrected_sensitivity(
    consumption_panel, "DK36_en", "Wind_c", "Temp_c", "Consumption"
  )
}

if (!exists("sens_corrected_emp")) {
  cat("Running corrected sensitivity (employment weights)...\n")
  sens_corrected_emp <- run_corrected_sensitivity(
    consumption_panel, "DK36_en", "Wind_emp", "Temp_emp", "Employment"
  )
}

if (!exists("sens_corrected_firm")) {
  cat("Running corrected sensitivity (firm count weights)...\n")
  sens_corrected_firm <- run_corrected_sensitivity(
    consumption_panel, "DK36_en", "Wind_firm", "Temp_firm", "Firm count"
  )
}

sens_corrected_all <- bind_rows(sens_corrected_c, sens_corrected_emp, sens_corrected_firm)


# ============================================================================
# TABLE 1: MAIN REPORTING TABLE (Consumption Weights — Thesis Body)
# Three-layer classification for Section 4.5.3
# ============================================================================

table1_main <- sens_corrected_c %>%
  mutate(
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate",
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    ),
    RV_q1_tier = case_when(
      RV_q1 >= 0.05 ~ "Robust",
      RV_q1 >= 0.01 ~ "Moderate",
      TRUE          ~ "Fragile"
    ),
    Survives_kd1 = Critical_kd != "1" & !is.na(Critical_kd)
  ) %>%
  arrange(desc(abs(t_cluster))) %>%
  select(
    Industry,
    `Cluster t` = t_cluster,
    `RF Category` = RF_category,
    `Partial R²` = Partial_R2,
    `RV (q=1)` = RV_q1,
    `RV Tier` = RV_q1_tier,
    `Critical kd` = Critical_kd,
    `Survives kd=1` = Survives_kd1
  )

cat("\n============================================================\n")
cat("TABLE 1: SENSITIVITY REPORTING (Consumption Weights)\n")
cat("============================================================\n")
print(table1_main, n = 35)

# Summary counts
cat("\nReduced-form significance (cluster-robust):\n")
cat("  Strong (|t| >= 3):", sum(table1_main$`RF Category` == "Strong"), "\n")
cat("  Moderate (2 <= |t| < 3):", sum(table1_main$`RF Category` == "Moderate"), "\n")
cat("  Borderline (1.5 <= |t| < 2):", sum(table1_main$`RF Category` == "Borderline"), "\n")
cat("  Insignificant (|t| < 1.5):", sum(table1_main$`RF Category` == "Insignificant"), "\n")
cat("\nSurvives kd=1 benchmark:", sum(table1_main$`Survives kd=1`), "/", nrow(table1_main), "\n")


# ============================================================================
# TABLE 2: OLS vs CLUSTER T-STATISTIC INFLATION (Appendix)
# Documents the correction and its magnitude
# ============================================================================

table2_inflation <- sens_corrected_c %>%
  arrange(desc(inflation)) %>%
  select(
    Industry,
    `OLS t-stat` = t_OLS,
    `Cluster t-stat` = t_cluster,
    `Inflation ratio` = inflation,
    `N observations` = N_obs,
    `N clusters` = N_clusters
  )

cat("\n============================================================\n")
cat("TABLE 2: T-STATISTIC INFLATION (OLS vs Cluster-Robust)\n")
cat("============================================================\n")
print(table2_inflation, n = 35)

cat("\nInflation summary:\n")
cat("  Mean:", round(mean(table2_inflation$`Inflation ratio`), 2), "\n")
cat("  Min: ", round(min(table2_inflation$`Inflation ratio`), 2), "\n")
cat("  Max: ", round(max(table2_inflation$`Inflation ratio`), 2), "\n")


# ============================================================================
# TABLE 3: CROSS-WEIGHT COMPARISON (Appendix)
# Shows sensitivity results across all three weighting schemes
# ============================================================================

table3_crossweight <- sens_corrected_all %>%
  mutate(
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate",
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    )
  ) %>%
  select(Industry, Weight, t_cluster, RV_q1, Critical_kd, RF_category) %>%
  arrange(Industry, Weight)

# Wide format: one row per industry, columns for each weight
table3_wide <- sens_corrected_all %>%
  select(Industry, Weight, t_cluster, RV_q1, Critical_kd) %>%
  pivot_wider(
    names_from = Weight,
    values_from = c(t_cluster, RV_q1, Critical_kd),
    names_sep = "_"
  ) %>%
  mutate(
    # Cross-weight consistency: do all three agree on RF category?
    cat_c = case_when(
      abs(t_cluster_Consumption) >= 3 ~ "Strong",
      abs(t_cluster_Consumption) >= 2 ~ "Moderate",
      abs(t_cluster_Consumption) >= 1.5 ~ "Borderline",
      TRUE ~ "Insignificant"
    ),
    cat_e = case_when(
      abs(t_cluster_Employment) >= 3 ~ "Strong",
      abs(t_cluster_Employment) >= 2 ~ "Moderate",
      abs(t_cluster_Employment) >= 1.5 ~ "Borderline",
      TRUE ~ "Insignificant"
    ),
    cat_f = case_when(
      abs(`t_cluster_Firm count`) >= 3 ~ "Strong",
      abs(`t_cluster_Firm count`) >= 2 ~ "Moderate",
      abs(`t_cluster_Firm count`) >= 1.5 ~ "Borderline",
      TRUE ~ "Insignificant"
    ),
    Category_consistent = (cat_c == cat_e) & (cat_e == cat_f)
  ) %>%
  select(
    Industry,
    `t (Cons)` = t_cluster_Consumption,
    `t (Emp)` = t_cluster_Employment,
    `t (Firm)` = `t_cluster_Firm count`,
    `RV (Cons)` = RV_q1_Consumption,
    `RV (Emp)` = RV_q1_Employment,
    `RV (Firm)` = `RV_q1_Firm count`,
    `kd (Cons)` = Critical_kd_Consumption,
    `kd (Emp)` = Critical_kd_Employment,
    `kd (Firm)` = `Critical_kd_Firm count`,
    `Category consistent` = Category_consistent
  ) %>%
  arrange(desc(abs(`t (Cons)`)))

cat("\n============================================================\n")
cat("TABLE 3: CROSS-WEIGHT SENSITIVITY COMPARISON\n")
cat("============================================================\n")
print(table3_wide, n = 35)

cat("\nCross-weight category consistency:",
    sum(table3_wide$`Category consistent`), "/", nrow(table3_wide), "\n")


# ============================================================================
# TABLE 4: TIER RECLASSIFICATION (Appendix)
# Compares uncorrected (OLS) vs corrected (cluster) tiers
# ============================================================================

# Need the uncorrected tiers from the earlier sensemakr run
if (exists("sens_consumption")) {
  table4_reclass <- sens_consumption %>%
    select(Industry, RV_old = RV_q1_alpha, Tier_old = Robustness_Tier) %>%
    left_join(
      sens_corrected_c %>%
        mutate(
          Tier_new = case_when(
            abs(t_cluster) >= 3.0 ~ "Strong RF",
            abs(t_cluster) >= 2.0 ~ "Moderate RF",
            abs(t_cluster) >= 1.5 ~ "Borderline RF",
            TRUE ~ "Insignificant RF"
          )
        ) %>%
        select(Industry, t_cluster, RV_new = RV_q1, Tier_new),
      by = "Industry"
    ) %>%
    mutate(
      Reclassified = case_when(
        Tier_old == "High"     & Tier_new != "Strong RF" ~ TRUE,
        Tier_old == "Moderate" & Tier_new == "Insignificant RF" ~ TRUE,
        Tier_old == "Fragile"  & Tier_new %in% c("Strong RF", "Moderate RF") ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    arrange(desc(abs(t_cluster)))
  
  cat("\n============================================================\n")
  cat("TABLE 4: TIER RECLASSIFICATION (OLS → Cluster-Robust)\n")
  cat("============================================================\n")
  print(table4_reclass, n = 35)
  
  cat("\nReclassified:", sum(table4_reclass$Reclassified), "/",
      nrow(table4_reclass), "\n")
} else {
  cat("NOTE: sens_consumption not found. Skipping reclassification table.\n")
  table4_reclass <- NULL
}


# ============================================================================
# TABLE 5: IDENTIFICATION STRENGTH SUMMARY (Thesis Body)
# Compact summary for Section 4.5.3 narrative
# ============================================================================

table5_summary <- sens_corrected_c %>%
  mutate(
    Group = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong identification",
      abs(t_cluster) >= 1.5 ~ "Borderline identification",
      TRUE                  ~ "Weak identification"
    ),
    Group = factor(Group, levels = c("Strong identification",
                                     "Borderline identification",
                                     "Weak identification"))
  ) %>%
  group_by(Group) %>%
  summarise(
    N_industries     = n(),
    Industries       = paste(Industry, collapse = "; "),
    Mean_t_cluster   = round(mean(abs(t_cluster)), 2),
    Mean_partial_R2  = round(mean(Partial_R2), 6),
    Mean_RV_q1       = round(mean(RV_q1), 4),
    N_survive_kd1    = sum(Critical_kd != "1" & !is.na(Critical_kd)),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("TABLE 5: IDENTIFICATION STRENGTH SUMMARY\n")
cat("============================================================\n")
print(table5_summary %>% select(-Industries), n = 5)
cat("\nStrong identification industries:\n")
cat(table5_summary$Industries[table5_summary$Group == "Strong identification"], "\n")


# ============================================================================
# FIGURE 1: SENSITIVITY SCATTER — t_cluster vs Partial R²
# ============================================================================

fig1_data <- sens_corrected_c %>%
  mutate(
    RF_category = case_when(
      abs(t_cluster) >= 3.0 ~ "Strong",
      abs(t_cluster) >= 2.0 ~ "Moderate",
      abs(t_cluster) >= 1.5 ~ "Borderline",
      TRUE                  ~ "Insignificant"
    ),
    RF_category = factor(RF_category,
                         levels = c("Strong", "Moderate", "Borderline", "Insignificant"))
  )

fig1 <- ggplot(fig1_data, aes(x = Partial_R2, y = abs(t_cluster))) +
  geom_hline(yintercept = c(1.5, 2, 3), linetype = "dashed",
             colour = c("grey70", "grey50", "grey30"), linewidth = 0.4) +
  geom_point(aes(colour = RF_category, shape = RF_category), size = 3, alpha = 0.8) +
  geom_text(aes(label = Industry), size = 2.2, vjust = -0.9,
            check_overlap = TRUE, colour = "grey30") +
  scale_colour_manual(
    values = c("Strong" = "#2E86AB", "Moderate" = "#F6AE2D",
               "Borderline" = "#F26157", "Insignificant" = "grey60")
  ) +
  scale_shape_manual(values = c(16, 17, 15, 1)) +
  annotate("text", x = max(fig1_data$Partial_R2) * 0.9, y = 3.2,
           label = "|t| = 3", size = 2.5, colour = "grey30") +
  annotate("text", x = max(fig1_data$Partial_R2) * 0.9, y = 2.2,
           label = "|t| = 2", size = 2.5, colour = "grey50") +
  annotate("text", x = max(fig1_data$Partial_R2) * 0.9, y = 1.7,
           label = "|t| = 1.5", size = 2.5, colour = "grey70") +
  labs(
    x = expression("Partial" ~ R^2 ~ "of wind instrument (OLS)"),
    y = "|t-statistic| (cluster-robust)",
    colour = "RF Category",
    shape = "RF Category",
    title = "Sensitivity Analysis: Instrument Strength vs Cluster-Robust Significance",
    subtitle = "Industries above |t| = 3 have statistically robust reduced forms"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(fig1)
ggsave("tables/fig_sensitivity_scatter.pdf", fig1, width = 10, height = 7)
ggsave("tables/fig_sensitivity_scatter.png", fig1, width = 10, height = 7, dpi = 300)


# ============================================================================
# FIGURE 2: OLS vs CLUSTER T-STATISTIC COMPARISON
# ============================================================================

fig2_data <- sens_corrected_c %>%
  arrange(desc(abs(t_cluster))) %>%
  mutate(Industry = factor(Industry, levels = rev(Industry)))

fig2 <- ggplot(fig2_data, aes(y = Industry)) +
  geom_segment(aes(x = t_cluster, xend = t_OLS, yend = Industry),
               colour = "grey70", linewidth = 0.5) +
  geom_point(aes(x = t_OLS), colour = "#F26157", size = 2.5, shape = 17) +
  geom_point(aes(x = t_cluster), colour = "#2E86AB", size = 2.5, shape = 16) +
  geom_vline(xintercept = c(-3, -2, 0, 2, 3),
             linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
             colour = "grey50", linewidth = 0.3) +
  labs(
    x = "t-statistic",
    y = NULL,
    title = "Reduced-Form t-Statistics: OLS vs Cluster-Robust",
    subtitle = "Blue circles = cluster-robust; Red triangles = OLS (inflated)"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 7))

print(fig2)
ggsave("tables/fig_t_inflation.pdf", fig2, width = 10, height = 8)
ggsave("tables/fig_t_inflation.png", fig2, width = 10, height = 8, dpi = 300)


# ============================================================================
# FIGURE 3: CROSS-WEIGHT t-STATISTICS
# ============================================================================

fig3_data <- sens_corrected_all %>%
  mutate(
    Industry = factor(Industry, levels = rev(levels(
      factor(sens_corrected_c$Industry[order(abs(sens_corrected_c$t_cluster))])
    )))
  )

fig3 <- ggplot(fig3_data, aes(x = t_cluster, y = Industry, colour = Weight)) +
  geom_vline(xintercept = c(-3, -2, 0, 2, 3),
             linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
             colour = "grey50", linewidth = 0.3) +
  geom_point(size = 2, alpha = 0.8, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = c("Consumption" = "#2E86AB",
                                 "Employment" = "#F6AE2D",
                                 "Firm count" = "#A23B72")) +
  labs(
    x = "Cluster-robust t-statistic (reduced form)",
    y = NULL,
    colour = "Weighting scheme",
    title = "Cross-Weight Sensitivity: Cluster-Robust Reduced-Form t-Statistics",
    subtitle = "Dashed lines at |t| = 3; dotted lines at |t| = 2"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  )

print(fig3)
ggsave("tables/fig_crossweight_sensitivity.pdf", fig3, width = 10, height = 8)
ggsave("tables/fig_crossweight_sensitivity.png", fig3, width = 10, height = 8, dpi = 300)


# ============================================================================
# EXPORT ALL TABLES
# ============================================================================

# CSV exports (for manual Word/LaTeX import)
write.csv(table1_main, "tables/table_sensitivity_main.csv", row.names = FALSE)
write.csv(table2_inflation, "tables/table_t_inflation.csv", row.names = FALSE)
write.csv(table3_wide, "tables/table_crossweight_sensitivity.csv", row.names = FALSE)
if (!is.null(table4_reclass)) {
  write.csv(table4_reclass, "tables/table_tier_reclassification.csv", row.names = FALSE)
}
write.csv(table5_summary %>% select(-Industries),
          "tables/table_identification_summary.csv", row.names = FALSE)

# Single Excel workbook with all tables as separate sheets
write_xlsx(
  list(
    "Main Sensitivity" = as.data.frame(table1_main),
    "T-stat Inflation" = as.data.frame(table2_inflation),
    "Cross-Weight"     = as.data.frame(table3_wide),
    "Reclassification" = if (!is.null(table4_reclass)) as.data.frame(table4_reclass) else data.frame(Note = "Not available"),
    "Summary"          = as.data.frame(table5_summary %>% select(-Industries))
  ),
  path = "tables/sensitivity_analysis_tables.xlsx"
)







## __________ ECOLOGICAL INFERENCE ______________ ####
# ============================================================================
# 1. WEIGHT DIVERGENCE ANALYSIS
# ============================================================================
# The three weighting schemes embed different assumptions about how firms
# within each DK36 industry are distributed across DK1 and DK2.
# Large divergence between weights signals high within-industry heterogeneity
# in the spatial distribution of economic activity — exactly the kind of
# compositional variation that drives ecological bias.

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# 1. How many rows survived the join?
cat("Rows in eco_merged:", nrow(eco_merged), "\n")

# 2. Check what the industry names look like in each source
cat("\n--- Names in weight_dispersion ---\n")
sort(unique(weight_dispersion$DK36Title))

cat("\n--- Names in elasticity_divergence ---\n")
sort(unique(elasticity_divergence$industry))

# 3. Check for NA/Inf in the join columns
cat("\n--- NAs in eco_merged ---\n")
eco_merged %>% summarise(
  n = n(),
  na_w = sum(is.na(w_range)),
  na_e = sum(is.na(range_elasticity)),
  inf_w = sum(is.infinite(w_range)),
  inf_e = sum(is.infinite(range_elasticity))
)

elasticity_divergence <- elasticity_divergence %>%
  mutate(industry = str_remove(industry, "^.*:\\s*"))


# Check
sort(unique(elasticity_divergence$industry))

# Re-run the join
eco_merged <- weight_dispersion %>%
  select(DK36Title, w_range) %>%
  inner_join(
    elasticity_divergence %>% select(industry, range_elasticity),
    by = c("DK36Title" = "industry")
  )

cat("Rows in eco_merged:", nrow(eco_merged), "\n")

# Now the correlation test should work
eco_cor <- cor.test(eco_merged$w_range, eco_merged$range_elasticity,
                    method = "spearman")
print(eco_cor)


results_all <- results_all %>%
  mutate(industry = str_remove(industry, "^.*sample:\\s*"))

# Extract unique industry-year weight combinations
w_tbl <- consumption_panel %>%
  filter(!DK36Code %in% c("-", "PR")) %>%
  distinct(Year, DK36Title, DK36Code, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  filter(!is.na(w_DK1_c), !is.na(w_DK1_emp), !is.na(w_DK1_firm))

# --- 1a. Pairwise weight correlations ---
# High correlation => spatial distribution of consumption, employment, and firms
# is similar => less scope for ecological bias from weight choice
weight_cors <- w_tbl %>%
  group_by(Year) %>%
  summarise(
    cor_c_emp  = cor(w_DK1_c, w_DK1_emp, use = "complete.obs"),
    cor_c_firm = cor(w_DK1_c, w_DK1_firm, use = "complete.obs"),
    cor_emp_firm = cor(w_DK1_emp, w_DK1_firm, use = "complete.obs"),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("TABLE: Pairwise Correlations Between DK1 Weights (by Year)\n")
cat("============================================================\n")
print(weight_cors)

# --- 1b. Industry-level weight dispersion ---
# For each industry, compute the range across the three weighting schemes.
# Industries with large range are most sensitive to ecological assumptions.
weight_dispersion <- w_tbl %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  mutate(
    w_range = pmax(w_DK1_c, w_DK1_emp, w_DK1_firm) -
      pmin(w_DK1_c, w_DK1_emp, w_DK1_firm),
    w_mean = (w_DK1_c + w_DK1_emp + w_DK1_firm) / 3,
    w_cv   = w_range / (w_mean + 1e-10)  # coefficient of variation (range-based)
  ) %>%
  arrange(desc(w_range))

cat("\n============================================================\n")
cat("TABLE: Weight Dispersion Across Schemes (Latest Year)\n")
cat("============================================================\n")
print(weight_dispersion %>% select(DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm, w_range), n = 35)

cat("\nSummary statistics for weight range across industries:\n")
cat("  Mean range:", round(mean(weight_dispersion$w_range, na.rm = TRUE), 4), "\n")
cat("  Median range:", round(median(weight_dispersion$w_range, na.rm = TRUE), 4), "\n")
cat("  Max range:", round(max(weight_dispersion$w_range, na.rm = TRUE), 4),
    "  (", weight_dispersion$DK36Title[1], ")\n")



# 2. ECOLOGICAL SENSITIVITY: ELASTICITY DIVERGENCE ACROSS WEIGHTS

# The key ecological diagnostic: if the aggregate elasticity were robust to
# within-industry compositional assumptions, estimates should be similar
# across weighting schemes. Large divergence signals ecological sensitivity.

# Requires results_all from IV_INSTRUMENT2_1 (bind_rows of res_c, res_emp, res_firm)
# If not available, reconstruct from the split models:
# res_c    <- extract_split_iv(IV_model_het_DK10, "fit_log_P_c",    "Consumption")
# res_emp  <- extract_split_iv(IV_model_het_Emp,  "fit_log_P_emp",  "Employment")
# res_firm <- extract_split_iv(IV_model_het_firm, "fit_log_P_firm", "Firm")
# results_all <- bind_rows(res_c, res_emp, res_firm)

elasticity_divergence <- results_all %>%
  select(industry, weight, estimate) %>%
  pivot_wider(names_from = weight, values_from = estimate) %>%
  mutate(
    range_elasticity = pmax(Consumption, Employment, Firm, na.rm = TRUE) -
      pmin(Consumption, Employment, Firm, na.rm = TRUE),
    mean_elasticity = (Consumption + Employment + Firm) / 3,
    # Sign consistency: do all three schemes agree on the sign?
    sign_consistent = (sign(Consumption) == sign(Employment)) &
      (sign(Employment) == sign(Firm))
  ) %>%
  arrange(desc(range_elasticity))

cat("\n============================================================\n")
cat("TABLE: Elasticity Divergence Across Weighting Schemes\n")
cat("============================================================\n")
print(elasticity_divergence %>%
        select(industry, Consumption, Employment, Firm,
               range_elasticity, sign_consistent), n = 35)

cat("\nSign consistency across all industries:",
    sum(elasticity_divergence$sign_consistent, na.rm = TRUE), "/",
    nrow(elasticity_divergence), "\n")
cat("Mean elasticity range:", round(mean(elasticity_divergence$range_elasticity, na.rm = TRUE), 4), "\n")
cat("Median elasticity range:", round(median(elasticity_divergence$range_elasticity, na.rm = TRUE), 4), "\n")



# 3. ECOLOGICAL DECOMPOSITION: WEIGHT DIVERGENCE vs ELASTICITY DIVERGENCE

# If ecological bias is operative, industries with larger weight divergence
# should also show larger elasticity divergence. A positive correlation here
# would be direct evidence that aggregation assumptions matter.

eco_merged <- weight_dispersion %>%
  select(DK36Title, w_range) %>%
  inner_join(
    elasticity_divergence %>% select(industry, range_elasticity),
    by = c("DK36Title" = "industry")
  )

eco_cor <- cor.test(eco_merged$w_range, eco_merged$range_elasticity,
                    method = "spearman")

cat("\n============================================================\n")
cat("ECOLOGICAL SENSITIVITY TEST\n")
cat("============================================================\n")
cat("Spearman correlation between weight range and elasticity range:\n")
cat("  rho =", round(eco_cor$estimate, 4), "\n")
cat("  p-value =", format.pval(eco_cor$p.value, digits = 4), "\n")
cat("  Interpretation: ",
    ifelse(eco_cor$p.value < 0.05,
           "Significant — aggregation assumptions materially affect estimates.",
           "Not significant — estimates are robust to aggregation assumptions."),
    "\n")



# 4. VISUALISATION: ECOLOGICAL SENSITIVITY SCATTER


p_eco <- ggplot(eco_merged, aes(x = w_range, y = range_elasticity)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
              colour = "steelblue", alpha = 0.2) +
  geom_text(aes(label = str_wrap(DK36Title, 20)),
            size = 2.3, vjust = -0.8, check_overlap = TRUE) +
  labs(
    x = "Weight Divergence Across Schemes (DK1 share range)",
    y = "Elasticity Divergence Across Schemes (absolute range)",
    title = "Ecological Sensitivity: Weight Divergence vs Elasticity Divergence",
    subtitle = paste0("Spearman rho = ", round(eco_cor$estimate, 3),
                      ", p = ", format.pval(eco_cor$p.value, digits = 3))
  ) +
  theme_minimal(base_size = 11)

print(p_eco)



# 5. SUMMARY TABLE FOR THESIS


eco_summary <- eco_merged %>%
  left_join(
    elasticity_divergence %>% select(industry, Consumption, Employment, Firm, sign_consistent),
    by = c("DK36Title" = "industry")
  ) %>%
  mutate(
    ecological_sensitivity = case_when(
      range_elasticity < 0.05 & sign_consistent ~ "Low",
      range_elasticity < 0.15 & sign_consistent ~ "Moderate",
      TRUE ~ "High"
    )
  ) %>%
  arrange(desc(range_elasticity))

cat("\n============================================================\n")
cat("TABLE: Ecological Sensitivity Classification\n")
cat("============================================================\n")
print(eco_summary %>%
        select(DK36Title, w_range, range_elasticity,
               sign_consistent, ecological_sensitivity), n = 35)

cat("\nClassification summary:\n")
cat("  Low ecological sensitivity:", sum(eco_summary$ecological_sensitivity == "Low"), "\n")
cat("  Moderate ecological sensitivity:", sum(eco_summary$ecological_sensitivity == "Moderate"), "\n")
cat("  High ecological sensitivity:", sum(eco_summary$ecological_sensitivity == "High"), "\n")




## __________ PLACEBO TEST _______________________ ####
###############################################################################
# FELTON & STEWART CHECKLIST: MISSING RESULTS
# Run after loading consumption_panel and IV_model_het_* from IV_INSTRUMENT2_1
###############################################################################

library(fixest)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

# ============================================================================
# 1. FIRST-STAGE COEFFICIENTS WITH CONFIDENCE INTERVALS
# ============================================================================

extract_first_stage <- function(fx_multi, instrument_term, weight_label) {
  models <- as.list(fx_multi)
  ids <- names(models)
  
  map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    fs <- summary(m, stage = 1)
    ct <- fixest::coeftable(fs)
    
    if (!(instrument_term %in% rownames(ct))) return(NULL)
    
    est <- ct[instrument_term, "Estimate"]
    se  <- ct[instrument_term, "Std. Error"]
    
    tibble(
      industry = str_remove(ids[i], "^.*sample:\\s*"),
      fs_coef = est,
      fs_se = se,
      fs_ci_low = est - 1.96 * se,
      fs_ci_high = est + 1.96 * se,
      weight = weight_label
    )
  })
}

fs_c    <- extract_first_stage(IV_model_het_DK10, "Wind_c",    "Consumption")
fs_emp  <- extract_first_stage(IV_model_het_Emp,  "Wind_emp",  "Employment")
fs_firm <- extract_first_stage(IV_model_het_firm, "Wind_firm", "Firm")
fs_all  <- bind_rows(fs_c, fs_emp, fs_firm)

cat("\n============================================================\n")
cat("FIRST-STAGE COEFFICIENTS\n")
cat("============================================================\n")
print(fs_all, n = 100)

# Summary for thesis text
cat("\nFirst-stage summary (consumption weights):\n")
cat("  Range of pi_1:", round(min(fs_c$fs_coef), 4), "to", round(max(fs_c$fs_coef), 4), "\n")
cat("  All negative:", all(fs_c$fs_coef < 0), "\n")


# ============================================================================
# 2. REDUCED-FORM ESTIMATES (INSTRUMENT → OUTCOME)
# ============================================================================

rf_consumption <- feols(
  log_consumption ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

rf_employment <- feols(
  log_consumption ~ Wind_emp + Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

rf_firm <- feols(
  log_consumption ~ Wind_firm + Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

extract_rf <- function(fx_multi, wind_var, weight_label) {
  models <- as.list(fx_multi)
  ids <- names(models)
  
  map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    ct <- fixest::coeftable(m)
    if (!(wind_var %in% rownames(ct))) return(NULL)
    
    est <- ct[wind_var, "Estimate"]
    se  <- ct[wind_var, "Std. Error"]
    pv  <- ct[wind_var, "Pr(>|t|)"]
    
    tibble(
      industry = str_remove(ids[i], "^.*sample:\\s*"),
      rf_coef = est,
      rf_se = se,
      rf_ci_low = est - 1.96 * se,
      rf_ci_high = est + 1.96 * se,
      rf_pvalue = pv,
      rf_sig = pv < 0.05,
      weight = weight_label
    )
  })
}

rf_c_res    <- extract_rf(rf_consumption, "Wind_c",    "Consumption")
rf_emp_res  <- extract_rf(rf_employment,  "Wind_emp",  "Employment")
rf_firm_res <- extract_rf(rf_firm,        "Wind_firm", "Firm")
rf_all      <- bind_rows(rf_c_res, rf_emp_res, rf_firm_res)

cat("\n============================================================\n")
cat("REDUCED-FORM ESTIMATES\n")
cat("============================================================\n")
print(rf_all, n = 100)

cat("\nReduced-form significance summary:\n")
cat("  Consumption weights - sig at 5%:", sum(rf_c_res$rf_sig),    "/", nrow(rf_c_res), "\n")
cat("  Employment weights  - sig at 5%:", sum(rf_emp_res$rf_sig),  "/", nrow(rf_emp_res), "\n")
cat("  Firm weights        - sig at 5%:", sum(rf_firm_res$rf_sig), "/", nrow(rf_firm_res), "\n")


# ============================================================================
# 3. PLACEBO TESTS: LAGGED WIND FORECASTS
# ============================================================================

# Create lead variables (future wind that should NOT predict current consumption)
consumption_panel <- consumption_panel %>%
  group_by(DK36Title) %>%
  arrange(TimeUTC, .by_group = TRUE) %>%
  mutate(
    Wind_c_lead24 = dplyr::lead(Wind_c, 24),
    Wind_c_lead48 = dplyr::lead(Wind_c, 48)
  ) %>%
  ungroup()

# Placebo regressions
placebo_24 <- feols(
  log_consumption ~ Wind_c_lead24 + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

placebo_48 <- feols(
  log_consumption ~ Wind_c_lead48 + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

extract_placebo <- function(fx_multi, wind_var, label) {
  models <- as.list(fx_multi)
  ids <- names(models)
  
  map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    ct <- fixest::coeftable(m)
    if (!(wind_var %in% rownames(ct))) return(NULL)
    
    est <- ct[wind_var, "Estimate"]
    se  <- ct[wind_var, "Std. Error"]
    pv  <- ct[wind_var, "Pr(>|t|)"]
    
    tibble(
      industry = str_remove(ids[i], "^.*sample:\\s*"),
      placebo_coef = est,
      placebo_se = se,
      placebo_pvalue = pv,
      placebo_sig = pv < 0.05,
      lag = label
    )
  })
}

plac_24 <- extract_placebo(placebo_24, "Wind_c_lead24", "24h lead")
plac_48 <- extract_placebo(placebo_48, "Wind_c_lead48", "48h lead")
plac_all <- bind_rows(plac_24, plac_48)

cat("\n============================================================\n")
cat("PLACEBO TEST RESULTS\n")
cat("============================================================\n")
cat("24h lead - significant at 5%:", sum(plac_24$placebo_sig), "/", nrow(plac_24), "\n")
cat("48h lead - significant at 5%:", sum(plac_48$placebo_sig), "/", nrow(plac_48), "\n")
print(plac_all, n = 70)

# Compare magnitudes: actual RF vs placebo
if (exists("rf_c_res")) {
  comparison <- rf_c_res %>%
    select(industry, actual_rf = rf_coef) %>%
    left_join(
      plac_24 %>% select(industry, placebo_24h = placebo_coef),
      by = "industry"
    ) %>%
    left_join(
      plac_48 %>% select(industry, placebo_48h = placebo_coef),
      by = "industry"
    ) %>%
    mutate(
      ratio_24 = abs(placebo_24h / actual_rf),
      ratio_48 = abs(placebo_48h / actual_rf)
    )
  
  print(comparison, n = 35)
  
  cat("\nMean |placebo/actual| ratio:\n")
  cat("  24h:", round(mean(comparison$ratio_24, na.rm = TRUE), 4), "\n")
  cat("  48h:", round(mean(comparison$ratio_48, na.rm = TRUE), 4), "\n")
}


# ============================================================================
# 4. F-STATISTIC EXTRACTION (from split feols)
# ============================================================================

extract_fstats <- function(fx_multi, weight_label) {
  models <- as.list(fx_multi)
  ids <- names(models)
  
  map_dfr(seq_along(models), function(i) {
    m <- models[[i]]
    fs <- fitstat(m, ~ivwald)
    
    tibble(
      industry = str_remove(ids[i], "^.*sample:\\s*"),
      F_stat = fs$ivwald$stat,
      weight = weight_label
    )
  })
}

fstat_c    <- extract_fstats(IV_model_het_DK10, "Consumption")
fstat_emp  <- extract_fstats(IV_model_het_Emp,  "Employment")
fstat_firm <- extract_fstats(IV_model_het_firm, "Firm")
fstat_all  <- bind_rows(fstat_c, fstat_emp, fstat_firm)

cat("\n============================================================\n")
cat("F-STATISTICS BY INDUSTRY\n")
cat("============================================================\n")
print(fstat_all %>% arrange(weight, desc(F_stat)), n = 100)

cat("\nF-statistic summary (consumption weights):\n")
cat("  Min:", round(min(fstat_c$F_stat, na.rm = TRUE), 1), "\n")
cat("  Max:", round(max(fstat_c$F_stat, na.rm = TRUE), 1), "\n")
cat("  Exceed F > 10:", sum(fstat_c$F_stat > 10, na.rm = TRUE), "/", nrow(fstat_c), "\n")
cat("  Exceed F > 16.38:", sum(fstat_c$F_stat > 16.38, na.rm = TRUE), "/", nrow(fstat_c), "\n")


# ============================================================================
# 5. COMPREHENSIVE SUMMARY TABLE FOR THESIS
# ============================================================================

# Merge everything into one industry-level summary (consumption weights)
summary_table <- fstat_c %>%
  select(industry, F_stat) %>%
  left_join(rf_c_res %>% select(industry, rf_coef, rf_pvalue, rf_sig), by = "industry") %>%
  left_join(
    results_all %>%
      filter(weight == "Consumption") %>%
      select(industry, iv_estimate = estimate, iv_se = se, iv_ci_low = conf.low, iv_ci_high = conf.high),
    by = "industry"
  ) %>%
  left_join(plac_24 %>% select(industry, placebo_24h = placebo_coef, plac_24_sig = placebo_sig), by = "industry") %>%
  arrange(desc(abs(iv_estimate)))

cat("\n============================================================\n")
cat("MASTER SUMMARY TABLE (Consumption Weights)\n")
cat("============================================================\n")
print(summary_table, n = 35)

