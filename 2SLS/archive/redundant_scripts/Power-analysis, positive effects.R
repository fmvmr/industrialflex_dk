# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ── Helper functions ───────────────────────────────────────────────────────####
power_sim_iv <- function(df,
                         iv_effect, true_effect,
                         endog_var, instrument_var, outcome_var,
                         temp_var,
                         sector_var,
                         fe_vars     = c("fe_hour", "fe_month", "fe_dow"),
                         controls    = c("log_gas", "log_coal", "log_carbon"),
                         cluster_var = "fe_week",
                         n_sims = 1,
                         seed = 123)  {
  
  set.seed(seed)
  
  controls_full <- c(temp_var, controls)
  
  needed <- c(sector_var, endog_var, instrument_var, outcome_var,
              fe_vars, controls_full, cluster_var)
  
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))
  }
  
  sectors <- sort(unique(df[[sector_var]]))
  
  # ── Pre-compute residual SD per sector ────────────────────────────────
  resid_formula <- stats::as.formula(
    paste0(
      outcome_var, " ~ ", paste(controls_full, collapse = " + "),
      " | ", paste(fe_vars, collapse = " + ")
    )
  )
  
  sector_resid_sd <- stats::setNames(
    vapply(sectors, function(sec) {
      df_s <- df[df[[sector_var]] == sec, , drop = FALSE]
      
      m <- suppressMessages(suppressWarnings(
        fixest::feols(
          resid_formula,
          data = df_s,
          warn = FALSE,
          notes = FALSE
        )
      ))
      
      stats::sd(stats::residuals(m), na.rm = TRUE)
    }, numeric(1)),
    sectors
  )
  
  permute_block_series <- function(data, value_var, cluster_var) {
    clusters <- unique(data[[cluster_var]])
    
    cluster_map <- tibble::tibble(
      !!cluster_var := clusters,
      source_cluster = sample(clusters)
    )
    
    value_lookup <- data %>%
      dplyr::arrange(.data[[cluster_var]]) %>%
      dplyr::group_by(.data[[cluster_var]]) %>%
      dplyr::mutate(.pos = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(c(cluster_var, value_var)), .pos) %>%
      dplyr::rename(
        source_cluster = !!rlang::sym(cluster_var),
        permuted_value = !!rlang::sym(value_var)
      )
    
    data %>%
      dplyr::arrange(.data[[cluster_var]]) %>%
      dplyr::group_by(.data[[cluster_var]]) %>%
      dplyr::mutate(.pos = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(cluster_map,  by = cluster_var) %>%
      dplyr::left_join(value_lookup, by = c("source_cluster", ".pos")) %>%
      dplyr::pull(permuted_value)
  }
  
  sims <- vector("list", n_sims)
  
  for (i in seq_len(n_sims)) {
    
    res_i <- lapply(sectors, function(sec) {
      
      df_sec <- df[df[[sector_var]] == sec, , drop = FALSE]
      resid_sd <- sector_resid_sd[[sec]]
      if (!is.finite(resid_sd)) resid_sd <- 0
      
      # ── 1) Block-permute instrument, endogenous variable, and outcome ──
      df_sec <- df_sec %>%
        dplyr::mutate(
          perm_instrument = permute_block_series(., instrument_var, cluster_var),
          perm_endog      = permute_block_series(., endog_var, cluster_var),
          perm_outcome    = permute_block_series(., outcome_var, cluster_var)
        )
      
      # ── 2) Impose first stage on permuted endogenous baseline ───────────
      df_sec <- df_sec %>%
        dplyr::mutate(
          sim_instrument = perm_instrument,
          sim_endog      = perm_endog + iv_effect * sim_instrument
        )
      
      # ── 3) Impose structural effect on permuted outcome baseline ────────
      df_sec <- df_sec %>%
        dplyr::mutate(
          sim_outcome = perm_outcome +
            true_effect * sim_endog +
            rnorm(dplyr::n(), mean = 0, sd = resid_sd)
        )
      
      # ── 4) Re-estimate FE-IV model ──────────────────────────────────────
      fml <- stats::as.formula(
        paste0(
          "sim_outcome ~ ", paste(controls_full, collapse = " + "),
          " | ", paste(fe_vars, collapse = " + "),
          " | sim_endog ~ sim_instrument"
        )
      )
      
      m <- fixest::feols(
        fml,
        data = df_sec,
        cluster = stats::as.formula(paste0("~", cluster_var))
      )
      
      # ── 5) Extract IV coefficient ───────────────────────────────────────
      ct        <- fixest::coeftable(m)
      coef_name <- "fit_sim_endog"
      
      if (!coef_name %in% rownames(ct)) {
        return(tibble::tibble(
          sim       = i,
          sector    = sec,
          estimate  = NA_real_,
          std_error = NA_real_,
          p_value   = NA_real_,
          sig       = NA_integer_
        ))
      }
      
      tibble::tibble(
        sim       = i,
        sector    = sec,
        estimate  = as.numeric(ct[coef_name, "Estimate"]),
        std_error = as.numeric(ct[coef_name, "Std. Error"]),
        p_value   = as.numeric(ct[coef_name, "Pr(>|t|)"]),
        sig       = as.integer(as.numeric(ct[coef_name, "Pr(>|t|)"]) < 0.05)
      )
    })
    
    sims[[i]] <- dplyr::bind_rows(res_i)
  }
  
  dplyr::bind_rows(sims) %>%
    dplyr::mutate(
      iv_effect = iv_effect,
      true_effect = true_effect
    )
}


analyze_iv_power_simulation_sector <- function(results) {
  if (!is.data.frame(results)) results <- dplyr::bind_rows(results)
  
  results <- results %>%
    dplyr::mutate(
      wrong_sign = dplyr::case_when(
        true_effect < 0 & estimate >  0 ~ 1L,
        true_effect < 0 & estimate <= 0 ~ 0L,
        true_effect > 0 & estimate <  0 ~ 1L,
        true_effect > 0 & estimate >= 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      est_ratio = dplyr::if_else(
        true_effect != 0,
        estimate / true_effect,
        NA_real_
      )
    )
  
  power <- results %>%
    dplyr::group_by(granularity, sector, iv_effect, true_effect) %>%
    dplyr::summarise(
      power = mean(sig, na.rm = TRUE),
      .groups = "drop"
    )
  
  aux <- results %>%
    dplyr::filter(sig == 1) %>%
    dplyr::group_by(granularity, sector, iv_effect, true_effect) %>%
    dplyr::summarise(
      wrong_sign = mean(wrong_sign, na.rm = TRUE),
      est_ratio  = mean(est_ratio, na.rm = TRUE),
      .groups    = "drop"
    )
  
  dplyr::left_join(
    power, aux,
    by = c("granularity", "sector", "iv_effect", "true_effect")
  )
}

# ── Power analysis summary table by aggregation level ──────────────────────####

compute_level_summary <- function(df, sector_var, level_label) {
  
  # Number of sectors and cluster counts
  sector_summary <- df %>%
    group_by(.data[[sector_var]]) %>%
    summarise(
      n_obs      = n(),
      n_clusters = n_distinct(fe_week),
      .groups    = "drop"
    )
  
  # Residual SD per sector (same regression as power analysis calibration)
  resid_sd <- df %>%
    group_by(.data[[sector_var]]) %>%
    group_map(~ {
      m <- fixest::feols(
        log_consumption ~ Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_dow + fe_month,
        data = .x
      )
      tibble::tibble(
        sector   = .y[[1]],
        resid_sd = sd(residuals(m), na.rm = TRUE)
      )
    }) %>%
    bind_rows()
  
  tibble::tibble(
    Granularity       = level_label,
    N_sectors         = nrow(sector_summary),
    Mean_clusters     = round(mean(sector_summary$n_clusters), 0),
    Min_clusters      = min(sector_summary$n_clusters),
    Max_clusters      = max(sector_summary$n_clusters),
    Mean_obs          = round(mean(sector_summary$n_obs), 0),
    Mean_residual_SD  = round(mean(resid_sd$resid_sd, na.rm = TRUE), 4),
    Median_residual_SD = round(median(resid_sd$resid_sd, na.rm = TRUE), 4),
    Max_residual_SD   = round(max(resid_sd$resid_sd, na.rm = TRUE), 4),
    Min_residual_SD   = round(min(resid_sd$resid_sd, na.rm = TRUE), 4)
  )
}

level_summary <- bind_rows(
  compute_level_summary(consumption_panel, "DK36Title", "DK36"),
  compute_level_summary(consumption_panel, "DK19Title", "DK19"),
  compute_level_summary(consumption_panel, "DK10Title", "DK10")
)


print(knitr::kable(
  level_summary,
  format = "simple",
  col.names = c("Level", "Sectors", "Mean clusters", "Min clusters", 
                "Max clusters", "Mean obs", "Mean σ̂", "Median σ̂", 
                "Max σ̂", "Min σ̂")
))


# ── Aggregation-level models (DK10 + DK19 splits) ──────────────────────────####
IV_DK10_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)

IV_DK10_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)

IV_DK10_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow| log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_dow | log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)


# ── Shared definitions  ────────────────────────────────────────────────────####
gran_to_var <- c(DK36 = "DK36_en", DK19 = "DK19Title", DK10 = "DK10Title")


run_power_by_gran <- function(power_inputs, df_panel,
                              endog_var, instrument_var, temp_var, weight_name,
                              n_sims = 100, seed = 123,
                              batch_size = 25,
                              save_dir = "power_sim_checkpoints") {
  
  save_dir <- file.path(path.expand(save_dir), weight_name)
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  
  power_inputs_indexed <- power_inputs %>%
    dplyr::mutate(row_id = dplyr::row_number())
  
  purrr::pmap_dfr(
    dplyr::select(power_inputs_indexed, row_id, sector, granularity, true_effect, iv_effect),
    function(row_id, sector, granularity, true_effect, iv_effect) {
      
      sv  <- gran_to_var[[granularity]]
      dat <- dplyr::filter(df_panel, .data[[sv]] == sector)
      
      if (nrow(dat) < 100) return(NULL)
      
      sector_safe <- gsub("[^[:alnum:]_\\-]", "_", as.character(sector))
      n_batches <- ceiling(n_sims / batch_size)
      batch_results <- vector("list", n_batches)
      
      for (b in seq_len(n_batches)) {
        
        sims_this_batch <- min(batch_size, n_sims - (b - 1) * batch_size)
        
        batch_file <- file.path(
          save_dir,
          paste0(
            "power_",
            weight_name, "_",
            granularity, "_",
            sector_safe, "_te_", round(true_effect, 6),
            "_iv_", round(iv_effect, 6),
            "_batch_", b, ".rds"
          )
        )
        
        if (file.exists(batch_file)) {
          message("Loading existing batch ", b, "/", n_batches,
                  " for ", sector, " (", granularity, ", ", weight_name, ")")
          batch_results[[b]] <- readRDS(batch_file)
          next
        }
        
        message("Running batch ", b, "/", n_batches,
                " for ", sector, " (", granularity, ", ", weight_name, ")")
        
        batch_seed <- seed + row_id * 10000 + b
        
        res <- tryCatch(
          {
            power_sim_iv(
              df = dat,
              iv_effect = iv_effect,
              true_effect = true_effect,
              endog_var = endog_var,
              instrument_var = instrument_var,
              temp_var = temp_var,
              outcome_var = "log_consumption",
              sector_var = sv,
              fe_vars = c("fe_hour", "fe_month", "fe_dow"),
              controls = c( "log_gas", "log_coal", "log_carbon"),
              cluster_var = "fe_week",
              n_sims = sims_this_batch,
              seed = batch_seed
            ) %>%
              dplyr::mutate(
                sim = sim + (b - 1) * batch_size,
                granularity = granularity,
                sector = sector,
                batch = b,
                weight_name = weight_name
              )
          },
          error = function(e) {
            message("Batch ", b, " failed for ", sector, " (", granularity, "): ", e$message)
            return(NULL)
          }
        )
        
        if (!is.null(res)) saveRDS(res, batch_file)
        batch_results[[b]] <- res
        
        gc()
      }
      
      dplyr::bind_rows(batch_results)
    },
    .progress = "Power simulation"
  )
}

# ── Compute artificial effects  ────────────────────────────────────────────####
# ── Extract observed first-stage coefficients per sector and granularity 

extract_first_stage <- function(model, granularity) {
  purrr::map_dfr(seq_along(names(model)), function(i) {
    m      <- model[[i]]
    sector <- names(model)[i]
    
    fs_ct    <- fixest::coeftable(m$iv_first_stage[[1]])
    inst_row <- rownames(fs_ct)[stringr::str_detect(rownames(fs_ct), "Wind")]
    
    tibble::tibble(
      sector      = stringr::str_remove(sector, "^sample\\.var:.*sample: "),
      granularity = granularity,
      iv_effect   = as.numeric(fs_ct[inst_row, "Estimate"])
    )
  })
}

# ── Consumption weights ───────────────────────────────────────────────────────
fs_inputs_c <- bind_rows(
  extract_first_stage(IV_model_het_DK10_W, "DK36"),
  extract_first_stage(IV_DK19_agg_c,       "DK19"),
  extract_first_stage(IV_DK10_agg_c,       "DK10")
)

# ── Employment weights ────────────────────────────────────────────────────────
fs_inputs_emp <- bind_rows(
  extract_first_stage(IV_model_het_Emp_w,  "DK36"),
  extract_first_stage(IV_DK19_agg_emp,     "DK19"),
  extract_first_stage(IV_DK10_agg_emp,     "DK10")
)

# ── Firm-count weights ────────────────────────────────────────────────────────
fs_inputs_firm <- bind_rows(
  extract_first_stage(IV_model_het_firm_w, "DK36"),
  extract_first_stage(IV_DK19_agg_firm,    "DK19"),
  extract_first_stage(IV_DK10_agg_firm,    "DK10")
)

# ── Literature-anchored effect grid ───────────────────────────────────────────
# Base elasticity from Hirth et al. (2024)
base_elasticity <- -0.045

# 5-point grid: 0.5×, 0.75×, 1.0×, 1.25×, 1.5× the literature benchmark
effect_grid <- base_elasticity * c(-0.25, -0.5, -0.75, -1.00, -1.5)

# ── Build simulation inputs: observed first stage × literature effect grid ────

build_power_inputs <- function(fs_inputs, effect_grid) {
  tidyr::crossing(
    fs_inputs,
    true_effect = effect_grid
  )
}

power_inputs_c    <- build_power_inputs(fs_inputs_c,    effect_grid)
power_inputs_emp  <- build_power_inputs(fs_inputs_emp,  effect_grid)
power_inputs_firm <- build_power_inputs(fs_inputs_firm, effect_grid)


# ── Simulation  ────────────────────────────────────────────────────────────####
power_results_c <- run_power_by_gran(
  power_inputs = power_inputs_c,
  df_panel = consumption_panel,
  endog_var = "log_P_c",
  instrument_var = "Wind_c",
  temp_var  = "Temp_c",
  weight_name = "consumption_weight"
)

saveRDS(power_results_c, "power_results_c_1.rds")

power_results_emp <- run_power_by_gran(
  power_inputs = power_inputs_emp,
  df_panel = consumption_panel,
  endog_var = "log_P_emp",
  instrument_var = "Wind_emp",
  temp_var = "Temp_emp",
  weight_name = "employment_weight"
)

saveRDS(power_results_emp, "power_results_emp_1.rds")

power_results_firm <- run_power_by_gran(
  power_inputs = power_inputs_firm,
  df_panel = consumption_panel,
  endog_var = "log_P_firm",
  instrument_var = "Wind_firm",
  temp_var = "Temp_firm",
  weight_name = "firm_weight"
)

saveRDS(power_results_firm, "power_results_firm_1.rds")


# ── Load pre-computed simulations ──────────────────────────────────────────####
power_results_c    <- readRDS("power_results_c_1.rds")
power_results_emp  <- readRDS("power_results_emp_1.rds")
power_results_firm <- readRDS("power_results_firm_1.rds")
# ── Power analysis summaries ───────────────────────────────────────────────####
power_analysis_c    <- analyze_iv_power_simulation_sector(power_results_c)
power_analysis_emp  <- analyze_iv_power_simulation_sector(power_results_emp)
power_analysis_firm <- analyze_iv_power_simulation_sector(power_results_firm)

power_all <- bind_rows(
  power_analysis_c    %>% mutate(weight = "Consumption"),
  power_analysis_emp  %>% mutate(weight = "Employment"),
  power_analysis_firm %>% mutate(weight = "Firm count")
) %>%
  mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    weight      = factor(weight, levels = c("Consumption", "Employment", "Firm count"))
  )




