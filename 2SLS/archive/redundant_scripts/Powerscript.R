# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ── Source helpers ──────────────────────────────────────────────────────####
source(here::here("2SLS/archive/redundant_scripts/sim_power_iv_clustering.R"))
source(here::here("2SLS/archive/redundant_scripts/iv_sim_prep.R"))

# ── Load pre-computed simulations ───────────────────────────────────────####
power_results_c    <- readRDS("power_results_c_1.rds")
power_results_emp  <- readRDS("power_results_emp_1.rds")
power_results_firm <- readRDS("power_results_firm_1.rds")

# ── Aggregation-level models (DK10 + DK19 splits) ───────────────────────####
IV_DK10_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month | log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month | log_P_c ~ Wind_c,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)

IV_DK10_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month | log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month | log_P_emp ~ Wind_emp,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)

IV_DK10_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month  | log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK10Title
)
IV_DK19_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month | log_P_firm ~ Wind_firm,
  data = consumption_panel, cluster = ~ fe_week, split = ~ DK19Title
)


# ── Shared definitions  ─────────────────────────────────────────────────####
gran_to_var <- c(DK36 = "DK36Title", DK19 = "DK19Title", DK10 = "DK10Title")

extract_for_power <- function(model, granularity) {
  purrr::map_dfr(seq_along(names(model)), function(i) {
    m      <- model[[i]]
    sector <- names(model)[i]
    ct     <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    fs_ct    <- fixest::coeftable(m$iv_first_stage[[1]])
    inst_row <- rownames(fs_ct)[stringr::str_detect(rownames(fs_ct), "Wind")]
    tibble::tibble(
      sector      = stringr::str_remove(sector, "^sample\\.var:.*sample: "),
      granularity = granularity,
      true_effect = as.numeric(ct[iv_row, "Estimate"]),
      p_value     = as.numeric(ct[iv_row, "Pr(>|t|)"]),
      iv_effect   = as.numeric(fs_ct[inst_row, "Estimate"]),
      fs_f        = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                             error = function(e) NA_real_)
    )
  })
}

run_power_by_gran <- function(power_inputs, df_panel,
                              endog_var, instrument_var, temp_var, weight_name,
                              n_sims = 50, seed = 123,
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
              fe_vars = c("fe_hour", "fe_month"),
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

# ── Compute artificial effects  ─────────────────────────────────────────####
# ── First stage effect  
# ── Extract observed first-stage coefficients per sector and granularity 
# These are fixed (not gridded) — one value per sector, used as-is in simulation

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
# Base elasticity from Hirth et al. (2024) — adjust this value as needed
base_elasticity <- -0.045

# 5-point grid: 0.5×, 0.75×, 1.0×, 1.25×, 1.5× the literature benchmark
effect_grid <- base_elasticity * c(0.25, 0.5, 0.75, 1.00, 1.5)

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


# ── Simulation  ─────────────────────────────────────────────────────────####
power_results_c <- run_power_by_gran(
  power_inputs = power_inputs_c,
  df_panel = consumption_panel,
  endog_var = "log_P_c",
  instrument_var = "Wind_c",
  temp_var  = "Temp_c",
  weight_name = "consumption_weight"
)

saveRDS(power_results_c, "power_results_c_2.rds")
# 
power_results_emp <- run_power_by_gran(
  power_inputs = power_inputs_emp,
  df_panel = consumption_panel,
  endog_var = "log_P_emp",
  instrument_var = "Wind_emp",
  temp_var = "Temp_emp",
  weight_name = "employment_weight"
)
# 
saveRDS(power_results_emp, "power_results_emp_2.rds")
# 
power_results_firm <- run_power_by_gran(
  power_inputs = power_inputs_firm,
  df_panel = consumption_panel,
  endog_var = "log_P_firm",
  instrument_var = "Wind_firm",
  temp_var = "Temp_firm",
  weight_name = "firm_weight"
)
# 
saveRDS(power_results_firm, "power_results_firm_2.rds")