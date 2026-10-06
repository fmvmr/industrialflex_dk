# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
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
