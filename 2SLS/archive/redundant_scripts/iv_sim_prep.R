# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
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