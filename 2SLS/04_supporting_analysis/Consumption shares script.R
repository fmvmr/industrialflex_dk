# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ── Check total industrial consumption ────────────────────────────────────────
consumption_panel %>%
  summarise(
    n_obs      = n(),
    total_MWh  = sum(Consumption_MWh, na.rm = TRUE),
    total_GWh  = total_MWh / 1000,
    total_TWh  = total_MWh / 1e6
  ) %>%
  print()

# ── Annual consumption per sector (deduplicated by hour) ─────────────────────
annual_by_sector <- consumption_panel %>%
  group_by(DK36_en, Year) %>%
  summarise(annual_GWh = sum(Consumption_MWh, na.rm = TRUE) / 1000,
            .groups = "drop") %>%
  filter(Year %in% c(2022, 2023, 2024)) %>%  # full years only
  group_by(DK36_en) %>%
  summarise(avg_annual_GWh = mean(annual_GWh))




# ── Join significance and sum ─────────────────────────────────────────────────
annual_by_sector %>%
  left_join(iv_c %>% mutate(
    sig_level = case_when(
      pval < 0.01 ~ "1%",
      pval < 0.05 ~ "5%",
      TRUE        ~ "Insignificant"
    )
  ), by = "DK36_en") %>%
  filter(sig_level %in% c("1%", "5%")) %>%
  group_by(sig_level) %>%
  summarise(total_GWh = sum(avg_annual_GWh)) %>%
  mutate(pct_danish = total_GWh / 197500 * 100) %>%
  bind_rows(
    tibble(
      sig_level = "All significant (1% + 5%)",
      total_GWh = sum(.$total_GWh),
      pct_danish = sum(.$total_GWh) / 197500 * 100
    )
  ) %>%
  print()