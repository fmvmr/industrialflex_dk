# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# ── Cross-scheme stability diagnostics ───────────────────────────────────────

# 1. Pairwise correlations between weighting scheme estimates
cor_matrix <- iv_c |>
  left_join(iv_emp,  by = "DK36_en", suffix = c("_c", "_emp")) |>
  left_join(iv_firm, by = "DK36_en") |>
  rename(coef_firm = coef) |>
  summarise(
    cor_c_emp    = cor(coef_c,    coef_emp),
    cor_c_firm   = cor(coef_c,    coef_firm),
    cor_emp_firm = cor(coef_emp,  coef_firm)
  )

print(cor_matrix)

# 2. Maximum divergence across schemes for any single sector
divergence <- iv_c |>
  left_join(iv_emp,  by = "DK36_en", suffix = c("_c", "_emp")) |>
  left_join(iv_firm, by = "DK36_en") |>
  rename(coef_firm = coef) |>
  mutate(
    max_diff = pmax(
      abs(coef_c   - coef_emp),
      abs(coef_c   - coef_firm),
      abs(coef_emp - coef_firm)
    )
  ) |>
  arrange(desc(max_diff)) |>
  select(DK36_en, coef_c, coef_emp, coef_firm, max_diff)

cat("\nTop 5 sectors by cross-scheme divergence:\n")
print(head(divergence, 5))

cat("\nMaximum divergence across all sectors:", 
    round(max(divergence$max_diff), 4), "\n")
cat("Mean divergence across all sectors:", 
    round(mean(divergence$max_diff), 4), "\n")