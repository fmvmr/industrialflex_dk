# 2SLS Analysis

Econometric analysis of sectoral price responsiveness (R). Run from the repo's RStudio project; see the top-level `README.md`.

**Main scripts: `03_main_analysis/Final Script.R` (main analysis) and `04_supporting_analysis/APPENDIX SCRIPT.R` (appendices).** Everything else is supporting or archived.

## 01_data_fetch
- `Datafetchscript 2.0.ipynb`: downloads the raw monthly datasets (spot prices, industry consumption, forecasts, temperature, gas/carbon/coal, generation, ...) from Energi Data Service into `data/raw/`, with a diagnostics section after each fetch. Optional, since the raw data is already in `data/raw/`.

## 02_data_prep
- `datacleaning.R`: cleans neighbouring-country prices and capacity factors for the energy model (outputs go to `data/Data_for_modelling/`). Needs raw source files that are not in this repo; its output is included.
- `DK19DK36DK10 Method.R`: pro-rata spatial disaggregation of industrial consumption to regions, with diagnostic plots.
- `Diagnostics data v0.1.R`: coverage checks on the downloaded monthly files.

## 03_main_analysis
- `Final Script.R`: **the main script.** Builds the hourly sector panel (`consumption_panel`), then runs the OLS and IV models, robustness checks, Anderson-Rubin, placebo tests and the power simulations. It also contains the power-simulation functions. Saved results: `data/power_results_*_1.rds`.

## 04_supporting_analysis
Most of these need the objects from `Final Script.R` (especially `consumption_panel`) to be in the R session. Run the first part of `Final Script.R` first.

| Script | Purpose |
|---|---|
| `APPENDIX SCRIPT.R` | Appendices 2 to 15: alternative specifications, negative prices, HAC vs clustered SEs, Q-Q diagnostics, missing observations, complier profiling, Anderson-Rubin, ecological inference, power curves, PACF |
| `Visuals.R` | Thesis figures: coefficient plots, power curves, weight comparisons, bubble plot, weather-station map |
| `Sensitivity Tables Export.R` | sensemakr robustness analysis with cluster-robust t-statistics; writes `data/tables/` |
| `tF valid inference.R` | tF-adjusted standard errors (Lee et al., 2022), compared with conventional and Anderson-Rubin intervals |
| `Estimations Year over Year.R` | Temporal stability of the IV estimates (fixed vs flexible contract structure) |
| `Identification chain.R` | Pooled identification-chain regressions (first stage, reduced form) with wind in GWh |
| `Lagged.R` | Robustness with lagged consumption |
| `Flexibillityband.R` | Sector-level flexibility bands (uses a hard-coded first-stage coefficient, `-0.026425`; update it if the first stage is re-estimated) |
| `Consumption shares script.R` | Annual consumption share per sector, joined with significance |
| `Correlation_weights.R` | Correlation and divergence of estimates across the three weighting schemes |

## archive
Superseded or duplicated scripts. **Not needed to reproduce the results.** They are kept for reference only and may contain paths that no longer work.

- `redundant_scripts/`: code that is already contained in `Final Script.R`, `APPENDIX SCRIPT.R` or `Visuals.R`:
  - Power simulation: `sim_power_iv_clustering.R`, `iv_sim_prep.R`, `Powerscript.R`, `Power-analysis, positive effects.R` (now in `Final Script.R`)
  - Appendix material: `missinghours.R` (App. 7), `appendix-complierprofiling.R` (App. 8), `HAC VS CLUSTERS.R` (App. 5), `thams_residual_pacf.R` (App. 15)
  - Figures: `Leafletmap of weather stations.R`, `Bubbleplot estimates.R` (now in `Visuals.R`)
  - `Placebo_uno.R`: probably an earlier version of the placebo section in `Final Script.R`
- `iv_script_versions/`: earlier iterations of the IV analysis (`IV INSTRUMENT` v1 to v5, and the separate DK10/DK19/DK36 estimation scripts). Several still contain hard-coded paths from the authors' computers.
- `other/`: early exam script, old fetch notebook and small tests.
