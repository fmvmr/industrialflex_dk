# industrialflex_dk

This project evaluates the price responsiveness of Danish industry at the sectoral level. The work is based on the master's thesis of Jacob Uno Joensen and Jes Broby Tinghuus Petersen.

The analysis applies two-stage least squares (2SLS) regression, complemented by a series of robustness tests, to estimate sectoral responses to electricity prices. The econometric analysis is accompanied by an energy system analysis that assesses the system-level implications of the estimated implicit demand-side flexibility.

## Repository layout

```
industrialflex_dk/
├── 2SLS/                      Econometric analysis (R, plus one Python notebook)
│   ├── 01_data_fetch/         Python notebook that downloads the raw data from Energi Data Service
│   ├── 02_data_prep/          Cleaning, regional disaggregation, data diagnostics
│   ├── 03_main_analysis/      Final Script.R = the main analysis (panel, IV, robustness, power)
│   ├── 04_supporting_analysis/ APPENDIX SCRIPT.R, figures, extra robustness tests
│   └── archive/               Superseded or duplicated scripts, kept for reference only
├── ESM/                       Energy system model (Julia notebook)
├── data/                      All input data + saved results (see data/README.md)
├── docs/                      Data manual and industry classification reference
└── industrialflex_dk.Rproj    Open this in RStudio
```

## Quick start

1. **Open `industrialflex_dk.Rproj`** in RStudio. Every R script begins with `setwd(here::here("data"))`, so
   the `data/` folder is the working directory and all paths are relative to it. No personal paths remain.
2. Install packages once: `install.packages(c("here", "arrow", "dplyr", "tidyr", "tibble", "purrr", "stringr", "lubridate", "readr", "readxl", "writexl", "glue", "ggplot2", "ggrepel", "patchwork", "scales", "fixest", "sensemakr", "moments", "knitr", "kableExtra", "data.table", "leaflet", "jsonlite", "httr", "rlang"))`
3. Run **`2SLS/03_main_analysis/Final Script.R`**. It loads the data from `data/raw/`, builds the hourly sector panel (`consumption_panel`), and runs the main estimations.
4. The scripts in `2SLS/04_supporting_analysis/` mostly **expect `consumption_panel` (and other objects) to already be in your R session**. Run the first part of `Final Script.R` (down to the OLS section) first, then run the supporting script you want.
5. Energy model: open `ESM/Notebook_modelling.ipynb` in Jupyter with a Julia kernel (needs CSV, DataFrames, Parquet2, Glob, Plots, JuMP and a solver).
6. Python notebook: `2SLS/01_data_fetch/Datafetchscript 2.0.ipynb` (needs pandas, numpy, requests, tqdm, pyarrow). Only needed to re-download the raw data.

## Run order (summary)

| Step | Script | What it does |
|---|---|---|
| 1 | `2SLS/01_data_fetch/Datafetchscript 2.0.ipynb` | Downloads raw hourly data via API into `data/raw/` (already included, so optional) |
| 2 | `2SLS/02_data_prep/*.R` | Cleaning, regional split, data coverage diagnostics |
| 3 | `2SLS/03_main_analysis/Final Script.R` | Panel construction, OLS/IV, Anderson-Rubin, placebo, power analysis |
| 4 | `2SLS/04_supporting_analysis/*.R` | Appendix material, figures, sensitivity, extra tests |
| 5 | `ESM/Notebook_modelling.ipynb` | Capacity-expansion model using the estimated flexibility |

See `2SLS/README.md` and `ESM/README.md` for per-script descriptions, and `data/README.md` for the data.

## Known issues and notes for newcomers

- **Not in the repo (too large or regenerable):** the R workspace snapshots (`*.RData`, 3 GB), `raw_industry.csv` (414 MB, not used by any script), and the 16,500 power-simulation checkpoint files. The scripts recreate the checkpoints if you rerun the simulations; the final simulation results are included as `data/power_results_*_1.rds`.
- **`2SLS/02_data_prep/datacleaning.R` cannot be run from this repo alone.** It reads a few source files that were never part of the project folder (`Sweden.csv`, `Germany.csv` etc. as downloaded from the price provider, a municipal capacity file, and the municipality-to-NUTS correspondence table). Its output, the files in `data/Data_for_modelling/`, is included, which is all the energy model needs.
- The power-simulation checkpoint folder is recreated under `data/power_sim_checkpoints/` if you rerun the simulations (the simulation code is in `Final Script.R`). The full run takes many hours.
- **What is in `2SLS/archive/`:** earlier IV script versions (`iv_script_versions/`) and scripts whose code is duplicated in `Final Script.R`, `APPENDIX SCRIPT.R` or `Visuals.R` (`redundant_scripts/`). The thesis results come from `Final Script.R` and `APPENDIX SCRIPT.R`. Archived scripts may contain hard-coded paths to the authors' computers and will not run without editing. See `2SLS/README.md` for the full list.
- One line at the top of `Final Script.R` referred to a model created later in the script, which stopped it from running in a fresh session. It is commented out (marked in the file); nothing else in the analysis code was changed.
