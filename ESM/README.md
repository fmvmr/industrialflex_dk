# Energy System Model

`Notebook_modelling.ipynb` (Julia): linear, cost-minimising capacity expansion model with hourly resolution. It builds flexibility envelopes from the estimated sector elasticities and compares scenarios: a baseline (RES + batteries), industrial demand-side flexibility (DSF) at different significance levels, and full DSF potential. It then analyses shadow prices, scarcity hours and a time-of-use tariff alignment test.

Inputs (read automatically relative to the repo): `data/Data_for_modelling/`, `data/DK_consumption_2025/`, `data/raw/consumption_industry_hourly_monthly/`.

Needs a Julia Jupyter kernel and the packages CSV, DataFrames, Dates, Glob, Parquet2, Statistics, Plots, JuMP and a solver. Launch Jupyter from the `ESM/` folder so the relative paths resolve.
