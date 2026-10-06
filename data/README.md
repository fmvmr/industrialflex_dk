# Data guide

All R scripts use this folder as their working directory.

| Path | Content | Source / produced by |
|---|---|---|
| `raw/elspotprices_monthly/` | Hourly day-ahead spot prices, DK1/DK2, monthly parquet | Energi Data Service (fetch notebook) |
| `raw/consumption_industry_hourly_monthly/` | Hourly electricity consumption by industry | Energi Data Service |
| `raw/consumption_category_hourly_monthly/`, `raw/consumption_gridarea_hourly_monthly/` | Hourly consumption by consumer category / grid area | Energi Data Service |
| `raw/consumption_dk10_region_year/` | Annual industry consumption by region (DK10) | Energi Data Service |
| `raw/forecast_hourly_monthly/`, `raw/forecast_hourly_monthly_compact/` | Wind and solar forecasts (compact = reduced columns, used in the analysis) | Energi Data Service |
| `raw/temp_station_hourly_monthly/`, `raw/temp_zone_hourly_monthly/` | Temperature by station and zone average | Energi Data Service |
| `raw/generation_prod_type_exchange/` | Generation by production type and exchange | Energi Data Service |
| `raw/gas_daily_balancing_price_monthly/` | Gas daily balancing prices | Energi Data Service |
| `raw/controls/` | Daily gas, carbon and coal price controls (parquet) | Built from the Datastream `.xlsx` files in `raw/` |
| `raw/datahub_pricelist/`, `raw/weather_station_selection/` | Tariff price list; selected weather stations | Energi Data Service |
| `DK_consumption_2025/` | Total Danish hourly consumption, 2025 (used by the energy model) | Energi Data Service |
| `Data_for_modelling/` | Cleaned inputs for the energy model: neighbouring-country prices, PV/wind capacity factors | `2SLS/02_data_prep/datacleaning.R` |
| `employees2021.csv` ... `employees2024.csv` | Employment per sector (used for employment weights) | *source to be confirmed by authors* |
| `DK36 / DK19 / DK127 Regional Split.xlsx` | Firms per sector and region (firm-count weights) | *source to be confirmed by authors* |
| `power_results_*_1.rds` | Final power-simulation results (consumption, employment, firm weights) | `Final Script.R` / `Powerscript.R` |
| `ar_with_estimates.rds` | Anderson-Rubin results joined with the estimates | read by `Final Script.R` |
| `tables/`, `figures/` | Saved result tables and diagnostics figures | analysis scripts |
| `submission_exports/` | Excel/CSV exports of the data as submitted with the thesis | thesis submission |

Folder naming convention: `<dataset>_<frequency>_monthly/` holds one parquet file per month, e.g. `elspot_2024_03.parquet`.
Sector codes: DK36 is the finest sector classification (the analysis unit), DK19 and DK10 are coarser aggregations. See `docs/Data manual.docx` for details.
