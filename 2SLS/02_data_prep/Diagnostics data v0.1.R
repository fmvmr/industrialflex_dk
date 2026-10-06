# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
###### DIAGNOSTICS DATA 
############################# INSPECT DATA #####################################
# ---------------------------------------------------------
# Helper: Check coverage for a folder of monthly parquet files
# ---------------------------------------------------------

check_hourly_folder <- function(path, time_col) {
  
  files <- list.files(path, pattern = "\\.parquet$", full.names = TRUE)
  
  if (length(files) == 0) {
    return(data.frame(
      Dataset = basename(path),
      Files = 0,
      Start = NA,
      End = NA
    ))
  }
  
  dates <- map_dfr(files, function(f) {
    df <- read_parquet(f, as_data_frame = FALSE) %>%
      select(!!time_col) %>%
      collect()
    
    tibble(
      start = min(df[[time_col]], na.rm = TRUE),
      end   = max(df[[time_col]], na.rm = TRUE)
    )
  })
  
  tibble(
    Dataset = basename(path),
    Files   = length(files),
    Start   = min(dates$start, na.rm = TRUE),
    End     = max(dates$end, na.rm = TRUE)
  )
}

# ---------------------------------------------------------
# Check all hourly datasets
# ---------------------------------------------------------

hourly_summary <- bind_rows(
  check_hourly_folder("raw/elspotprices_monthly", "HourUTC"),
  check_hourly_folder("raw/forecast_hourly_monthly_compact", "HourUTC"),
  check_hourly_folder("raw/temp_zone_hourly_monthly", "HourUTC"),
  check_hourly_folder("raw/consumption_industry_hourly_monthly", "TimeUTC"),
  check_hourly_folder("raw/consumption_category_hourly_monthly", "TimeUTC"),
  check_hourly_folder("raw/consumption_gridarea_hourly_monthly", "TimeUTC")
)

# ---------------------------------------------------------
# Daily controls (single files)
# ---------------------------------------------------------

gas <- read_parquet("raw/controls/gas_daily_2020_2025.parquet")
carbon <- read_parquet("raw/controls/carbon_daily_2020_2025.parquet")

daily_summary <- tibble(
  Dataset = c("Gas", "Carbon"),
  Files   = c(1, 1),
  Start   = c(min(gas$Date), min(carbon$Date)),
  End     = c(max(gas$Date), max(carbon$Date))
)

# ---------------------------------------------------------
# Final Coverage Table
# ---------------------------------------------------------

coverage <- bind_rows(hourly_summary, daily_summary) %>%
  arrange(Start)

print(coverage)

# Window defined in Danish local time, converted to UTC for filtering hourly data
start_cet <- ymd_hms("2021-07-31 00:00:00", tz = "Europe/Copenhagen")
end_cet   <- ymd_hms("2025-09-30 23:59:59", tz = "Europe/Copenhagen")
start_dt  <- with_tz(start_cet, "UTC")
end_dt    <- with_tz(end_cet, "UTC")

temp_window <- temp %>%
  filter(HourUTC >= start_dt,
         HourUTC <= end_dt)

nrow(temp_window)

carbon_window <- carbon %>%
  filter(Date >= as.Date(start_cet),
         Date <= as.Date(end_cet))

gas_window <- gas %>%
  filter(Date >= as.Date(start_cet),
         Date <= as.Date(end_cet))

coal_window <- coal %>%
  filter(Date >= as.Date(start_cet),
         Date <= as.Date(end_cet))

nrow(carbon_window)
nrow(gas_window)
nrow(coal_window)

missing_hours <- prices %>%
  select(HourUTC, PriceArea) %>%
  anti_join(forecast %>% select(HourUTC, PriceArea),
            by = c("HourUTC","PriceArea"))

nrow(missing_hours)



count_rows_in_period_folder <- function(path, time_col) {
  files <- list.files(path, pattern="\\.parquet$", full.names=TRUE)
  if (length(files) == 0) {
    return(tibble(Dataset = basename(path), Files = 0L, Rows_in_window = 0L))
  }
  
  counts <- map_int(files, function(f) {
    df <- read_parquet(f)
    t <- df[[time_col]]
    if (!inherits(t, "POSIXct")) t <- ymd_hms(t, tz="UTC")
    sum(t >= start_dt & t <= end_dt, na.rm = TRUE)
  })
  
  tibble(
    Dataset = basename(path),
    Files = length(files),
    Rows_in_window = sum(counts)
  )
}

# Annual DK10-region files: count rows for years in window (by filename)
count_rows_annual_yearfiles <- function(path, start_year, end_year, pattern = ".*_(\\d{4})\\.parquet$") {
  files <- list.files(path, pattern="\\.parquet$", full.names=TRUE)
  if (length(files) == 0) {
    return(tibble(Dataset = basename(path), Files = 0L, Rows_in_window = 0L))
  }
  
  years <- str_match(files, pattern)[,2]
  years <- suppressWarnings(as.integer(years))
  
  keep <- !is.na(years) & years >= start_year & years <= end_year
  files_keep <- files[keep]
  
  if (length(files_keep) == 0) {
    return(tibble(Dataset = basename(path), Files = 0L, Rows_in_window = 0L))
  }
  
  total_rows <- sum(map_int(files_keep, \(f) nrow(read_parquet(f))))
  
  tibble(
    Dataset = basename(path),
    Files = length(files_keep),
    Rows_in_window = total_rows
  )
}

results <- bind_rows(
  count_rows_in_period_folder("raw/elspotprices_monthly", "HourUTC"),
  count_rows_in_period_folder("raw/forecast_hourly_monthly_compact", "HourUTC"),
  count_rows_in_period_folder("raw/temp_zone_hourly_monthly", "HourUTC"),
  count_rows_in_period_folder("raw/consumption_industry_hourly_monthly", "TimeUTC"),
  count_rows_in_period_folder("raw/consumption_category_hourly_monthly", "TimeUTC"),
  count_rows_annual_yearfiles(
    "raw/consumption_dk10_region_year",
    start_year = year(start_cet),
    end_year   = year(end_cet)
  )
) %>% arrange(desc(Rows_in_window))

print(results)

library(dplyr)

weighted_prices %>%
  summarise(
    equal_hours = sum(DK1 == DK2, na.rm = TRUE),
    total_hours = sum(!is.na(DK1) & !is.na(DK2)),
    share_equal = equal_hours / total_hours
  )


weighted_prices %>%
  summarise(
    mean_spread = mean(abs(DK1 - DK2), na.rm = TRUE),
    p95_spread  = quantile(abs(DK1 - DK2), 0.95, na.rm = TRUE),
    max_spread  = max(abs(DK1 - DK2), na.rm = TRUE)
  )



