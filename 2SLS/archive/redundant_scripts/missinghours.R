# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------



# ── Missing hours pattern analysis ────────────────────────────────────────####

# Build the full expected grid and find missing combinations
full_grid <- expand.grid(
  TimeUTC   = seq(min(consumption_panel$TimeUTC),
                  max(consumption_panel$TimeUTC),
                  by = "hour"),
  DK36Title = unique(consumption_panel$DK36Title),
  stringsAsFactors = FALSE
) %>% as_tibble()

observed <- consumption_panel %>%
  select(TimeUTC, DK36Title) %>%
  mutate(observed = TRUE)

missing_hours <- full_grid %>%
  left_join(observed, by = c("TimeUTC", "DK36Title")) %>%
  filter(is.na(observed)) %>%
  mutate(
    hour_of_day = lubridate::hour(TimeUTC),
    day_of_week = lubridate::wday(TimeUTC, label = TRUE, abbr = TRUE),
    month       = lubridate::month(TimeUTC, label = TRUE, abbr = TRUE),
    year        = lubridate::year(TimeUTC),
    date        = as.Date(TimeUTC)
  )

cat("=== Total missing sector-hours:", format(nrow(missing_hours), big.mark = ","), "===\n\n")

# ── 1. By hour of day ──────────────────────────────────────────────────────####
cat("--- By hour of day ---\n")
missing_hours %>%
  count(hour_of_day) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  print(n = 24)

# ── 2. By day of week ──────────────────────────────────────────────────────####
cat("\n--- By day of week ---\n")
missing_hours %>%
  count(day_of_week) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  print()

# ── 3. By month ────────────────────────────────────────────────────────────####
cat("\n--- By month ---\n")
missing_hours %>%
  count(year, month) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  arrange(year, month) %>%
  print(n = Inf)

# ── 4. Are missing hours clustered on specific dates? ─────────────────────####
cat("\n--- Top 20 dates with most missing sector-hours ---\n")
missing_hours %>%
  count(date, sort = TRUE) %>%
  mutate(
    n_sectors_affected = n,  # each row = one sector missing that hour
    pct_of_sectors     = round(100 * n / n_distinct(consumption_panel$DK36Title), 1)
  ) %>%
  head(20) %>%
  print()

# ── 5. Are specific sectors always the ones missing? ──────────────────────####
cat("\n--- Missing hours by sector ---\n")
missing_hours %>%
  count(DK36Title, sort = TRUE) %>%
  mutate(pct = round(100 * n / nrow(missing_hours), 1)) %>%
  left_join(distinct(consumption_panel, DK36Title, DK36_en), by = "DK36Title") %>%
  select(DK36_en, n, pct) %>%
  print(n = Inf)

# ── 6. Are missings correlated across sectors (same hour missing for all)? ####
cat("\n--- Are missings system-wide (all sectors missing same hour)? ---\n")
missing_hours %>%
  count(TimeUTC) %>%
  mutate(share_sectors = round(n / n_distinct(consumption_panel$DK36Title), 2)) %>%
  summarise(
    pct_hours_all_sectors_missing  = round(100 * mean(share_sectors == 1), 2),
    pct_hours_half_sectors_missing = round(100 * mean(share_sectors >= 0.5), 2),
    pct_hours_one_sector_missing   = round(100 * mean(share_sectors < 0.1), 2)
  ) %>%
  print()
# ── Missing hours visualisation ───────────────────────────────────────────####
library(ggplot2)
library(patchwork)

theme_thesis <- theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 11),
    plot.subtitle   = element_text(size = 9, color = "grey40"),
    axis.title      = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ── 1. Hour of day ────────────────────────────────────────────────────────####
p1 <- missing_hours %>%
  count(hour_of_day) %>%
  ggplot(aes(x = hour_of_day, y = n)) +
  geom_col(fill = "#2C6E9E", width = 0.8) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "By hour of day",
       subtitle = "Peak missings 08:00–14:00",
       x = "Hour (UTC)", y = "Missing sector-hours") +
  theme_thesis

# ── 2. Day of week ────────────────────────────────────────────────────────####
p2 <- missing_hours %>%
  count(day_of_week) %>%
  mutate(weekend = day_of_week %in% c("Sat", "Sun")) %>%
  ggplot(aes(x = day_of_week, y = n, fill = weekend)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("FALSE" = "#2C6E9E", "TRUE" = "#C0392B"),
                    guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "By day of week",
       subtitle = "Weekends (red) account for 60% of gaps",
       x = NULL, y = "Missing sector-hours") +
  theme_thesis

# ── 3. Over time (year-month heatmap) ─────────────────────────────────────####
p3 <- missing_hours %>%
  mutate(year_num  = as.integer(year),
         month_num = as.integer(month)) %>%
  count(year_num, month_num, month) %>%
  ggplot(aes(x = month, y = factor(year_num), fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(n > 200, scales::comma(n), "")),
            size = 2.8, color = "white") +
  scale_fill_gradient(low = "#D6E8F5", high = "#1A4971",
                      labels = scales::comma, name = "Missing\nhours") +
  labs(title = "Over time",
       subtitle = "Missing sector-hours by year and month",
       x = NULL, y = NULL) +
  theme_thesis +
  theme(legend.position = "right")

# ── 4. System-wide vs idiosyncratic per missing timestamp ─────────────────####
p4 <- missing_hours %>%
  count(TimeUTC) %>%
  mutate(
    share_sectors = n / n_distinct(consumption_panel$DK36Title),
    type = case_when(
      share_sectors == 1   ~ "All sectors (100%)",
      share_sectors >= 0.5 ~ "Majority (50–99%)",
      TRUE                 ~ "Idiosyncratic (<50%)"
    ),
    type = factor(type, levels = c("All sectors (100%)", "Majority (50–99%)", "Idiosyncratic (<50%)"))
  ) %>%
  count(type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ggplot(aes(x = type, y = n, fill = type)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("All sectors (100%)" = "#C0392B",
                               "Majority (50–99%)"  = "#E67E22",
                               "Idiosyncratic (<50%)" = "#2C6E9E"),
                    guide = "none") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "System-wide vs. idiosyncratic",
       subtitle = "Each bar is a mutually exclusive bin of missing timestamps by scope",
       x = NULL, y = "N missing timestamps") +
  theme_thesis

# ── Combine & save ────────────────────────────────────────────────────────####
combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title    = "Missing Data Patterns in the Consumption Panel",
    subtitle = "28,867 missing sector-hours | July 2021 – September 2025",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey40")
    )
  )

ggsave("figures/missing_hours_diagnostics.png",
       combined, width = 12, height = 8, dpi = 300, bg = "white")

combined

total_possible <- n_distinct(consumption_panel$DK36Title) *
  as.integer(difftime(max(consumption_panel$TimeUTC),
                      min(consumption_panel$TimeUTC),
                      units = "hours")) + 1L

total_observed <- nrow(consumption_panel)
total_missing  <- nrow(missing_hours)

missing_pct <- round(100 * total_missing / total_possible, 2)

cat("Total possible sector-hours:", format(total_possible, big.mark = ","), "\n")
cat("Total observed sector-hours:", format(total_observed, big.mark = ","), "\n")
cat("Total missing sector-hours: ", format(total_missing,  big.mark = ","), "\n")
cat("Missing share:              ", missing_pct, "%\n")


# ── Missing wind ──────────────────────────────────────────────────────────────####

p1 <- ggplot(missing_by_month, aes(x = month, y = n)) +
  geom_col(fill = "#2C6E9E", width = 25) +
  labs(
    title = "By month",
    subtitle = "Missing wind observations across the sample period",
    x = NULL,
    y = "Missing observations"
  ) +
  scale_y_continuous(labels = comma) 

p2 <- ggplot(missing_by_hour, aes(x = fe_hour, y = n)) +
  geom_col(fill = "#2C6E9E", width = 0.8) +
  labs(
    title = "By hour of day",
    subtitle = "Intraday pattern of missing wind observations",
    x = "Hour",
    y = "Missing observations"
  ) +
  scale_y_continuous(labels = comma) 

p3 <- ggplot(missing_by_day, aes(x = factor(date), y = n)) +
  geom_col(fill = "#2C6E9E", width = 0.8) +
  labs(
    title = "By day",
    subtitle = "Daily count of missing wind observations",
    x = NULL,
    y = "Missing observations"
  ) +
  scale_y_continuous(labels = comma) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

(p1 / p2 / p3) +
  plot_annotation(
    title = "Pattern of missing wind data",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13)
    )
  )




