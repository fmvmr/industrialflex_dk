# IV INSTRUMENT 3.0 VERSION: 
setwd("~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")
fixest::setFixest_notes(FALSE)
# =================================================================================
# = PART ONE: Data preparation    =================================================
# =================================================================================
# ── Packages ──────────────────────────────────────────────────────────────────####

invisible(lapply(
  c("arrow", "dplyr", "lubridate", "stringr", "purrr", "glue", "readxl",
    "tidyr", "fixest", "readr", "ggplot2", "rlang", "tibble", "patchwork","progress",
    "plm", "AER", "ivreg", "lmtest", "sandwich", "stargazer"),
  library, character.only = TRUE
))



# ── Load data ─────────────────────────────────────────────────────────────────####
load_data <- function(years, months = 1:12) {
  
  safe_read <- function(path) if (file.exists(path)) arrow::read_parquet(path)
  bind_nn   <- function(lst)  dplyr::bind_rows(Filter(Negate(is.null), lst))
  yms       <- unlist(lapply(years, function(y) sprintf("%d_%02d", y, months)))
  
  monthly_types <- list(
    prices      = "elspotprices_monthly/elspot",
    forecast    = "forecast_hourly_monthly_compact/forecast_compact",
    temp        = "temp_zone_hourly_monthly/temp_zone",
    industry    = "consumption_industry_hourly_monthly/consumption_industry_hour",
    consumption = "consumption_category_hourly_monthly/consumption_cat_hour"
  )
  
  c(
    lapply(monthly_types, function(p)
      bind_nn(lapply(yms, function(ym) safe_read(glue("data/{p}_{ym}.parquet"))))
    ),
    list(
      industry_annual = bind_nn(lapply(years, function(y)
        safe_read(glue("data/consumption_dk10_region_year/consumption_dk10_region_{y}.parquet"))
      )),
      gas    = arrow::read_parquet("data/controls/gas_daily_2020_2025.parquet"),
      carbon = arrow::read_parquet("data/controls/carbon_daily_2020_2025.parquet"),
      coal   = arrow::read_parquet("data/controls/coal_daily_2020_2025.parquet")
    )
  )
}
# ── Pull into environment ─────────────────────────────────────────────────────####
d <- load_data(2021:2025)
list2env(d[c("prices","forecast","temp","industry","industry_annual","gas","carbon","coal","consumption")], .GlobalEnv)



# ── DK19 → DK10 mapping ───────────────────────────────────────────────────────#####
dk19_to_dk10 <- tibble::tribble(
  ~DK19Title,                                               ~DK10Title,
  "Landbrug, skovbrug og fiskeri",                          "Landbrug, skovbrug og fiskeri",
  "Råstofindvinding & Vandforsyning og renovation",         "Industri, råstofindvinding og forsyningsvirksomhed",
  "Industri",                                               "Industri, råstofindvinding og forsyningsvirksomhed",
  "Energiforsyning",                                        "Industri, råstofindvinding og forsyningsvirksomhed",
  "Bygge og anlæg",                                         "Bygge og anlæg",
  "Handel",                                                 "Handel og transport",
  "Transport",                                              "Handel og transport",
  "Hoteller og restauranter",                               "Handel og transport",
  "Information og kommunikation",                           "Information og kommunikation",
  "Finansiering og forsikring",                             "Finansiering og forsikring",
  "Ejendomshandel og udlejning",                            "Ejendomshandel og udlejning",
  "Videnservice",                                           "Erhvervsservice",
  "Rejsebureauer, rengøring og anden operationel service",  "Erhvervsservice",
  "Offentlig administration, forsvar og politi",            "Offentlig administration, undervisning og sundhed",
  "Undervisning",                                           "Offentlig administration, undervisning og sundhed",
  "Sundhed og socialvæsen",                                 "Offentlig administration, undervisning og sundhed",
  "Kultur og fritid",                                       "Offentlig administration, undervisning og sundhed",
  "Andre serviceydelser mv",                                "Offentlig administration, undervisning og sundhed",
  "Privat",                                                 NA_character_,
  "Uoplyst aktivitet",                                      NA_character_
)

DK2_REGIONS <- c("Region Hovedstaden", "Region Sjælland")
DK1_REGIONS <- c("Region Syddanmark", "Region Midtjylland", "Region Nordjylland")

# ── Annual consumption zone weights ───────────────────────────────────────────#####
annual_zone_share <- industry_annual %>%
  filter(DK10Title != "Privat") %>%
  mutate(
    Year = as.integer(format(Year, "%Y")),
    Zone = case_when(
      RegionName %in% DK2_REGIONS ~ "DK2",
      RegionName %in% DK1_REGIONS ~ "DK1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Zone)) %>%
  group_by(Year, DK10Title, Zone) %>%
  summarise(Cons = sum(ConsumptionkWh, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Zone, values_from = Cons, values_fill = 0) %>%
  mutate(w_DK1_c = if_else(DK1 + DK2 > 0, DK1 / (DK1 + DK2), NA_real_)) %>%
  select(Year, DK10Title, w_DK1_c)

industry <- industry %>%
  left_join(dk19_to_dk10, by = "DK19Title") %>%
  mutate(Year = as.integer(format(TimeDK, "%Y"))) %>%
  left_join(annual_zone_share, by = c("Year", "DK10Title"))

# ── DK36 mapping ──────────────────────────────────────────────────────────────####
### Some industries are aggregated due to anonomization requirements
dk36_mapping <- tibble(DK36_group = unique(industry$DK36Code)) %>%
  filter(DK36_group != "-") %>%
  mutate(IndustryCode = str_split(DK36_group, "_")) %>%
  unnest(IndustryCode)

# ── Employee weights ──────────────────────────────────────────────────────────#####
total_employees <- bind_rows(lapply(
  c("employees2021.csv", "Employees2022.csv", "employees2023.csv", "employees2024.csv"),
  read_delim, delim = ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE
)) %>%
  select(-X1) %>%
  rename(year = X2, status = X3, age = X4, industry = X5,
         region_hovedstaden = X6, region_sjaelland = X7,
         region_syddanmark = X8, region_midtjylland = X9, region_nordjylland = X10) %>%
  group_by(year, industry) %>%
  summarise(across(starts_with("region_"), sum, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    DK1          = region_syddanmark + region_midtjylland + region_nordjylland,
    DK2          = region_hovedstaden + region_sjaelland,
    IndustryCode = str_extract(industry, "^[A-Z]+")
  ) %>%
  left_join(dk36_mapping, by = "IndustryCode") %>%
  group_by(year, DK36_group) %>%
  summarise(DK1 = sum(DK1, na.rm = TRUE), DK2 = sum(DK2, na.rm = TRUE), .groups = "drop") %>%
  mutate(w_DK1_emp = DK1 / (DK1 + DK2)) %>%
  {bind_rows(., filter(., year == 2024) %>% mutate(year = 2025))}

# ── Firm-count weights ────────────────────────────────────────────────────────####
firms_long <- read_excel("DK36 Regional Split.xlsx") %>%
  pivot_longer(cols = c(`2021`, `2022`, `2023`), names_to = "year", values_to = "n_firms") %>%
  mutate(
    year = as.integer(year),
    zone = case_when(
      Region %in% DK2_REGIONS ~ "DK2",
      Region %in% DK1_REGIONS ~ "DK1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(zone)) %>%
  group_by(DK36_Code, year, zone) %>%
  summarise(firms = sum(n_firms, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = zone, values_from = firms, values_fill = 0) %>%
  left_join(dk36_mapping, by = c("DK36_Code" = "IndustryCode")) %>%
  filter(!is.na(DK36_group)) %>%
  group_by(year, DK36_group) %>%
  summarise(DK1 = sum(DK1, na.rm = TRUE), DK2 = sum(DK2, na.rm = TRUE), .groups = "drop") %>%
  mutate(w_DK1_firm = DK1 / (DK1 + DK2)) %>%
  {bind_rows(.,
             filter(., year == 2023) %>% mutate(year = 2024),
             filter(., year == 2023) %>% mutate(year = 2025)
  )} %>%
  select(w_DK1_firm, year, DK36_group)

# ── Attach all weights to industry ────────────────────────────────────────────####
industry <- industry %>%
  mutate(Year = as.integer(format(TimeUTC, "%Y"))) %>%
  left_join(select(total_employees, year, DK36_group, w_DK1_emp),
            by = c("Year" = "year", "DK36Code" = "DK36_group")) %>%
  left_join(firms_long,
            by = c("Year" = "year", "DK36Code" = "DK36_group"))
# ── Weight distribution comparison ────────────────────────────────────────────####
weight_dist <- bind_rows(
  annual_zone_share %>%
    select(weight = w_DK1_c) %>%
    mutate(method = "Consumption (DK10)"),
  total_employees %>%
    filter(!is.na(DK36_group)) %>%
    select(weight = w_DK1_emp) %>%
    mutate(method = "Employment (DK36)"),
  firms_long %>%
    select(weight = w_DK1_firm) %>%
    mutate(method = "Firm count (DK36)")
) %>%
  filter(!is.na(weight)) %>%
  mutate(method = factor(method, levels = c(
    "Consumption (DK10)", "Employment (DK36)", "Firm count (DK36)"
  )))

pal <- c(
  "Consumption (DK10)" = "#2166AC",
  "Employment (DK36)"  = "#B2182B",
  "Firm count (DK36)"  = "#1B7837"
)

ggplot(weight_dist, aes(x = weight, fill = method, colour = method)) +
  geom_density(alpha = 0.15, linewidth = 0.75) +
  geom_vline(
    xintercept = 0.5, linetype = "dashed",
    colour = "grey50", linewidth = 0.45
  ) +
  annotate(
    "text", x = 0.513, y = Inf,
    label = "Equal split", hjust = 0, vjust = 1.6,
    size = 2.9, colour = "grey45", fontface = "italic"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = scales::label_percent(accuracy = 1),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  labs(
    title    = "Distribution of DK1 Weights Across Sectors and Years",
    subtitle = "Three weighting approaches to allocate sectoral activity to western Denmark (DK1)",
    x        = "DK1 share",
    y        = "Density",
    fill     = NULL, colour = NULL,
    caption  = paste(
      "Consumption weights are constructed at the DK10 × year level;",
      "employment and firm-count weights at the DK36 × year level. All years pooled."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13,
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9.5, colour = "grey35",
                                      margin = margin(b = 10)),
    plot.caption       = element_text(size = 7.5, colour = "grey50",
                                      hjust = 0, margin = margin(t = 10)),
    legend.position    = "top",
    legend.text        = element_text(size = 9.5),
    legend.key.size    = unit(0.45, "cm"),
    legend.spacing.x   = unit(0.3, "cm"),
    axis.title.x       = element_text(size = 9.5, margin = margin(t = 6)),
    axis.title.y       = element_text(size = 9.5, margin = margin(r = 6)),
    axis.text          = element_text(size = 9, colour = "grey30"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4),
    plot.margin        = margin(12, 16, 10, 12)
  )

ggsave("weight_distributions.pdf", width = 7, height = 4.5)


# ── Filter invalid sector codes ───────────────────────────────────────────────####
industry <- industry %>%
  filter(!DK36Code %in% c("-", "PR"))

# ── Weighted electricity prices ───────────────────────────────────────────────####
prices_wide <- prices %>%
  arrange(HourDK, PriceArea) %>%
  select(HourUTC, PriceArea, SpotPriceEUR) %>%
  pivot_wider(names_from = PriceArea, values_from = SpotPriceEUR, values_fn = first)

weighted_prices <- industry %>%
  distinct(TimeUTC, DK10Title, DK19Title, DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  left_join(prices_wide, by = c("TimeUTC" = "HourUTC")) %>%
  mutate(
    P_weighted_c    = w_DK1_c    * DK1 + (1 - w_DK1_c)    * DK2,
    P_weighted_emp  = w_DK1_emp  * DK1 + (1 - w_DK1_emp)  * DK2,
    P_weighted_firm = w_DK1_firm * DK1 + (1 - w_DK1_firm) * DK2
  ) %>%
  rename(DK1_P = DK1, DK2_P = DK2)

# ── Weighted price comparison ─────────────────────────────────────────────────####
pal_methods <- c(
  "Consumption" = "#2166AC",
  "Employment"  = "#B2182B",
  "Firm count"  = "#1B7837"
)

th <- theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 11, margin = margin(b = 4)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4),
    axis.text          = element_text(size = 9, colour = "grey30"),
    axis.title         = element_text(size = 9.5)
  )

price_long <- weighted_prices %>%
  filter(!is.na(P_weighted_c), P_weighted_c > 0) %>%
  select(TimeUTC, P_weighted_c, P_weighted_emp, P_weighted_firm) %>%
  pivot_longer(-TimeUTC, names_to = "method", values_to = "price") %>%
  mutate(method = dplyr::case_match(method,
                                    "P_weighted_c"    ~ "Consumption",
                                    "P_weighted_emp"  ~ "Employment",
                                    "P_weighted_firm" ~ "Firm count"
  ))

price_diff <- weighted_prices %>%
  filter(!is.na(P_weighted_c), P_weighted_c > 0) %>%
  mutate(
    Employment  = P_weighted_emp  - P_weighted_c,
    `Firm count` = P_weighted_firm - P_weighted_c
  ) %>%
  select(TimeUTC, Employment, `Firm count`) %>%
  pivot_longer(-TimeUTC, names_to = "method", values_to = "diff") %>%
  filter(!is.na(diff))

p1 <- ggplot(price_long, aes(x = price, fill = method, colour = method)) +
  geom_density(alpha = 0.15, linewidth = 0.75) +
  scale_x_continuous(labels = scales::label_number(suffix = " €"), limits = c(0, NA)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = pal_methods) +
  scale_colour_manual(values = pal_methods) +
  labs(title = "A. Weighted price distributions",
       x = "Price (EUR/MWh)", y = "Density", fill = NULL, colour = NULL) +
  th +
  theme(
    legend.position      = c(0.97, 0.97),
    legend.justification = c(1, 1),
    legend.background    = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(0.4, "cm"),
    legend.text          = element_text(size = 9)
  )

p2 <- ggplot(price_diff, aes(x = diff, fill = method, colour = method)) +
  geom_density(alpha = 0.15, linewidth = 0.75) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.45) +
  annotate("text", x = 0.3, y = Inf, label = "No difference",
           hjust = 0, vjust = 1.6, size = 2.8, colour = "grey45", fontface = "italic") +
  scale_x_continuous(labels = scales::label_number(suffix = " €")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = pal_methods[c("Employment", "Firm count")]) +
  scale_colour_manual(values = pal_methods[c("Employment", "Firm count")]) +
  labs(title = "B. Price deviation from consumption-based weight",
       x = "\u0394 Price (EUR/MWh)", y = "Density", fill = NULL, colour = NULL) +
  th +
  theme(
    legend.position      = c(0.97, 0.97),
    legend.justification = c(1, 1),
    legend.background    = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(0.4, "cm"),
    legend.text          = element_text(size = 9)
  )

p1 + p2 +
  plot_annotation(
    title    = "Sensitivity of Sector-Level Electricity Prices to Weighting Approach",
    subtitle = "Panel B uses consumption-based weight as baseline; all sectors and years pooled",
    caption  = "P = w\u2081 \u00D7 P\u1D30\u1D37\u00B9 + (1 \u2212 w\u2081) \u00D7 P\u1D30\u1D37\u00B2, where w\u2081 is the DK1 share under each approach.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 9.5, colour = "grey35", margin = margin(b = 8)),
      plot.caption  = element_text(size = 7.5, colour = "grey50", hjust = 0, margin = margin(t = 8))
    )
  )

# How similar are the three weights per sector?
weighted_prices %>%
  distinct(DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  mutate(
    diff_emp_c   = abs(w_DK1_emp  - w_DK1_c),
    diff_firm_c  = abs(w_DK1_firm - w_DK1_c)
  ) %>%
  summarise(
    mean_diff_emp_c   = mean(diff_emp_c,  na.rm = TRUE),
    median_diff_emp_c = median(diff_emp_c, na.rm = TRUE),
    max_diff_emp_c    = max(diff_emp_c,   na.rm = TRUE),
    mean_diff_firm_c  = mean(diff_firm_c, na.rm = TRUE),
    median_diff_firm_c= median(diff_firm_c,na.rm = TRUE),
    max_diff_firm_c   = max(diff_firm_c,  na.rm = TRUE)
  )

# Which sectors diverge most between methods?
weighted_prices %>%
  distinct(DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  mutate(diff_emp_c = abs(w_DK1_emp - w_DK1_c)) %>%
  arrange(desc(diff_emp_c)) %>%
  head(20)

# 1. Check if Kemiskindustri drives the result — is it a large share of consumption?
industry %>%
  group_by(DK36Title) %>%
  summarise(total_MWh = sum(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(share = total_MWh / sum(total_MWh)) %>%
  filter(str_detect(DK36Title, "Kemi|Forlag|Telekommunikation")) %>%
  arrange(desc(share))

# 2. For those outlier sectors, how large is the resulting price difference?
weighted_prices %>%
  filter(str_detect(DK36Title, "Kemi|Forlag|Telekommunikation")) %>%
  mutate(
    diff_emp_c  = P_weighted_emp  - P_weighted_c,
    diff_firm_c = P_weighted_firm - P_weighted_c
  ) %>%
  group_by(DK36Title) %>%
  summarise(
    mean_diff_emp  = mean(diff_emp_c,  na.rm = TRUE),
    sd_diff_emp    = sd(diff_emp_c,    na.rm = TRUE),
    max_diff_emp   = max(abs(diff_emp_c), na.rm = TRUE),
    .groups = "drop"
  )
# Does the price difference correlate with the spread?
# i.e. does the method only matter during high-congestion periods?
weighted_prices %>%
  filter(str_detect(DK36Title, "Kemi")) %>%
  mutate(
    spread     = DK1_P - DK2_P,
    diff_emp_c = P_weighted_emp - P_weighted_c
  ) %>%
  summarise(
    cor_spread_diff = cor(spread, diff_emp_c, use = "complete.obs"),
    share_large_diff = mean(abs(diff_emp_c) > 5, na.rm = TRUE)
  )


# ── Price spread diagnostic ───────────────────────────────────────────────────####

# 1. How often do DK1 and DK2 prices differ?
weighted_prices %>%
  filter(!is.na(DK1_P), !is.na(DK2_P)) %>%
  distinct(TimeUTC, DK1_P, DK2_P) %>%
  mutate(spread = DK1_P - DK2_P) %>%
  summarise(
    mean_spread      = mean(spread,            na.rm = TRUE),
    sd_spread        = sd(spread,              na.rm = TRUE),
    share_identical  = mean(spread == 0,       na.rm = TRUE),
    share_within_1eu = mean(abs(spread) < 1,   na.rm = TRUE),
    share_within_5eu = mean(abs(spread) < 5,   na.rm = TRUE),
    max_spread       = max(abs(spread),        na.rm = TRUE)
  )

# 2. How different are the weights between methods?
weighted_prices %>%
  distinct(DK36Title, w_DK1_c, w_DK1_emp, w_DK1_firm) %>%
  mutate(
    diff_emp_c   = abs(w_DK1_emp  - w_DK1_c),
    diff_firm_c  = abs(w_DK1_firm - w_DK1_c)
  ) %>%
  summarise(across(starts_with("diff_"), list(mean = mean, max = max), na.rm = TRUE))

# 3. Correlation between weighted prices
weighted_prices %>%
  filter(!is.na(P_weighted_c), !is.na(P_weighted_emp), !is.na(P_weighted_firm)) %>%
  summarise(
    cor_c_emp  = cor(P_weighted_c, P_weighted_emp,  use = "complete.obs"),
    cor_c_firm = cor(P_weighted_c, P_weighted_firm, use = "complete.obs")
  )

# 4. Plot: spread over time (monthly average)
monthly_spread <- weighted_prices %>%
  distinct(TimeUTC, DK1_P, DK2_P) %>%
  filter(!is.na(DK1_P), !is.na(DK2_P)) %>%
  mutate(
    spread = DK1_P - DK2_P,
    month  = lubridate::floor_date(TimeUTC, "month")
  ) %>%
  group_by(month) %>%
  summarise(
    mean_spread   = mean(spread,      na.rm = TRUE),
    share_nonzero = mean(spread != 0, na.rm = TRUE),
    .groups = "drop"
  )

scale_factor <- max(abs(monthly_spread$mean_spread), na.rm = TRUE)

ggplot(monthly_spread, aes(x = month)) +
  geom_col(aes(y = share_nonzero), fill = "#2166AC", alpha = 0.3,
           width = 25 * 24 * 3600) +          # 25 days in seconds
  geom_line(aes(y = mean_spread / scale_factor),
            colour = "#B2182B", linewidth = 0.7) +
  scale_y_continuous(
    name     = "Share of hours with non-zero spread",
    labels   = scales::label_percent(),
    sec.axis = sec_axis(~ . * scale_factor,
                        name = "Mean spread DK1 \u2212 DK2 (EUR/MWh)")
  ) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(
    title    = "DK1 vs DK2 Price Spread Over Time",
    subtitle = "Bars: share of hours with non-zero spread  |  Line: monthly mean spread (right axis)",
    x        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, colour = "grey35"),
    plot.caption       = element_text(size = 8, colour = "grey50", hjust = 0),
    panel.grid.minor   = element_blank(),
    axis.title.y.right = element_text(colour = "#B2182B"),
    axis.text.y.right  = element_text(colour = "#B2182B")
  )


# ── Wind and temperature instruments ──────────────────────────────────────────####
wind_supply <- forecast %>%
  select(HourUTC, PriceArea, Wind_DayAhead) %>%
  pivot_wider(names_from = PriceArea, values_from = Wind_DayAhead, values_fn = first) %>%
  rename(Wind_DK1 = DK1, Wind_DK2 = DK2)

temp_combined <- temp %>%
  select(HourUTC, PriceArea, TempC) %>%
  pivot_wider(names_from = PriceArea, values_from = TempC, values_fn = first) %>%
  rename(Temp_DK1 = DK1, Temp_DK2 = DK2)

wind_temp <- left_join(temp_combined, wind_supply, by = "HourUTC")

industry <- industry %>%
  left_join(wind_temp, by = c("TimeUTC" = "HourUTC")) %>%
  mutate(
    Wind_c    = w_DK1_c    * Wind_DK1 + (1 - w_DK1_c)    * Wind_DK2,
    Wind_emp  = w_DK1_emp  * Wind_DK1 + (1 - w_DK1_emp)  * Wind_DK2,
    Wind_firm = w_DK1_firm * Wind_DK1 + (1 - w_DK1_firm) * Wind_DK2,
    Temp_c    = w_DK1_c    * Temp_DK1 + (1 - w_DK1_c)    * Temp_DK2,
    Temp_emp  = w_DK1_emp  * Temp_DK1 + (1 - w_DK1_emp)  * Temp_DK2,
    Temp_firm = w_DK1_firm * Temp_DK1 + (1 - w_DK1_firm) * Temp_DK2
  )

# Diagnostic: NA share in wind instrument
industry %>%
  summarise(n = n(), n_na = sum(is.na(Wind_DK1)), share_na = mean(is.na(Wind_DK1)))

# ── Fuel controls (pre-joined) ────────────────────────────────────────────────####
fuel <- gas %>%
  mutate(Date = as.Date(Date)) %>% select(Date, Gas_EUR_MWh) %>%
  left_join(carbon %>% mutate(Date = as.Date(Date)) %>% select(Date, EUA_EUR_ton),  by = "Date") %>%
  left_join(coal   %>% mutate(Date = as.Date(Date)) %>% select(Date, Coal_USD_ton), by = "Date")

# ── Main consumption panel ────────────────────────────────────────────────────####
consumption_panel <- industry %>%
  left_join(
    weighted_prices %>%
      select(TimeUTC, DK36Title, P_weighted_c, P_weighted_emp, P_weighted_firm, DK1_P, DK2_P),
    by = c("TimeUTC", "DK36Title")
  ) %>%
  mutate(
    fe_hour  = factor(lubridate::hour(TimeUTC)),
    fe_month = factor(format(TimeUTC, "%Y-%m")),
    fe_year  = factor(format(TimeUTC, "%Y")),
    fe_week = factor(format(TimeUTC, "%Y-%U"))
  ) %>%
  filter(P_weighted_c > 0, P_weighted_emp > 0, P_weighted_firm > 0, Consumption_MWh > 0) %>%
  mutate(Date = as.Date(TimeUTC)) %>%
  left_join(fuel, by = "Date") %>%
  mutate(
    log_gas         = log(Gas_EUR_MWh),
    log_carbon      = log(EUA_EUR_ton),
    log_coal        = log(Coal_USD_ton),
    log_consumption = log(Consumption_MWh),
    log_P_c         = log(P_weighted_c),
    log_P_emp       = log(P_weighted_emp),
    log_P_firm      = log(P_weighted_firm)
  )

consumption_panel %>%
  dplyr::distinct(DK36Title) %>%
  dplyr::arrange(DK36Title) %>%
  print(n = Inf)

exclude_sectors <- c(
  "Energiforsyning",
  "Råstofindvinding & Vandforsyning og renovation"
)

consumption_panel <- consumption_panel %>%
  dplyr::filter(!DK36Title %in% exclude_sectors)



# ── Panel dimensions ──────────────────────────────────────────────────────────#####
list(
  N_sectors  = n_distinct(consumption_panel$DK36Title),
  T_hours    = n_distinct(consumption_panel$TimeUTC),
  N_obs      = nrow(consumption_panel),
  start      = format(min(consumption_panel$TimeUTC), "%B %Y"),
  end        = format(max(consumption_panel$TimeUTC), "%B %Y"),
  balanced   = n_distinct(consumption_panel$DK36Title) *
    n_distinct(consumption_panel$TimeUTC) == nrow(consumption_panel)
)

# ── Summary statistics ────────────────────────────────────────────────────────####
consumption_panel %>%
  select(
    Consumption_MWh, P_weighted_c, Wind_c, Temp_c,
    Gas_EUR_MWh, EUA_EUR_ton, Coal_USD_ton
  ) %>%
  as.data.frame() %>%
  stargazer(
    type  = "text",
    title = "Descriptive Statistics — Consumption Panel",
    covariate.labels = c(
      "Consumption (MWh)",
      "Electricity price, cons. weight (EUR/MWh)",
      "Wind forecast, cons. weight (MWh)",
      "Temperature (\\textdegree C)",
      "Gas price (EUR/MWh)",
      "Carbon price (EUR/ton)",
      "Coal price (USD/ton)"
    ),
    summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
    digits       = 2
  )

# ── Sector breakdown ──────────────────────────────────────────────────────────####
consumption_panel %>%
  group_by(DK36Title) %>%
  summarise(
    N_obs     = n(),
    Total_MWh = sum(Consumption_MWh, na.rm = TRUE),
    Mean_MWh  = mean(Consumption_MWh, na.rm = TRUE),
    SD_MWh    = sd(Consumption_MWh,   na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(Share_pct = round(100 * Total_MWh / sum(Total_MWh), 2)) %>%
  arrange(desc(Total_MWh)) %>%
  print(n = Inf)

# ── Panel coverage heatmap (sector × year-month) ──────────────────────────────####
consumption_panel %>%
  mutate(month = lubridate::floor_date(TimeUTC, "month")) %>%
  group_by(DK36Title, month) %>%
  summarise(n_obs = n(), .groups = "drop") %>%
  ggplot(aes(x = month, y = reorder(DK36Title, n_obs), fill = n_obs)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#deebf7", high = "#2166AC",
                      labels = scales::label_comma()) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(
    title    = "Panel Coverage by Sector and Month",
    subtitle = "Cell colour = number of hourly observations",
    x = NULL, y = NULL, fill = "Hours"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "grey35"),
    axis.text.y     = element_text(size = 7),
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 8),
    panel.grid      = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm")
  )


# =================================================================================
# = PART Two: IV INSTRUMENT.      =================================================
# =================================================================================
# ── IV-model specifications  ──────────────────────────────────────────────────####
IV_model_het_DK10 <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

IV_model_het_DK10_W <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36Title
)


IV_model_het_Emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_emp ~ Wind_emp,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

IV_model_het_Emp_w <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_emp ~ Wind_emp,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36Title
)


IV_model_het_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_firm ~ Wind_firm,
  data = consumption_panel,
  cluster = ~ fe_month,
  split = ~ DK36Title
)

IV_model_het_firm_w <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_firm ~ Wind_firm,
  data = consumption_panel,
  cluster = ~ fe_week,
  split = ~ DK36Title
)


# ── EXTRACT RESULTS ───────────────────────────────────────────────────────────####
extract_iv <- function(model, weight, cluster) {
  sector_names <- names(model)
  
  purrr::map_dfr(seq_along(sector_names), function(i) {
    m      <- model[[i]]           # ← integer index, not character
    s      <- sector_names[i]      # ← name fetched separately
    ct     <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    if (length(iv_row) == 0) return(NULL)
    
    tibble::tibble(
      sector   = s,
      weight   = weight,
      cluster  = cluster,
      estimate = as.numeric(ct[iv_row, "Estimate"]),
      se       = as.numeric(ct[iv_row, "Std. Error"]),
      p_value  = as.numeric(ct[iv_row, "Pr(>|t|)"]),
      fs_f     = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                          error = function(e) NA_real_)
    )
  })
}

results <- bind_rows(
  extract_iv(IV_model_het_DK10,   "Consumption", "Month"),
  extract_iv(IV_model_het_DK10_W, "Consumption", "Week"),
  extract_iv(IV_model_het_Emp,    "Employment",  "Month"),
  extract_iv(IV_model_het_Emp_w,  "Employment",  "Week"),
  extract_iv(IV_model_het_firm,   "Firm count",  "Month"),
  extract_iv(IV_model_het_firm_w, "Firm count",  "Week")
) %>%
  mutate(
    sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    ci_lo   = estimate - 1.96 * se,
    ci_hi   = estimate + 1.96 * se,
    weight  = factor(weight,  levels = c("Consumption", "Employment", "Firm count")),
    cluster = factor(cluster, levels = c("Month", "Week"))
  )

results <- results %>%
  mutate(sector = stringr::str_remove(sector, 
                                      "^sample\\.var: DK36Title; sample: "))


# apply sector ordering after rename
sector_order <- results %>%
  filter(weight == "Consumption", cluster == "Month") %>%
  arrange(estimate) %>%
  pull(sector)

results <- results %>%
  mutate(sector = factor(sector, levels = sector_order))

# Quick first-stage diagnostic
results %>%
  filter(cluster == "Month") %>%
  group_by(weight) %>%
  summarise(
    n_weak     = sum(fs_f < 10,  na.rm = TRUE),
    n_moderate = sum(fs_f >= 10 & fs_f < 20, na.rm = TRUE),
    n_strong   = sum(fs_f >= 20, na.rm = TRUE)
  )
# ── Shared theme ──────────────────────────────────────────────────────────────####

th_coef <- theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0,
                                      margin = margin(t = 8)),
    axis.text.y        = element_text(size = 7.5),
    axis.text.x        = element_text(size = 9),
    axis.title.x       = element_text(size = 9.5),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9)
  )

th_coef
# ── MAIN COEFFICIENT PLOT  (consumption weight, month cluster) ────────────────####
p_main <- results %>%
  filter(weight == "Consumption", cluster == "Month") %>%
  ggplot(aes(x = estimate, y = sector, colour = p_value < 0.05)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25, linewidth = 0.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c("TRUE" = "#2166AC", "FALSE" = "grey65"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
    name   = NULL
  ) +
  labs(
    title    = "Price Elasticity of Electricity Demand by Sector",
    subtitle = "IV estimates \u2014 consumption-based weights, SE clustered by month",
    x        = "Price elasticity",
    y        = NULL,
    caption  = "95% confidence intervals. Instrument: sector-weighted wind power forecast."
  ) +
  th_coef

p_main
# ── ROBUSTNESS PLOT  (all 3 weights × 2 clusters) ─────────────────────────────####
p_robustness <- results %>%
  ggplot(aes(x = estimate, y = sector,
             colour = cluster, shape = cluster)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_errorbarh(
    aes(xmin = ci_lo, xmax = ci_hi),
    height   = 0, linewidth = 0.4, alpha = 0.5,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(size = 1.5, position = position_dodge(width = 0.6)) +
  scale_colour_manual(
    values = c("Month" = "#2166AC", "Week" = "#B2182B"),
    name   = "SE cluster"
  ) +
  scale_shape_manual(
    values = c("Month" = 16, "Week" = 17),
    name   = "SE cluster"
  ) +
  facet_wrap(~ weight, ncol = 3) +
  labs(
    title    = "Robustness: Price Elasticity Across Weighting Methods",
    subtitle = "Each panel shows a different weighting approach; clustering varies within panel",
    x        = "Price elasticity",
    y        = NULL,
    caption  = "95% confidence intervals. Sector order fixed by consumption-weight estimate."
  ) +
  th_coef +
  theme(
    axis.text.y = element_text(size = 6.5),
    strip.text  = element_text(face = "bold", size = 10)
  )

p_robustness
# ── FIRST-STAGE F-STAT HEATMAP ────────────────────────────────────────────────####
p_fstat <- results %>%
  filter(cluster == "Week") %>%
  ggplot(aes(x = weight, y = sector, fill = fs_f)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  geom_text(aes(label = round(fs_f, 0)), size = 2.4, colour = "grey10") +
  scale_fill_gradient2(
    low      = "#d7191c",
    mid      = "#ffffbf",
    high     = "#1a9641",
    midpoint = 10,
    limits   = c(0, NA),
    name     = "First-stage F",
    na.value = "grey85"
  ) +
  labs(
    title    = "First-Stage F-Statistics by Sector and Weighting Method",
    subtitle = "Staiger-Stock weak instrument threshold: F = 10",
    x        = NULL,
    y        = NULL,
    caption  = "SE clustered by week. Red = weak instrument (F < 10), green = strong."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, colour = "grey35"),
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0),
    axis.text.y      = element_text(size = 7.5),
    axis.text.x      = element_text(size = 9, face = "bold"),
    panel.grid       = element_blank(),
    legend.position  = "right",
    legend.key.height = unit(1.5, "cm")
  )

p_fstat 
# ── Estimate TABLES ───────────────────────────────────────────────────────────####
# ── Elasticity Estimate ───────────────────────────────────────────────────────
est_wide <- results %>%
  mutate(cell = sprintf("%.3f%s (%.3f)", estimate, sig, se),
         col  = paste0(weight, " / ", cluster)) %>%
  select(sector, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell)

cat("\nIV Estimates of Price Elasticity by Sector\n")
cat("Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.10\n\n")
print(knitr::kable(est_wide, format = "simple",
                   col.names = c("Sector",
                                 "Cons/Month", "Cons/Week",
                                 "Emp/Month",  "Emp/Week",
                                 "Firm/Month", "Firm/Week")))

# ── Consumption shares ────────────────────────────────────────────────────────####
sector_shares <- consumption_panel %>%
  group_by(DK36Title) %>%
  summarise(total_MWh = sum(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(share_pct = 100 * total_MWh / sum(total_MWh))

# ── Significant sectors (main spec) ───────────────────────────────────────────
sig_data <- results %>%
  filter(weight == "Consumption", cluster == "Week", p_value < 0.05) %>%
  left_join(sector_shares, by = c("sector" = "DK36Title")) %>%
  arrange(desc(share_pct))

cat(sprintf("Significant sectors: %d of %d (%.1f%% of total consumption)\n",
            nrow(sig_data),
            n_distinct(results$sector),
            sum(sig_data$share_pct, na.rm = TRUE)))

# ── Plot ──────────────────────────────────────────────────────────────────────
coverage <- round(sum(sig_data$share_pct, na.rm = TRUE), 1)

ggplot(sig_data,
       aes(x = reorder(sector, share_pct), y = share_pct, fill = estimate)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = sprintf("%.3f%s", estimate, sig)),
    hjust   = -0.1,
    size    = 3,
    colour  = "grey20"
  ) +
  scale_fill_gradient2(
    low      = "#B2182B",
    mid      = "#f7f7f7",
    high     = "#2166AC",
    midpoint = 0,
    name     = "Elasticity"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.18))
  ) +
  coord_flip() +
  labs(
    title    = "Consumption Share of Sectors with Significant Price Elasticity",
    subtitle = sprintf(
      "%d sectors | %.1f%% of total consumption covered | p < 0.05, IV consumption weights, SE by week",
      nrow(sig_data), coverage),
    x       = NULL,
    y       = "Share of total consumption (%)",
    caption = "Bar colour indicates direction and magnitude of price elasticity.\nEstimate and significance stars shown on bars."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0,
                                      margin = margin(t = 8)),
    axis.text.y        = element_text(size = 9),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "right",
    legend.key.height  = unit(1.2, "cm")
  )

# =================================================================================
# = PART Three: Power.      =======================================================
source("sim_power_iv_clustering.R")
source("iv_sim_prep.R")
# =================================================================================
# ── Reload power simulations instead of running again ─────────────────────────####
power_results_c <- readRDS("power_results_c.rds")
power_results_emp <- readRDS("power_results_emp.rds")
power_results_firm <- readRDS("power_results_firm.rds")
# =================================================================================
# ── Power analysis for Consumption ────────────────────────────────────────────####

# ── Step 1: Run models at each granularity ────────────────────────────────────
IV_DK10_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data    = consumption_panel,
  cluster = ~ fe_week,
  split   = ~ DK10Title
)

IV_DK19_agg_c <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data    = consumption_panel,
  cluster = ~ fe_week,
  split   = ~ DK19Title
)

# ── Step 2: Extract LATE + first-stage coefficient ────────────────────────────
extract_for_power <- function(model, granularity) {
  purrr::map_dfr(seq_along(names(model)), function(i) {
    m      <- model[[i]]
    sector <- names(model)[i]
    
    ct     <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    
    fs_ct    <- fixest::coeftable(m$iv_first_stage[[1]])
    inst_row <- rownames(fs_ct)[stringr::str_detect(rownames(fs_ct), "Wind")]
    
    tibble::tibble(
      sector      = stringr::str_remove(sector, "^sample\\.var:.*sample: "),
      granularity = granularity,
      true_effect = as.numeric(ct[iv_row, "Estimate"]),
      p_value     = as.numeric(ct[iv_row, "Pr(>|t|)"]),   # ← add this
      iv_effect   = as.numeric(fs_ct[inst_row, "Estimate"]),
      fs_f        = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                             error = function(e) NA_real_)
    )
  })
}


power_inputs_c <- bind_rows(
  extract_for_power(IV_model_het_DK10_W, "DK36"),  
  extract_for_power(IV_DK19_agg_c,       "DK19"),
  extract_for_power(IV_DK10_agg_c,       "DK10")
)

# ── Step 3: Build level-specific effect grids ─────────────────────────────────

power_inputs_c <- power_inputs_c %>%
  filter(
    p_value <= 0.05   # only significant estimates
  )



# One row per sector per granularity → becomes the effect_grid for that level
power_inputs_c %>%
  group_by(granularity) %>%
  summarise(
    mean_late    = mean(true_effect, na.rm = TRUE),
    mean_iv_coef = mean(iv_effect,   na.rm = TRUE),
    mean_fsf     = mean(fs_f,        na.rm = TRUE)
  )


# ── Step 4: Run simulation using level specific inputs ────────────────────────
# ── Granularity → sector column lookup ──────────────────────────────────
gran_to_var <- c(DK36 = "DK36Title", DK19 = "DK19Title", DK10 = "DK10Title")

# ── Worker function ──────────────────────────────────────────────────────
run_power_by_gran <- function(power_inputs, df_panel,
                              endog_var      = "log_P_c",
                              instrument_var = "Wind_c",
                              temp           = "Temp_c",
                              n_sims = 500, seed = 123) {
  purrr::pmap_dfr(
    dplyr::select(power_inputs, sector, granularity, true_effect, iv_effect),
    function(sector, granularity, true_effect, iv_effect) {

      sv  <- gran_to_var[[granularity]]
      dat <- dplyr::filter(df_panel, .data[[sv]] == sector)

      if (nrow(dat) < 100) return(NULL)

      power_sim_iv(
        df             = dat,
        iv_effect      = iv_effect,
        true_effect    = true_effect,
        endog_var      = endog_var,
        instrument_var = instrument_var,
        outcome_var    = "log_consumption",
        sector_var     = sv,
        fe_vars        = c("fe_hour", "fe_month", "fe_year"),
        controls       = c(temp, "log_gas", "log_coal", "log_carbon"),
        cluster_var    = "fe_week",
        n_sims         = n_sims,
        seed           = seed
      ) %>%
        dplyr::mutate(granularity = granularity)
    },
    .progress = "Power simulation"
  )
}


# ── Simulate all sectors ──────────────────────────────────────────────────

# power_results_c <- run_power_by_gran(
#   power_inputs   = power_inputs_c,
#   df_panel       = consumption_panel,
#   n_sims         = 1000,
#   seed           = 123
# )
# 
# saveRDS(power_results_c,    "power_results_c.rds")

# ── Power visuals: Statistical power ──────────────────────────────────────────####
power_analysis_c <- analyze_iv_power_simulation_sector(power_results_c)


pal <- c(DK10 = "#2166ac", DK19 = "#f4a582", DK36 = "#ca0020")

plot_data <- power_analysis_c %>%
  dplyr::mutate(granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")))

# ── Plot A: Power vs LATE ───────────────────────────────────────────────
p_scatter_c <- plot_data %>%
  ggplot(aes(x = true_effect, y = power,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0, y = 0.83,
           label = "80% threshold", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(x      = "LATE ( price elasticity)",
       y      = "Statistical power (\u03b1 = 0.05)",
       colour = NULL, shape = NULL, size = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

# ── Plot B: Distribution by granularity ──────────────────────────────────
p_box_c <- plot_data %>%
  ggplot(aes(x = granularity, y = power, fill = granularity)) +
  geom_hline(yintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_boxplot(alpha = 0.65, outlier.shape = 21,
               outlier.size = 2, width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# ── Combine ───────────────────────────────────────────────────────────────
power_plot_c <- (p_scatter_c | p_box_c) +
  plot_layout(widths = c(2.5, 1)) +
  plot_annotation(
    title    = "Statistical power by sector and aggregation level",
    subtitle = "500 simulations per sector; calibrated to observed LATE and first-stage coefficient",
    theme    = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9,  colour = "grey40")
    )
  )

power_plot_c


# ── Power visuals: TYPE S-ERRORS ──────────────────────────────────────────────####
type_s_data <- power_analysis_c %>%
  dplyr::mutate(granularity = factor(granularity, levels = c("DK10", "DK19", "DK36"))) %>%
  dplyr::filter(!is.na(wrong_sign))   # drop sectors with zero rejections

p_type_s_c <- type_s_data %>%
  ggplot(aes(x = power, y = wrong_sign,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_vline(xintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dotted",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 0.52,
           label = "Random sign (0.5)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 0.98,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type S error rate (wrong sign \u2223 reject H\u2080)",
    colour   = NULL, shape = NULL, size = NULL,
    title    = "Type S errors by sector and aggregation level",
    subtitle = "Conditional on statistical significance; sectors with zero rejections excluded"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

p_type_s_c
# ── Power visuals: TYPE M-ERRORS ──────────────────────────────────────────────####
type_m_data <- power_analysis_c %>%
  dplyr::mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    type_m      = abs(est_ratio)
  ) %>%
  dplyr::filter(!is.na(est_ratio))

n_clipped <- sum(type_m_data$type_m > 5, na.rm = TRUE)

p_type_m_c <- type_m_data %>%
  ggplot(aes(x = power, y = type_m,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 1.08,
           label = "No exaggeration (1\u00d7)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 4.8,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  coord_cartesian(ylim = c(0, 5)) +   # clip without dropping — extreme outliers noted below
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = 1:5,
                     labels = paste0(1:5, "\u00d7")) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type M error (exaggeration ratio \u2223 reject H\u2080)",
    colour   = NULL, shape = NULL, size = NULL,
    title    = "Type M errors by sector and aggregation level",
    subtitle = glue::glue("Conditional on statistical significance; {n_clipped} sector(s) with |ratio| > 5\u00d7 not displayed")
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

p_type_m_c


# ── Identify robust estimates ─────────────────────────────────────────────────####
robust_estimates_c <- power_analysis_c %>%
  dplyr::group_by(sector) %>%
  dplyr::filter(
    all(power      >= 0.8),
    all(wrong_sign == 0, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(sector, granularity)

robust_estimates_c



# =================================================================================
# ── Power analysis for Employees ──────────────────────────────────────────────####
# ── Step 1: Run models at each granularity ────────────────────────────────────
IV_DK10_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_emp ~ Wind_emp,
  data    = consumption_panel,
  cluster = ~ fe_week,
  split   = ~ DK10Title
)

IV_DK19_agg_emp <- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_emp ~ Wind_emp,
  data    = consumption_panel,
  cluster = ~ fe_week,
  split   = ~ DK19Title
)

# ── Step 2: Extract LATE + first-stage coefficient ────────────────────────────
extract_for_power <- function(model, granularity) {
  purrr::map_dfr(seq_along(names(model)), function(i) {
    m      <- model[[i]]
    sector <- names(model)[i]
    
    ct     <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    
    fs_ct    <- fixest::coeftable(m$iv_first_stage[[1]])
    inst_row <- rownames(fs_ct)[stringr::str_detect(rownames(fs_ct), "Wind")]
    
    tibble::tibble(
      sector      = stringr::str_remove(sector, "^sample\\.var:.*sample: "),
      granularity = granularity,
      true_effect = as.numeric(ct[iv_row, "Estimate"]),
      p_value     = as.numeric(ct[iv_row, "Pr(>|t|)"]),   # ← add this
      iv_effect   = as.numeric(fs_ct[inst_row, "Estimate"]),
      fs_f        = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                             error = function(e) NA_real_)
    )
  })
}


power_inputs_emp <- bind_rows(
  extract_for_power(IV_model_het_Emp_w, "DK36"),  
  extract_for_power(IV_DK19_agg_emp,       "DK19"),
  extract_for_power(IV_DK10_agg_emp,       "DK10")
)

power_inputs_emp <- power_inputs_emp %>%
  filter(
    p_value <= 0.05   # only significant estimates
  )



# ── Step 3: Build level-specific effect grids ─────────────────────────────────

# One row per sector per granularity → becomes the effect_grid for that level
power_inputs_emp %>%
  group_by(granularity) %>%
  summarise(
    mean_late    = mean(true_effect, na.rm = TRUE),
    mean_iv_coef = mean(iv_effect,   na.rm = TRUE),
    mean_fsf     = mean(fs_f,        na.rm = TRUE)
  )


# ── Step 4: Run simulation using level specific inputs ────────────────────────
# ── Granularity → sector column lookup ──────────────────────────────────
gran_to_var <- c(DK36 = "DK36Title", DK19 = "DK19Title", DK10 = "DK10Title")

# ── Worker function ──────────────────────────────────────────────────────
run_power_by_gran <- function(power_inputs, df_panel,
                              endog_var      = "log_P_emp",
                              instrument_var = "Wind_emp",
                              temp           = "Temp_emp",
                              n_sims = 500, seed = 123) {
  purrr::pmap_dfr(
    dplyr::select(power_inputs, sector, granularity, true_effect, iv_effect),
    function(sector, granularity, true_effect, iv_effect) {
      
      sv  <- gran_to_var[[granularity]]
      dat <- dplyr::filter(df_panel, .data[[sv]] == sector)
      
      if (nrow(dat) < 100) return(NULL)
      
      power_sim_iv(
        df             = dat,
        iv_effect      = iv_effect,
        true_effect    = true_effect,
        endog_var      = endog_var,
        instrument_var = instrument_var,
        outcome_var    = "log_consumption",
        sector_var     = sv,
        fe_vars        = c("fe_hour", "fe_month", "fe_year"),
        controls       = c(temp, "log_gas", "log_coal", "log_carbon"),
        cluster_var    = "fe_week",
        n_sims         = n_sims,
        seed           = seed
      ) %>%                                        # ← single ) closes power_sim_iv
        dplyr::mutate(granularity = granularity)
    },
    .progress = "Power simulation"
  )
}

# ── Simulate all sectors ──────────────────────────────────────────────────

# power_results_emp <- run_power_by_gran(
#   power_inputs   = power_inputs_emp,
#   df_panel       = consumption_panel,
#   n_sims         = 1000,
#   seed           = 123
# )

# saveRDS(power_results_emp,  "power_results_emp.rds")

# ── Power visuals: Statistical power ──────────────────────────────────────────####

power_analysis_emp <- analyze_iv_power_simulation_sector(power_results_emp)

pal <- c(DK10 = "#2166ac", DK19 = "#f4a582", DK36 = "#ca0020")

plot_data <- power_analysis_emp %>%
  dplyr::mutate(granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")))

# ── Plot A: Power vs LATE ───────────────────────────────────────────────
p_scatter_emp <- plot_data %>%
  ggplot(aes(x = true_effect, y = power,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0, y = 0.83,
           label = "80% threshold", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(x      = "LATE ( price elasticity)",
       y      = "Statistical power (\u03b1 = 0.05)",
       colour = NULL, shape = NULL, size = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

# ── Plot B: Distribution by granularity ──────────────────────────────────
p_box_emp <- plot_data %>%
  ggplot(aes(x = granularity, y = power, fill = granularity)) +
  geom_hline(yintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_boxplot(alpha = 0.65, outlier.shape = 21,
               outlier.size = 2, width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# ── Combine ───────────────────────────────────────────────────────────────
power_plot_emp <- (p_scatter_emp | p_box_emp) +
  plot_layout(widths = c(2.5, 1)) +
  plot_annotation(
    title    = "Statistical power by sector and aggregation level",
    subtitle = "500 simulations per sector; calibrated to observed LATE and first-stage coefficient",
    theme    = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9,  colour = "grey40")
    )
  )

power_plot_emp





# ── Power visuals: TYPE S-ERRORS ──────────────────────────────────────────────####
type_s_data <- power_analysis_emp %>%
  dplyr::mutate(granularity = factor(granularity, levels = c("DK10", "DK19", "DK36"))) %>%
  dplyr::filter(!is.na(wrong_sign))   # drop sectors with zero rejections

p_type_s_emp <- type_s_data %>%
  ggplot(aes(x = power, y = wrong_sign,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_vline(xintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dotted",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 0.52,
           label = "Random sign (0.5)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 0.98,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type S error rate (wrong sign \u2223 reject H\u2080)",
    colour   = NULL, shape = NULL, size = NULL,
    title    = "Type S errors by sector and aggregation level",
    subtitle = "Conditional on statistical significance; sectors with zero rejections excluded"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

p_type_s_emp
# ── Power visuals: TYPE M-ERRORS ──────────────────────────────────────────────####
type_m_data <- power_analysis_emp %>%
  dplyr::mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    type_m      = abs(est_ratio)
  ) %>%
  dplyr::filter(!is.na(est_ratio))

n_clipped <- sum(type_m_data$type_m > 5, na.rm = TRUE)

p_type_m_emp <- type_m_data %>%
  ggplot(aes(x = power, y = type_m,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 1.08,
           label = "No exaggeration (1\u00d7)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 4.8,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  coord_cartesian(ylim = c(0, 5)) +   # clip without dropping — extreme outliers noted below
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = 1:5,
                     labels = paste0(1:5, "\u00d7")) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type M error (exaggeration ratio \u2223 reject H\u2080)",
    colour   = NULL, shape = NULL, size = NULL,
    title    = "Type M errors by sector and aggregation level",
    subtitle = glue::glue("Conditional on statistical significance; {n_clipped} sector(s) with |ratio| > 5\u00d7 not displayed")
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

p_type_m_emp


# ── Identify robust estimates ─────────────────────────────────────────────────####
robust_estimates_emp <- power_analysis_emp %>%
  dplyr::group_by(sector) %>%
  dplyr::filter(
    all(power      >= 0.8),
    all(wrong_sign == 0, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(sector, granularity)

robust_estimates_emp






# =================================================================================
# ── Power analysis for Firms  ─────────────────────────────────────────────────####

# ── Step 1: Run models at each granularity ────────────────────────────────────
IV_DK10_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_firm ~ Wind_firm,
  data    = consumption_panel,
  cluster = ~ fe_week,
  split   = ~ DK10Title
)

IV_DK19_agg_firm <- feols(
  log_consumption ~ Temp_firm + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_firm ~ Wind_firm,
  data    = consumption_panel,
  cluster = ~ fe_week,
  split   = ~ DK19Title
)

# ── Step 2: Extract LATE + first-stage coefficient ────────────────────────────
extract_for_power <- function(model, granularity) {
  purrr::map_dfr(seq_along(names(model)), function(i) {
    m      <- model[[i]]
    sector <- names(model)[i]
    
    ct     <- fixest::coeftable(m)
    iv_row <- rownames(ct)[stringr::str_detect(rownames(ct), "^fit_")]
    
    fs_ct    <- fixest::coeftable(m$iv_first_stage[[1]])
    inst_row <- rownames(fs_ct)[stringr::str_detect(rownames(fs_ct), "Wind")]
    
    tibble::tibble(
      sector      = stringr::str_remove(sector, "^sample\\.var:.*sample: "),
      granularity = granularity,
      true_effect = as.numeric(ct[iv_row, "Estimate"]),
      p_value     = as.numeric(ct[iv_row, "Pr(>|t|)"]),   # ← add this
      iv_effect   = as.numeric(fs_ct[inst_row, "Estimate"]),
      fs_f        = tryCatch(fixest::fitstat(m, "ivf")[[1]]$stat,
                             error = function(e) NA_real_)
    )
  })
}


power_inputs_firm <- bind_rows(
  extract_for_power(IV_model_het_firm_w, "DK36"),  
  extract_for_power(IV_DK19_agg_firm,       "DK19"),
  extract_for_power(IV_DK10_agg_firm,       "DK10")
)

# ── Step 3: Build level-specific effect grids ─────────────────────────────────
power_inputs_firm <- power_inputs_firm %>%
  filter(
    p_value <= 0.05   # only significant estimates
  )



# One row per sector per granularity → becomes the effect_grid for that level
power_inputs_firm %>%
  group_by(granularity) %>%
  summarise(
    mean_late    = mean(true_effect, na.rm = TRUE),
    mean_iv_coef = mean(iv_effect,   na.rm = TRUE),
    mean_fsf     = mean(fs_f,        na.rm = TRUE)
  )


# ── Step 4: Run simulation using level specific inputs ────────────────────────
# ── Granularity → sector column lookup ──────────────────────────────────
gran_to_var <- c(DK36 = "DK36Title", DK19 = "DK19Title", DK10 = "DK10Title")

# ── Worker function ──────────────────────────────────────────────────────
run_power_by_gran <- function(power_inputs, df_panel,
                              endog_var      = "log_P_firm",
                              instrument_var = "Wind_firm",
                              temp           = "Temp_firm",
                              n_sims = 500, seed = 123) {
  purrr::pmap_dfr(
    dplyr::select(power_inputs, sector, granularity, true_effect, iv_effect),
    function(sector, granularity, true_effect, iv_effect) {
      
      sv  <- gran_to_var[[granularity]]
      dat <- dplyr::filter(df_panel, .data[[sv]] == sector)
      
      if (nrow(dat) < 100) return(NULL)
      
      power_sim_iv(
        df             = dat,
        iv_effect      = iv_effect,
        true_effect    = true_effect,
        endog_var      = endog_var,
        instrument_var = instrument_var,
        outcome_var    = "log_consumption",
        sector_var     = sv,
        fe_vars        = c("fe_hour", "fe_month", "fe_year"),
        controls       = c(temp, "log_gas", "log_coal", "log_carbon"),
        cluster_var    = "fe_week",
        n_sims         = n_sims,
        seed           = seed
      ) %>%                                        # ← single ) closes power_sim_iv
        dplyr::mutate(granularity = granularity)
    },
    .progress = "Power simulation"
  )
}

# ── Simulate all sectors ──────────────────────────────────────────────────

# power_results_firm <- run_power_by_gran(
#   power_inputs   = power_inputs_firm,
#   df_panel       = consumption_panel,
#   n_sims         = 1000,
#   seed           = 123
# )
# 
# saveRDS(power_results_firm, "power_results_firm.rds")

# ── Power visuals: Statistical power ──────────────────────────────────────────####

power_analysis_firm <- analyze_iv_power_simulation_sector(power_results_firm)

pal <- c(DK10 = "#2166ac", DK19 = "#f4a582", DK36 = "#ca0020")

plot_data <- power_analysis_firm %>%
  dplyr::mutate(granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")))

# ── Plot A: Power vs LATE ───────────────────────────────────────────────
p_scatter_f <- plot_data %>%
  ggplot(aes(x = true_effect, y = power,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0, y = 0.83,
           label = "80% threshold", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(x      = "LATE ( price elasticity)",
       y      = "Statistical power (\u03b1 = 0.05)",
       colour = NULL, shape = NULL, size = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

# ── Plot B: Distribution by granularity ──────────────────────────────────
p_box_f <- plot_data %>%
  ggplot(aes(x = granularity, y = power, fill = granularity)) +
  geom_hline(yintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_boxplot(alpha = 0.65, outlier.shape = 21,
               outlier.size = 2, width = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = pal, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# ── Combine ───────────────────────────────────────────────────────────────
power_plot_f <- (p_scatter_f | p_box_f) +
  plot_layout(widths = c(2.5, 1)) +
  plot_annotation(
    title    = "Statistical power by sector and aggregation level",
    subtitle = "500 simulations per sector; calibrated to observed LATE and first-stage coefficient",
    theme    = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9,  colour = "grey40")
    )
  )

power_plot_f


# ── Power visuals: TYPE S-ERRORS ──────────────────────────────────────────────####
type_s_data <- power_analysis_firm %>%
  dplyr::mutate(granularity = factor(granularity, levels = c("DK10", "DK19", "DK36"))) %>%
  dplyr::filter(!is.na(wrong_sign))

p_type_s_f <- type_s_data %>%
  ggplot(aes(x = power, y = wrong_sign,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_vline(xintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dotted",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 0.52,
           label = "Random sign (0.5)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 0.98,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type S error rate (wrong sign \u2223 reject H\u2080)",
    colour   = NULL, shape = NULL, size = NULL,
    title    = "Type S errors by sector and aggregation level",
    subtitle = "Conditional on statistical significance; sectors with zero rejections excluded"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

p_type_s_f
# ── Power visuals: TYPE M-ERRORS ──────────────────────────────────────────────####
type_m_data <- power_analysis_firm %>%
  dplyr::mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    type_m      = abs(est_ratio)
  ) %>%
  dplyr::filter(!is.na(est_ratio))

n_clipped <- sum(type_m_data$type_m > 5, na.rm = TRUE)

p_type_m_f <- type_m_data %>%
  ggplot(aes(x = power, y = type_m,
             colour = granularity, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.8, linetype = "dashed",
             colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 1.08,
           label = "No exaggeration (1\u00d7)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 4.8,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.85) +
  coord_cartesian(ylim = c(0, 5)) +   # clip without dropping — extreme outliers noted below
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = 1:5,
                     labels = paste0(1:5, "\u00d7")) +
  scale_colour_manual(values = pal) +
  scale_shape_manual(values  = c(DK10 = 16, DK19 = 17, DK36 = 15)) +
  scale_size_manual(values   = c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)) +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type M error (exaggeration ratio \u2223 reject H\u2080)",
    colour   = NULL, shape = NULL, size = NULL,
    title    = "Type M errors by sector and aggregation level",
    subtitle = glue::glue("Conditional on statistical significance; {n_clipped} sector(s) with |ratio| > 5\u00d7 not displayed")
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank())

p_type_m_f


# ── Identify robust estimates ─────────────────────────────────────────────────####
robust_estimates_firm <- power_analysis_firm %>%
  dplyr::group_by(sector) %>%
  dplyr::filter(
    all(power      >= 0.8),
    all(wrong_sign == 0, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(sector, granularity)

robust_estimates_firm






# =================================================================================
# ── Compare across models ─────────────────────────────────────────────────────####

# Step 1: Compare LATE, iv_effect and F across schemes for same sector
power_comparison <- bind_rows(
  power_analysis_c    %>% mutate(weight = "Consumption"),
  power_analysis_emp  %>% mutate(weight = "Employment"),
  power_analysis_firm %>% mutate(weight = "Firm count")
)


plot_data_all <- power_comparison %>%
  mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    weight      = factor(weight, levels = c("Consumption", "Employment", "Firm count"))
  ) %>%
  filter(granularity == "DK36")

plot_data_all <- power_comparison %>%
  mutate(
    granularity = factor(granularity, levels = c("DK10", "DK19", "DK36")),
    weight      = factor(weight, levels = c("Consumption", "Employment", "Firm count"))
  )


# Which observations have positive true_effect after stable_sectors filter?
plot_data_all %>%
  filter(true_effect > 0) %>%
  select(sector, granularity, weight, true_effect, power) %>%
  arrange(desc(true_effect))






# Step 2: Which sectors appear in all three? Which drop out?
sector_coverage <- power_comparison %>%
  group_by(sector, granularity) %>%
  summarise(
    n_weights     = n_distinct(weight),
    weights_found = paste(sort(weight), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(n_weights)

print(sector_coverage, n = Inf)

# Step 3: For sectors in all three — how much do estimates diverge?
power_comparison %>%
  group_by(sector, granularity) %>%
  filter(n_distinct(weight) == 3) %>%
  summarise(
    late_c    = true_effect[weight == "Consumption"],
    late_emp  = true_effect[weight == "Employment"],
    late_firm = true_effect[weight == "Firm count"],
    late_range = max(true_effect) - min(true_effect),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(late_range))) %>%
  print(n = 75)

# ── Shared aesthetics ─────────────────────────────────────────────────────────####
pal_weight <- c(
  "Consumption" = "#2166ac",
  "Employment"  = "#b2182b",
  "Firm count"  = "#1b7837"
)
shape_gran <- c(DK10 = 16, DK19 = 17, DK36 = 15)
size_gran  <- c(DK10 = 3.5, DK19 = 2.5, DK36 = 1.8)


# ── Power plot ────────────────────────────────────────────────────────────────####
p_scatter_all <- plot_data_all %>%
  ggplot(aes(x = true_effect, y = power,
             colour = weight, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0, y = 0.83,
           label = "80% threshold", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.75) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal_weight) +
  scale_shape_manual(values = shape_gran) +
  scale_size_manual(values = size_gran, guide = "none") +
  labs(x = "LATE (price elasticity)", y = "Statistical power (\u03b1 = 0.05)",
       colour = "Weight", shape = "Granularity") +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))

p_box_all <- plot_data_all %>%
  ggplot(aes(x = granularity, y = power, fill = weight)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_boxplot(alpha = 0.65, position = position_dodge(width = 0.75),
               outlier.shape = 21, outlier.size = 1.5, width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = pal_weight, guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

power_plot_all <- (p_scatter_all | p_box_all) +
  plot_layout(widths = c(2.5, 1)) +
  plot_annotation(
    title    = "Statistical power by sector, aggregation level and weighting scheme",
    subtitle = "1000 simulations per sector; calibrated to observed LATE and first-stage coefficient",
    theme    = theme(
      plot.title    = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9,  colour = "grey40")
    )
  )
power_plot_all

# ── Type S plot ───────────────────────────────────────────────────────────────####
p_type_s_all <- plot_data_all %>%
  filter(!is.na(wrong_sign)) %>%
  ggplot(aes(x = power, y = wrong_sign,
             colour = weight, shape = granularity, size = granularity)) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_hline(yintercept = 0.5, linetype = "dotted", colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 0.52,
           label = "Random sign (0.5)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 0.98,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.75) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = pal_weight) +
  scale_shape_manual(values = shape_gran) +
  scale_size_manual(values = size_gran, guide = "none") +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type S error rate (wrong sign \u2223 reject H\u2080)",
    colour   = "Weight", shape = "Granularity",
    title    = "Type S errors by sector, aggregation level and weighting scheme",
    subtitle = "Conditional on statistical significance; sectors with zero rejections excluded"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))

p_type_s_all

# ── Type M plot ───────────────────────────────────────────────────────────────####
type_m_all <- plot_data_all %>%
  mutate(type_m = abs(est_ratio)) %>%
  filter(!is.na(est_ratio))

n_clipped_all <- sum(type_m_all$type_m > 5, na.rm = TRUE)

p_type_m_all <- type_m_all %>%
  ggplot(aes(x = power, y = type_m,
             colour = weight, shape = granularity, size = granularity)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  annotate("text", x = 0.01, y = 1.08,
           label = "No exaggeration (1\u00d7)", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = 0.82, y = 4.8,
           label = "80% power", hjust = 0, size = 3, colour = "grey40") +
  geom_point(alpha = 0.75) +
  coord_cartesian(ylim = c(0, 5)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = 1:5, labels = paste0(1:5, "\u00d7")) +
  scale_colour_manual(values = pal_weight) +
  scale_shape_manual(values = shape_gran) +
  scale_size_manual(values = size_gran, guide = "none") +
  labs(
    x        = "Statistical power (rejection rate, \u03b1 = 0.05)",
    y        = "Type M error (exaggeration ratio \u2223 reject H\u2080)",
    colour   = "Weight", shape = "Granularity",
    title    = "Type M errors by sector, aggregation level and weighting scheme",
    subtitle = glue::glue(
      "Conditional on statistical significance; {n_clipped_all} observation(s) with |ratio| > 5\u00d7 not displayed"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        panel.grid.minor = element_blank()) +
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))

p_type_m_all











