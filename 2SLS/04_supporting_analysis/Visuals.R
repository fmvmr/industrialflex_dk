# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
# THESIS SCRIPT VISUALS: 
# ── Shared theme ───────────────────────────────────────────────────────────####
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

# ── Colour / shape schemes ────────────────────────────────────────────────────
pal_gran   <- c(DK10 = "#2166ac", DK19 = "#f4a582", DK36 = "#ca0020")
pal_weight <- c(Consumption = "#2166ac", Employment = "#b2182b", `Firm count` = "#1b7837")
shape_gran <- c(DK10 = 16, DK19 = 17, DK36 = 15)
size_gran  <- c(DK10 = 4.5, DK19 = 3.0, DK36 = 1.8)

shape_weight <- c(Consumption = 16, Employment = 17, `Firm count` = 15)

# ── Common theme ──────────────────────────────────────────────────────────────
th_pow <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    panel.spacing = unit(1.25, "lines")
  )




# ── FIGURE 3: Weight distribution comparison ───────────────────────────────####
industry %>%
  left_join(distinct(consumption_panel, DK36Title, DK36_en), by = "DK36Title") %>%
  group_by(DK36_en) %>%
  summarise(
    Consumption  = mean(w_DK1_c,       na.rm = TRUE),
    Employment   = mean(w_DK1_emp,     na.rm = TRUE),
    `Firm count` = mean(w_DK1_firm,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na() %>% 
  pivot_longer(-DK36_en, names_to = "scheme", values_to = "w_DK1") %>%
  mutate(scheme = factor(scheme, levels = c("Consumption", "Employment", "Firm count", "Geo. mean"))) %>%
  # order sectors by consumption weight descending (same as original plot)
  ggplot(aes(x = scheme, y = DK36_en, fill = w_DK1)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = round(w_DK1, 2)), size = 3, colour = "grey20") +
  scale_fill_gradient2(
    low      = "#d73027",
    mid      = "#ffffbf",
    high     = "#4575b4",
    midpoint = 0.5,
    limits   = c(0, 1),
    name     = "DK1 weight"
  ) +
  labs(
    title    = "DK1 zone weight by sector and weighting scheme",
    subtitle = "> 0.5 = majority in DK1 (West Denmark)",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x  = element_text(face = "bold"),
    axis.text.y  = element_text(size = 9),
    panel.grid   = element_blank(),
    legend.position = "right"
  )
# ── FIGURE 4: LEAFLET map of weather stations ──────────────────────────────####
library(dplyr)
library(leaflet)


stations <- tibble::tibble(
  zone = c(rep("DK1", 10), rep("DK2", 10)),
  stationId = c(
    "06118","06093","06088","06104","06102","06039","06072","06049","06051","06032",
    "06141","06154","06136","06135","06170","06180","06186","06156","06181","06169"
  ),
  name = c(
    "Sønderborg Lufthavn","Vester Vedsted","Nordby","Billund Lufthavn","HORSENS/BYGHOLM",
    "Galten","Ødum","Hald Vest","Vestervig","Stenhøj",
    "Abed","Brandelev","Tystofte","Flakkebjerg","Roskilde Lufthavn",
    "Københavns Lufthavn","Landbohøjskolen","Holbæk","Jægersborg","Gniben"
  ),
  lat = c(
    54.9616,55.2908,55.4483,55.7379,55.8680,56.1618,56.3027,56.5604,56.7637,57.3828,
    54.8275,55.2075,55.2465,55.3224,55.5867,55.6140,55.6814,55.7154,55.7664,56.0067
  ),
  lon = c(
    9.7930,8.6551,8.4003,9.1674,9.7872,9.9033,10.1272,10.0929,8.3207,10.3349,
    11.3292,11.8605,11.3285,11.3879,12.1366,12.6455,12.5403,11.7088,12.5263,11.2805
  )
)

pal <- colorFactor(palette = c("blue", "red"), domain = stations$zone)

leaflet(stations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 6,
    color = ~pal(zone),
    stroke = TRUE, weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0("<b>", name, "</b><br>",
                    "Zone: ", zone, "<br>",
                    "Station ID: ", stationId, "<br>",
                    "Lat: ", lat, " | Lon: ", lon)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~zone,
    title = "Price Zone",
    opacity = 1
  )
leaflet(stations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = 6,
    color = ~pal(zone),
    stroke = TRUE, weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0("<b>", name, "</b><br>",
                    "Zone: ", zone, "<br>",
                    "Station ID: ", stationId, "<br>",
                    "Lat: ", lat, " | Lon: ", lon)
  ) %>%
  addLegend(
    "bottomright",
    pal = pal, values = ~zone,
    title = "Price Zone",
    opacity = 1
  )



# ── TABLE 3: Descriptive statitics ─────────────────────────────────────────####
consumption_panel %>%
  select(
    # Outcome
    Consumption_MWh,
    # Prices — three weighting schemes
    P_weighted_c, P_weighted_emp, P_weighted_firm,
    # Raw spot prices
    DK1_P, DK2_P,
    # Instruments — wind forecasts
    Wind_c, Wind_emp, Wind_firm,
    Wind_DK1, Wind_DK2,
    # Temperature controls
    Temp_c, Temp_emp, Temp_firm,
    Temp_DK1, Temp_DK2,
    # Fuel prices
    Gas_EUR_MWh, EUA_EUR_ton, Coal_USD_ton
  ) %>%
  as.data.frame() %>%
  stargazer(
    type  = "text",
    title = "Descriptive Statistics - Consumption Panel",
    covariate.labels = c(
      # Outcome
      "Consumption (MWh)",
      # Prices
      "Electricity price, cons. weight (EUR/MWh)",
      "Electricity price, emp. weight (EUR/MWh)",
      "Electricity price, firm weight (EUR/MWh)",
      "Spot price DK1 (EUR/MWh)",
      "Spot price DK2 (EUR/MWh)",
      # Instruments
      "Wind forecast, cons. weight (MWh)",
      "Wind forecast, emp. weight (MWh)",
      "Wind forecast, firm weight (MWh)",
      "Wind forecast DK1 (MWh)",
      "Wind forecast DK2 (MWh)",
      # Temperature
      "Temperature, cons. weight (°C)",
      "Temperature, emp. weight (°C)",
      "Temperature, firm weight (°C)",
      "Temperature DK1 (°C)",
      "Temperature DK2 (°C)",
      # Fuel prices
      "Gas price (EUR/MWh)",
      "Carbon price (EUR/ton)",
      "Coal price (USD/ton)"
    ),
    summary.stat = c("n", "mean", "sd", "min", "p25", "median", "p75", "max"),
    digits       = 2
  )

# ── FIGURE 5: Partial F-stats  ─────────────────────────────────────────────####
first_stage_results %>%
  filter(cluster == "Week") %>%
  mutate(weight = factor(weight, levels = c("Consumption", "Employment", "Firm count"))) %>%
  ggplot(aes(x = fs_f_robust, y = weight, fill = weight, color = weight)) +
  geom_boxplot(alpha = 0.15, width = 0.4) +
  geom_vline(xintercept = 23.11, linetype = "dashed", color = "#A32D2D", linewidth = 0.7) +
  annotate("text", x = 23.11, y = 0.55, label = "F = 23.11", color = "#A32D2D",
           hjust = -0.1, vjust = 0, size = 3, fontface = "italic") +
  scale_fill_manual(values = c(
    "Consumption" = "#B5D4F4",
    "Employment"  = "#F4C0D1",
    "Firm count"  = "#C0DD97"
  )) +
  scale_color_manual(values = c(
    "Consumption" = "#185FA5",
    "Employment"  = "#993556",
    "Firm count"  = "#3B6D11"
  )) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title    = "First-stage F-statistics by weighting method",
    subtitle = "Distribution across all sectors, week-clustered standard errors",
    x        = "Cluster-robust partial F-statistic (t²)",
    y        = NULL,
    caption  = "Dashed line = Olea-Pflueger (2013) weak instrument threshold (F = 23.11).\nRobust partial F computed as the square of the week-clustered first-stage t-statistic."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(color = "grey50", size = 10, margin = margin(b = 8)),
    plot.caption     = element_text(color = "grey60", size = 8, margin = margin(t = 8)),
    axis.text.y      = element_text(size = 11),
    axis.text.x      = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )
# ── FIGURE 7: First stage effect box plots   ───────────────────────────────#####
p_monotonicity <- first_stage_results %>%
  mutate(weight = factor(weight, levels = c("Consumption", "Employment", "Firm count"))) %>%
  ggplot(aes(x = fs_estimate, y = weight, fill = weight)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_boxplot(alpha = 0.4, outlier.shape = 21, outlier.size = 1.5, width = 0.5) +
  scale_fill_manual(values = c(
    "Consumption" = "#2166AC",
    "Employment"  = "#B2182B",
    "Firm count"  = "#1B7837"
  )) +
  scale_y_discrete() +
  labs(
    title    = "First-Stage Coefficients: Wind Forecast on Spot Price",
    subtitle = "All coefficients negative across specifications, consistent with monotonicity",
    x        = "First-stage coefficient (effect of wind on price)",
    y        = NULL,
    fill     = NULL,
    caption  = "Distribution across all sectors and weigting choices"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9, colour = "grey35", margin = margin(b = 8)),
    plot.caption       = element_text(size = 7.5, colour = "grey50", hjust = 0,
                                      margin = margin(t = 8)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "none"
  )

p_monotonicity
# ── FIGURE 8: SUTVA Plot: Consumption shares   ─────────────────────────────####
sector_shares <- consumption_panel %>%
  group_by(DK36_en) %>%
  summarise(total_MWh = sum(Consumption_MWh, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    share_panel = 100 * total_MWh / sum(total_MWh)
  ) %>%
  arrange(desc(share_panel))

plot_data <- sector_shares %>%
  arrange(share_panel) %>%
  mutate(rank = row_number())

ggplot(sector_shares, aes(x = share_panel)) +
  geom_density(
    fill = pal_gran["DK10"],
    alpha = 0.35,
    colour = pal_gran["DK10"],
    linewidth = 1
  ) +
  geom_vline(
    xintercept = max(sector_shares$share_panel),
    linetype = "dashed",
    linewidth = 1,
    colour = pal_weight["Consumption"]
  ) +
  labs(
    title = "Distribution of sector shares in industrial electricity consumption",
    x = "Sector share of total consumption (%)",
    y = "Density"
  ) +
  th_coef +
  theme(
    legend.position = "none"
  )
# ── FIGURE 9: First stage - residualised    ────────────────────────────────####

vars_needed <- c(
  "log_P_c", "Wind_c",
  "Temp_c", "log_gas", "log_coal", "log_carbon",
  "fe_hour", "fe_month", "fe_dow"
)

panel_fs <- consumption_panel %>%
  select(all_of(vars_needed)) %>%
  tidyr::drop_na(all_of(vars_needed))

fe_logP <- feols(log_P_c ~ Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow, data = panel_fs)
fe_wind <- feols(Wind_c  ~ Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow, data = panel_fs)



panel_fs <- panel_clean %>% 
  mutate(
    resid_logP = as.numeric(residuals(fe_logP),na.rm = TRUE),
    resid_wind = as.numeric(residuals(fe_wind),na.rm = TRUE)
  )

n_bins = 20

fs_plot_data <- panel_fs %>%
  mutate(wind_bin = ntile(resid_wind, n_bins)) %>%
  group_by(wind_bin) %>%
  summarise(
    mean_wind = mean(resid_wind, na.rm = TRUE),
    mean_logP = mean(resid_logP, na.rm = TRUE),
    .groups   = "drop"
  )

ggplot(fs_plot_data, aes(x = mean_wind, y = mean_logP)) +
  geom_line(colour = "black", linewidth = 0.5) +
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.7) +
  labs(
    title    = "First Stage: Wind Forecast → Log Electricity Price",
    subtitle = "Residualised on fixed effects and controls",
    x        = "Wind forecast (residualised)",
    y        = "Log electricity price (residualised)",
    caption  = "Binned scatter (20 equal-count bins). Consumption-weighted price."
  ) + th_coef

# ── FIGURE 10: Reduced form ────────────────────────────────────────────────####

vars_needed <- c(
  "log_consumption", "Wind_c",
  "Temp_c", "log_gas", "log_coal", "log_carbon",
  "fe_hour", "fe_month", "fe_dow"
)

panel_rf <- consumption_panel %>%
  select(all_of(vars_needed)) %>%
  tidyr::drop_na(all_of(vars_needed))

fe_consump <- feols(log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |  fe_hour + fe_month + fe_dow, data = panel_rf)
fe_wind    <- feols(Wind_c ~ Temp_c + log_gas + log_coal + log_carbon |  fe_hour + fe_month + fe_dow, data = panel_rf)

panel_rf <- panel_rf %>%
  mutate(
    resid_consump = as.numeric(residuals(fe_consump)),
    resid_wind    = as.numeric(residuals(fe_wind))
  )


n_bins <- 20

rf_plot_data <- panel_rf %>%
  mutate(wind_bin = ntile(resid_wind, n_bins)) %>%
  group_by(wind_bin) %>%
  summarise(
    mean_wind    = mean(resid_wind,    na.rm = TRUE),
    mean_consump = mean(resid_consump, na.rm = TRUE),
    .groups      = "drop"
  )

ggplot(rf_plot_data, aes(x = mean_wind, y = mean_consump)) +
  geom_line(colour = "black", linewidth = 0.5) +
  geom_point(shape = 21, fill = "white", colour = "black", size = 2.5, stroke = 0.7) +
  labs(
    title    = "Reduced Form: Wind Forecast → Log Electricity Consumption",
    subtitle = "Residualised on fixed effects and controls",
    x        = "Wind forecast (residualised)",
    y        = "Log electricity consumption (residualised)",
    caption  = "Binned scatter (20 equal-count bins). Consumption-weighted price."
  ) + th_coef

# ── TABLE 5: OLS VS IV, IDENTIFICATION CHAIN  ──────────────────────────────####
# Rescale wind to GWh for readable coefficients
consumption_panel <- consumption_panel |>
  mutate(across(c(Wind_c, Wind_emp, Wind_firm), ~ .x / 1000,
                .names = "{.col}_GWh"))

# Pooled identification chain regressions
fs_pooled  <- consumption_panel |>
  feols(log_P_c         ~ Wind_c_GWh + Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow,
        cluster = ~ fe_week)

rf_pooled  <- consumption_panel |>
  feols(log_consumption ~ Wind_c_GWh + Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow,
        cluster = ~ fe_week)

ols_pooled <- consumption_panel |>
  feols(log_consumption ~ log_P_c    + Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow,
        cluster = ~ fe_week)

iv_pooled  <- consumption_panel |>
  feols(log_consumption ~ Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_month + fe_dow |
          log_P_c ~ Wind_c_GWh,
        cluster = ~ fe_week)

# Build table
fmt <- function(m, v) c(formatC(coef(m)[v], 4, format = "f"),
                        paste0("(", formatC(se(m)[v], 4, format = "f"), ")"))

chain_table <- tibble(
  ` `            = c("Coefficient", "Estimate", "SE"),
  `First Stage`  = c("$\\pi_1^{Fs}$",         fmt(fs_pooled,  "Wind_c_GWh")),
  `Reduced Form` = c("$\\pi_1^{Rf}$",         fmt(rf_pooled,  "Wind_c_GWh")),
  OLS            = c("$\\hat{\\beta}^{OLS}$", fmt(ols_pooled, "log_P_c")),
  `2SLS`         = c("$\\beta$",              fmt(iv_pooled,  "fit_log_P_c"))
) |>
  print()
# ── TABLE 6: Sector-level elasticitites with F-stat ────────────────────────#####

extract_split_coef <- function(model, coef_name) {
  map_dfr(seq_along(model), function(i) {
    ct <- coeftable(model[[i]])
    tibble(
      DK36_en = sub(".*sample: ", "", names(model)[i]),
      coef    = ct[coef_name, "Estimate"],
      se      = ct[coef_name, "Std. Error"],
      pval    = ct[coef_name, "Pr(>|t|)"],
      stars   = case_when(
        pval < 0.01 ~ "***",
        pval < 0.05 ~ "**",
        pval < 0.10 ~ "*",
        TRUE        ~ ""
      )
    )
  })
}

extract_fstat <- function(model) {
  map_dfr(seq_along(model), function(i) {
    fs_model <- tryCatch(model[[i]]$iv_first_stage[[1]], error = function(e) NULL)
    if (is.null(fs_model)) return(NULL)
    
    fs_ct    <- coeftable(fs_model)
    inst_row <- rownames(fs_ct)[str_detect(rownames(fs_ct), "Wind")]
    if (length(inst_row) == 0) return(NULL)
    
    tibble(
      DK36_en = sub(".*sample: ", "", names(model)[i]),
      fstat   = as.numeric(fs_ct[inst_row[1], "t value"])^2
    )
  })
}

# Extract coefficients
iv_c    <- extract_split_coef(IV_model_het_DK10_W, "fit_log_P_c")
iv_emp  <- extract_split_coef(IV_model_het_Emp_w,  "fit_log_P_emp")
iv_firm <- extract_split_coef(IV_model_het_firm_w, "fit_log_P_firm")
ols_c   <- extract_split_coef(OLS_model_het_c_w,   "log_P_c")

# Extract partial F-stats
fstat_c    <- extract_fstat(IV_model_het_DK10_W) |> rename(F_cons = fstat)
fstat_emp  <- extract_fstat(IV_model_het_Emp_w)  |> rename(F_emp  = fstat)
fstat_firm <- extract_fstat(IV_model_het_firm_w) |> rename(F_firm = fstat)

# Format: coefficient*** (SE)
fmt <- function(df, colname) {
  df |>
    mutate(cell = paste0(
      formatC(coef, digits = 3, format = "f"), stars,
      " (", formatC(se, digits = 3, format = "f"), ")"
    )) |>
    select(DK36_en, cell) |>
    rename(!!colname := cell)
}

# Build  table
results_table <- fmt(iv_c,   "IV_cons") |>
  left_join(fmt(iv_emp,  "IV_emp"),  by = "DK36_en") |>
  left_join(fmt(iv_firm, "IV_firm"), by = "DK36_en") |>
  left_join(fmt(ols_c,   "OLS"),     by = "DK36_en") |>
  left_join(fstat_c,                 by = "DK36_en") |>
  left_join(fstat_emp,               by = "DK36_en") |>
  left_join(fstat_firm,              by = "DK36_en") |>
  mutate(across(starts_with("F_"), ~ formatC(.x, digits = 1, format = "f"))) |>
  arrange(DK36_en) %>% 
  print(n = Inf)




# ── FIGURE 11: Plot of sectors and elasticities  ───────────────────────────####
# ── Consumption shares ────────────────────────────────────────────────────────
consumption_shares <- consumption_panel %>%
  group_by(DK36_en) %>%
  summarise(total_consumption = sum(Consumption_MWh, na.rm = TRUE)) %>%
  mutate(share = total_consumption / sum(total_consumption) * 100) %>%
  select(DK36_en, share)

# ── Join and flag significance ────────────────────────────────────────────────
plot_data <- iv_c %>%
  left_join(consumption_shares, by = "DK36_en") %>%
  mutate(
    significant = case_when(
      pval < 0.01 ~ "1%",
      pval < 0.05 ~ "5%",
      pval < 0.10 ~ "10%",
      TRUE        ~ "Insignificant"
    ),
    sig_binary = pval < 0.05,
    sig_label  = factor(significant,
                        levels = c("1%", "5%", "10%", "Insignificant"))
  )

# ── Number significant sectors by elasticity magnitude ───────────────────────
sig_sectors <- plot_data %>%
  filter(sig_binary) %>%
  arrange(coef) %>%
  mutate(sector_num = row_number())

plot_data_numbered <- plot_data %>%
  left_join(sig_sectors %>% select(DK36_en, sector_num), by = "DK36_en")

# ── Legend text vector ────────────────────────────────────────────────────────
legend_labels <- paste0(sig_sectors$sector_num, ".  ", sig_sectors$DK36_en)
n_sig         <- nrow(sig_sectors)
y_top         <- 0.050
y_step        <- 0.012
legend_y      <- seq(y_top, y_top - (n_sig - 1) * y_step, by = -y_step)

# ── Plot ───────────────────────────────────────────────────────────────────────
ggplot(plot_data_numbered,
       aes(x = share, y = coef,
           size   = share,
           colour = sig_label,
           alpha  = sig_label)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey50", linewidth = 0.4) +
  geom_point() +
  geom_text(
    data        = plot_data_numbered %>% filter(sig_binary),
    aes(x = share, y = coef, label = sector_num),
    colour      = "white",
    size        = 3.2,
    fontface    = "bold",
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x        = 13.8,
    y        = legend_y,
    label    = legend_labels,
    hjust    = 1,
    size     = 2.8,
    colour   = "black",
    fontface = "plain"
  ) +
  scale_size_continuous(range = c(3, 13), guide = "none") +
  scale_colour_manual(
    values = c(
      "1%"            = "#1a3a5c",
      "5%"            = "#2e86ab",
      "10%"           = "#a8c5da",
      "Insignificant" = "grey75"
    ),
    name = "Significance"
  ) +
  scale_alpha_manual(
    values = c(
      "1%"            = 1,
      "5%"            = 1,
      "10%"           = 0.8,
      "Insignificant" = 0.4
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 14)
  ) +
  labs(
    x       = "Share of total industrial electricity consumption (%)",
    y       = "2SLS price elasticity (consumption weight)",
    title   = NULL,
    caption = paste0(
      "Note: Bubble size proportional to consumption share. ",
      "Numbers identify sectors significant at the 5% level or better.\n"
    )
  ) +
  th_coef

# ── FIGURE 12:  ANDERSON-RUBIN INDUSTRY PLOT: WALD vs AR  ──────────────────####
# ── Derived variables ─────────────────────────────────────────────────────────

ar_with_estimates <- ar_with_estimates %>%
  mutate(
    AR_excludes_zero = !(AR_Lower <= 0 & AR_Upper >= 0),
    Wald_excludes_zero = !(Wald_Lower <= 0 & Wald_Upper >= 0),
    sign = ifelse(estimate < 0, "Negative", "Positive")
  ) %>%
  arrange(estimate) %>%
  mutate(industry = factor(industry, levels = unique(industry)))

# ── PLOT 1: AR vs Wald coefficient plot ───────────────────────────────────────

p1 <- ggplot(ar_with_estimates, aes(y = industry)) +
  # Wald interval (wider, lighter)
  geom_segment(
    aes(x = Wald_Lower, xend = Wald_Upper, yend = industry),
    linewidth = 2.5, color = "grey75", alpha = 0.7
  ) +
  # AR interval (narrower, darker)
  geom_segment(
    aes(x = AR_Lower, xend = AR_Upper, yend = industry, color = AR_excludes_zero),
    linewidth = 1.5
  ) +
  # Point estimate
  geom_point(aes(x = estimate), size = 1.8, shape = 16, color = "black") +
  # Zero reference line
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  # Colors: blue if AR excludes zero, red if it doesn't
  scale_color_manual(
    values = c("TRUE" = "#2166AC", "FALSE" = "#B2182B"),
    labels = c("TRUE" = "AR excludes zero", "FALSE" = "AR includes zero"),
    name = NULL
  ) +
  # Labels
  labs(
    title = "Industry-Level 2SLS Estimates with Anderson\u2013Rubin and Wald 95% Confidence Intervals",
    subtitle = "Grey bars = Wald CI (cluster-robust SE). Coloured bars = AR confidence set. Consumption weights.",
    x = "Estimated Price Elasticity of Electricity Demand",
    y = NULL,
    caption = "Notes: AR confidence sets constructed by grid search (\u03B2 \u2208 [-0.35, 0.05], step = 0.001). Wald intervals use cluster-robust SEs (year-week).\nAll specifications include hour-of-day, day-of-week, and year-month fixed effects. Sample: June 2021\u2013September 2025."
  ) +
  theme_minimal(base_family = "serif", base_size = 11) + th_coef
  
p1

# ── TABLE 7:   Sensitivity results table ───────────────────────────────────####

table_wide <- sens_corrected_all %>%
  select(Industry, Weight, t_OLS, t_cluster, Partial_R2, Critical_kd) %>%
  pivot_wider(
    names_from  = Weight,
    values_from = c(t_OLS, t_cluster, Partial_R2, Critical_kd),
    names_sep   = "_"
  ) %>%
  arrange(Industry) %>%
  select(
    Industry,
    # Consumption weights
    `t_OLS (Cons)`        = t_OLS_Consumption,
    `t_cluster (Cons)`    = t_cluster_Consumption,
    `Partial R² (Cons)`   = Partial_R2_Consumption,
    `Critical k (Cons)`   = Critical_kd_Consumption,
    # Employment weights
    `t_OLS (Emp)`         = t_OLS_Employment,
    `t_cluster (Emp)`     = t_cluster_Employment,
    `Partial R² (Emp)`    = Partial_R2_Employment,
    `Critical k (Emp)`    = Critical_kd_Employment,
    # Firm count weights
    `t_OLS (Firm)`        = `t_OLS_Firm count`,
    `t_cluster (Firm)`    = `t_cluster_Firm count`,
    `Partial R² (Firm)`   = `Partial_R2_Firm count`,
    `Critical k (Firm)`   = `Critical_kd_Firm count`
  )





# ── TABLE 8:  Aggregation difference tables ────────────────────────────────####

consumption_panel %>%
  group_by(DK36Title) %>%
  summarise(n = n(), .groups = "drop") %>%
  summarise(level = "DK36", n_sectors = n(), mean_obs = mean(n), 
            min_obs = min(n), median_obs = median(n))

consumption_panel %>%
  group_by(DK19Title) %>%
  summarise(n = n(), .groups = "drop") %>%
  summarise(level = "DK19", n_sectors = n(), mean_obs = mean(n), 
            min_obs = min(n), median_obs = median(n))

consumption_panel %>%
  group_by(DK10Title) %>%
  summarise(n = n(), .groups = "drop") %>%
  summarise(level = "DK10", n_sectors = n(), mean_obs = mean(n), 
            min_obs = min(n), median_obs = median(n))


# ── Power analysis summary table by aggregation level ─────────────────────────

compute_level_summary <- function(df, sector_var, level_label) {
  
  # Number of sectors and cluster counts
  sector_summary <- df %>%
    group_by(.data[[sector_var]]) %>%
    summarise(
      n_obs      = n(),
      n_clusters = n_distinct(fe_week),
      .groups    = "drop"
    )
  
  # Residual SD per sector (same regression as power analysis calibration)
  resid_sd <- df %>%
    group_by(.data[[sector_var]]) %>%
    group_map(~ {
      m <- fixest::feols(
        log_consumption ~ Temp_c + log_gas + log_coal + log_carbon | fe_hour + fe_dow + fe_month,
        data = .x
      )
      tibble::tibble(
        sector   = .y[[1]],
        resid_sd = sd(residuals(m), na.rm = TRUE)
      )
    }) %>%
    bind_rows()
  
  tibble::tibble(
    Granularity       = level_label,
    N_sectors         = nrow(sector_summary),
    Mean_clusters     = round(mean(sector_summary$n_clusters), 0),
    Min_clusters      = min(sector_summary$n_clusters),
    Max_clusters      = max(sector_summary$n_clusters),
    Mean_obs          = round(mean(sector_summary$n_obs), 0),
    Mean_residual_SD  = round(mean(resid_sd$resid_sd, na.rm = TRUE), 4),
    Median_residual_SD = round(median(resid_sd$resid_sd, na.rm = TRUE), 4),
    Max_residual_SD   = round(max(resid_sd$resid_sd, na.rm = TRUE), 4),
    Min_residual_SD   = round(min(resid_sd$resid_sd, na.rm = TRUE), 4)
  )
}

level_summary <- bind_rows(
  compute_level_summary(consumption_panel, "DK36Title", "DK36"),
  compute_level_summary(consumption_panel, "DK19Title", "DK19"),
  compute_level_summary(consumption_panel, "DK10Title", "DK10")
)


print(knitr::kable(
  level_summary,
  format = "simple",
  col.names = c("Level", "Sectors", "Mean clusters", "Min clusters", 
                "Max clusters", "Mean obs", "Mean σ̂", "Median σ̂", 
                "Max σ̂", "Min σ̂")
))





# ── FIGURE 13/14/15:  Power curves     ─────────────────────────────────────####
# ── 13: Median power curve (all weights overlaid, faceted by granularity) ──####
p_median_all <- power_all %>%
  group_by(weight, granularity, true_effect) %>%
  summarise(
    median_power = median(power, na.rm = TRUE),
    q25          = quantile(power, 0.25, na.rm = TRUE),
    q75          = quantile(power, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = true_effect, y = median_power, colour = weight, fill = weight)) +
  geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(-0.07, -0.01, by = 0.03),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_colour_manual(values = pal_weight) +
  scale_fill_manual(values = pal_weight) +
  facet_wrap(~ granularity, ncol = 3) +
  labs(
    x = "Imposed effect size",
    y = "Statistical power",
    colour = NULL,
    fill = NULL,
    title = "Median power curve by aggregation level and weighting method",
    subtitle = "Ribbon = interquartile range across sectors"
  ) +
  th_pow +
  theme(panel.spacing.x = unit(1.5, "cm"))

p_median_all 

# ── 14: Type S (binned average by power level) ─────────────────────────────####
power_all_binned <- power_all %>%
  mutate(
    power_bin = cut(
      power,
      breaks = seq(0, 1, by = 0.2),
      include.lowest = TRUE,
      labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%")
    ),
    power_bin = as.character(power_bin),
    power_mid = case_when(
      power_bin == "0–20%"   ~ 0.1,
      power_bin == "20–40%"  ~ 0.3,
      power_bin == "40–60%"  ~ 0.5,
      power_bin == "60–80%"  ~ 0.7,
      power_bin == "80–100%" ~ 0.9,
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(granularity, weight, power_bin, power_mid) %>%
  summarise(
    mean_type_s = mean(wrong_sign, na.rm = TRUE),
    .groups = "drop"
  )


p_type_s_clean <- ggplot(
  power_all_binned,
  aes(x = power_mid, y = mean_type_s, colour = weight, group = weight)
) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(size = 2.5) +
  facet_wrap(~ granularity, ncol = 3) +
  scale_x_continuous(
    breaks = c(0.1, 0.3, 0.5, 0.7, 0.9),
    labels = c("0–20", "20–40", "40–60", "60–80", "80–100"),
    limits = c(0.05, 0.95)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_colour_manual(values = pal_weight) +
  labs(
    x = "Statistical power (%)",
    y = "Average Type S error rate",
    colour = NULL,
    title = "Type S error rates by power level",
    subtitle = "Averaged across sectors within power bins"
  ) +
  th_pow +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

p_type_s_clean

# ── 15: Clean Type M plot (all observations + binned medians) ──────────────####
type_m_all_plot <- power_all %>%
  mutate(
    type_m = abs(est_ratio),
    power_bin = cut(
      power,
      breaks = seq(0, 1, by = 0.2),
      include.lowest = TRUE,
      labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%")
    ),
    power_bin = as.character(power_bin),
    power_mid = case_when(
      power_bin == "0–20%"   ~ 0.1,
      power_bin == "20–40%"  ~ 0.3,
      power_bin == "40–60%"  ~ 0.5,
      power_bin == "60–80%"  ~ 0.7,
      power_bin == "80–100%" ~ 0.9,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(type_m), !is.na(power), !is.na(power_mid))

type_m_binned <- type_m_all_plot %>%
  group_by(granularity, weight, power_bin, power_mid) %>%
  summarise(
    med_type_m = median(type_m, na.rm = TRUE),
    q25        = quantile(type_m, 0.25, na.rm = TRUE),
    q75        = quantile(type_m, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_type_m_clean <- ggplot() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.8, linetype = "dashed", colour = "grey40", linewidth = 0.4) +
  geom_point(
    data = type_m_all_plot,
    aes(x = power, y = type_m, colour = weight, shape = weight),
    alpha = 0.25,
    size = 1.8,
    position = position_jitter(width = 0.01, height = 0)
  ) +
  geom_ribbon(
    data = type_m_binned,
    aes(x = power_mid, ymin = q25, ymax = q75, fill = weight, group = weight),
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(
    data = type_m_binned,
    aes(x = power_mid, y = med_type_m, colour = weight, group = weight),
    linewidth = 1.2,
    alpha = 0.95
  ) +
  geom_point(
    data = type_m_binned,
    aes(x = power_mid, y = med_type_m, colour = weight),
    size = 2.5
  ) +
  facet_wrap(~ granularity, ncol = 3) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.2)
  ) +
  scale_y_continuous(
    breaks = 1:5,
    labels = paste0(1:5, "\u00d7")
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 5)) +
  scale_colour_manual(values = pal_weight) +
  scale_fill_manual(values = pal_weight, guide = "none") +
  scale_shape_manual(values = shape_weight) +
  labs(
    x = "Statistical power",
    y = "Exaggeration ratio",
    colour = NULL,
    shape = NULL,
    title = "Type M errors by power level",
    subtitle = "Points show all sector-level observations; lines show median values within power bins; shaded areas show the interquartile range"
  ) +
  th_pow +
  theme(panel.spacing = unit(1.5, "lines"))

p_type_m_clean


