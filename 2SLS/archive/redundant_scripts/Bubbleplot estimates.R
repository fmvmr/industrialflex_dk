# --- Project setup ------------------------------------------------------------
# Open industrialflex_dk.Rproj (or start R in the repo root). Scripts use the
# data/ folder as working directory; see README.md.
setwd(here::here("data"))
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

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
      "Numbers identify sectors significant at the 5% level or better.\n",
      "Clustered standard errors (week level). Sample: June 2021-2025."
    )
  ) +
  th_coef


# ── Consumption by significant sector by year ─────────────────────────────────
sig_sector_names <- sig_sectors$DK36_en

consumption_panel %>%
  filter(DK36_en %in% sig_sector_names) %>%
  group_by(Year, DK36_en) %>%
  summarise(consumption_GWh = sum(Consumption_MWh, na.rm = TRUE) / 1000,
            .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = consumption_GWh) %>%
  left_join(sig_sectors %>% select(DK36_en, sector_num, coef), by = "DK36_en") %>%
  arrange(sector_num) %>%
  mutate(across(where(is.numeric) & !c(sector_num, coef),
                ~ round(.x, 1))) %>%
  mutate(coef = round(coef, 3)) %>%
  select(sector_num, DK36_en, coef, everything()) %>%
  rename(
    `#`        = sector_num,
    Sector     = DK36_en,
    Elasticity = coef
  ) %>%
  print(n = Inf)


