#Power test: 

########### POWER #####################################################
setwd("~/CBS - Copenhagen Business School/Jacob and Jes Thesis - Thesis/Thesis/Data")
source("sim_power_iv_clustering.R")
source("iv_sim_prep.R")

sector_test <- "Føde-, drikke- og tobaksvareindustri"
consumption_panel_sec <- consumption_panel %>%
  filter(DK36Title == sector_test)

first_stage_sec <- feols(
  log_P_c ~ Wind_c + Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel_sec,
  cluster = ~ fe_week
)

iv_sec <- feols(
  log_consumption ~ Temp_c + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_c ~ Wind_c,
  data = consumption_panel_sec,
  cluster = ~ fe_week
)


coef(iv_sec)["fit_log_P_c"]
coef(first_stage_sec)["Wind_c"]

# Define range of first stage effects; 
iv_effect_values <- seq(-0.0003, -0.0009, length.out = 8)

# Define range of LATEs (log price -> log consumption)
true_effect_values <- c(-0.01, -0.02, -0.03, -0.04)  # pick around your IV estimates

effect_grid <- expand.grid(
  iv_effect = iv_effect_values,
  true_effect = true_effect_values)
### Consumption weight Power:
res_c <- vector("list", nrow(effect_grid))

for (i in seq_len(nrow(effect_grid))) {
  message("Running extended model simulation ", i, " of ", nrow(effect_grid))
  
  res_c[[i]] <- suppressMessages(suppressWarnings(
    power_sim_iv(
      df = consumption_panel_sec,
      iv_effect = effect_grid$iv_effect[i],
      true_effect = effect_grid$true_effect[i],
      endog_var = "log_P_c",
      instrument_var = "Wind_c",
      outcome_var = "log_consumption",
      controls = c("Temp_c","log_gas","log_coal","log_carbon"),
      n_sims = 50,
      seed = 1000 + i
    )
  ))
}

power_c <- analyze_iv_power_simulation_sector(res_c, wrong_sign = "negative")

### Employee weight Power:

res_emp <- vector("list", nrow(effect_grid))

for (i in seq_len(nrow(effect_grid))) {
  message("Running extended model simulation ", i, " of ", nrow(effect_grid))
  
  res_emp[[i]] <- suppressMessages(suppressWarnings(
    power_sim_iv(
      df = consumption_panel_sec,
      iv_effect = effect_grid$iv_effect[i],
      true_effect = effect_grid$true_effect[i],
      endog_var = "log_P_emp",
      instrument_var = "Wind_emp",
      outcome_var = "log_consumption",
      controls = c("Temp_emp","log_gas","log_coal","log_carbon"),
      n_sims = 50,
      seed = 1000 + i
    )
  ))
}

power_emp <- analyze_iv_power_simulation_sector(res_emp, wrong_sign = "negative")

### Firms weight power. 
res_firm <- vector("list", nrow(effect_grid))

for (i in seq_len(nrow(effect_grid))) {
  message("Running extended model simulation ", i, " of ", nrow(effect_grid))
  
  res_firm[[i]] <- suppressMessages(suppressWarnings(
    power_sim_iv(
      df = consumption_panel_sec,
      iv_effect = effect_grid$iv_effect[i],
      true_effect = effect_grid$true_effect[i],
      endog_var = "log_P_firm",
      instrument_var = "Wind_firm",
      outcome_var = "log_consumption",
      controls = c("Temp_firm","log_gas","log_coal","log_carbon"),
      n_sims = 250,
      seed = 1000 + i
    )
  ))
}

power_firm <- analyze_iv_power_simulation_sector(res_firm, wrong_sign = "negative")



### GRAPHS: 
#Consumption ########

vline_pi <- -0.00058  # your first stage estimate

power_p_c <- ggplot(power_c, aes(x = iv_effect, y = power)) +
  geom_line(aes(group = true_effect)) +
  geom_point() +
  facet_wrap(~ true_effect) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_vline(xintercept = vline_pi, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1)) +
  labs(
    title = paste("Power curves –", sector_test),
    x = "Effect of Instrument on Endogenous Variable",
    y = "Power (share significant at 5%)"
  ) +
  theme_bw()

sign_p_c <- ggplot(power_c, aes(x = iv_effect, y = wrong_sign)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ true_effect) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Type S Error (Sign Error)",
    x = "Effect of Instrument on Endogenous Variable",
    y = "Proportion of Wrong Sign Estimates\n(Among Stat. Significant Results)"
  ) +
  theme_bw()

mag_p_c <- ggplot(power_c, aes(x = iv_effect, y = est_ratio)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ true_effect) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = vline_pi, lty = 2)+
  labs(
    title = "Type M Error (Magnitude Distortion)",
    x = "Effect of Instrument on Endogenous Variable",
    y = "Estimate / True Effect Ratio\n(Among Stat. Significant Results)"
  ) +
  theme_bw()



(power_p_c / sign_p_c / mag_p_c) 


# Employees #####
first_stage_sec_emp <- feols(
  log_P_emp ~ Wind_emp + Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year,
  data = consumption_panel_sec,
  cluster = ~ fe_week
)

iv_sec_emp<- feols(
  log_consumption ~ Temp_emp + log_gas + log_coal + log_carbon |
    fe_hour + fe_month + fe_year |
    log_P_emp ~ Wind_emp,
  data = consumption_panel_sec,
  cluster = ~ fe_week
)

coef(iv_sec)["fit_log_P_emp"]
coef(first_stage_sec_emp)["Wind_emp"]



vline_pi <- -0.00057  # your first stage estimate

power_p_emp <- ggplot(power, aes(x = iv_effect, y = power)) +
  geom_line(aes(group = true_effect)) +
  geom_point() +
  facet_wrap(~ true_effect) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  geom_vline(xintercept = vline_pi, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.1)) +
  labs(
    title = paste("Power curves –", sector_test),
    x = "Effect of Instrument on Endogenous Variable",
    y = "Power (share significant at 5%)"
  ) +
  theme_bw()

sign_p_emp <- ggplot(power, aes(x = iv_effect, y = wrong_sign)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ true_effect) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Type S Error (Sign Error)",
    x = "Effect of Instrument on Endogenous Variable",
    y = "Proportion of Wrong Sign Estimates\n(Among Stat. Significant Results)"
  ) +
  theme_bw()

mag_p_emp <- ggplot(power, aes(x = iv_effect, y = est_ratio)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ true_effect) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = vline_pi, lty = 2)+
  labs(
    title = "Type M Error (Magnitude Distortion)",
    x = "Effect of Instrument on Endogenous Variable",
    y = "Estimate / True Effect Ratio\n(Among Stat. Significant Results)"
  ) +
  theme_bw()


(power_p_emp / sign_p_emp / mag_p_emp)

########



res_c <- vector("list", nrow(effect_grid))
res_e <- vector("list", nrow(effect_grid))
res_f <- vector("list", nrow(effect_grid))

for (i in seq_len(nrow(effect_grid))) {
  
  res_c[[i]] <- power_sim_iv(
    df = consumption_panel,
    iv_effect = effect_grid$iv_effect[i],
    true_effect = effect_grid$true_effect[i],
    endog_var = "log_P_c",
    instrument_var = "Wind_c",
    outcome_var = "log_consumption",
    controls = c("Temp_c","log_gas","log_coal","log_carbon"),
    n_sims = 1,
    seed = 1000 + i
  )
  
  res_e[[i]] <- power_sim_iv(
    df = consumption_panel,
    iv_effect = effect_grid$iv_effect[i],
    true_effect = effect_grid$true_effect[i],
    endog_var = "log_P_emp",
    instrument_var = "Wind_emp",
    outcome_var = "log_consumption",
    controls = c("Temp_emp","log_gas","log_coal","log_carbon"),
    n_sims = 0,
    seed = 2000 + i
  )
  
  res_f[[i]] <- power_sim_iv(
    df = consumption_panel,
    iv_effect = effect_grid$iv_effect[i],
    true_effect = effect_grid$true_effect[i],
    endog_var = "log_P_firm",
    instrument_var = "Wind_firm",
    outcome_var = "log_consumption",
    controls = c("Temp_firm","log_gas","log_coal","log_carbon"),
    n_sims = 0,
    seed = 3000 + i
  )
}











