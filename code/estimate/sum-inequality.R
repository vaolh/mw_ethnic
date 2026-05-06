#################################################
################# Clean Memory ##################
#################################################

rm(list = ls())
options(scipen = 999)

### REPLICATION FILE: sum-inequality
### R VERSION: 4.5+
### AUTHORS: Matías Carrasco, Victor Ortega Le Hénanff
### DATE: 2026-03-03

#################################################
############## Load + Packages ##################
#################################################

if (!require(pacman)) install.packages("pacman")
p_load("dplyr", "ggplot2", "data.table", "viridis",
       "survey", "convey", "Hmisc", "cowplot")

load("../../data/clean/enigh/enigh-indlevel-year.RData")

if (!dir.exists("../../paper/figures")) dir.create("../../paper/figures", recursive = TRUE)

#################################################
############ Inequality Measures ################
#################################################

dec_cen_list  <- list()
dec_cen_listT <- list()

options(survey.lonely.psu = "adjust")

for (i in years) {
  message(format(Sys.time(), "%H:%M:%S"), " | Inequality estimation for year ", i)
  cross_list[[paste0("cross_section", i)]] <- cross_list[[paste0("cross_section", i)]] %>%
    mutate(upm = as.factor(upm), est_dis = as.factor(est_dis),
           factor = as.numeric(factor))

  svydesign_i <- svydesign(
    ids = ~upm, weights = ~factor, strata = ~est_dis,
    nest = TRUE, data = cross_list[[paste0("cross_section", i)]]
  )

  dec_cen_list[[paste0("deciles_ictpc_", i)]]  <- svyby(~ictpc, ~deciles_ictpc, svydesign_i, svymean, na.rm = TRUE)
  dec_cen_list[[paste0("centiles_ictpc_", i)]] <- svyby(~ictpc, ~centiles_ictpc, svydesign_i, svymean, na.rm = TRUE)

  dec_cen_listT[[paste0("deciles_ictpc_", i)]] <- svyby(~ict, ~deciles_ictpc, svydesign_i, svytotal, na.rm = TRUE)
  dec_cen_listT[[paste0("centiles_ictpc_", i)]] <- svyby(~ict, ~centiles_ictpc, svydesign_i, svytotal, na.rm = TRUE)
}

# --- Gini ---
gini_list <- list()
for (i in years) {
  message(format(Sys.time(), "%H:%M:%S"), " | Gini estimation for year ", i)
  svydesign_i <- svydesign(
    ids = ~upm, weights = ~factor, strata = ~est_dis,
    nest = TRUE, data = cross_list[[paste0("cross_section", i)]]
  )
  svydesign_i <- convey_prep(svydesign_i)
  gini_list[[paste0("gini_ictpc_", i)]] <- svygini(~ictpc, svydesign_i, na.rm = TRUE)
}

# --- Lorenz ---
lorenz_list <- list()
for (i in years) {
  message(format(Sys.time(), "%H:%M:%S"), " | Lorenz estimation for year ", i)
  svydesign_i <- svydesign(
    ids = ~upm, weights = ~factor, strata = ~est_dis,
    nest = TRUE, data = cross_list[[paste0("cross_section", i)]]
  )
  lorenz_list[[paste0("lorenz_ictpc_", i)]] <- svylorenz(~ictpc, svydesign_i, na.rm = TRUE)
}

#################################################
#################### Plots ######################
#################################################

#################################################
############ Centile Means (log) ################
#################################################

ggplot() +
  geom_point(data = dec_cen_list$centiles_ictpc_2016,
             aes(x = centiles_ictpc, y = log(ictpc), color = "2016"), shape = 15, alpha = 0.9) +
  geom_point(data = dec_cen_list$centiles_ictpc_2018,
             aes(x = centiles_ictpc, y = log(ictpc), color = "2018"), shape = 16, alpha = 0.9) +
  geom_point(data = dec_cen_list$centiles_ictpc_2020,
             aes(x = centiles_ictpc, y = log(ictpc), color = "2020"), shape = 17, alpha = 0.9) +
  geom_point(data = dec_cen_list$centiles_ictpc_2022,
             aes(x = centiles_ictpc, y = log(ictpc), color = "2022"), shape = 18, alpha = 0.9) +
  geom_point(data = dec_cen_list$centiles_ictpc_2024,
             aes(x = centiles_ictpc, y = log(ictpc), color = "2024"), shape = 18, alpha = 0.9) +
  labs(y = "Mean Per Capita Household Income (log)", x = "Centiles") +
  scale_color_viridis(discrete = TRUE, option = "H") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("../../paper/figures/sum-centile-ictpc.png", width = 10, height = 6)

#################################################
############ Centile Totals (log) ###############
#################################################

ggplot() +
  geom_point(data = dec_cen_listT$centiles_ictpc_2016,
             aes(x = centiles_ictpc, y = log(ict), color = "2016"), shape = 15, alpha = 0.9) +
  geom_point(data = dec_cen_listT$centiles_ictpc_2018,
             aes(x = centiles_ictpc, y = log(ict), color = "2018"), shape = 16, alpha = 0.9) +
  geom_point(data = dec_cen_listT$centiles_ictpc_2020,
             aes(x = centiles_ictpc, y = log(ict), color = "2020"), shape = 17, alpha = 0.9) +
  geom_point(data = dec_cen_listT$centiles_ictpc_2022,
             aes(x = centiles_ictpc, y = log(ict), color = "2022"), shape = 18, alpha = 0.9) +
  geom_point(data = dec_cen_listT$centiles_ictpc_2024,
             aes(x = centiles_ictpc, y = log(ict), color = "2024"), shape = 18, alpha = 0.9) +
  labs(y = "Total Income by Centile (log)", x = "Centiles") +
  scale_color_viridis(discrete = TRUE, option = "H") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("../../paper/figures/sum-centile-total.png", width = 10, height = 6)

#################################################
############# Top Centile Share #################
#################################################

proportion_list <- list()
for (i in years) {
  centiles_ictpc_i <- dec_cen_list[[paste0("centiles_ictpc_", i)]]
  centile_100 <- centiles_ictpc_i[centiles_ictpc_i$centiles_ictpc == 100, 2]
  centiles_ictpc_i <- centiles_ictpc_i %>%
    mutate(ing_prop = ictpc / centile_100)
  proportion_list[[paste0("proportion_ictpc_", i)]] <- centiles_ictpc_i
}

ggplot() +
  geom_point(data = proportion_list$proportion_ictpc_2016 %>% filter(centiles_ictpc <= 50),
             aes(x = centiles_ictpc, y = ing_prop, color = "2016"), shape = 15, alpha = 0.9) +
  geom_point(data = proportion_list$proportion_ictpc_2018 %>% filter(centiles_ictpc <= 50),
             aes(x = centiles_ictpc, y = ing_prop, color = "2018"), shape = 16, alpha = 0.9) +
  geom_point(data = proportion_list$proportion_ictpc_2020 %>% filter(centiles_ictpc <= 50),
             aes(x = centiles_ictpc, y = ing_prop, color = "2020"), shape = 17, alpha = 0.9) +
  geom_point(data = proportion_list$proportion_ictpc_2022 %>% filter(centiles_ictpc <= 50),
             aes(x = centiles_ictpc, y = ing_prop, color = "2022"), shape = 18, alpha = 0.9) +
  geom_point(data = proportion_list$proportion_ictpc_2024 %>% filter(centiles_ictpc <= 50),
             aes(x = centiles_ictpc, y = ing_prop, color = "2024"), shape = 18, alpha = 0.9) +
  labs(y = "Proportion of Income to Top Centile", x = "Centiles",
       title = "Income Proportion to Top Centile in Mexico (2016-2024)") +
  scale_color_viridis(discrete = TRUE, option = "H") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("../../paper/figures/sum-proportion-top.png", width = 10, height = 6)

#################################################
################ Lorenz Curve ####################
#################################################

lorenz_plot_data <- do.call(rbind, lapply(years, function(yr) {
  lz <- as.data.frame(lorenz_list[[paste0("lorenz_ictpc_", yr)]])
  lz$year <- as.character(yr)
  lz$p <- seq(0, 1, length.out = nrow(lz))
  lz
}))

ggplot(lorenz_plot_data, aes(x = p, y = lorenz, color = year)) +
  geom_line(linewidth = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(y = "Cumulative Share of Income",
       x = "Cumulative Share of Population",
       title = "Lorenz Curve of Household Income in Mexico (2016-2024)") +
  scale_color_viridis(discrete = TRUE, option = "H") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
ggsave("../../paper/figures/sum-lorenz.png", width = 10, height = 6)

# NOTE: State-level Gini map is now generated by sum-mapgini.py (Python)

message("✓ Saved inequality plots to ../../paper/figures/")
