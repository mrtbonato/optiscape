###################################################

# Title: Trade-offs analysis - policy-based scenarios
# Purpose: This code is used to analyse the trade-offs in the policy-based scenarios and compare them with the baseline scenario. Include the plotting of the Pareto frontiers and the calculation of the range of trade-offs
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 23 January 2025

###################################################


## Upload required packages
library(tidyverse)
library(sf)
library(patchwork)
library(plotly)
library(tmap)
library(ggplot2)


## Set working directories
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024")
path_bas = paste(getwd(),'Baseline', sep="/")
path_scen_fa = paste(getwd(),'Scenario_fallow', sep = "/")
path_scen_fe = paste(getwd(),'Scenario_fert', sep = "/")
path_scen_fa_fe = paste(getwd(),'Scenario_fallow_fert', sep = "/")


## Upload fitness of Best solutions
# Baseline scenario
BS_fitness_baseline <- read.csv(paste0(path_bas,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
names(BS_fitness_baseline) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")

# Reduced ertilization scenario
BS_fitness_scenario_fe <- read.csv(paste0(path_scen_fe,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
names(BS_fitness_scenario_fe) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")

# Mandatory fallow scenario
BS_fitness_scenario_fa <- read.csv(paste0(path_scen_fa,'/pareto_fitness.txt'), h = F, as.is=T, sep = "")
names(BS_fitness_scenario_fa) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")

# Combined reduced fertilization and mandatory fallow scenario
BS_fitness_scenario_fa_fe <- read.csv(paste0(path_scen_fa_fe,'/pareto_fitness.txt'), h = F, as.is=T, sep = "")
names(BS_fitness_scenario_fa_fe) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


# Create dataframe with status quo value
StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=4))
names(StatusQuo) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
StatusQuo[1,] <- c(0.00161, 0.0508, -6038.0, 59083.86)




###  ANALYSIS PARETO FRONTIER
# Plot complete Pareto frontier for baseline and reduced fertilization scenario
p1 <- ggplot() +
  # plot status quo
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 5, fill= "black", color = "gray50") +
  # plot baseline scenario
  geom_point(data = BS_fitness_baseline, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  # plot fertilization scenario
  geom_point(data = BS_fitness_scenario_fe, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  lims(y = c(55500, 59500)) +
  scale_fill_viridis_c(name = "Probability of 
connectivity", limits = c(0.00161, 0.00196)) +
  scale_size(name = "Fertilizer
run-off", limits = c(0.0508, 0.058)) +
  theme(text=element_text(size=24))

p1

# Save plot
ggsave(file = "plots/ParetoFrontier_confscenario_bas_fert.png",
       width = 270, height = 297, units = "mm")



# Plot complete Pareto frontier for baseline and reduced fertilization scenario
p2 <- ggplot() +
  # plot status quo
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 5, fill= "black", color = "gray50") +
  # plot fallow scenario
  geom_point(data = BS_fitness_scenario_fa, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  # plot fallow and fertilization scenarios
  geom_point(data = BS_fitness_scenario_fa_fe, aes(x = WtrQlt, y = AgrPrd,fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
     y = "Crop yield [grain unit]") +
  lims(y = c(55500, 59500)) +
  scale_fill_viridis_c(name = "Probability of 
connectivity", limits = c(0.00161, 0.00196)) +
  scale_size(name = "Fertilizer
run-off", limits = c(0.0508, 0.058)) +
  theme(text=element_text(size=24))

p2

# Save plot
ggsave(file = "plots/ParetoFrontier_confscenario_fallow_fert.png",
       width = 270, height = 297, units = "mm")




### ANALYSIS RANGE OF THE TRADE-OFF CURVE
# To repeat for each scenario

# Range of trade-offs: variation between min and max
# Decrease AgrPrd
AgrPrd_dec <- min(BS_fitness_baseline$AgrPrd) - max(BS_fitness_baseline$AgrPrd)
# Increase WtrQlt
WtrQlt_inc <- min(BS_fitness_baseline$WtrQlt) - max(BS_fitness_baseline$WtrQlt)
# Increase HbtCnt
HabCnt_inc <- max(BS_fitness_baseline$HabCnt) - min(BS_fitness_baseline$HabCnt)
# Increase HbtQlt
HabQlt_inc <- max(BS_fitness_baseline$HabQlt) - min(BS_fitness_baseline$HabQlt) 


# Range of trade-offs: percentage variation between min and max
# Variation AgrPrd
AgrPrd_var <- ((min(BS_fitness_baseline$AgrPrd)/max(BS_fitness_baseline$AgrPrd)) * 100) - 100
# Increase WtrQlt
WtrQlt_var <- 100 - ((max(BS_fitness_baseline$WtrQlt)/min(BS_fitness_baseline$WtrQlt)) * 100)
# Increase HbtCnt
HabCnt_var <- ((max(BS_fitness_baseline$HabCnt)/min(BS_fitness_baseline$HabCnt)) * 100) - 100
# Increase HbtQlt
HabQlt_var <- ((max(BS_fitness_baseline$HabQlt)/min(BS_fitness_baseline$HabQlt)) * 100) - 100


# Percentage increse/decrease from status quo
# Decrease AgrPrd
AgrPrd_decSQ <- ((StatusQuo$AgrPrd - min(BS_fitness_baseline$AgrPrd)) / StatusQuo$AgrPrd) * 100
# Increase WtrQlt
WtrQlt_incSQ <- ((StatusQuo$WtrQlt - max(BS_fitness_baseline$WtrQlt)) / StatusQuo$WtrQlt) * 100
# Increase HbtCnt
HabCnt_incSQ <- ((max(BS_fitness_baseline$HabCnt) - (StatusQuo$HabCnt)) / StatusQuo$HabCnt) * 100
# Increase HbtQlt
HabQlt_incSQ <- ((max(BS_fitness_baseline$HabQlt) - (StatusQuo$HabQlt)) / StatusQuo$HabQlt) * 100








#### Other trials of plotting  ################################################

# Plotting HbtCnt in x axis
ggplot() +
  
  # plot status quo
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 5, fill= "black", color = "gray50") +
  #geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 3, fill= "black") +
  
  # plot baseline scenario
  geom_point(data = BS_fitness_baseline, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  
  # plot fallow scenario
  geom_point(data = BS_fitness_scenario_fa, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  
  # plot fertilization scenario
  geom_point(data = BS_fitness_scenario_fe, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  
  # plot fallow and fertilization scenarios
  geom_point(data = BS_fitness_scenario_fa_fe, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  
# Change names labels
labs(x = "Probability of connectivity",
     y = "Crop yield [grain unit]") +
  lims(x = c(0.00160, 0.00196),
             y = c(55500, 59500)) +
  scale_fill_viridis_c(name = "Phorsphorus 
load [kg/year]", limits = c(-6050, -4200)) +
  scale_size(name = "Fertilizer
run-off", limits = c(0.0508, 0.058)) +
  theme(text=element_text(size=24))


# Save plot
ggsave(file = "plots/ParetoFrontier_scenarios_fert.png",
       width = 270, height = 297, units = "mm")





library(ggnewscale)
ggplot() +
  
  # plot status quo
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 5, fill= "black", color = "gray50") +
  
  # plot fallow scenario
  geom_point(data = BS_fitness_scenario_fa, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  scale_fill_distiller(palette = "Greens", limits = c(-6050, -4200)) +
  
  new_scale("fill") +
  
  # plot baseline scenario
  geom_point(data = BS_fitness_baseline, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black", alpha = 0.5) +
  scale_fill_distiller(palette = "Greys", limits = c(-6050, -4200))+
  
  new_scale("fill") +
  
  # plot fallow and fertilization scenarios
  geom_point(data = BS_fitness_scenario_fa_fe, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  scale_fill_distiller(palette = "Reds", limits = c(-6050, -4200)) +
  
  new_scale("fill") +
  
    # plot fertilization scenario
  geom_point(data = BS_fitness_scenario_fe, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black", alpha = 0.5) +
  scale_fill_distiller(palette = "YlOrBr", limits = c(-6050, -4200)) +
  
  
# Change names labels
labs(x = "Probability of connectivity",
     y = "Crop yield [grain unit]") +
  lims(y = c(55500, 59500), x = c(0.00160, 0.00196)) +
  #scale_fill_viridis_c(name = "Phosporous load [kg/year]", limits = c(-6050, -4200)) +
  scale_size(name = "Fertilizer
run-off", limits = c(0.0508, 0.058)) +
  # lims(fill = c(0.00161, 0.00196)) + 
  # lims(size = c(0.051, 0.057)) +
  theme(text=element_text(size=24))

p1






### Win-win analysis of fallow land scenario ##################################
BS_fitness_scenario_fa_ww <- BS_fitness_scenario_fa %>%
  filter(AgrPrd >= 59083.86)

# # # Percentage increse/decrease from status quo
# # Decrease AgrPrd
AgrPrd_decSQ <- ((StatusQuo$AgrPrd - min(BS_fitness_scenario_fa_ww$AgrPrd)) / StatusQuo$AgrPrd) * 100
# # Increase WtrQlt
WtrQlt_incSQ <- ((StatusQuo$WtrQlt - max(BS_fitness_scenario_fa_ww$WtrQlt)) / StatusQuo$WtrQlt) * 100
# # Increase HbtCnt
HabCnt_incSQ <- ((max(BS_fitness_scenario_fa_ww$HabCnt) - (StatusQuo$HabCnt)) / StatusQuo$HabCnt) * 100
# # Increase HbtQlt
HabQlt_incSQ <- ((max(BS_fitness_scenario_fa_ww$HabQlt) - (StatusQuo$HabQlt)) / StatusQuo$HabQlt) * 100






