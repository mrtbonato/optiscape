###################################################

# Title: Trade_offs_comparison
# Purpose: This code is used to compare the Pareto frontiers of the baseline and policy-related scenarios.
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 09 July 2024

###################################################


## Upload required packages
library(tidyverse)
library(sf)
library(patchwork)
library(plotly)
library(tmap)
library(ggplot2)


## Set working directories
# Working directory for land use map
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024")
path_bas = paste(getwd(),'Baseline', sep="/")
path_scen_fa = paste(getwd(),'Scenario_fallow', sep = "/")
path_scen_fe = paste(getwd(),'Scenario_fert', sep = "/")
path_scen_fa_fe = paste(getwd(),'Scenario_fallow_fert', sep = "/")


## Upload fitness of Best solutions (baseline)
BS_fitness_baseline <- read.csv(paste0(path_bas,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
## Rename columns
names(BS_fitness_baseline) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


## Upload fitness of Best solutions (fertilization scenario)
BS_fitness_scenario_fa <- read.csv(paste0(path_scen_fa,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
## Rename columns
names(BS_fitness_scenario_fa) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


## Upload fitness of Best solutions (fertilization scenario)
BS_fitness_scenario_fe <- read.csv(paste0(path_scen_fe,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
## Rename columns
names(BS_fitness_scenario_fe) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


## Upload fitness of Best solutions (fertilization scenario)
BS_fitness_scenario_fa_fe <- read.csv(paste0(path_scen_fa_fe,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
## Rename columns
names(BS_fitness_scenario_fa_fe) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


# Create dataframe with status quo value
StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=4))
names(StatusQuo) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
StatusQuo[1,] <- c(0.00161, 0.0508, -6038.0, 59083.86)




# ANALYSIS MAGNITUDE OF THE TRADE-OFF CURVE
### Percentage variation between min and max
# Variation AgrPrd
AgrPrd_var <- ((min(BS_fitness$AgrPrd)/max(BS_fitness$AgrPrd)) * 100) - 100
# Increase WtrQlt
WtrQlt_var <- 100 - ((max(BS_fitness$WtrQlt)/min(BS_fitness$WtrQlt)) * 100)
# Increase HbtCnt
HabCnt_var <- ((max(BS_fitness$HabCnt)/min(BS_fitness$HabCnt)) * 100) - 100
# Increase HbtQlt
HabQlt_var <- ((max(BS_fitness$HabQlt)/min(BS_fitness$HabQlt)) * 100) - 100


## Variation between min and max
# Decrease AgrPrd
AgrPrd_dec <- min(BS_fitness$AgrPrd) - max(BS_fitness$AgrPrd)
# Increase WtrQlt
WtrQlt_inc <- min(BS_fitness$WtrQlt) - max(BS_fitness$WtrQlt)
# Increase HbtCnt
HabCnt_inc <- max(BS_fitness$HabCnt) - min(BS_fitness$HabCnt)
# Increase HbtQlt
HabQlt_inc <- max(BS_fitness$HabQlt) - min(BS_fitness$HabQlt) 


# # # # Percentage increse/decrease from status quo
# # # Decrease AgrPrd
# AgrPrd_decSQ <- ((StatusQuo$AgrPrd - min(BestSol_fit$AgrPrd)) / StatusQuo$AgrPrd) * 100
# # # Increase WtrQlt
# WtrQlt_incSQ <- ((StatusQuo$WtrQlt - max(BestSol_fit$WtrQlt)) / StatusQuo$WtrQlt) * 100
# # # Increase HbtCnt
# HabCnt_incSQ <- ((max(BestSol_fit$HabCnt) - (StatusQuo$HabCnt)) / StatusQuo$HabCnt) * 100
# # # Increase HbtQlt
# HabQlt_incSQ <- ((max(BestSol_fit$HabQlt) - (StatusQuo$HabQlt)) / StatusQuo$HabQlt) * 100





###  ANALYSIS PARETO FRONTIER
# Plot complete Pareto frontier
p1 <- ggplot() +
  
  # plot status quo
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  
  # plot baseline scenario
  geom_point(data = BS_fitness_baseline, aes(shape = 21, x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  
  # plot fallow scenario
  geom_point(data = BS_fitness_scenario_fa, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  
  # plot fertilization scenario
  geom_point(data = BS_fitness_scenario_fe, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  
  # plot fallow and fertilization scenarios
  geom_point(data = BS_fitness_scenario_fa_fe, aes(x = WtrQlt, y = AgrPrd,fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  
  #scale_color_distiller(palette = "Blues", name = "Probability of connectivity") +
  #new_scale_color() +
  #scale_color_distiller(palette = "Greens", name = "Probability of connectivity") +
  
  scale_fill_viridis_c(name = "Probability of connectivity") +
  scale_size(name = "Habitat quality") +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))

p1


# Save plot
ggsave(file = "plots/ParetoFrontier_confscenario_all_0608.png",
       width = 297, height = 210, units = "mm")