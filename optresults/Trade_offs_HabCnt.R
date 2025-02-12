
###################################################

# Title: Trade offs analysis - Habitat Connectivity index
# Purpose: This code is used to analyse the trade-offs when considering the single animal classes for the calculation on the habitat connectivity index
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 23 January 2025

###################################################


## Upload required packages
library(tidyverse)
library(sf)
library(plotly)
library(tmap)
library(ggnewscale)



## Set working directories
# Working directory for land use map
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path_Hab = paste(getwd(),'CoMOLA_results_june2024', sep="/")
path = paste(getwd(),'CoMOLA_results_june2024/Baseline', sep="/")


## Upload fitness of Best solutions
BS_fitness <- read.csv(paste0(path,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
## Rename columns
names(BS_fitness) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
# Add ID
BS_fitness$ID <- c(1:1070)


## Upload fitness of Best solutions
BS_fitness_HabCnt <- read.csv(paste0(path_Hab,'/PC_output_HabCnt.csv'), h = F, as.is=T, sep = ";")
# Select fitness values
BS_fitness_HabCnt <- BS_fitness_HabCnt %>%
  select(303:306)
## Rename columns
names(BS_fitness_HabCnt) <- c("HabCnt_or", "HabCnt_ins", "HabCnt_bird", "HabCnt_mamm")
# Add ID
BS_fitness_HabCnt$ID <- c(1:1070)


# Join
BS_fitness_join <- BS_fitness %>%
  left_join(BS_fitness_HabCnt, by = "ID")


# Create dataframe with status quo value
StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=7))
names(StatusQuo) <- c("HabCnt", "HabCnt_ins", "HabCnt_bird", "HabCnt_mamm","HabQlt", "WtrQlt", "AgrPrd")
StatusQuo[1,] <- c(0.00161, 0.000688, 0.00189, 0.00215, 0.0508, -6038.0, 59083.86)




###  ANALYSIS PARETO FRONTIER
# Plot the plots of the 3 key-taxa together
ggplot () +
  geom_point(data = BS_fitness_join, aes(x = HabCnt_ins, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  geom_point(data = BS_fitness_join, aes(x = HabCnt_bird, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  geom_point(data = BS_fitness_join, aes(x = HabCnt_mamm, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  geom_point(data = StatusQuo, aes(x = HabCnt_ins, y = AgrPrd), shape = 17) +
  geom_point(data = StatusQuo, aes(x = HabCnt_bird, y = AgrPrd), shape = 17) +
  geom_point(data = StatusQuo, aes(x = HabCnt_mamm, y = AgrPrd), shape = 17) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Phorsphorus load
[kg/year]") +
  scale_size(name = "Habitat quality") + 
  theme(text=element_text(size=18)) +
  
  new_scale("fill") +
  geom_point(data = BS_fitness_join, aes(x = HabCnt, y = AgrPrd, fill = WtrQlt, size = HabQlt), shape = 21, color = "black") +
  scale_fill_distiller(palette = "Greys") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 17, color = "grey")


# Save plot
ggsave(file = "plots/ParetoFrontier_HabCnt3_legend.png",
       width = 297, height = 160, units = "mm")



### ANALYSIS RANGE OF THE TRADE-OFFS
# Range of trade-offs: variation between min and max
# Decrease AgrPrd
#AgrPrd_dec <- min(BS_fitness_HabCnt$AgrPrd) - max(BS_fitness_HabCnt$AgrPrd)
# Increase HbtCnt insects
HabCnt_ins_inc <- max(BS_fitness_HabCnt$HabCnt_ins) - min(BS_fitness_HabCnt$HabCnt_ins)
# Increase HbtCnt birds
HabCnt_bird_inc <- max(BS_fitness_HabCnt$HabCnt_bird) - min(BS_fitness_HabCnt$HabCnt_bird)
# Increase HbtCnt insects
HabCnt_mamm_inc <- max(BS_fitness_HabCnt$HabCnt_mamm) - min(BS_fitness_HabCnt$HabCnt_mamm)



# Range of trade-offs: percentage variation between min and max
HabCnt_ins_var <- ((max(BS_fitness_HabCnt$HabCnt_ins)/min(BS_fitness_HabCnt$HabCnt_ins)) * 100) - 100
HabCnt_bird_var <- ((max(BS_fitness_HabCnt$HabCnt_bird)/min(BS_fitness_HabCnt$HabCnt_bird)) * 100) - 100
HabCnt_mamm_var <- ((max(BS_fitness_HabCnt$HabCnt_mamm)/min(BS_fitness_HabCnt$HabCnt_mamm)) * 100) - 100


# Percentage increse/decrease from status quo
# Increase HbtCnt
HabCnt_ins_incSQ <- ((max(BS_fitness_HabCnt$HabCnt_ins) - (StatusQuo$HabCnt_ins)) / StatusQuo$HabCnt_ins) * 100
HabCnt_bird_incSQ <- ((max(BS_fitness_HabCnt$HabCnt_bird) - (StatusQuo$HabCnt_bird)) / StatusQuo$HabCnt_bird) * 100
HabCnt_mamm_incSQ <- ((max(BS_fitness_HabCnt$HabCnt_mamm) - (StatusQuo$HabCnt_mamm)) / StatusQuo$HabCnt_mamm) * 100








### Plot Pareto-frontier for single taxa #######################################

# Insects
p1 <- ggplot() +
  geom_point(data = BS_fitness_join, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt_ins, size = HabQlt), shape = 21, color = "black") +
  #geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load
[kg/year]",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Probability of 
connectivity (birds)") +
  scale_size(name = "Habitat quality") + 
  #scale_shape_manual(values = c(21, 21, 21, 21)) +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))

p1



# Plot complete Pareto frontier - Birds
p2 <- ggplot() +
  geom_point(data = BS_fitness_join, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt_bird, size = HabQlt), shape = 21, color = "black") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Probability of 
connectivity (birds)") +
  scale_size(name = "Habitat quality") + 
  #scale_shape_manual(values = c(21, 21, 21, 21)) +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))

p2



p3 <- ggplot() +
  geom_point(data = BS_fitness_join, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt_mamm, size = HabQlt), shape = 21, color = "black") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Probability of 
connectivity (birds)") +
  scale_size(name = "Habitat quality") + 
  #scale_shape_manual(values = c(21, 21, 21, 21)) +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))

p3