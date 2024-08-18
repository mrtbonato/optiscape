
###################################################

# Title: CoMOLA postprocessing
# Purpose: This code is used to plot the hypervolume of the optimization procedure and retrieve fitness values and genome
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 09 July 2024
###################################################


# Run the code 4 times, one for each of the defined paths
## Define path to your CoMOLA folder
path <- 'Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024/Baseline'
path <- 'Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024/Scenario_fallow'
path <- 'Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024/Scenario_fert'
path <- 'Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024/Scenario_fallow_fert'


## Define names of objectives (fit1, fit2, etc. must correspond with your 
## specifications in models/SWAT.R)
fit1 <- 'Habitat connectivity' # give a more meaningful name for objective 1
fit2 <- 'Habitat quality' # give a more meaningful name for objective 2
fit3 <- 'P load' # give a more meaningful name for objective 3
fit4 <- 'Crop production' # give a more meaningful name for objective 4


## Execute the code below (do not modify)

# get functions
setwd(paste0(path,'/output_analysis'))
source('functions_postprocessing.R')

foo1(c('mco', 'dplyr', 'tidyverse', 'ggplot2', 'viridis'))

## extract results
pareto <- get_pareto()

## calculate hypervolume development
HV <- hv_generations()




# Extract info on fitness and genome and save
BS_fitness <- pareto$fitness
BS_genomes <- pareto$genomes

# Write table
write.table(BS_fitness, "BS_fitness.csv", append = FALSE, sep = ";", col.names = FALSE, row.names = FALSE)
write.table(BS_genomes, "BS_genomes.csv", append = FALSE, sep = ";", col.names = FALSE, row.names = FALSE)




# Plot hypervolume for all scenarios

# Baseline scenario
## plot hypervolumes for each generation
p1 <- ggplot(HV, aes(Generation, HV)) +
  geom_line() +
  geom_point() + 
  theme(text = element_text(size = 20))


# Fallow land scenario
## plot hypervolumes for each generation
p2 <- ggplot(HV, aes(Generation, HV)) +
  geom_line() +
  geom_point() + 
  theme(text = element_text(size = 20))


# Fertilization scenario
## plot hypervolumes for each generation
p3 <- ggplot(HV, aes(Generation, HV)) +
  geom_line() +
  geom_point() + 
  theme(text = element_text(size = 20))


# Fallow land and fertilization scenario
## plot hypervolumes for each generation
p4 <- ggplot(HV, aes(Generation, HV)) +
  geom_line() +
  geom_point() + 
  theme(text = element_text(size = 20))


# ggsave(ggsave(file = "plots/baseline_HV.png",
#                width = 120, height = 210, units = "mm"))




## Put together plots
library(patchwork)

patchwork <- (p1 | p2) /
  (p3 |p4) 


# Add letters A and B to the plots
patchwork + plot_layout(ncol = 1, heights = c(12,5,5)) + plot_annotation(tag_levels = 'a')
patchwor5 + plot_layout(ncol = 1, heights = c(14,6,6)) + plot_annotation(tag_levels = 'a')


# Save plot
ggsave(file = "plots/HV_scenarios.png",
       width = 210, height = 250, units = "mm")





## plot Pareto solutions
#plot_2D(mode=3)


# Use mode to modify the assignment of objectives to axis
# for 3-dimensional plots
# mode = 1: x = fit1, y = fit2, color = fit3
# mode = 2: x = fit2, y = fit3, color = fit1
# mode = 3: x = fit3, y = fit1, color = fit2
#
# for 4-dimensional plots
# mode = 1: x = fit1, y = fit2, color = fit3, size = fit4
# mode = 2: x = fit2, y = fit3, color = fit4, size = fit1
# mode = 3: x = fit3, y = fit4, color = fit1, size = fit2
# mode = 4: x = fit4, y = fit1, color = fit2, size = fit3


