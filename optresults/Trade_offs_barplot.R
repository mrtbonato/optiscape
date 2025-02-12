###################################################

# Title: Trade-offs analysis - barplots
# Purpose: This code is used to plot barplots of the range of trade-offs for the baseline and policy-based scenarios and for the each couple of objectives in the baseline scenario
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 23 January 2025

###################################################


## Upload required packages
library(tidyverse)


## Set working directories
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path = paste(getwd(),'CoMOLA_results_june2024', sep="/")
perc_range <- read.csv(paste0(path,'/Perc_range_barplot_mod.csv'))
perc_range_sc <- read.csv(paste0(path,'/Perc_range_barplot_scenarios_sq.csv'))



# Barplot baseline
ggplot(perc_range, aes(fill = priority, color = priority)) + 
  geom_vline(xintercept = 0) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  scale_fill_manual(values = c("grey78", "grey58", "grey78", "grey58", "grey78", "grey58", "grey78", "grey58", "grey78", "grey58", "grey78", "grey58")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")) +
  theme_void() +
  theme(legend.position = "none")


ggsave(file = "CoMOLA_results_june2024/plots/Barplot_all.png",
       width = 220, height = 660, units = "mm")




# Barplot scenarios
ggplot(perc_range_sc, aes(fill = priority, color = priority)) + 
  geom_vline(xintercept = 0) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  scale_fill_manual(values = c("grey78", "grey68","grey58","grey48", "grey78", "grey68","grey58","grey48", "grey78", "grey68", "grey58", "grey48", "grey78", "grey68", "grey58", "grey48"))+
  scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")) +
  theme_void() +
  # theme(
  #   panel.background = element_rect(fill = NA),
  #   panel.grid.major.x = element_line(color = "black", linetype="dashed", size = 0.4),
  #   panel.grid.minor.x = element_line(color = "black", linetype="dashed", size = 0.4),
  #   panel.ontop = FALSE
  # ) +
  # scale_x_continuous(minor_breaks = seq(-6,30,1))+
  theme(legend.position = "none")


ggsave(file = "CoMOLA_results_june2024/plots/Barplot_all_scenarios_1102.png",
       width = 630, height = 880, units = "mm")
