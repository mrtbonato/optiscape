
###################################################

# Title: Trade-offs analysis - baseline scenario
# Purpose: This code is used to analyse the trade-offs in the baseline scenario. Include the plotting of the Pareto frontier and the calculation of the range of trade-offs
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



## Set working directories
# Working directory for land use map
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024")
path = paste(getwd(),'Baseline', sep="/")


## Upload fitness of Best solutions
BS_fitness <- read.csv(paste0(path,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
## Rename columns
names(BS_fitness) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


# Create dataframe with status quo value
StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=4))
names(StatusQuo) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
StatusQuo[1,] <- c(0.00161, 0.0508, -6038.0, 59083.86)





####  ALL OBJECTIVES ##########################################################
###  ANALYSIS PARETO FRONTIER
# Plot complete Pareto frontier
p1 <- ggplot() +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "gray50") +
  #geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  theme(text=element_text(size=18)) +
  scale_fill_viridis_c(name = "Probability of
connectivity") +
  scale_size(name = "Habitat quality
index")

p1

# Save plot
ggsave(file = "plots/ParetoFrontier_baseline_040325.png",
       width = 297, height = 130, units = "mm")



### ANALYSIS RANGE OF THE TRADE-OFF
# Range of trade-offs: variation between min and max
# Decrease AgrPrd
AgrPrd_dec <- min(BS_fitness$AgrPrd) - max(BS_fitness$AgrPrd)
# Increase WtrQlt
WtrQlt_inc <- min(BS_fitness$WtrQlt) - max(BS_fitness$WtrQlt)
# Increase HbtCnt
HabCnt_inc <- max(BS_fitness$HabCnt) - min(BS_fitness$HabCnt)
# Increase HbtQlt
HabQlt_inc <- max(BS_fitness$HabQlt) - min(BS_fitness$HabQlt) 


# Range of trade-offs: percentage variation between min and max
# Variation AgrPrd
AgrPrd_var <- ((min(BS_fitness$AgrPrd)/max(BS_fitness$AgrPrd)) * 100) - 100
# Increase WtrQlt
WtrQlt_var <- 100 - ((max(BS_fitness$WtrQlt)/min(BS_fitness$WtrQlt)) * 100)
# Increase HbtCnt
HabCnt_var <- ((max(BS_fitness$HabCnt)/min(BS_fitness$HabCnt)) * 100) - 100
# Increase HbtQlt
HabQlt_var <- ((max(BS_fitness$HabQlt)/min(BS_fitness$HabQlt)) * 100) - 100


# Percentage increse/decrease from status quo
# Decrease AgrPrd
AgrPrd_decSQ <- ((StatusQuo$AgrPrd - min(BS_fitness$AgrPrd)) / StatusQuo$AgrPrd) * 100
# Increase AgrPrd
AgrPrd_inccSQ <- ((StatusQuo$AgrPrd - max(BS_fitness$AgrPrd)) / StatusQuo$AgrPrd) * 100
# Increase WtrQlt
WtrQlt_incSQ <- ((StatusQuo$WtrQlt - max(BS_fitness$WtrQlt)) / StatusQuo$WtrQlt) * 100
# # Increase HbtCnt
HabCnt_incSQ <- ((max(BS_fitness$HabCnt) - (StatusQuo$HabCnt)) / StatusQuo$HabCnt) * 100
# Increase HbtQlt
HabQlt_incSQ <- ((max(BS_fitness$HabQlt) - (StatusQuo$HabQlt)) / StatusQuo$HabQlt) * 100



# Percentage increse/decrease from status quo only for win-win solutions
# select win-win solutions
ww <- BS_fitness %>%
  filter(AgrPrd >= 59083.86)

WtrQlt_max <- ((StatusQuo$WtrQlt - max(ww$WtrQlt)) / StatusQuo$WtrQlt) * 100
WtrQlt_min <- ((StatusQuo$WtrQlt - min(ww$WtrQlt)) / StatusQuo$WtrQlt) * 100
HabCnt_max <- ((max(ww$HabCnt) - (StatusQuo$HabCnt)) / StatusQuo$HabCnt) * 100
HabCnt_min <- ((min(ww$HabCnt) - (StatusQuo$HabCnt)) / StatusQuo$HabCnt) * 100
HabQlt_max <- ((max(ww$HabQlt) - (StatusQuo$HabQlt)) / StatusQuo$HabQlt) * 100
HabQlt_min <- ((min(ww$HabQlt) - (StatusQuo$HabQlt)) / StatusQuo$HabQlt) * 100








### SINGLE COUPLES OF OBJECTIVES ##############################################

# In case we want the elipses of the clusters
# Upload Best solutions with cluster number
#BS_fitness <- read.csv(paste0(path,'/clusters_k13.csv'), h = T, as.is=T, sep = ",")


# With plotting of the Pareto frontier for each couple of considered objectives (red line)
# Create line 
pareteo.two.dim<-function(data = BS_fitness, column_A, column_B){
  column.A<-data[,column_A]; column.B<-data[,column_B]
  # (i) get the highest value of column B, when column A is at its max value (low end of frontier)
  low.end<-max(column.B[which(column.A==max(column.A))])
  
  # (ii) get rid of all values that are below the low end of the frontier and order the remaining ones
  ordered<-order(column.B[which(column.B>=low.end)], column.A[which(column.B>=low.end)]*(-1))
  ordered<-data[which(column.B>=low.end),][ordered,]
  column.A<-ordered[,column_A]; column.B<-ordered[,column_B]
  
  # (iii) some identical values in column A will be the result of different  scenarios - make sure you only consider 
  # the ones with highest P value
  ordered<-ordered[match(unique(column.B), column.B),]
  column.A<-ordered[,column_A]; column.B<-ordered[,column_B]
  # this works, because they are ordered in a way that the if column B values are identical, the one with the higher
  # column A value comes first. Hence, we can use match (which always returns the first match) to get rid of identical 
  # values
  
  # filter the values out that are below the pareteo frontier
  repeat{
    ordered$order.column_A<-order(ordered[column_A]*(-1))
    ordered$order.column_B<-order(ordered[column_B])
    ordered$order.column_A2<-match(ordered$order.column_B, ordered$order.column_A)
    ordered$diff<-ordered$order.column_B - ordered$order.column_A2
    if(is.element(F, unique(ordered$order.column_A2==ordered$order.column_B))==F ){break}
    ordered<-ordered[which(ordered$diff>=0),]}
  
  return(ordered)
}


# 2-dimensional pareteo frontier between Agricultural production and Water Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=3))

p2 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = -6038.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd), size = 2, color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 5 , fill= "black", color = "gray50") +
  #geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #stat_ellipse(data = BS_fitness %>% filter(cluster != 0), aes(x = WtrQlt, y = AgrPrd, group = cluster, color = cluster)) +
  scale_color_distiller(palette = "Dark2") +
  #geom_point(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred', size = 2) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/y]",
       y = "Crop yield [grain unit]") +
  theme(text=element_text(size=34))

p2

# Save plot
ggsave(file = "plots/TO_agr_wtr.png",
       width = 297, height = 150, units = "mm")


# Range of trade-offs: percentage variation between min and max
AgrPrd_var <- ((max(ordered$AgrPrd)/min(ordered$AgrPrd)) * 100) - 100
WtrQlt_var <- 100 - ((max(ordered$WtrQlt)/min(ordered$WtrQlt)) * 100)

# Percentage increse/decrease from status quo
AgrPrd_decSQ <- ((StatusQuo$AgrPrd - min(ordered$AgrPrd)) / StatusQuo$AgrPrd) * 100
WtrQlt_decSQ <- ((StatusQuo$WtrQlt - min(ordered$WtrQlt)) / StatusQuo$WtrQlt) * 100



# 2-dimensional pareteo frontier between Agricultural production and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=1))

p3 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF", alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = AgrPrd), size = 2, color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 5 , fill= "black", color = "gray50") +
  scale_color_distiller(palette = "Dark2") +
  #geom_point(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred', size = 2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield [grain unit]") + 
  theme(text=element_text(size=34))

p3

# Save plot
ggsave(file = "plots/TO_agr_conn.png",
       width = 297, height = 150, units = "mm")


# Range of trade-offs: percentage variation between min and max
AgrPrd_var <- ((max(ordered$AgrPrd)/min(ordered$AgrPrd)) * 100) - 100
HabCnt_inc <- ((max(ordered$HabCnt)/min(ordered$HabCnt)) * 100) - 100

# Percentage increse/decrease from status quo
AgrPrd_decSQ <- ((StatusQuo$AgrPrd - min(ordered$AgrPrd)) / StatusQuo$AgrPrd) * 100
HabCnt_decSQ <- ((min(ordered$HabCnt) - StatusQuo$HabCnt) / StatusQuo$HabCnt) * 100



# 2-dimensional pareteo frontier between Agricultural production and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=2)) 

p4 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = AgrPrd), size = 2, color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = AgrPrd), shape = 25, size = 5, fill= "black", color = "gray50") +
  #geom_point(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred', size = 2) +
  # Change names labels
  labs(x = "Habitat quality index",
       y = "Crop yield [grain unit]") + 
  theme(text=element_text(size=34))

p4

# Save plot
ggsave(file = "plots/TO_agr_qual.png",
       width = 297, height = 150, units = "mm")


# Range of trade-offs: percentage variation between min and max
AgrPrd_var <- ((max(ordered$AgrPrd)/min(ordered$AgrPrd)) * 100) - 100
HabQlt_inc <- ((max(ordered$HabQlt)/min(ordered$HabQlt)) * 100) - 100

# Percentage increse/decrease from status quo
AgrPrd_decSQ <- ((StatusQuo$AgrPrd - min(ordered$AgrPrd)) / StatusQuo$AgrPrd) * 100
HabQlt_decSQ <- ((min(ordered$HabQlt) - StatusQuo$HabQlt) / StatusQuo$HabQlt) * 100



# 2-dimensional pareteo frontier between Water Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=3, column_B=1))

p5 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = WtrQlt), size = 2, color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = WtrQlt), shape = 25, size = 5, fill= "black", color = "gray50") +
  #geom_point(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred', size = 2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load [kg/y]") + 
  theme(text=element_text(size=34))

p5

# Save plot
ggsave(file = "plots/TO_wtr_conn.png",
       width = 297, height = 150, units = "mm")


# Range of trade-offs: percentage variation between min and max
WtrQlt_var <- 100 - ((max(ordered$WtrQlt)/min(ordered$WtrQlt)) * 100)
HabCnt_inc <- ((max(ordered$HabCnt)/min(ordered$HabCnt)) * 100) - 100

# Percentage increse/decrease from status quo
WtrQlt_decSQ <- ((StatusQuo$WtrQlt - min(ordered$WtrQlt)) / StatusQuo$WtrQlt) * 100
HabCnt_decSQ <- ((min(ordered$HabCnt) - StatusQuo$HabCnt) / StatusQuo$HabCnt) * 100



# 2-dimensional pareteo frontier between Water Quality and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=3, column_B=2))

p6 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = WtrQlt), size = 2, color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = WtrQlt), shape = 25, size = 5, fill= "black", color = "gray50") +
  #geom_point(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred', size = 2) +
  # Change names labels
  labs(x = "Habitat quality index",
       y = "Phorsphorus load [kg/y]") + 
  theme(text=element_text(size=34)) 

p6

# Save plot
ggsave(file = "plots/TO_wtr_qual.png",
       width = 297, height = 150, units = "mm")


# Range of trade-offs: percentage variation between min and max
WtrQlt_var <- 100 - ((max(ordered$WtrQlt)/min(ordered$WtrQlt)) * 100)
HabQlt_inc <- ((max(ordered$HabQlt)/min(ordered$HabQlt)) * 100) - 100

# Percentage increse/decrease from status quo
WtrQlt_decSQ <- ((StatusQuo$WtrQlt - min(ordered$WtrQlt)) / StatusQuo$WtrQlt) * 100
HabQlt_decSQ <- ((min(ordered$HabQlt) - StatusQuo$HabQlt) / StatusQuo$HabQlt) * 100



# 2-dimensional pareteo frontier between Habitat Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=2, column_B=1))

p7 <- ggplot() +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_hline(yintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$HabCnt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(y = HabCnt, x = HabQlt), size = 2, color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(y = HabCnt, x = HabQlt), shape = 25, size = 5, fill= "black", color = "gray50") +
  geom_line(data = ordered, aes(y = HabCnt, x = HabQlt), colour = 'darkred', size = 2) +
  #geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  # Change names labels
  labs(y = "Probability of connectivity",
       x = "Habitat quality index") + 
  theme(text=element_text(size=32))

p7

# Save plot
ggsave(file = "plots/TO_conn_qual.png",
       width = 297, height = 150, units = "mm")


# Range of trade-offs: percentage variation between min and max
HabCnt_inc <- ((max(ordered$HabCnt)/min(ordered$HabCnt)) * 100) - 100
HabQlt_inc <- ((max(ordered$HabQlt)/min(ordered$HabQlt)) * 100) - 100

# Percentage increse/decrease from status quo
HabCnt_decSQ <- ((min(ordered$HabCnt) - StatusQuo$HabCnt) / StatusQuo$HabCnt) * 100
HabQlt_decSQ <- ((min(ordered$HabQlt) - StatusQuo$HabQlt) / StatusQuo$HabQlt) * 100




## Save all the plot in one figure
patchwork <- p1 / 
  (p2 | p3 | p4) /
  (p5 | p6 | p7) 


# Add letters A and B to the plots
patchwork + plot_layout(ncol = 1, heights = c(12,5,5)) + plot_annotation(tag_levels = 'a')
patchwork + plot_layout(ncol = 1, heights = c(14,6,6)) + plot_annotation(tag_levels = 'a')

# # Save plot
ggsave(file = "plots/Fig_ParetoFontier_2103.png",
       width = 210, height = 250, units = "mm")
