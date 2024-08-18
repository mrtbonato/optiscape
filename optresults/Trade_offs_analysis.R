
###################################################

# Title: Trade_offs_analysis
# Purpose: This code is used to analyse the trade-offs: magnitude and Pareto-frontier
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
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Probability of connectivity") +
  scale_size(name = "Habitat quality") + 
  #scale_shape_manual(values = c(21, 21, 21, 21)) +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))

p1


# Save plot
ggsave(file = "plots/ParetoFrontier_baseline.png",
       width = 297, height = 210, units = "mm")





### Plot trade-offs among single couples of objectives

# In case we want the elipses of the clusters
# Upload Best solutions with cluster number
BS_fitness <- read.csv(paste0(path,'/clusters_k13.csv'), h = T, as.is=T, sep = ",")


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
  #theme_light() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = -6038.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd), color = "#21908CFF") +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #stat_ellipse(data = BS_fitness %>% filter(cluster != 0), aes(x = WtrQlt, y = AgrPrd, group = cluster, color = cluster)) +
  scale_color_distiller(palette = "Dark2") +
  #geom_point(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Agricultural production and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=1))

# colnames(BS_fitness)
p3 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = AgrPrd), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "1"),], aes(x = HabCnt, y = AgrPrd), color = "red") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "2"),], aes(x = HabCnt, y = AgrPrd), color = "orange") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "3"),], aes(x = HabCnt, y = AgrPrd), color = "yellow") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "4"),], aes(x = HabCnt, y = AgrPrd), color = "green") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "5"),], aes(x = HabCnt, y = AgrPrd), color = "blue") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "6"),], aes(x = HabCnt, y = AgrPrd), color = "lightblue") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "7"),], aes(x = HabCnt, y = AgrPrd), color = "violet") +
  #stat_ellipse(data = BS_fitness %>% filter(cluster != 0), aes(x = HabCnt, y = AgrPrd, group = cluster, color = cluster)) +
  #scale_color_distiller(palette = "Dark2") +
  #geom_point(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  #geom_line(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield") + 
  theme(text=element_text(size=18))



# 2-dimensional pareteo frontier between Agricultural production and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=2))

p4 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = AgrPrd), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Crop yield") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Water Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=3, column_B=1))

p5 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = WtrQlt), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Water Quality and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=3, column_B=2))

p6 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = WtrQlt), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18)) 


# 2-dimensional pareteo frontier between Habitat Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=2, column_B=1))

p7 <- ggplot() +
  geom_hline(yintercept = 0.0508, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$HabQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = HabQlt), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = HabQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Habitat quality") + 
  theme(text=element_text(size=18))


# Plot plots close to one another
patchwork4 <- (p2 | p3 | p4) /
  (p7 | p5 | p6)
patchwork4

# Save plot
ggsave(file = "plots/TO_winwinarea_Baseline.png",
       width = 297, height = 210, units = "mm")






## Put together 2 plots
patchwork5 <- p1 / 
  (p2 | p3 | p4) /
  (p5 | p6 | p7) 


# Add letters A and B to the plots
patchwork5 + plot_layout(ncol = 1, heights = c(12,5,5)) + plot_annotation(tag_levels = 'a')
patchwork5 + plot_layout(ncol = 1, heights = c(14,6,6)) + plot_annotation(tag_levels = 'a')

# # Save plot
ggsave(file = "plots/Fig_ParetoFontier_2103.png",
       width = 210, height = 250, units = "mm")




# 3d plots
# 2-dimensional pareteo frontier between Agricultural production and Water Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=3))

p21 <- ggplot() +
  theme_light() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = -6038.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd, color = HabCnt)) +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #stat_ellipse(data = BS_fitness_sub %>% filter(sub != 0), aes(x = WtrQlt, y = AgrPrd, group = sub, color = sub)) +
  #geom_point(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  scale_color_distiller(palette = "Greens", direction = 1, name = "Probability of connectivity") +
  #scale_color_viridis_c(name = "Probability of connectivity") +
  geom_line(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.35,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size


p22 <- ggplot() +
  #theme_light() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = -6038.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd, color = HabQlt)) +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  scale_color_distiller(palette = "Purples", direction = 1, name = "Habitat quality") +
  #scale_color_viridis_c(name = "Habitat quality") +
  geom_line(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.25,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size




# 2-dimensional pareteo frontier between Agricultural production and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=1))

p31 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = AgrPrd, color = WtrQlt)) +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  scale_color_distiller(palette = "Blues", direction = 1, name = "Phosporus load") +
  #scale_color_viridis_c(name = "Phorsporus load") +
  geom_line(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.25,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size


p32 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = AgrPrd, color = HabQlt)) +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  scale_color_distiller(palette = "Purples", direction = 1, name = "Habitat Quality") +
  #scale_color_viridis_c(name = "Habitat Quality") +
  geom_line(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.25,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size




# 2-dimensional pareteo frontier between Agricultural production and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=4, column_B=2))

p41 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = AgrPrd, color = WtrQlt)) +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  scale_color_distiller(palette = "Blues", direction = 1, name = "Phosporus load") +
  #scale_color_viridis_c(name = "Phorsporus load") +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Crop yield") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.25,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size


p42 <- ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = AgrPrd, color = HabCnt)) +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  scale_color_distiller(palette = "Greens", direction = 1, name = "Probability of connectivity") +
  #scale_color_viridis_c(name = "Habitat connectivity") +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Crop yield") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.25,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size



# 2-dimensional pareteo frontier between Water Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=3, column_B=1))

p51 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = WtrQlt, color = AgrPrd)) +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  scale_color_distiller(palette = "YlOrBr", name = "Crop Yield") +
  #scale_color_viridis_c(name = "Crop Yield") +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.8,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size


p52 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = WtrQlt, color = HabQlt)) +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  scale_color_distiller(palette = "Purples", direction = 1, name = "Habitat quality") +
  #scale_color_viridis_c(name = "Habitat Quality") +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.8,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size



# 2-dimensional pareteo frontier between Water Quality and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=3, column_B=2))

p61 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = WtrQlt, color = AgrPrd)) +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  scale_color_distiller(palette = "YlOrBr", name = "Crop Yield") +
  #scale_color_viridis_c(name = "Crop Yield") +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.8,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size


p62 <- ggplot() +
  geom_hline(yintercept = -6038.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabQlt, y = WtrQlt, color = HabCnt)) +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  scale_color_distiller(palette = "Greens", direction = 1, name = "Probability of connectivity") +
  #scale_color_viridis_c(name = "Probability of Connectivity") +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.8,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size




# 2-dimensional pareteo frontier between Habitat Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BS_fitness, column_A=2, column_B=1))

p71 <- ggplot() +
  geom_hline(yintercept = 0.0508, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$HabQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = HabQlt, color = AgrPrd)) +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = HabQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  #geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  scale_color_distiller(palette = "YlOrBr", name = "Crop Yield") +
  # scale_color_viridis_c(name = "Crop Yield") +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Habitat quality") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.8,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size


p72 <- ggplot() +
  geom_hline(yintercept = 0.0508, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$HabQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness, aes(x = HabCnt, y = HabQlt, color = WtrQlt)) +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = HabQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  #geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  scale_color_distiller(palette = "Blues", direction = 1, name = "Phosporus load") +
  #scale_color_viridis_c(name = "Phosporus Load") +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Habitat quality") + 
  theme(text=element_text(size=18), #change text size
        legend.position = c(0.8,0.25),#change legend position
        legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=9)) #change legend text font size


# Plot plots close to one another
patchwork4 <- (p21 | p31 | p41) /
  (p22 | p32 | p42) 
patchwork4

# Save plot
ggsave(file = "plots/Trade_offs_Baseline_color.png",
       width = 297, height = 210, units = "mm")


patchwork5 <- (p51 | p61 | p71) /
  (p52 | p62 | p72)
patchwork5

# Save plot
ggsave(file = "plots/Sinergies_Baseline_color.png",
       width = 297, height = 210, units = "mm")






library(ggbiplot)


BS_fitness_conn <- BS_fitness %>%
filter(AgrPrd >= 58900)


X11()
plot(x = BS_fitness_conn$HabCnt, y = BS_fitness_conn$AgrPrd)

#with(BS_fitness_conn, ggbiplot(HabCnt, AgrPrd))


selectedPoints <- fhs(BS_fitness_conn) 



ggplot() +
  geom_hline(yintercept = 59083.86, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness_conn, aes(x = HabCnt, y = AgrPrd), color = "#21908CFF") +
  #geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "1"),], aes(x = HabCnt, y = AgrPrd), color = "red") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "2"),], aes(x = HabCnt, y = AgrPrd), color = "orange") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "3"),], aes(x = HabCnt, y = AgrPrd), color = "yellow") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "4"),], aes(x = HabCnt, y = AgrPrd), color = "green") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "5"),], aes(x = HabCnt, y = AgrPrd), color = "blue") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "6"),], aes(x = HabCnt, y = AgrPrd), color = "lightblue") +
  #geom_point(data = BS_fitness[which(BS_fitness$cluster == "7"),], aes(x = HabCnt, y = AgrPrd), color = "violet") +
  #stat_ellipse(data = BS_fitness %>% filter(cluster != 0), aes(x = HabCnt, y = AgrPrd, group = cluster, color = cluster)) +
  #scale_color_distiller(palette = "Dark2") +
  #geom_point(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  #geom_line(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
# Change names labels
labs(x = "Probability of connectivity",
     y = "Crop yield") + 
  theme(text=element_text(size=18))



