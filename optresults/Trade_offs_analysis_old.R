
###################################################

# Title: Trade_offs_analysis
# Purpose: This code is used to analyse the trade-offs: magnitude and Pareto-frontier
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 25 April 2024

###################################################


## Upload required packages
library(tidyverse)
library(sf)
library(patchwork)
library(plotly)
library(tmap)



## Set working directories
# Working directory for land use map
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path = paste(getwd(),'DATA', sep="/")
path_input = paste(path, 'input',sep="/")
list.files(path_input)

# Working directory for optimization result
path_res = paste(getwd(),'CoMOLA_results_2202', sep="/")
list.files(path_res)




# PREPARE DATA
## Upload and clean Best Solutions
BestSol <- read.csv(paste0(path_res,'/22-02-2024_14-37-39_best_solutions.csv'), h = F, skip=1, as.is=T)

# Remove parenthesis
BestSol2 <- BestSol %>%
  mutate(V1 = as.integer(str_replace_all(string = BestSol$V1,
                                         pattern = "[\\[\\]]",
                                         replacement = ""))) %>%
  mutate(V301 = as.integer(str_replace_all(string = BestSol$V301,
                                           pattern = "[\\[\\]]",
                                           replacement = ""))) %>%
  mutate(V302 = as.numeric(str_replace_all(string = BestSol$V302,
                                           pattern = "[\\[\\]]",
                                           replacement = ""))) %>%
  mutate(V303 = as.numeric(str_replace_all(string = BestSol$V303,
                                           pattern = "[\\[\\]]",
                                           replacement = ""))) %>%
  mutate(V304 = as.numeric(str_replace_all(string = BestSol$V304,
                                           pattern = "[\\[\\]]",
                                           replacement = ""))) %>%
  mutate(V305 = as.numeric(str_replace_all(string = BestSol$V305,
                                           pattern = "[\\[\\]]",
                                           replacement = "")))


## Select data for Pareto frontier analysis
# Select only fitness values
BestSol_fit <- BestSol2[, 302:305]
names(BestSol_fit) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


# Create dataframe with status quo value
StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=4))
names(StatusQuo) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
StatusQuo[1,] <- c(0.00161, 0.0508, -8630.0, 54306.687)





# ANALYSIS MAGNITUDE OF THE TRADE-OFF CURVE
### Percentage variation between min and max
# Variation AgrPrd
AgrPrd_var <- ((min(BestSol_fit$AgrPrd)/max(BestSol_fit$AgrPrd)) * 100) - 100
# Increase WtrQlt
WtrQlt_var <- 100 - ((max(BestSol_fit$WtrQlt)/min(BestSol_fit$WtrQlt)) * 100)
# Increase HbtCnt
HabCnt_var <- ((max(BestSol_fit$HabCnt)/min(BestSol_fit$HabCnt)) * 100) - 100
# Increase HbtQlt
HabQlt_var <- ((max(BestSol_fit$HabQlt)/min(BestSol_fit$HabQlt)) * 100) - 100


## Variation between min and max
# Decrease AgrPrd
AgrPrd_dec <- min(BestSol_fit$AgrPrd) - max(BestSol_fit$AgrPrd)
# Increase WtrQlt
WtrQlt_inc <- min(BestSol_fit$WtrQlt) - max(BestSol_fit$WtrQlt)
# Increase HbtCnt
HabCnt_inc <- max(BestSol_fit$HabCnt) - min(BestSol_fit$HabCnt)
# Increase HbtQlt
HabQlt_inc <- max(BestSol_fit$HabQlt) - min(BestSol_fit$HabQlt) 


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
  geom_point(data = BestSol_fit, aes(x = WtrQlt, y = AgrPrd, color = HabCnt, size = HabQlt)) +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 2, fill= "magenta", color = "magenta") +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield") +
  scale_color_viridis_c(name = "Probability of connectivity") +
  scale_size(name = "Habitat quality") + 
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))

p1


# Save plot
ggsave(file = "plots/ParetoFrontiers_0404_newrun.png",
       width = 297, height = 210, units = "mm")



# Alternative 3D plot
# plot_ly(x=BestSol3$WtrQlt, y=BestSol3$HabCnt, z=BestSol3$AgrPrd, type="scatter3d", mode="markers", color=BestSol3$HabQlt) %>%
#   layout(scene = list(xaxis = list(title = 'Phorsphorus load'),
#                       yaxis = list(title = 'Probability of connectivity'),
#                       zaxis = list(title = 'Agricultural production')),
#          annotations = list(
#            x = 1,
#            y = 1,
#            text = 'Habitat quality',
#            xref = 'paper',
#            yref = 'paper',
#            showarrow = FALSE))




  
### Plot trade-offs among single couples of objectives

# With plotting of the Pareto frontier for each couple of considered objectives (red line)
# Create line 
pareteo.two.dim<-function(data = BestSol_fit, column_A, column_B){
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
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=4, column_B=3))

p2 <- ggplot() +
  #theme_light() +
  geom_hline(yintercept = 54306.887, color = "gray50") +
  geom_vline(xintercept = -8630.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BestSol_fit, aes(x = WtrQlt, y = AgrPrd), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #stat_ellipse(data = BestSol_fit_sub %>% filter(sub != 0), aes(x = WtrQlt, y = AgrPrd, group = sub, color = sub)) +
  #geom_point(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Agricultural production and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=4, column_B=1))

# colnames(BestSol_fit)
p3 <- ggplot() +
  geom_hline(yintercept = 54306.887, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BestSol_fit, aes(x = HabCnt, y = AgrPrd), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Agricultural production and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=4, column_B=2))

p4 <- ggplot() +
  geom_hline(yintercept = 54306.887, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BestSol_fit, aes(x = HabQlt, y = AgrPrd), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = AgrPrd), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Crop yield") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Water Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=3, column_B=1))

p5 <- ggplot() +
  geom_hline(yintercept = -8630.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BestSol_fit, aes(x = HabCnt, y = WtrQlt), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabCnt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Water Quality and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=3, column_B=2))

p6 <- ggplot() +
  geom_hline(yintercept = -8630.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BestSol_fit, aes(x = HabQlt, y = WtrQlt), color = "#21908CFF") +
  geom_point(data = StatusQuo, aes(x = HabQlt, y = WtrQlt), shape = 25, size = 2, fill= "#21908CFF", color = "#21908CFF") +
  #geom_point(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Phorsphorus load") + 
  theme(text=element_text(size=18)) 


# 2-dimensional pareteo frontier between Habitat Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=2, column_B=1))

p7 <- ggplot() +
  geom_hline(yintercept = 0.0508, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$HabQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BestSol_fit, aes(x = HabCnt, y = HabQlt), color = "#21908CFF") +
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
ggsave(file = "plots/TO_winwinarea_0404_newrun.png",
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






###############################################################################
# Alternatives
# Plot without red line

# Plot agriculture - water quality
p2 <- ggplot() +
  #theme_light() +
  geom_point(data = BestSol3, aes(x = WtrQlt, y = AgrPrd), color = "#21908CFF") +
  geom_hline(yintercept = 3763.081, color = "gray50") +
  geom_vline(xintercept = -9127.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield")


# Plot agriculture - habitat connectivity
p3 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabCnt, y = AgrPrd), color = "#21908CFF") +
  geom_hline(yintercept = 3763.081, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield")


# Plot agriculture - habitat quality
p4 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabQlt, y = AgrPrd), color = "#21908CFF") +
  geom_hline(yintercept = 3763.081, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Crop yield")


# Plot water quality - habitat connectivity
p5 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabCnt, y = WtrQlt), color = "#21908CFF") +
  geom_hline(yintercept = -9127.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load")


# Plot water quality - habitat connectivity
p6 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabQlt, y = WtrQlt), color = "#21908CFF") +
  geom_hline(yintercept = -9127.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Phorsphorus load") 


# Plot habitat quality - habitat connectivity
p7 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabCnt, y = HabQlt), color = "#21908CFF") +
  geom_hline(yintercept = 0.0508, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$HabQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Habitat quality")


# Plot plots close to one another
patchwork <- (p2 | p3 | p4) /
  (p5 | p6 | p7)
patchwork





### Additional plotting of sub-optimal solutions (grey)
# Upload and clean Best Solutions
RandSol <- read.csv(paste0(path_res,'/Random_Sol.csv'), header = FALSE)

# Remove parenthesis
RandSol2 <- RandSol %>%
  mutate(V3 = as.numeric(str_replace_all(string = RandSol$V3,
                                         pattern = "[\\[\\]]",
                                         replacement = ""))) %>%
  mutate(V6 = as.numeric(str_replace_all(string = RandSol$V6,
                                         pattern = "[\\[\\]]",
                                         replacement = "")))

## Select data for Pareto frontier analysis
# Select only fitness values
RandSol_fit <- RandSol2[, 3:6]
names(RandSol_fit) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")



# Plot agriculture - water quality
p2 <- ggplot() +
  #theme_light() +
  geom_point(data = RandSol_fit, aes(x = WtrQlt, y = AgrPrd), color = "lightgrey") +
  geom_point(data = BestSol3, aes(x = WtrQlt, y = AgrPrd), color = "#21908CFF") +
  geom_hline(yintercept = 3763.081, color = "gray50") +
  geom_vline(xintercept = -9127.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield")


# Plot agriculture - habitat connectivity
p3 <- ggplot() +
  geom_point(data = RandSol_fit, aes(x = HabCnt, y = AgrPrd), color = "lightgrey") +
  geom_point(data = BestSol3, aes(x = HabCnt, y = AgrPrd), color = "#21908CFF") +
  geom_hline(yintercept = 3763.081, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield")


# Plot agriculture - habitat quality
p4 <- ggplot() +
  geom_point(data = RandSol_fit, aes(x = HabQlt, y = AgrPrd), color = "lightgrey") +
  geom_point(data = BestSol3, aes(x = HabQlt, y = AgrPrd), color = "#21908CFF") +
  geom_hline(yintercept = 3763.081, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Crop yield")


# Plot water quality - habitat connectivity
p5 <- ggplot() +
  geom_point(data = RandSol_fit, aes(x = HabCnt, y = WtrQlt), color = "lightgrey") +
  geom_point(data = BestSol3, aes(x = HabCnt, y = WtrQlt), color = "#21908CFF") +
  geom_hline(yintercept = -9127.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load")


# Plot water quality - habitat connectivity
p6 <- ggplot() +
  geom_point(data = RandSol_fit, aes(x = HabQlt, y = WtrQlt), color = "lightgrey") +
  geom_point(data = BestSol3, aes(x = HabQlt, y = WtrQlt), color = "#21908CFF") +
  geom_hline(yintercept = -9127.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Phorsphorus load") 


# Plot habitat quality - habitat connectivity
p7 <- ggplot() +
  geom_point(data = RandSol_fit, aes(x = HabCnt, y = HabQlt), color = "lightgrey") +
  geom_point(data = BestSol3, aes(x = HabCnt, y = HabQlt), color = "#21908CFF") +
  geom_hline(yintercept = 0.0508, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$HabQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Habitat quality")


# Plot plots close to one another
patchwork3 <- (p2 | p3 | p4) /
  (p5 | p6 | p7)
patchwork3






# Plot also number of AEP implemented

# Create line 
pareteo.two.dim<-function(data = BestSol_fit, column_A, column_B){
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
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=4, column_B=3))

p2 <- ggplot() +
  #theme_light() +
  geom_point(data = BestSol3, aes(x = WtrQlt, y = AgrPrd, color = AEP_count)) +
  geom_hline(yintercept = 54306.887, color = "gray50") +
  geom_vline(xintercept = -8630.0, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$WtrQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  #geom_point(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= WtrQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield") +
  scale_color_viridis_c(name = "AEP count") + 
  theme(text=element_text(size=18), legend.position = "none")


# 2-dimensional pareteo frontier between Agricultural production and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=4, column_B=1))

colnames(BestSol_fit)
p3 <- ggplot() +
  geom_point(data = BestSol_fit, aes(x = HabCnt, y = AgrPrd, color = AEP_count)) +
  geom_hline(yintercept = 54306.887, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  #geom_point(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield") +
  scale_color_viridis_c(name = "AEP count") + 
  theme(text=element_text(size=18), legend.position = "none")


# 2-dimensional pareteo frontier between Agricultural production and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=4, column_B=2))

p4 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabQlt, y = AgrPrd, color = AEP_count)) +
  geom_hline(yintercept = 54306.887, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  #geom_point(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt, y = AgrPrd), colour = 'darkred') +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Crop yield") +
  scale_color_viridis_c(name = "AEP count") + 
  theme(text=element_text(size=18))


# 2-dimensional pareteo frontier between Water Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=3, column_B=1))

p5 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabCnt, y = WtrQlt, color = AEP_count)) +
  geom_hline(yintercept = -8630.0, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  #geom_point(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabCnt, y = WtrQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Phorsphorus load") +
  scale_color_viridis_c(name = "AEP count") + 
  theme(text=element_text(size=18), legend.position = "none")


# 2-dimensional pareteo frontier between Water Quality and Habitat Quality
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=3, column_B=2))

p6 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabQlt, y = WtrQlt, color = AEP_count)) +
  geom_hline(yintercept = -8630.0, color = "gray50") +
  geom_vline(xintercept = 0.0508, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabQlt, xmax = Inf,
                ymin = StatusQuo$WtrQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  #geom_point(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  geom_line(data = ordered, aes(x= HabQlt , y = WtrQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Habitat quality",
       y = "Phorsphorus load") +
  scale_color_viridis_c(name = "AEP count") + 
  theme(text=element_text(size=18), legend.position = "none")


# 2-dimensional pareteo frontier between Habitat Quality and Habitat Connectivity
ordered <- suppressWarnings(pareteo.two.dim(data=BestSol_fit, column_A=2, column_B=1))

p7 <- ggplot() +
  geom_point(data = BestSol3, aes(x = HabCnt, y = HabQlt, color = AEP_count)) +
  geom_hline(yintercept = 0.0508, color = "gray50") +
  geom_vline(xintercept = 0.00161, color = "gray50") +
  geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
                ymin = StatusQuo$HabQlt, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  #geom_line(data = ordered, aes(x= HabCnt, y = HabQlt), colour = 'darkred') +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Habitat quality") +
  scale_color_viridis_c(name = "AEP count") + 
  theme(text=element_text(size=18), legend.position = "none")


# Plot plots close to one another
patchwork4 <- (p2 | p3 | p4) /
  (p7 | p5 | p6)
patchwork4

# Save plot
ggsave(file = "plots/AEP_count.png",
       width = 360, height = 210, units = "mm")

