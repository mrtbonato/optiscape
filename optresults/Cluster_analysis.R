
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



# test different number of clusters
# Create empty dataframe to registet twss values
twss <- data.frame(matrix(data=NA, nrow=17, ncol=7))
names(twss) <- c("Ncluster", "twss1", "twss2", "twss3", "twss4", "twss5", "twss6")
twss$Ncluster <- c(4:20)

#1
for (n in 4:20) {
  # run k-means clusteing
  cl_analysis_1 <- kmeans(BS_fitness, n, iter.max = 10, nstart = 1)
  # Register twss values
  twss[n-3,2] <- cl_analysis_1$tot.withinss
}

#2
for (n in 4:20) {
  # run k-means clusteing
  cl_analysis_2 <- kmeans(BS_fitness, n, iter.max = 10, nstart = 1)
  # Register twss values
  twss[n-3,3] <- cl_analysis_2$tot.withinss
}

#3
for (n in 4:20) {
  # run k-means clusteing
  cl_analysis_3 <- kmeans(BS_fitness, n, iter.max = 10, nstart = 1)
  # Register twss values
  twss[n-3,4] <- cl_analysis_3$tot.withinss
}

#4
for (n in 4:20) {
  # run k-means clusteing
  cl_analysis_4 <- kmeans(BS_fitness, n, iter.max = 10, nstart = 1)
  # Register twss values
  twss[n-3,5] <- cl_analysis_4$tot.withinss
}

#5
for (n in 4:20) {
  # run k-means clusteing
  cl_analysis_5 <- kmeans(BS_fitness, n, iter.max = 10, nstart = 1)
  # Register twss values
  twss[n-3,6] <- cl_analysis_5$tot.withinss
}

#6
for (n in 4:20) {
  # run k-means clusteing
  cl_analysis_6 <- kmeans(BS_fitness, n, iter.max = 10, nstart = 1)
  # Register twss values
  twss[n-3,7] <- cl_analysis_6$tot.withinss
}

# save dataframes
#write.csv(cl_analysis_6$cluster, "cl_analysis_6.csv")


# pivot
twss_pivot <- twss %>%
  pivot_longer(cols = c("twss1", "twss2", "twss3", "twss4", "twss5", "twss6"))

ggplot(twss_pivot, aes(x = Ncluster, y = value, color = name, group = name)) +
  geom_point() +
  geom_line()




# Plot twss values and save plot
p6 <- ggplot(twss, aes(x = Ncluster, y = twss6)) +
  geom_point() +
  geom_line() +
  labs(x = "Number of Clusters",
       y = "twss") +
  theme_light()



patchwork <- (p1 | p2) /
  (p3 | p4) /
  (p5 | p6)
patchwork

ggsave(file = "plots/twss_patchwork.png",
       width = 210, height = 210, units = "mm")





# Cluster analysis
## Upload fitness of Best solutions
BS_fitness <- read.csv(paste0(path,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
## Rename columns
names(BS_fitness) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")

# k = 13

#kmeans(x, centers, iter.max = 10, nstart = 1)
cl_analysis <- kmeans(BS_fitness, 10, iter.max = 10, nstart = 1)

# extract clusters
cl_number <- cl_analysis$cluster

# join cluster no to best solutions
BS_fitness$cluster <- cl_number

# save
write.csv(BS_fitness, "clusters_k10.csv")



# Plot elipses clusters
BS_fitness <- read.csv('Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024/Baseline/clusters_k13.csv', h = T, as.is=T, sep = ",")


# Plot agriculture - water quality
kplot <- ggplot() +
  #theme_light() +
  #geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd), color = "grey") +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd, color = cluster)) +
  stat_ellipse(data = BS_fitness%>% filter(cluster != 0), aes(x = WtrQlt, y = AgrPrd, group = cluster, color = cluster)) +
  scale_color_distiller(palette = "Set2") +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  theme(text=element_text(size=18), legend.position = "none")


 ggsave(file = "plots/clusters_k13_0708.png",
        width = 297, height = 210, units = "mm")
 
 
 
# plot for poster conference
 # Create dataframe with status quo value
 StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=4))
 names(StatusQuo) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
 StatusQuo[1,] <- c(0.00161, 0.0508, -6038.0, 59083.86)
 
 BS_fitness <-  BS_fitness %>%
   mutate(cluster2 = case_when(
     cluster == 1 ~ 0,
     cluster == 2 ~ 0,
     cluster == 3 ~ 0,
     cluster == 4 ~ 0,
     cluster == 5 ~ 0,
     cluster == 6 ~ 6,
     cluster == 7 ~ 7,
     cluster == 8 ~ 0,
     cluster == 9 ~ 9,
     cluster == 10 ~ 0,
     cluster == 11 ~ 0,
     cluster == 12 ~ 12,
     cluster == 13 ~ 0,
     ))
 
 
ggplot() +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt), shape = 21, color = "black") +
  scale_fill_viridis_c(name = "Probability of connectivity") +
  geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  ggnewscale::new_scale_fill()+
  stat_ellipse(data = BS_fitness%>% filter(cluster2 != 0), aes(x = WtrQlt, y = AgrPrd, group = cluster2), color = "black", lwd = 1.2) +
  #scale_color_distiller(palette = "Set2") +
   # Change names labels
   labs(x = "Phorsphorus load [kg/year]",
        y = "Crop yield [grain unit]") +
      scale_size(name = "Habitat quality") + 
   #scale_shape_manual(values = c(21, 21, 21, 21)) +
   theme(text=element_text(size=18))
 #geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))
 

 # Save plot
 ggsave(file = "plots/ParetoFrontiers_1408_poster.png",
        width = 297, height = 210, units = "mm")



################################
# Create subsets based on cluster code
BS_fitness_sub1 <- BS_fitness %>%
  filter(cluster == 1)
BS_fitness_sub2 <- BS_fitness %>%
  filter(cluster == 2)
BS_fitness_sub3 <- BS_fitness %>%
  filter(cluster == 3)
BS_fitness_sub4 <- BS_fitness %>%
  filter(cluster == 4)
BS_fitness_sub5 <- BS_fitness %>%
  filter(cluster == 5)
BS_fitness_sub6 <- BS_fitness %>%
  filter(cluster == 6)
# Max env - Min AP
BS_fitness_sub7 <- BS_fitness %>%
  filter(cluster == 7)
BS_fitness_sub8 <- BS_fitness %>%
  filter(cluster == 8)
BS_fitness_sub9 <- BS_fitness %>%
  filter(cluster == 9)
BS_fitness_sub10 <- BS_fitness %>%
  filter(cluster == 10)
BS_fitness_sub11 <- BS_fitness %>%
  filter(cluster == 11)
# max AP - min env
BS_fitness_sub12 <- BS_fitness %>%
  filter(cluster == 12)
BS_fitness_sub13 <- BS_fitness %>%
  filter(cluster == 13)


# Plot agriculture - water quality
ggplot() +
  #theme_light() +
  geom_point(data = BS_fitness, aes(x = WtrQlt, y = AgrPrd), color = "grey") +
  geom_point(data = BS_fitness[which(BS_fitness$cluster == 10),], aes(x = WtrQlt, y = AgrPrd), color = "red") +
  stat_ellipse(data = BS_fitness%>% filter(cluster != 0), aes(x = WtrQlt, y = AgrPrd, group = cluster, color = cluster)) +
  scale_color_distiller(palette = "Dark2") +
  # Change names labels
  labs(x = "Phorsphorus load",
       y = "Crop yield") +
  theme(text=element_text(size=18), legend.position = "none")











  