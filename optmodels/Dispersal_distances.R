
###################################################

# Title: Mean dispersal distances distribution
# Purpose: This code is used to calculate the distribution of mean dispersal distances of three animal classes
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 23 April 2024

###################################################


# load required packages
library(tidyverse)
library(patchwork)


# Set working directory
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path = paste(getwd(),'DATA', sep="/")
path_input = paste(path, 'input',sep = "/")
path_plots = paste(path, "plots", sep = "/")
list.files(path_input)


# Quickly change size of all text in the plots
# theme_light(base_size = 14)


# Dispersal distances for calculation pij
# Upload table with dispersal distances collected from literature review
dd_meas <- read.csv(paste0(path_input,'/dd_measurements.csv'))


# Prepare data
# Transform as tibble
# Transform Class as factor 
dd_meas_fact <- dd_meas %>% 
  as_tibble() %>%
  mutate(Class = factor(Class,
                        levels = c("Insects", "Birds", "Small mammals")))


# Data visualization
# Plot histograms divided by classes
p1 <- dd_meas_fact %>%
  ggplot(aes(x = Mean.dispersal.distance)) +
  geom_histogram(aes(fill = Class), col = "black", linewidth = 0.25,
                 position = "identity", bins = 80, alpha = 0.4) +
  xlab("Mean dispersal distance") + 
  ylab("Count") +
  labs(x = "Mean dispersal distribution (m)",
       y = "Count") +
  #scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF")) +
  theme(legend.position = "none")


# Plot distribution divided by class
p2 <- dd_meas_fact %>%
  ggplot(aes(x = Mean.dispersal.distance, fill = Class)) +
  geom_density(alpha = 0.4, adjust = 5, linewidth = 0.25) +  # adjust is for curve smoothness
  labs(x = "Mean dispersal distribution (m)",
       y = "Density",
       fill = "Animal classes") +
  scale_fill_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF"))



# Plot distribution divided by class for figure methods
dd_meas_fact %>%
  ggplot(aes(x = Mean.dispersal.distance, fill = Class, color = Class)) +
  geom_density(alpha = 0.2, adjust = 5, linewidth = 0.25) +  # adjust is for curve smoothness
  labs(x = "Mean dispersal distribution (m)",
       y = "Density",
       fill = "Animal classes") +
  scale_fill_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF")) +
  scale_color_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))



#Plot histogram and distribution together
# dd_meas_fact %>%
#   ggplot(dd_meas, aes(x =Mean.dispersal.distance, color = Class, fill = Class)) +
#   geom_histogram(aes(y= after_stat(density)), position = "identity", bins = 80, alpha = 0.4) +
#   geom_density(alpha = 0.1, adjust = 5) +
#   scale_color_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF")) +
#   scale_fill_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF"))


# Plot plots close to one another
patchwork <- p1 + p2
# Add letters A and B to the plots
patchwork + plot_annotation(tag_levels = 'A')
# Save plot
ggsave(file = "plots/Mean_dispersal_distribution_mamm.png",
       width = 10, height = 5)




# Data analysis
# Repeat the analysis for each animal class draw density function and use the density to draw 100 random mean dispersal distribution 

# Insects
# Filter instcts animal class 
dd_meas_ins <- dd_meas_fact %>%
  filter(Class == "Insects") 

# Estimate density function
f.dens_ins <- density(dd_meas_ins$Mean.dispersal.distance, adjust = 5)

# Generate cumulative distribution function
cdf.estimate_ins <- cumsum(f.dens_ins$y) / cumsum(f.dens_ins$y)[length(f.dens_ins$y)]

#gen.sample_ins = replicate(N.draw, f.dens_ins$x[findInterval(runif(1), cdf.estimate_ins)+1])
# Create empty list 
gen.sample_ins <- c()

# Draw 1000 random values of mean dispersal distribution from
repeat{
  if(length(gen.sample_ins) == 1000) {break}
  x <- replicate(1, f.dens_ins$x[findInterval(runif(1), cdf.estimate_ins)+1])
  # attach to the empty list
  if (x>0) {gen.sample_ins <- c(gen.sample_ins,x)} 
}

# Plot histogram
hist(gen.sample_ins)

# Plot density curves of original and sample values
plot(f.dens_ins, col = "#440154FF", xlim= c(0, 18000), ylim = c(0, 0.0015))
lines(density(gen.sample_ins), col='red')
legend('topright',legend=c('Density Function estimate', 
                           'Generated sample density'), 
       col=c("#440154FF", "red"), 
       lty=c(1,1), 
       cex=1.5)



# Birds
# Filter amphibian animal class 
dd_meas_birds <- dd_meas_fact %>%
  filter(Class == "Birds") 

# Estimate density function
f.dens_birds <- density(dd_meas_birds$Mean.dispersal.distance, adjust = 5)

# Generate cumulative distribution function
cdf.estimate_birds <- cumsum(f.dens_birds$y) / cumsum(f.dens_birds$y)[length(f.dens_birds$y)]
#plot(cdf.estimate_birds, type = 'l', main = 'Cumulative distribution birds')

# Create empty list 
gen.sample_birds <- c()

# Draw 1000 random values of mean dispersal distribution from
repeat{
  if(length(gen.sample_birds) == 1000) {break}
  x <- replicate(1, f.dens_birds$x[findInterval(runif(1), cdf.estimate_birds)+1])
  # attach to the empty list
  if (x>0) {gen.sample_birds <- c(gen.sample_birds,x)} 
}

# Plot density curves of original and sample values
plot(f.dens_birds, col = "#21908CFF", xlim= c(0, 18000), ylim = c(0, 0.0015))
lines(density(gen.sample_birds), col='red')
legend('topright',legend=c('Density Function estimate', 
                           'Generated sample density'), 
       col=c("#21908CFF", "red"), 
       lty=c(1,1), 
       cex=1.5)



# Small mammals
# Filter amphibian animal class 
dd_meas_mamm <- dd_meas_fact %>%
  filter(Class == "Small mammals") 

# Estimate density function
f.dens_mamm <- density(dd_meas_mamm$Mean.dispersal.distance, adjust = 5)

# Generate cumulative distribution function
cdf.estimate_mamm <- cumsum(f.dens_mamm$y) / cumsum(f.dens_mamm$y)[length(f.dens_mamm$y)]
#plot(cdf.estimate_amph, type = 'l', main = 'Cumulative distribution amphibians')

# Create empty list 
gen.sample_mamm <- c()

# Draw 1000 random values of mean dispersal distribution from
repeat{
  if(length(gen.sample_mamm) == 1000) {break}
  x <- replicate(1, f.dens_mamm$x[findInterval(runif(1), cdf.estimate_mamm)+1])
  # attach to the empty list
  if (x>0) {gen.sample_mamm <- c(gen.sample_mamm,x)} 
}

# Plot density curves of original and sample values
plot(f.dens_mamm, col = "#440154FF", xlim= c(00, 18000), ylim = c(0, 0.0015))
lines(density(gen.sample_mamm), col='red')
legend('topright',legend=c('Density Function estimate', 
                           'Generated sample density'), 
       col=c("#440154FF", "red"), 
       lty=c(1,1), 
       cex=1.5)




# Individuate deciles for each animal classes
# Create empty dataframe
disp_dist <- data.frame(matrix(data=NA, nrow=27, ncol=4))
names(disp_dist) <- c("Class", "Decile" , "dd_m", "PC")

disp_dist[1:9, 1] <- c("Insects")
disp_dist[10:18, 1] <- c("Birds")
disp_dist[19:27, 1] <- c("Small mammals")

disp_dist[1:9, 2] <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
disp_dist[10:18, 2] <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
disp_dist[19:27, 2] <- c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")


# Calculate decile of dispersal distances and add to dataframe
disp_dist[1:9,3] <- quantile(gen.sample_ins, probs = seq(.1, .9, by = .1), na.rm = TRUE)
disp_dist[10:18, 3] <- quantile(gen.sample_birds, probs = seq(.1, .9, by = .1), na.rm = TRUE)
disp_dist[19:27, 3] <- quantile(gen.sample_mamm, probs = seq(.1, .9, by = .1), na.rm = TRUE)


# Write output
# Necessary as imput for connectivity index
z <- file.path(path, "disp_distances_07_12.csv")  
write.table(disp_dist, z, append=FALSE ,sep =";",col.names=TRUE ,row.names=FALSE)






##############################################################################


# Image for supplementary material
# Upload table with dispersal distances of inscets, birds, small mammals and amphibians
dd_meas_SM <- read.csv(paste0(path_input,'/dd_measurements_SM.csv'))


# Prepare data
# Transform as tibble
# Transform Class as factor 
dd_meas_fact <- dd_meas_SM %>% 
  as_tibble() %>%
  mutate(Class = factor(Class,
                        levels = c("Insects", "Amphibians", "Birds", "Small mammals")))


# Data visualization
# Plot histograms divided by classes
p1 <- dd_meas_fact %>%
  ggplot(aes(x = Mean.dispersal.distance)) +
  geom_histogram(aes(fill = Class), col = "black", linewidth = 0.25,
                 position = "identity", bins = 80, alpha = 0.4) +
  xlab("Mean dispersal distance") + 
  ylab("Count") +
  labs(x = "Mean dispersal distribution (m)",
       y = "Count") +
  #scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("#440154FF", "#39568CFF", "#21908CFF", "#FDE725FF")) +
  theme(legend.position = "none")


# Plot distribution divided by class
p2 <- dd_meas_fact %>%
  ggplot(aes(x = Mean.dispersal.distance, fill = Class)) +
  geom_density(alpha = 0.4, adjust = 5, linewidth = 0.25) +  # adjust is for curve smoothness
  labs(x = "Mean dispersal distribution (m)",
       y = "Density",
       fill = "Animal classes") +
  scale_fill_manual(values = c("#440154FF","#39568CFF", "#21908CFF", "#FDE725FF"))



#Plot histogram and distribution together
# dd_meas_fact %>%
#   ggplot(dd_meas, aes(x =Mean.dispersal.distance, color = Class, fill = Class)) +
#   geom_histogram(aes(y= after_stat(density)), position = "identity", bins = 80, alpha = 0.4) +
#   geom_density(alpha = 0.1, adjust = 5) +
#   scale_color_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF")) +
#   scale_fill_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF"))


# Plot plots close to one another
patchwork <- p1 + p2
# Add letters A and B to the plots
patchwork + plot_annotation(tag_levels = 'A')
# Save plot
ggsave(file = "plots/Mean_dispersal_distribution_SM.png",
       width = 10, height = 5)
