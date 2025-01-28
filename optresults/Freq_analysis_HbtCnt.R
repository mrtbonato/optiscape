###################################################

# Title: Frequency Analysis - Habitat Connectivity
# Purpose: This code is used to analyse the frequency of AEP implementation for 2 subsets of the Pareto-optimal solutions (low and high Habitat Connectivity values)
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 28 January 2025

###################################################


## Upload required packages
library(tidyverse)
library(sf)
library(patchwork)
library(plotly)
library(tmap)
library(units) # for drop units


##  Set working directories
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path = paste(getwd(),'CoMOLA_results_june2024/Baseline', sep="/")
path_input = paste(getwd(),'DATA/input', sep="/")


## Fitness Best Solutions
# Upload fitness of Best solutions
BS_fitness <- read.csv(paste0(path,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")
# Rename columns
names(BS_fitness) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
# Add id column for join
BS_fitness$X <- c(1:1070)
#BS_fitness$X <- c(1:1246)


## Genome Best Solutions
# Upload genome of Best solutions 
BS_genome <- read.csv(paste0(path,'/BS_genomes.csv'), h = F, as.is=T, sep = ";")
# Add id column
BS_genome <- BS_genome %>%
  mutate(id = c(1:302), .before = V1)


# # Invert rows and columns
BS_genome <- data.frame(t(BS_genome[-1]))

# Add id
BS_genome <- BS_genome %>%
  mutate(X = c(1:1070), .before = X1)

# # Join with fitness values
BS_genome_join <- BS_fitness %>%
   left_join(BS_genome, by = "X")





# Select solutions over a certain value of agricultural production
BS_fitness_sub <- BS_genome_join %>%
  filter(AgrPrd >= 58900)


# Create 2 clusters - 1 low connectivity, 1 high connectivity
BS_fitness_sub <- BS_fitness_sub %>%
  mutate(cluster = case_when(
    
    (HabQlt < 0.052 & HabCnt < 0.00168) ~ 1,
    (HabQlt >= 0.052 & HabQlt < 0.0525 & HabCnt < 0.00170) ~ 1,
    (HabQlt >= 0.0525 & HabQlt < 0.053 & HabCnt < 0.001725) ~ 1,
    (HabQlt >= 0.053 & HabQlt < 0.0535 & HabCnt < 0.00174) ~ 1,
    (HabQlt >= 0.0535 & HabQlt < 0.054 & HabCnt < 0.001752) ~ 1,
    (HabQlt >= 0.0542 & HabCnt < 0.0018) ~ 1,
  ))

BS_fitness_sub$cluster[is.na(BS_fitness_sub$cluster)] <- 2




### PLOT PARETO FRONTIER ######################################################
# for Habitat connectivity and quality
ggplot() +
  geom_point(data = BS_genome_join, aes(x = HabCnt, y = AgrPrd), color = "grey", size = 2) +
  geom_point(data = BS_fitness_sub, aes(x = HabCnt, y = AgrPrd, color = cluster), size = 2) +
  scale_color_distiller(palette = "Set2") +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield [grain unit]") + 
  theme(text=element_text(size=26), 
        legend.position = 'none') 


# for Habitat connectivity and agricultural production
ggplot() +
  geom_point(data = BS_genome_join, aes(x = HabCnt, y = HabQlt), color = "grey", size = 2) +
  geom_point(data = BS_fitness_sub, aes(x = HabCnt, y = HabQlt, color = cluster), size = 2) +
  scale_color_distiller(palette = "Set2") +
    # Change names labels
  labs(x = "Probability of connectivity",
       y = "Habitat quality") + 
  theme(text=element_text(size=26), 
        legend.position = 'none') 


# Save plot
ggsave(file = paste0("plot_highHbtCnt_HabQlt.png"),
       width = 210, height = 210, units = "mm")




### FREQUENCY ANALYSIS AND MAPS ###############################################

# Create list of measure typologies
meas <- c("lowtillcc", "pond", "grassslope", "buffer", "hedge", "fallow")


# loop through clusters
for (n in 1:2){
  
  BS_genome_cl <- BS_fitness_sub %>%
    filter(cluster == n) 
  BS_genome_cl <- BS_genome_cl %>%
    select(c(7:length(BS_genome_cl)))
  
  nrows <- nrow(BS_genome_cl)
  
  # Invert rows and columns
  BS_genome2 <- data.frame(t(BS_genome_cl))
  
  # Add id column
  BS_genome2 <- BS_genome2 %>%
    mutate(id = c(1:302), .before = X1)
  
  
  # Join Best Solution to genome 
  ##### When analyzing the subsets chose here the subset of Best_Sol2_2 to analyse and to join with genome_hru_separate ####
  Impl_AEP <- genome_hru_separate %>%
    left_join(BS_genome2, by = "id") 
  
  # Pivot 
  Impl_AEP_pivot <- Impl_AEP %>%
    pivot_longer(cols = c("code1", "code2", "code3", "code4", "code5", "code6", "code7", "code8", "code9", "code10", "code11", "code12", "code13", "code14", "code15", "code16", "code17", "code18", "code19", "code20", "code21", "code22", "code23", "code24", "code25", "code26", "code27", "code28", "code29", "code30", "code31", "code32", "code33", "code34", "code35"), names_to = "code", values_to = "name_new") %>%
    relocate(name_new, .after = obj_id)%>%
    drop_na(name_new)
  
  
  # Eliminate space before some "name_new"
  Impl_AEP_pivot$name_new <- str_remove(Impl_AEP_pivot$name_new, " ")
  
  
  # Assign priority to AEP
  Impl_AEP_pivot_prior <- Impl_AEP_pivot %>%
    mutate(priority = case_when(
      nswrm == "hedge" ~ 2,
      nswrm == "buffer" ~ 3,
      nswrm == "grassslope" ~ 4,
      nswrm == "pond" ~ 1,
      nswrm == "lowtillcc" ~ 5), .after = nswrm) %>%
    # order data frame based on priority
    arrange(priority)
  
  
  
  # Define polygons with 1/2/3 measures implemented 
  # Identify polygons with more than one implementable measure
  # Identify duplicate polygons code
  Impl_AEP_subset <- subset(Impl_AEP_pivot_prior, duplicated(name_new)) %>%
    select(name_new)
  Impl_AEP_multimeas <- unique(Impl_AEP_subset)
  
  # Polygons with 3 implementable measures
  Impl_AEP_3 <- Impl_AEP_subset %>%
    subset(duplicated(name_new)) %>%
    mutate(multimeas = "3")
  
  Impl_AEP_3meas <- Impl_AEP_pivot_prior %>%
    right_join(Impl_AEP_3, by = "name_new") 
  
  # Impl_AEP_3meas_sum <- Impl_AEP_3meas %>% 
  #   group_by(name_new) %>% 
  #   summarise_if(is.numeric, sum, na.rm = TRUE)
  
  
  # Polygons with 2 implementable measures
  Impl_AEP_multimeas <- Impl_AEP_multimeas %>%
    left_join(Impl_AEP_3, by = "name_new")
  
  Impl_AEP_2 <- Impl_AEP_multimeas %>%
    filter(is.na(multimeas)) %>%
    mutate(multimeas = "2")
  
  Impl_AEP_2meas <- Impl_AEP_pivot_prior %>%
    right_join(Impl_AEP_2, by = "name_new") 
  
  # Impl_AEP_2meas_sum <- Impl_AEP_2meas %>% 
  #   group_by(name_new) %>% 
  #   summarise_if(is.numeric, sum, na.rm = TRUE)
  
  
  # Polygons with 1 implementable measure
  Impl_AEP_multimeas <- Impl_AEP_2 %>%
    rbind(Impl_AEP_3)
  
  x <- Impl_AEP_pivot_prior %>% 
    left_join(Impl_AEP_multimeas, by = "name_new")
  
  Impl_AEP_singlemeas <-  x %>%
    filter(is.na(multimeas))
  
  
  
  ## Modify genome based on priorities
  n_genomes = nrow(BS_genome_cl)  # equal to no. best solutions
  
  # 3 measures implementable
  Impl_AEP_3meas_copy <- Impl_AEP_3meas
  
  polygon_ids = as.vector(Impl_AEP_3$name_new)
  
  for (polygon in polygon_ids){
    for (genome in 1:n_genomes){
      polygon_genome_column = Impl_AEP_3meas[Impl_AEP_3meas$name_new == polygon, 6+genome]
      polygon_genome_column
      if (polygon_genome_column[1,]==2){
        polygon_genome_column[2:3,]=1
      } else if (polygon_genome_column[2,]==2){
        polygon_genome_column[3,]=1
      }
      
      # Replace value in original table
      Impl_AEP_3meas[Impl_AEP_3meas$name_new == polygon, 6+genome] = polygon_genome_column
    }
  }
  
  
  
  # 2 measures implementable
  Impl_AEP_2meas_copy <- Impl_AEP_2meas
  
  polygon_ids = as.vector(Impl_AEP_2$name_new)
  
  for (polygon in polygon_ids){
    for (genome in 1:n_genomes){
      polygon_genome_column = Impl_AEP_2meas[Impl_AEP_2meas$name_new == polygon, 6+genome]
      polygon_genome_column
      if (polygon_genome_column[1,]==2){
        polygon_genome_column[2,]=1
        # } else if (polygon_genome_column[2,]==2){
        #   polygon_genome_column[3,]=1
      }
      
      # Replace value in original table
      Impl_AEP_2meas[Impl_AEP_2meas$name_new == polygon, 6+genome] = polygon_genome_column
    }
  }
  
  
  # Create new genome table with 
  # Bind rows together
  Impl_AEP_multimeas_bind <- Impl_AEP_2meas %>%
    rbind(Impl_AEP_3meas)
  
  Impl_AEP_bind <- Impl_AEP_singlemeas %>%
    rbind(Impl_AEP_multimeas_bind)
  
  
  # save new genome
  # write.csv(Impl_AEP_bind,"Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/CoMOLA_results_june2024/Baseline/BS_genome_priority.csv")
  
  
  
  ### FREQUENCY ANALYSIS  ######################################################
  # Number of times a measures is implemented across the Best solutions
  Impl_AEP_bind$count <- apply(Impl_AEP_bind[7:(6 + nrows)], 1, 
                               function(x) length(which(x =="2")))
  
  # Calculate frequency
  Impl_AEP_bind <- Impl_AEP_bind%>%
    relocate(count, .after = name_new) %>%
    mutate(freq = (count / nrow(BS_genome_cl))*100, .after = count)
  
  
  # write csv file
  #z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "Impl_AEP_bind.csv")
  #write_csv(Impl_AEP_bind, z)
  
  
  
  # Plot frequency of implementation divided per AEP typology
  # Calculate mean of frequency implementation for AEP typology
  Mean_freq <- Impl_AEP_bind %>%
    group_by(nswrm) %>%
    summarise(freq_mean = mean(freq, na.rm = TRUE)) %>%
    mutate(priority = c("C","D","B","E","A"))
  
  
  # grouped violin plot
  vplot <- Impl_AEP_bind%>%
    group_by(nswrm) %>%
    mutate(freq_mean = mean(freq, na.rm = TRUE)) %>%
    ggplot(aes(x = priority, y = freq, fill = nswrm)) +
    geom_violin() +
    scale_fill_manual(values = c("#dbf1fd", "#daf1c5","#e5e1fb", "#fff5c5","#ceddf9"))+
    stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black") +
    #stat_summary(fun.y=mean, geom="text", vjust=-0.7) +
    geom_text(data = Mean_freq, aes(label = round(freq_mean, digits = 2), y = freq_mean + 3)) +
    theme(legend.position="none", text=element_text(size=20)) +
    labs(x = "AEP typologies",
         y = "Frequency of implementation (%)") + 
    # change labels name
    scale_x_discrete(labels = c("A" = "Retention pond", "B" = "Hedgerows ", "C" = "Riparian buffers ", "D" = "Grassed waterways ", "E" = "Reduced tillage
and cover crops"))
  
  
  # Save plot
  ggsave(file = paste0("Frequency_violinplot_conn_sub", n, "_1009_new.png"),
         width = 297, height = 210, units = "mm")
  
  
  
  
  
  ###  FREQUENCY MAPS  ########################################################
  # Show the frequence with which an AEP is implemented in every polygons
  
  # Join 
  lu_Schoeps_AEP_freq <- lu_Schoeps2 %>%
    left_join(Impl_AEP_bind, select(c(count, freq)), by = "name_new")
  
  
  # All measures
  # Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
  lu_Schoeps_AEP_freq_group <- lu_Schoeps_AEP_freq %>%
    group_by(name_new) %>%
    summarize(freq2 = sum(freq))
  
  # Write shp
  z <- file.path(file = paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_map_biod_", n, "0510.shp"))
  write_sf(lu_Schoeps_AEP_freq_group, z)
  
  
  # Loop through the measure typologies
  for (m in meas){
    
    # Filter measure
    lu_Schoeps_AEP_freq_meas <- lu_Schoeps_AEP_freq %>%
      filter(nswrm == m)
    
    # Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
    lu_Schoeps_AEP_freq_meas_group <- lu_Schoeps_AEP_freq_meas %>%
      group_by(name_new) %>%
      summarize(freq2 = sum(freq))
    
    # Write shp
    z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_0708/habcnt_freq_map_", m, "cluster_", n, "_1009_new.shp"))
    write_sf(lu_Schoeps_AEP_freq_meas_group, z)

    # Create buffer around polygon
    meas_buffer <- lu_Schoeps_AEP_freq_meas_group %>%
      st_buffer(35.00)
    
    # Write shp buffer
    z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_map_biod_", m, "_buffer_cluster_", n, "_0510.shp"))
    write_sf(meas_buffer, z)
  }
}






