###################################################

# Title: Frequency analysis
# Purpose: 
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 19 July 2024

###################################################


## Upload required packages
library(tidyverse)
library(sf)
library(patchwork)
library(plotly)
library(tmap)
library(colorspace) # for color violin plot
library(units) # for drop units



## Set working directories
# Working directory for land use map 
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path = paste(getwd(),'CoMOLA_results_june2024/Baseline', sep="/")
path_input = paste(getwd(),'DATA/input', sep="/")


## Upload fitness of Best solutions WITH CLUSTER CODE
BS_fitness <- read.csv(paste0(path,'/clusters_k13.csv'), h = T, as.is=T, sep = ",")
## Rename columns
#names(BS_fitness) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")


# GENOME: prepare and join it
# Upload genome of Best solutions
BS_genome <- read.csv(paste0(path,'/BS_genomes.csv'), h = F, as.is=T, sep = ";")
# Add id column
BS_genome <- BS_genome %>%
  mutate(id = c(1:302), .before = V1)
# Invert rows and columns
BS_genome <- data.frame(t(BS_genome[-1]))
# # Add id
BS_genome <- BS_genome %>%
  mutate(X = c(1:1070), .before = X1)
# Join with fitness values
BS_genome_join <- BS_fitness %>%
  left_join(BS_genome, by = "X")
  


### MODIFY GENOME TABLE BASED ON PRIORITY OF IMPEMENTATION OF AEP
# becauSe 2 or more AEP cannot be implemented at the same time in the same hru

# Associate Best Solutions to genome hru 
# Upload genome_hru
genome_hru <- read.csv(paste0(path_input,'/measure_location_new.csv'))

#Separate values in obj_id
genome_hru_separate <- genome_hru %>%
  separate(obj_id, c("code1", "code2", "code3", "code4", "code5", "code6", "code7","code8", "code9", "code10", "code11", "code12", "code13", "code14", "code15", "code16", "code17", "code18", "code19", "code20", "code21", "code22", "code23", "code24", "code25", "code26", "code27", "code28", "code29", "code30", "code31", "code32", "code33", "code34", "code35"), sep = ',', remove = FALSE)


# For frequency maps
# Associate Best Solutions to land use map
### Upload input lu map
lu_Schoeps <- read_sf(paste(path_input,'/lu_Schoeps_final','.shp', sep="")) 
lu_Schoeps <- st_buffer(lu_Schoeps, 0.0) #clean geometry
lu_Schoeps$OBJECTID <- c(1:dim(lu_Schoeps)[1])

# Change name in land use map
# Modify hru name, deleting "hru" and the 0s before the number
lu_Schoeps$name_new <- str_remove(lu_Schoeps$name, "hru")
lu_Schoeps$name_new <- str_remove(lu_Schoeps$name_new, "^0+")
#lu_Schoeps$name_new <- as.integer(lu_Schoeps$name_new)

# Select only important columns
lu_Schoeps2 <- lu_Schoeps %>%
  select(c("name", "meas_1", "meas_2", "meas_3", "meas_4", "name_new"))





# Save number of solutions in cluster
nsol <- data.frame(matrix(data = NA, nrow = 13, ncol = 2))
names(nsol) <- c("Ncluster", "NBestSol")
nsol[,1] <- c(1:13)

# Create list of measure typologies
meas <- c("lowtillcc", "pond", "grassslope", "buffer", "hedge")




# loop through all clusters
for (n in 1:13){
  
  BS_genome_cl <- BS_genome_join %>%
    filter(cluster == n) 
  BS_genome_cl <- BS_genome_cl %>%
    select(c(7:length(BS_genome_cl)))

  # Save number of solutions in cluster
  nrows <- nrow(BS_genome_cl)
  nsol[n,2] <- nrows
  
  # Invert rows and columns
  BS_genome2 <- data.frame(t(BS_genome_cl))
  
  # Add id column
  BS_genome2 <- BS_genome2 %>%
    mutate(id = c(1:302), .before = X1)
  
  # Join Best Solution to genome 
  ##### When analyzing the subsets chose here the subset of Best_Sol2_2 to analyse and
  #to join with genome_hru_separate ####
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
    Impl_AEP_3meas[Impl_AEP_3meas$name_new == polygon, 6+genome] =   polygon_genome_column
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




  ### FREQUENCY ANALYSIS
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
    #scale_fill_qualitativex(palette="Pastel 1") +
    scale_fill_manual(values = c("#dbf1fd", "#daf1c5","#e5e1fb", "#fff5c5","#ceddf9"))+
    #scale_fill_manual(values = c("#AF58BA", "#FFC61E", "#009ADE","#F28522", "#FF1F5B"))+
    stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black") +
    #stat_summary(fun.y=mean, geom="text", vjust=-0.7) +
    geom_text(data = Mean_freq, aes(label = round(freq_mean, digits = 2), y = freq_mean + 3)) +
    theme(legend.position="none", text=element_text(size=20)) +
    labs(x = "AEP typologies",
         y = "Frequency of implementation (%)") + 
    # change labels name
    scale_x_discrete(labels = c("A" = "Retention pond
[n = 9]", "B" = "Hedgerows
[n = 51]", "C" = "Riparian buffers
[n = 42]", "D" = "Grassed waterways
[n = 70]", "E" = "Reduced tillage 
and cover crops
[n = 632]"))

  # Save plot
  ggsave(file = paste0("Frequency_violinplot_cl_", n, "1608.png"),
         width = 297, height = 210, units = "mm")





  # Join 
  Impl_AEP_bind_sel <- Impl_AEP_bind %>%
    select(c("nswrm", "name_new", "count", "freq"))
  
  lu_Schoeps_AEP_freq <- lu_Schoeps2 %>%
    left_join(Impl_AEP_bind_sel, by = "name_new")

  
    
  # Area weighted frequency of implementation
  lu_Schoeps_AEP_freq <- lu_Schoeps_AEP_freq %>%
    mutate(area_m = st_area(lu_Schoeps_AEP_freq), .after = name_new) %>%
    mutate(area = drop_units(area_m), .after = area_m)
  
  
  # pond
  lu_Schoeps_AEP_freq_pond <- lu_Schoeps_AEP_freq %>%
    filter(nswrm == "pond") %>%
    mutate(area_norm = ((area - 0)/(max(area) - 0)), .after = area) %>%
    mutate(freq_area = freq * area_norm) %>%
    mutate(priority = "A")
  
  # hedge
  lu_Schoeps_AEP_freq_hedge <- lu_Schoeps_AEP_freq %>%
    filter(nswrm == "hedge") %>%
    mutate(area_norm = ((area - 0)/(max(area) - 0)), .after = area) %>%
    mutate(freq_area = freq * area_norm) %>%
    mutate(priority = "B")
  
  areaweight <- lu_Schoeps_AEP_freq_pond %>%
    bind_rows(lu_Schoeps_AEP_freq_hedge)
  
  # buffer
  lu_Schoeps_AEP_freq_buffer <- lu_Schoeps_AEP_freq %>%
    filter(nswrm == "buffer") %>%
    mutate(area_norm = ((area - 0)/(max(area) - 0)), .after = area) %>%
    mutate(freq_area = freq * area_norm) %>%
    mutate(priority = "C")
  
  areaweight <- areaweight %>%
    bind_rows(lu_Schoeps_AEP_freq_buffer)
  
  # grassslope
  lu_Schoeps_AEP_freq_grassslope <- lu_Schoeps_AEP_freq %>%
    filter(nswrm == "grassslope") %>%
    mutate(area_norm = ((area - 0)/(max(area) - 0)), .after = area) %>%
    mutate(freq_area = freq * area_norm)%>%
    mutate(priority = "D") 
  
  areaweight <- areaweight %>%
    bind_rows(lu_Schoeps_AEP_freq_grassslope)
  
  # lowtillcc
  lu_Schoeps_AEP_freq_lowtillcc <- lu_Schoeps_AEP_freq %>%
    filter(nswrm == "lowtillcc") %>%
    mutate(area_norm = ((area - 0)/(max(area) - 0)), .after = area) %>%
    mutate(freq_area = freq * area_norm)%>%
    mutate(priority = "E") 
  
  areaweight <- areaweight %>%
    bind_rows(lu_Schoeps_AEP_freq_lowtillcc)



  
  # Plot frequency of implementation divided per AEP typology
  # Calculate mean of frequency implementation for AEP typology
  # Mean_freq2 <- areaweight %>%
  #   group_by(nswrm) %>%
  #   summarise(freq_mean = mean(freq_area, na.rm = TRUE)) %>%
  #   mutate(priority = c("C","D","B","E","A"))
  
  
  # grouped violin plot
vplot2 <- Impl_AEP_bind %>%
    group_by(nswrm) %>%
    mutate(freq_mean = mean(freq, na.rm = TRUE)) %>%
    ggplot(aes(x = priority, y = freq, fill = nswrm)) +
    geom_violin() +
    geom_point(data = areaweight, aes(x = priority, y = freq, size = area_norm),  position = position_jitter(seed = 1, width = 0.2), shape = 19, color = "darkgrey", alpha = 0.25) +
    scale_fill_manual(values = c("#dbf1fd", "#daf1c5","#e5e1fb", "#fff5c5","#ceddf9"))+
    stat_summary(fun.y = mean, geom="point", shape=20, size=4, color="black", fill="black") +
    geom_text(data = Mean_freq, aes(label = round(freq_mean, digits = 2), y = freq_mean + 3)) +
    theme(legend.position="none", text=element_text(size=20)) +
    labs(x = "AEP typologies",
         y = "Frequency of implementation (%)") + 
    # change labels name
    scale_x_discrete(labels = c("A" = "Retention pond
[n = 9]", "B" = "Hedgerows
[n = 51]", "C" = "Riparian buffers
[n = 42]", "D" = "Grassed waterways
[n = 70]", "E" = "Reduced tillage 
and cover crops
[n = 632]"))

  # Save plot
  ggsave(file = paste0("Frequency_violinplot_cl_", n, "areapoint_jitter_1608.png"),
          width = 297, height = 210, units = "mm")
  
  
  
  # Plot frequency of implementation divided per AEP typology
  # Calculate mean of frequency implementation for AEP typology
  Mean_freq2 <- areaweight %>%
    group_by(nswrm) %>%
    summarise(freq_mean = mean(freq_area, na.rm = TRUE)) %>%
    mutate(priority = c("C","D","B","E","A"))
  
  
  # grouped violin plot
  vplot3 <- areaweight %>%
    group_by(nswrm) %>%
    mutate(freq_mean = mean(freq, na.rm = TRUE)) %>%
    mutate(freq_mean2 = mean(freq_area, na.rm = TRUE)) %>%
    ggplot(aes(x = priority)) +
    geom_violin(aes(y = freq, fill = nswrm)) +
    geom_violin(aes( y = freq_area), color = "grey", fill = "grey", alpha = 0.25) +
    scale_fill_manual(values = c("#dbf1fd", "#daf1c5","#e5e1fb", "#fff5c5","#ceddf9"))+
    stat_summary(aes(y = freq), fun.y = mean, geom="point", shape=20, size=4, color="black", fill="black") +
    stat_summary(aes(y = freq_area), fun.y = mean, geom="point", shape=20, size=4, color="gray47", fill="gray47") +
    geom_text(data = Mean_freq, aes(label = round(freq_mean, digits = 2),  y = freq_mean + 1), nudge_x = 0.2) +
    geom_text(data = Mean_freq2, aes(label = round(freq_mean, digits = 2),  y = freq_mean + 1), color = "gray47", nudge_x = 0.2) +
    theme(legend.position="none", text=element_text(size=20)) +
    labs(x = "AEP typologies",
         y = "Frequency of implementation (%)") + 
    # change labels name
    scale_x_discrete(labels = c("A" = "Retention pond
[n = 9]", "B" = "Hedgerows
[n = 51]", "C" = "Riparian buffers
[n = 42]", "D" = "Grassed waterways
[n = 70]", "E" = "Reduced tillage 
and cover crops
[n = 632]"))
  
  # Save plot
  ggsave(file = paste0("Frequency_violinplot_cl_", n, "areaweight_1608.png"),
         width = 297, height = 210, units = "mm")

  
  
  
  ###  FREQUENCY MAPS
  # Show the frequence with which an AEP is implemented in every polygons

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
    z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_0708/freq_map_", m, "cluster_", n, "_0708.shp"))
    write_sf(lu_Schoeps_AEP_freq_meas_group, z)
  
  # Create buffer around polygon
    meas_buffer <- lu_Schoeps_AEP_freq_meas_group %>%
      st_buffer(35.00)
  
  # Write shp buffer
  #   z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_0708/freq_map_", m, "_buffer_cluster_", n, "_0708.shp"))
  #   write_sf(meas_buffer, z)
  }
}


### end loop and frequency analysis ###







######### Analysis HabCnt ######

BS_fitness_conn <- BS_genome_join %>%
  filter(AgrPrd >= 58900)

# Create 6 clusters - 3 low connectivity, 3 hig connectivity
BS_fitness_sub <- BS_fitness_conn %>%
  mutate(cluster = case_when(
    # High AP - Low Biod
    (AgrPrd >= 59150 & HabCnt < 0.00168) ~ 1,
    (AgrPrd >= 59150 & HabCnt > 0.00168) ~ 2,
    (AgrPrd >= 59100 & AgrPrd < 59150 & HabCnt < 0.001694 ) ~ 1,
    (AgrPrd >= 59100 & AgrPrd < 59120 & HabCnt < 0.00170 ) ~ 1,
    (AgrPrd >= 59100 & AgrPrd < 59150 & HabCnt > 0.001694 ) ~ 2,
    
    (AgrPrd >= 59000 & AgrPrd < 59100 &  HabCnt < 0.00172 ) ~ 3,
    (AgrPrd >= 59000 & AgrPrd < 59050 & HabCnt < 0.001735 ) ~ 3,
    (AgrPrd >= 59000 & AgrPrd < 59100 &  HabCnt > 0.00172 ) ~ 4,
    
    (AgrPrd < 59000 & HabCnt < 0.001755 ) ~ 5,
    (AgrPrd < 59000 & AgrPrd >= 58900 & HabCnt > 0.001735 ) ~ 6
  ))


# x <- replace_na(BS_fitness_sub$cluster, 2)
# BS_fitness_sub$cluster <- x
# 
# 
# # Check up
# BS_fitness_sub1 <- BS_fitness_sub %>%
#   filter(sub == 1)


ggplot() +
  #geom_hline(yintercept = 59083.86, color = "gray50") +
  #geom_vline(xintercept = 0.00161, color = "gray50") +
  #geom_rect(aes(xmin = StatusQuo$HabCnt, xmax = Inf,
  #              ymin = StatusQuo$AgrPrd, ymax = Inf), fill = "#21908CFF" , alpha = .2) +
  geom_point(data = BS_fitness_sub, aes(x = HabCnt, y = AgrPrd, color = cluster)) +
  stat_ellipse(data = BS_fitness_sub%>% filter(cluster != 0), aes(x = HabCnt, y = AgrPrd, color = cluster)) +
  scale_color_distiller(palette = "Set2") +
  #geom_point(data = BS_fitness_sub, aes(x = HabCnt, y = AgrPrd), color = "red") +
  # Change names labels
  labs(x = "Probability of connectivity",
       y = "Crop yield [grain unit]") + 
  theme(text=element_text(size=18), legend.position = "none")



# table <- data.frame(matrix(data=NA, nrow=55, ncol=8))
# #names(table) <- c("1_id", "1_meas", "1_freq", "2_id", "2_meas", "2_freq", "3_id", "3_meas",  "3_freq", "4_id", "4_meas", "4_freq", "5_id", "5_meas",  "5_freq", "6_id", "6_meas", "6_freq" )
# 
# names(table) <- c("name_new", "nswrm", "1_freq", "2_freq", "3_freq", "4_freq", "5_freq", "6_freq" )
# table[,1] <- lu_Schoeps_AEP_freq$name_new
# table[,2] <- lu_Schoeps_AEP_freq$nswrm




# loop through all clusters
for (n in 1:6){
  
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
  
  
  
    ### FREQUENCY ANALYSIS
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
  ggsave(file = paste0("Frequency_violinplot_conn_sub", n, "_0708.png"),
          width = 297, height = 210, units = "mm")
  
  
  
  
  
  ###  FREQUENCY MAPS
  # Show the frequence with which an AEP is implemented in every polygons
  
  # Join 
  lu_Schoeps_AEP_freq <- lu_Schoeps2 %>%
    left_join(Impl_AEP_bind, select(c(count, freq)), by = "name_new")
  
  
  # All measures
  # Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
  # lu_Schoeps_AEP_freq_group <- lu_Schoeps_AEP_freq %>%
  #   group_by(name_new) %>%
  #   summarize(freq2 = sum(freq))
  
  # Write shp
  # z <- file.path(file = paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_map_biod_", n, ".shp"))
  # write_sf(lu_Schoeps_AEP_freq_group, z)
  
  
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
    z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_0708/habcnt_freq_map_", m, "cluster_", n, "_0708.shp"))
    write_sf(lu_Schoeps_AEP_freq_meas_group, z)
    
    # Create buffer around polygon
    meas_buffer <- lu_Schoeps_AEP_freq_meas_group %>%
      st_buffer(35.00)
    
    # Write shp buffer
    z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_0708/habcnt_freq_map_", m, "_buffer_cluster_", n, "_0708.shp"))
    write_sf(meas_buffer, z)
  }
}


# z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/table.csv")
# write.csv(table, z)


