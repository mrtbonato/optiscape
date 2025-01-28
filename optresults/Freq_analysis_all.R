###################################################

# Title: Frequency Analysis - all solutions
# Purpose: This code is used to analyse the frequency of AEP implementation when considering all the Best Solutions together
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 24 January 2025

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




### MODIFY GENOME TABLE BASED ON PRIORITY OF IMPEMENTATION OF AEP ##############
# becauSe 2 or more AEP cannot be implemented at the same time in the same hru

# Associate Best Solutions to genome hru 
# Upload genome_hru
genome_hru <- read.csv(paste0(path_input,'/measure_location_new.csv'))

#Separate values in obj_id
genome_hru_separate <- genome_hru %>%
  separate(obj_id, c("code1", "code2", "code3", "code4", "code5", "code6", "code7","code8", "code9", "code10", "code11", "code12", "code13", "code14", "code15", "code16", "code17", "code18", "code19", "code20", "code21", "code22", "code23", "code24", "code25", "code26", "code27", "code28", "code29", "code30", "code31", "code32", "code33", "code34", "code35"), sep = ',', remove = FALSE)


# Land use map (for frequency map)
# Upload input lu map
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



# Join Best Solution to genome 
### When analyzing the subsets 
## chose here the subset of Best_Sol2_2 to analyse and to join with genome_hru_separate ####
Impl_AEP <- genome_hru_separate %>%
  left_join(BS_genome, by = "id") 

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
n_genomes = 1070                                    # equal to no. best solutions

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





### FREQUENCY ANALYSIS #########################################################
# Number of times a measures is implemented across the Best solutions
Impl_AEP_bind$count <- apply(Impl_AEP_bind[7:1076], 1, 
                             function(x) length(which(x =="2")))   # based on no. best solutions

# Calculate frequency
Impl_AEP_bind <- Impl_AEP_bind%>%
  relocate(count, .after = name_new) %>%
  mutate(freq = (count / 1070) * 100, .after = count)          # equal to no. best solutions


# write csv file
# z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "Impl_AEP_bind_0608.csv")
# write_csv(Impl_AEP_bind, z)


# Group the polygons of the same AEP
Impl_AEP_type <- Impl_AEP_bind %>%
  group_by(name, nswrm) %>%
  summarise(freq_2 = mean(freq, na.rm = TRUE))


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
  #geom_boxplot(width = 0.04, color="darkgrey", alpha = 0.5) +
  geom_point(aes(x = priority, y = freq),  position = position_jitter(seed = 1, width = 0.05), shape = 19, size = 2, color = "gray43", alpha = 0.30) +
  #scale_fill_brewer(palette="Mint") +
  #scale_fill_manual(values = c("#46AEA0", "#7CCBA2", "#089099","#B7E6A5", "#00718B"))+
  scale_fill_manual(values = c("#dbf1fd", "#daf1c5","#e5e1fb", "#fff5c5","#ceddf9"))+
  #scale_fill_manual(values = c("#AF58BA", "#FFC61E", "#009ADE","#F28522", "#FF1F5B"))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black") +
  #stat_summary(fun.y=mean, geom="text", vjust=-0.7) +
  geom_text(data = Mean_freq, aes(label = round(freq_mean, digits = 1), y = freq_mean + 0.5), size = 5, nudge_x = 0.20) +
  theme(legend.position="none", text=element_text(size=20)) +
  labs(x = "Agri-Environmental Practices",
       y = "Frequency of implementation (%)") + 
  # change labels name
  # change labels name
  scale_x_discrete(labels = c("A" = "Retention
ponds
[n = 9]", "B" = "Hedgerows
[n = 28]", "C" = "Riparian
buffers
[n = 34]", "D" = "Grassed 
waterways
[n = 30]", "E" = "Reduced 
tillage and
cover crops
[n = 201]"))

vplot



# Save plot
ggsave(file = "Frequency_violinplot_all_1112_jitter.png",
          width = 297, height = 170, units = "mm")





###  FREQUENCY MAPS  ##########################################################
# Show the frequency with which an AEP is implemented in every polygons

# Join 
lu_Schoeps_AEP_freq <- lu_Schoeps2 %>%
  left_join(Impl_AEP_bind, select(c(nswrm, count, freq)), by = "name_new")


# All measures together
# Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
lu_Schoeps_AEP_freq_group <- lu_Schoeps_AEP_freq %>%
  group_by(name_new) %>%
  summarize(freq2 = sum(freq))

# Write shp
# z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_map_all_0608.shp")
# write_sf(lu_Schoeps_AEP_freq_group, z)



# Single measure typologies
# Create list of measure typologies
meas <- c("lowtillcc", "pond", "grassslope", "buffer", "hedge")

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
  z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_map_", m, "_0608.shp"))
  write_sf(lu_Schoeps_AEP_freq_meas_group, z)
  
  # Create buffer around polygon
  meas_buffer <- lu_Schoeps_AEP_freq_meas_group %>%
    st_buffer(35.00)
  
  # Write shp buffer
  z <- file.path(paste0("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/freq_map_", m, "_buffer_0608.shp"))
  write_sf(meas_buffer, z)
}





