###################################################

# Title: Frequency analysis subsets
# Purpose: This code is used to analyse the frequency of implementation of AEP of selected subsets
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



## Select data for land use maps analysis
# Select only fitness values
BestSol_genome <- BestSol2[, 1:301]

# Pivot table
BestSol_genome2 <- data.frame(t(BestSol_genome))

# Add id column
BestSol_genome2 <- BestSol_genome2 %>%
  mutate(id = c(1:301), .before = X1)


# Create dataframe with status quo value
StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=4))
names(StatusQuo) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
StatusQuo[1,] <- c(0.00161, 0.0508, -8630.0, 54306.687)





## Create subgroups to analyse
BestSol_fit$sub <- 0

BestSol_fit_sub <- BestSol_fit %>%
  mutate(sub = case_when(
    # High AP - Low Biod
    (AgrPrd >= 54225.00 & HabCnt < 0.001650) ~ 1,
    # High AP - High Biod
    (AgrPrd >= 54225.00 & HabCnt >= 0.001850) ~ 2,
    # Low AP - High Biod
    (AgrPrd < 53300 & HabCnt >= 0.001852) ~3,
    # Low AP - High Wtr
    (AgrPrd < 53300 & WtrQlt >= -7380) ~ 4,
    # Tipping point water
    (AgrPrd < 54000 & AgrPrd >= 53800 & WtrQlt >= - 7445) ~ 5,
    # Compromise solution
    (AgrPrd <= 54000 & AgrPrd >= 53920 & WtrQlt >= - 7550 & HabCnt >= 0.00185) ~ 6
  )) 


# Select subset of data to analyse
# High AP - Low Biod
BestSol2_sub1 <- BestSol2 %>%
  filter(V305 >= 54225.0 & V302 <= 0.001650)

# High AP - High Biod
BestSol2_sub2 <- BestSol2 %>%
  filter(V305 >= 54225.0 & V302 >= 0.00185)

# Low AP - High Biod
BestSol2_sub3 <- BestSol2 %>%
  filter(V305 < 53300 & V302 >= 0.001852)

# Low AP - High Wtr
BestSol2_sub4 <- BestSol2 %>%
  filter(V305 < 53300 & V304 >= -7380)

# Tipping point water
BestSol2_sub5 <- BestSol2 %>%
  filter(V305 <= 54000 & V305 >= 53800 & V304 >= - 7445) 

# Compromise solution
BestSol2_sub6 <- BestSol2 %>%
  filter(V305 <= 54000 & V305 >= 53920 & V304 >= - 7550 & V302 >= 0.00185) 



# BestSol2_sub7 <- BestSol2 %>%
#   filter(V302 <= 0.001775 & V303 >= 0.05375 & V303 <= 0.054)
# 
# BestSol2_sub8 <- BestSol2 %>%
#   filter(V302 >= 0.001775 & V303 >= 0.05375 & V303 <= 0.054) 
# 
# 
# BestSol2_sub9 <- BestSol2 %>%
#   filter(V302 <= 0.0018 & V303 >= 0.05425 & V303 <= 0.05475)
# 
# BestSol2_sub10 <- BestSol2 %>%
#   filter(V302 >= 0.0018 & V303 >= 0.05425 & V303 <= 0.05475) 




# Plot



### MODIFY GENOME TABLE BASED ON PRIORITY OF IMPEMENTATION OF AEP
# becauSe 2 or more AEP cannot be implemented at the same time in the same hru


# Associate Best Solutions to genome hru 
# Upload genome_hru
genome_hru <- read.csv(paste0(path_input,'/measure_location.csv'))

#Separate values in obj_id
genome_hru_separate <- genome_hru %>%
  separate(obj_id, c("code1", "code2", "code3", "code4", "code5", "code6", "code7","code8", "code9", "code10", "code11", "code12", "code13", "code14", "code15", "code16", "code17", "code18", "code19", "code20", "code21", "code22", "code23", "code24", "code25", "code26", "code27", "code28", "code29", "code30", "code31", "code32", "code33", "code34", "code35"), sep = ',', remove = FALSE)

# Join Best Solution to genome hru
# When analyzing the subsets chose here the subset of Best_Sol2_2 to analyse and to join with genome_hru_separate
Impl_AEP <- genome_hru_separate %>%
  left_join(BestSol_genome2, by = "id") 

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
n_genomes = 496  # equal to no. best solutions

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





### FREQUENCY ANALYSIS
# Number of times a measures is implemented across the Best solutions
Impl_AEP_bind$count <- apply(Impl_AEP_bind[7:502], 1, # change based on solutions number!!!
                             function(x) length(which(x =="2")))

# Calculate frequency
Impl_AEP_bind <- Impl_AEP_bind%>%
  relocate(count, .after = name_new) %>%
  mutate(freq = (count / 496)*100, .after = count) # change based on solutions number!!!

# write csv file
z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "Impl_AEP_bind.csv")
write_csv(Impl_AEP_bind, z)



# # Load subgroups
# #sub1
# Impl_AEP_bind_sub1 <- read.csv("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/Impl_AEP_bind_sub1.csv", h = T)
# #sub2
# Impl_AEP_bind_sub2 <- read.csv("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/Impl_AEP_bind_sub2.csv", h = T)
# #sub3
# Impl_AEP_bind_sub3 <- read.csv("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/Impl_AEP_bind_sub3.csv", h = T)
# #sub4
# Impl_AEP_bind_sub4 <- read.csv("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/Impl_AEP_bind_sub4.csv", h = T)
# #sub5
# Impl_AEP_bind_sub5 <- read.csv("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/Impl_AEP_bind_sub5.csv", h = T)
# #sub6
# Impl_AEP_bind_sub6 <- read.csv("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output/Impl_AEP_bind_sub6.csv", h = T)



# Plot frequency of implementation divided per AEP typology
# Calculate mean of frequency implementation for AEP typology
Mean_freq <- Impl_AEP_bind %>%
  group_by(nswrm) %>%
  summarise(freq_mean = mean(freq, na.rm = TRUE)) %>%
  mutate(priority = c("C","D","B","E","A"))

# grouped violin plot
v2 <- Impl_AEP_bind%>%
  group_by(nswrm) %>%
  mutate(freq_mean = mean(freq, na.rm = TRUE)) %>%
  ggplot(aes(x = priority, y = freq, fill = nswrm)) +
  geom_violin() +
  #scale_fill_brewer(palette="Mint") +
  #scale_fill_manual(values = c("#46AEA0", "#7CCBA2", "#089099","#B7E6A5", "#00718B"))+
  scale_fill_manual(values = c("#AF58BA", "#FFC61E", "#009ADE","#F28522", "#FF1F5B"))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="black", fill="black") +
  #stat_summary(fun.y=mean, geom="text", vjust=-0.7) +
  geom_text(data = Mean_freq, aes(label = round(freq_mean, digits = 2), y = freq_mean + 3)) +
  theme(legend.position="none", text=element_text(size=20)) +
  labs(x = "AEP typologies",
       y = "Frequency of implementation (%)") + 
  # change labels name
  scale_x_discrete(labels = c("A" = "Retention pond", "B" = "Hedgerows ", "C" = "Riparian buffers ", "D" = "Grassed waterways ", "E" = "Reduced tillage
and cover crops"))

v2


# Save plot
ggsave(file = "plots/Frequency_violinplot_subcompromise_2603.png",
       width = 297, height = 210, units = "mm")




###  FREQUENCY MAPS
# Show the frequence with which an AEP is implemented in every polygons

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

# Join 
lu_Schoeps_AEP_freq <- lu_Schoeps2 %>%
  left_join(Impl_AEP_bind, select(c(count, freq)), by = "name_new")


# Summarize frequency for polygon (based on same hru) to not double account polygons
lu_Schoeps_AEP_freq_group <- lu_Schoeps_AEP_freq %>%
  group_by(name.x) %>%
  summarize(freq2 = sum(freq))


# Draw frequency map
m1 <- tm_shape(lu_Schoeps_AEP_freq_group) + 
  tm_fill('freq2', breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  tm_layout(legend.position = c("right", "bottom"))

m1 



# Plot separate frequency maps forAEP typologies

# Landscape elements
# Filter landscape elements
lu_Schoeps_AEP_freq_landelem <- lu_Schoeps_AEP_freq %>%
  filter(nswrm == "pond" | nswrm == "grassslope" | nswrm == "buffer" | nswrm == "hedge")

# Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
lu_Schoeps_AEP_freq_landelem_group <- lu_Schoeps_AEP_freq_landelem %>%
  group_by(name_new) %>%
  summarize(freq2 = sum(freq))

# # Write shp
z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "lu_Schoeps_AEP_freq_sub6_SM_2603.shp")
write_sf(lu_Schoeps_AEP_freq_landelem_group, z)



# Ponds
# Filter pond
lu_Schoeps_AEP_freq_pond <- lu_Schoeps_AEP_freq %>%
  filter(nswrm == "pond")

# Write shp
z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "lu_Schoeps_AEP_freq_sub4_pond.shp")
write_sf(lu_Schoeps_AEP_freq_pond, z)



# Grassed waterways
# Filter grassed waterways
lu_Schoeps_AEP_freq_grassslope <- lu_Schoeps_AEP_freq %>%
  filter(nswrm == "grassslope")

# Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
lu_Schoeps_AEP_freq_grassslope_group <- lu_Schoeps_AEP_freq_grassslope %>%
  group_by(name_new) %>%
  summarize(freq2 = sum(freq))

# Classify frequency in low/medium/high
grassslope_sub5 <- lu_Schoeps_AEP_freq_grassslope_group  %>%
  mutate(class = case_when(
    freq2 == 0 ~ "0",
    freq2 > 0 & freq2 < 33 ~ "Low",
    freq2 >= 33 & freq2 < 66 ~ "Medium",
    freq2 >= 66 & freq2 < 100 ~ "High",
    freq2 == 100 ~ "100"
  ))

# Write shp
z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "lu_Schoeps_AEP_freq_sub5_grassslope_class.shp")
write_sf(grassslope_sub5, z)



# Buffer strips
# Filter buffer strips
lu_Schoeps_AEP_freq_buffer <- lu_Schoeps_AEP_freq %>%
  filter(nswrm == "buffer")

# Write shp
z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "lu_Schoeps_AEP_freq_sub4_buffer.shp")
write_sf(lu_Schoeps_AEP_freq_buffer, z)



# Hedgerows
# Filter hedges
lu_Schoeps_AEP_freq_hedge <- lu_Schoeps_AEP_freq %>%
  filter(nswrm == "hedge")

# Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
lu_Schoeps_AEP_freq_hedge_group <- lu_Schoeps_AEP_freq_hedge %>%
  group_by(name_new) %>%
  summarize(freq2 = sum(freq))

# Classify frequency in low/medium/high
hedge_sub5 <- lu_Schoeps_AEP_freq_hedge_group  %>%
  mutate(class = case_when(
    freq2 == 0 ~ "0",
    freq2 > 0 & freq2 < 33 ~ "Low",
    freq2 >= 33 & freq2 < 66 ~ "Medium",
    freq2 >= 66 & freq2 < 100 ~ "High",
    freq2 == 100 ~ "100"
  ))

# Write shp
z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "lu_Schoeps_AEP_freq_sub5_hedge_class.shp")
write_sf(hedge_sub5, z)



# Reduced tillage
# Filter reduced tillage
lu_Schoeps_AEP_freq_lowtill <- lu_Schoeps_AEP_freq %>%
  filter(nswrm == "lowtillcc")

# Summarize frequency for polygon (based on same hru) to not have overlapping polygons in the map
lu_Schoeps_AEP_freq_lowtill_group <- lu_Schoeps_AEP_freq_lowtill %>%
  group_by(name_new) %>%
  summarize(freq2 = sum(freq))

# Classify frequency in low/medium/high
lowtill_sub5 <- lu_Schoeps_AEP_freq_lowtill_group  %>%
  mutate(class = case_when(
    freq2 == 0 ~ "0",
    freq2 > 0 & freq2 < 33 ~ "Low",
    freq2 >= 33 & freq2 < 66 ~ "Medium",
    freq2 >= 66 & freq2 < 100 ~ "High",
    freq2 == 100 ~ "100"
  ))

# Write shp
z <- file.path("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis/DATA/output", "lu_Schoeps_AEP_freq_sub6_LT_2603.shp")
write_sf(lu_Schoeps_AEP_freq_lowtill_group, z)