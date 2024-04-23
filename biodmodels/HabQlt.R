
###################################################

# Title: Habitat Quality model
# Purpose: This code is used to model habitat quality
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 23 April 2024

###################################################


setwd("F:/bonato/CoMOLA_CS1_m/models/HabQlt")
sink("F:/bonato/CoMOLA_CS1_m/models/HabQlt/console.txt", append=FALSE)



### LOAD REQUIRED PACKAGES
library(tidyverse)
library(sf)
library(terra)
library(lwgeom)  # to calculate perimeter and area
library(units)  # to drop the units



### SET WORKING DIRECTORY
path = getwd()
path_input = paste(path, 'input', sep = "/")


### UPLOAD ALL NECESSARY INPUTS
# Import input lu map
# necessary for the calculation of all indicators
lu_Schoeps <- read_sf(paste(path_input,'/lu_Schoeps_final','.shp', sep=""))
lu_Schoeps <- st_buffer(lu_Schoeps, 0.0) #clean geometry
lu_Schoeps$OBJECTID <- c(1:dim(lu_Schoeps)[1])

# Calculate catchment area
# Calculate area for each polygon
lu_Schoeps$Area_sqm <- st_area(lu_Schoeps)
lu_Schoeps$Area <- drop_units(lu_Schoeps$Area_sqm) # drop units

# Sum polygons area for all catchment
Schoeps_area_tot <- sum(lu_Schoeps$Area_sqm)
#Schoeps_area_tot <- 136910545 (m2) 


# Import fertilization data
# These come from SWAT+ run
fert_sq <- read.table(paste(path_input,'/status_quo_hru_nb_aa', '.txt', sep = ""), header = TRUE, sep = "", dec = ".")
fert_till <- read.table(paste(path_input,'/lowtill_covercrop_hru_nb_aa', '.txt', sep = ""), header = TRUE, sep = "", dec = ".")

# Change name hru
fert_sq$name  <- as.numeric(str_remove(fert_sq$name, 'hru'))
fert_till$name  <- as.numeric(str_remove(fert_till$name, 'hru'))



### Code for optimization in COMOLA
# Duplicate lu map to keep an original 
lu_Schoeps_orig <- lu_Schoeps

# Change name hru
lu_Schoeps$name  <- as.numeric(str_remove(lu_Schoeps$name, 'hru'))

# Upload hru list for genome and genome
genome_hru <- read.csv(paste0(path_input,'/measure_location.csv'))
genome <- read.csv(paste0(path,'/genom.csv'), as.is=T)



lu_Schoeps$lowtill <- NA
lu_Schoeps$field_gras_w <- NA
lu_Schoeps$field_hedge <- NA

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}



# Start loop to update land use based on the genome (measures activated or not)
for(i in 1:dim(genome)[1]){
  if(genome[i,] == 2){
    # derive index of hrus representing the measure*
    #idx <- which(is.na(genome_hru[i,c(2:length(genome_hru))])==F)+1
    idx2 <- which(lu_Schoeps$name %in% as.numeric(unlist(strsplit(genome_hru$obj_id[i],","))))
    
    if(substr(genome_hru$name[i],1,4) %in% c("gras","buff")){
      lu_Schoeps$field_gras_w[idx2] <- lu_Schoeps$lu_3[idx2]
      lu_Schoeps$lu_1[idx2] <- "rnge" 
      lu_Schoeps$lu_3[idx2] <- "rnge" ## lu_3 update if measure should be not considered as field
      
    }
    if(substr(genome_hru$name[i],1,4) == "hedg"){
      lu_Schoeps$field_hedge[idx2] <- lu_Schoeps$lu_3[idx2]
      lu_Schoeps$lu_1[idx2] <- "rngb" ## these areas should not be considered as fields in field size calculation!!
      lu_Schoeps$lu_3[idx2] <- "rngb" ## problem also is that new field names shoud be introduced for fields splitted by hedges
      
    }
    if(substr(genome_hru$name[i],1,4) == "pond"){
      lu_Schoeps$lu_1[idx2] <- "watr" ## these areas should not be considered as fields in field size calculation!!
      
    }
    if(substr(genome_hru$name[i],1,4) == "lowt"){
      lu_Schoeps$lowtill[idx2] <- 1
      
    }
  }
}




### FERTILIZATION INFO
# Join fertilization information on lu map 
# Normalize and select important rows (status quo)
fert_sq <- fert_sq %>%
  # normalize value
  mutate(fertn_sq = fertn/max(fertn)) %>%
  mutate(fertp_sq = fertp/max(fertp)) %>%
  # Select only interesting columns
  select(name, fertn_sq)

# Normalize and select important rows (implementable measures)
fert_till <- fert_till %>%
  # normalize value
  mutate(fertn_till = fertn/max(fertn)) %>%
  mutate(fertp_till = fertp/max(fertp)) %>%
  # Select only interesting columns
  select(name, fertn_till)

# Join columns
lu_Schoeps <- lu_Schoeps %>%
  left_join(fert_sq, by = "name") %>%
  left_join(fert_till, by = "name")

# Assign nutrient input value based on tillage (conventional or reduced)
lu_Schoeps <- lu_Schoeps %>% 
  rowwise() %>% 
  mutate(fert = ifelse(is.na(lowtill) == TRUE,
                       fertn_sq * 1,
                       fertn_till * 1
                       ))

# Filter polygons characterized by agricultural land
lu_Schoeps_agr <- lu_Schoeps %>%
  filter(lu_1 == "field" | lu_1 == "orcd" | lu_1 == "meadow_2cuts" |
           lu_1 == "meadow_3cuts" | lu_1 == "meadow_4cuts") %>%
  # select only column with fertilization information
  select(fert)



# HABITAT
# Filter polygon habitats
# field margins, when implemented in the scenario, are not considered habitat but they act as a screen to protect snh

# rnge
rnge <- lu_Schoeps %>%
  # Select rnge patches
  filter(lu_1 == 'rnge') %>%
  # Dissolve adjacent polygons
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf()
  
# rngb
rngb <- lu_Schoeps %>%
  # Select rngb patches
  filter(lu_1 == 'rngb') %>%
  # Dissolve adjacent polygons
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf()

# wetl
wetl <- lu_Schoeps %>%
  # Select wetl patches
  filter(lu_1 == 'wetl') %>%  
  # Dissolve adjacent polygons
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf()

# forest edges
# at the moment width = 12.5 m
# Select forest patches
forest <- lu_Schoeps %>%
  filter(lu_1 == 'frst')
# Remove all roads and rivers
lu_Schoeps_sel <- lu_Schoeps %>%
  filter(lu_1 != 'utrn')
# Buffer all non-forest polygons
nonFOR_buffer <- lu_Schoeps_sel %>%
  filter(lu_1 != "frst") %>%
  st_buffer(12.5) 
# Derive forest edge by intersection with non-forest buffer
forest_edge <- st_intersection(forest, nonFOR_buffer)
forest_edge <- st_buffer(forest_edge, 0)
# Keep only forest layer attributes
forest_edge <- forest_edge %>%
  select(c(1:94)) %>% # this need to be changed in accordance with the number of columns to keep
  # Dissolve adjacent polygons
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf()

# Merge together habitat patches
habitat_bind <- bind_rows(rnge, rngb, wetl, forest_edge)

   
# Dissolve adjacent habitat patches
#habitat_dissolve <- st_union(habitat_bind)
#habitat_dissolve_singlepol <- st_cast(habitat_dissolve, "POLYGON")
#habitat <- st_as_sf(habitat_dissolve_singlepol)

# Add / Calculate additional information for each habitat patch
# Add field with snh id
habitat <- habitat_bind
habitat$snh_id <- c(1: nrow(habitat))

# Calculate perimeter length of each habitat patch
habitat$Perim_m <- st_perimeter(habitat)
habitat$Perim <- drop_units(habitat$Perim_m)

# Calculate area of each habitat patch 
habitat$Area_sqm <- st_area(habitat)
habitat$Area <- drop_units(habitat) # drop units



### Calculate HQ (habitat quality index)
# Intersect habitat patches with field polygons
agr_hab_int <- st_intersection(habitat, lu_Schoeps_agr)

# calculate length of touching polygons
agr_hab_int$length_m <- st_length(agr_hab_int)
agr_hab_int$length <- drop_units(agr_hab_int$length_m) # drop units

# Calculate possible run-off of fertilizer 
# based on multiplication of fertilizer quantity and shared snh-field border
agr_hab_int <- agr_hab_int %>%
  mutate(fert_ro = (fert * length))


### Calculate patch-wise habitat quality 
# Aggregate at patch level
fert_runoff <- aggregate(fert_ro ~ snh_id, agr_hab_int, FUN=sum)

habitat_HQ <- habitat %>%
  # join habitat and habitat run-off
  left_join(fert_runoff, by = "snh_id") 

# Assign 0 to NAs
habitat_HQ$fert_ro[is.na(habitat_HQ$fert_ro)] <- 0

# Calculate HQ index
habitat_HQ <- habitat_HQ %>%
  # divide by perimeter
  mutate(HQ = 1 - (fert_ro/Perim))

#z <- file.path(path, "habitat_HQ_all.shp")  
#st_write(habitat_HQ, z)  

### Calculate catchment-wise habitat quality
# Calculate HQ index
# The value is between 0 and 1
HQ_tot <- (sum(habitat_HQ$HQ)/nrow(habitat_HQ)) * (sum(habitat_HQ$Area_sqm)/Schoeps_area_tot) 


# Write table
write.table(HQ_tot, "HabQlt_output.csv", append = FALSE, sep = ";", col.names = FALSE, row.names = FALSE)
  




















































































































































































sink()
