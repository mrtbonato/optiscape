setwd("F:/bonato/CoMOLA_fert_0610/models/model_all")
sink("F:/bonato/CoMOLA_fert_0610/models/model_all/console.txt", append=FALSE)
###################################################

# Title: All models for optimization
# Purpose: This code is used to model habitat quality, habitat connectivity and SWAT related indicators
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 11 June 2024

###################################################





### LOAD REQUIRED PACKAGES
library(tidyverse)
library(sf)
library(terra)
library(lwgeom)  # to calculate perimeter and area
library(units)  # to drop the units

# install and load required packages for connectivity index calculation
library(devtools)
library(remotes)
#install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")
library(Makurhini)




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
# These files need to be changed when running the reduced fertilization scenario
fert_sq <- read.table(paste(path_input,'/hru_nb_aa_statusquo', '.txt', sep = ""), header = TRUE, sep = "", dec = ".")
fert_till <- read.table(paste(path_input,'/hru_nb_aa_lowtillcc', '.txt', sep = ""), header = TRUE, sep = "", dec = ".")

# Change name hru
fert_sq$name  <- as.numeric(str_remove(fert_sq$name, 'hru'))
fert_till$name  <- as.numeric(str_remove(fert_till$name, 'hru'))


### Upload table with dispersal distances
disp_dist <- read.csv(paste0(path_input,'/disp_distances_07_12.csv'), sep = ";")



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
    
    if(substr(genome_hru$name[i],1,4) == "lowt"){
      lu_Schoeps$lowtill[idx2] <- 1
    }
    
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
  }
}




# CALCULATION HABITAT QUALITY ################################################

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
  select(c(1:94)) # this need to be changed in accordance with the number of columns to keep

# Dissolve adjacent polygons
forest_edge_diss <- forest_edge %>%
  st_union() %>%
  st_cast("POLYGON") %>%
  st_as_sf()

# Merge together habitat patches
habitat_bind <- bind_rows(rnge, rngb, wetl, forest_edge_diss)


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
fit2 <- (sum(habitat_HQ$HQ)/nrow(habitat_HQ)) * (sum(habitat_HQ$Area_sqm)/Schoeps_area_tot) 


# Write table
#write.table(HQ_tot, "HabQlt_output.csv", append = FALSE, sep = ";", col.names = FALSE, row.names = FALSE)






# CALCULATION HABITAT CONNECTIVITY ############################################

### Identify habitat lu classes
## Select semi-natural habitat
lu_Schoeps_snh <- lu_Schoeps %>%
  filter(lu_1 == 'rnge'| lu_1 == 'rngb' | lu_1 == 'wetl')

# ## Select forest edge
# # remove all roads and rivers
# lu_Schoeps_sel <- lu_Schoeps %>%
#   filter(lu_1 != 'utrn')
# 
# # buffer all non-forest polygons
# # nonFOR_buffer <- st_buffer(lu_Schoeps_sel[which(lu_Schoeps_sel$lu_1 != "frst"),], 12.5)   # lenght to decide
# nonFOR_buffer <- st_buffer(filter(lu_Schoeps_sel, lu_1 != "frst"), 12.5)   # lenght to decide
# 
# # Derive forest edge by intersection with non-forest buffer
# # Select forest polygons
# forest <- lu_Schoeps %>%
#   filter(lu_1 == 'frst')
# 
# # Intersect forest polygons with buffer
# forest_edge <- st_intersection(forest, nonFOR_buffer)
# forest_edge <- st_buffer(forest_edge, 0)
# 
# # Select only columns of the initial forest layer
# forest_edge <- forest_edge %>%
#   select(c(1:90))


# Merge together snh and forest edges
lu_Schoeps_habitat <- bind_rows(lu_Schoeps_snh, forest_edge)

# Dissolve adjacent snh
snh_dissolve <- lu_Schoeps_habitat %>% 
  st_buffer(0.0000001) %>% # make a buffer around all parts (to avoid slivers)
  st_union() %>% # unite to a geometry object
  st_sf()# make the geometry a data frame object
#mutate(centrum = T) # return back the data value 

snh_dissolve_singlepol <- st_cast(snh_dissolve, "POLYGON")
lu_Schoeps_snh_dissolve <- st_as_sf(snh_dissolve_singlepol)
lu_Schoeps_snh_dissolve$weight <- as.numeric(1)


## Select fallow lands
# Calculate percentage of time that a field (row) was fallow land
# In the same years for which the SWAT simulations run, i.e. 2009 -2020
lu_Schoeps$fallow_count <- apply(lu_Schoeps[56:67], 1,
                                 function(x) length(which(x=="no_crop_mgt"))) / 12

# select fallow lands
lu_Schoeps_fl <- lu_Schoeps %>%
  filter(lu_1 == 'field' & fallow_count > 0) %>%
  # weight area based on the amount of time that the fallow land is in the rotation
  mutate(weight = fallow_count) %>%
  # select only fields to join
  select(weight)

# Merge together snh, forest edges and fallow land
lu_Schoeps_snh_dissolve <- bind_rows(lu_Schoeps_snh_dissolve, lu_Schoeps_fl)


# Calculate area SNH
lu_Schoeps_snh_dissolve$Area_sqm <- st_area(lu_Schoeps_snh_dissolve)
lu_Schoeps_snh_dissolve$Area <- drop_units(lu_Schoeps_snh_dissolve$Area_sqm) # drop units
lu_Schoeps_snh_dissolve$Area_w <- lu_Schoeps_snh_dissolve$Area * lu_Schoeps_snh_dissolve$weight
#Schoeps_area_snh <- sum(lu_Schoeps_snh_dissolve$Area_sqm)



# Dispersal distances for calculation pij
# Create loop in order to calculate PC for different species
# Calculation connectivity index
for (i in 1:nrow(disp_dist)){
  PC <- MK_dPCIIC(
    nodes=lu_Schoeps_snh_dissolve,
    attribute = "Area_w",
    area_unit = "m2",
    metric = "PC",
    probability = 0.5,
    distance_thresholds = disp_dist$dd_m[i], # written in meters
    onlyoverall=TRUE)
  
  # Add PC value to dataframe
  disp_dist[i, 4] <- PC[1,2]
} 


# Divide for the area of catchment to have value between 0 and 1
disp_dist <- disp_dist %>%
  mutate(PC_norm = PC / (Schoeps_area_tot * Schoeps_area_tot))


#Average PC values for the considered species
# The value is between 0 and 1
fit1 <- mean(disp_dist$PC_norm)


# Write table
#write.table(PC_average, "HabCnt_output.csv", append = FALSE, sep = ";", col.names = FALSE, row.names = FALSE)



#-------------------------------------------------------------------------------
### Run SWAT+
#-------------------------------------------------------------------------------

### 0 - Clean workspace except fitness values
rm(list=setdiff(ls(), c('fit1','fit2')))

### 1 - Load libraries and functions -------------------------------------------
source('calc_opt_indis.R')

library(data.table)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

library(SWATmeasR)

### 2 - Define paths -----------------------------------------------------------
wd <- getwd()
txt_path <- paste0(wd,'/txt') # path to SWAT+ model txt folder

### 3 - Implement measures and run SWAT ---------------------------------
# Load the measR project which is located in the project path.
measr_file <- 'schoeps_240522.measr'
load_measr(paste0(txt_path, '/', measr_file))
# assign the data of the measr project with a specific name to the generic 
# variable with the name 'measr'
assign('measr', get(gsub('.measr$', '', measr_file)))

# Reset SWAT files
measr$reset()

# Read genome
genome <- read.csv('genom.csv', header=T)

# Define HRUs subject of NSWRM implementation
idx <- which(genome == 2)

# Implement NSWRMs
if(is.integer0(idx) == F){
  measr$implement_nswrm(nswrm_id = idx)
  measr$write_swat_inputs()
}

# Run SWAT
set2wd(txt_path)

swat_exe <- list.files(txt_path, '.exe$')
system(swat_exe)

### 4 - Calculate indicators ---------------------------------------------------
##
#
# If you want to consider crop yield as an optimization objective, specify
# grain units to normalize the basin wide sum of crop yields by crop-specific
# nutritional values, please specify grain units for relevant crops
# The grain units must be applicable to dry mass!!!
grain_units <- data.frame('wbar' = 1.163, 
                          'csil' = 1.071, 
                          'wwht' = 1.209, 
                          'wira' = 1.429,
                          'barl' = 1.163,
                          'akgs' = 0.682, 
                          'wiry' = 1.174, 
                          'sgbt' = 1)

# Define objectives. Please keep the naming syntax with fit1, fit2, ...
fit3 <- ind_cha_aa(txt_path, 'cha0926')[3] * -1 #loads should be minimized
fit4 <- ind_bsn_aa_crp(txt_path, names(grain_units), out_type = "yield", grain_units)[1]

#yield_tons <- ind_bsn_aa_crp(txt_path, names(grain_units), out_type = "yield", grain_units)[1]


# Add the fit variables here. Please do not rename 'out'.
out <- t(cbind.data.frame(fit1, fit2, fit3, fit4))

write.table(out, paste0(wd,'/all_output.csv'), 
            row.names = F, quote= F, col.names = F)











































































































































































































































sink()
