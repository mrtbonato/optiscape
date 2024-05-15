
###################################################

# Title: Habitat Connectivity model
# Purpose: This code is used to model habitat connectivity
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 23 April 2024

###################################################


setwd("D:/bonato/Opt_05_24/CoMOLA_fallow/models/HabCnt")
sink("D:/bonato/Opt_05_24/CoMOLA_fallow/models/HabCnt/console.txt", append=FALSE)



### LOAD REQUIRED PACKAGES
library(tidyverse)
library(sf) # for vector spatial analysis
library(terra) # for raster spatial analysis
library(units)  # to drop the units


# install and load required packages for connectivity index calculation
library(devtools)
library(remotes)
#install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")
library(Makurhini)



### SET WORKING DIRECTORY
path = getwd()
path_input = paste(path, 'input',sep="/")


### UPLOAD ALL NECESSARY INPUTS
### Upload input lu map
# Necessary for the calculation of all indicators
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
	
    if(substr(genome_hru$name[i],1,4) %in% c("gras","buff", "fall")){
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




### Identify habitat lu classes
## Select semi-natural habitat
lu_Schoeps_snh <- lu_Schoeps %>%
  filter(lu_1 == 'rnge'| lu_1 == 'rngb' | lu_1 == 'wetl')

## Select forest edge
# remove all roads and rivers
lu_Schoeps_sel <- lu_Schoeps %>%
  filter(lu_1 != 'utrn')

# buffer all non-forest polygons
# nonFOR_buffer <- st_buffer(lu_Schoeps_sel[which(lu_Schoeps_sel$lu_1 != "frst"),], 12.5)   # lenght to decide
nonFOR_buffer <- st_buffer(filter(lu_Schoeps_sel, lu_1 != "frst"), 12.5)   # lenght to decide

# Derive forest edge by intersection with non-forest buffer
# Select forest polygons
forest <- lu_Schoeps %>%
  filter(lu_1 == 'frst')

# Intersect forest polygons with buffer
forest_edge <- st_intersection(forest, nonFOR_buffer)
forest_edge <- st_buffer(forest_edge, 0)

# Select only columns of the initial forest layer
forest_edge <- forest_edge %>%
  select(c(1:90))


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

# Merge together snh and forest edges
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
PC_average <- mean(disp_dist$PC_norm)
 

# Write table
write.table(PC_average, "HabCnt_output.csv", append = FALSE, sep = ";", col.names = FALSE, row.names = FALSE)




###############################################################################

# Equivalent Connected Area
#MK_dECA(
#nodes,
#attribute = NULL,
#area_unit = "m2",
#distance = list(type = "centroid", resistance = NULL),
#metric = "IIC",
#probability = NULL,
#distance_thresholds = NULL,
#LA = NULL,
#plot = FALSE,
#parallel = NULL,
#write = NULL,
#intern = TRUE
#)


# Estimate the integral index of connectivity (IIC) and probability of connectivity
#MK_dPCIIC(
 # nodes,
  #attribute = NULL,
  #area_unit = "m2",
  #restoration = NULL,
  #distance = list(type = "centroid", resistance = NULL),
  #metric = c("IIC", "PC"),
  #probability = NULL,
  #distance_thresholds = NULL,
  #overall = FALSE,
  #onlyoverall = FALSE,
  #LA = NULL,
  #rasterparallel = NULL,
  #write = NULL,
  #intern = TRUE
#)


















































































































































































sink()
