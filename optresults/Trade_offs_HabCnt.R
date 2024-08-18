
###################################################

# Title: Trade-offs HabCnt
# Purpose: This code is used to model habitat connectivity separately for the different animal species considered. 
#          It is run using as input the Best Solution coming from the optimization (baseline scenario)
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 17 August 2024

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
setwd("F:/bonato")
path = paste(getwd(),'HabCnt_classes', sep="/")
# Working directory for land use map
path_input = paste(path, 'input',sep="/")
list.files(path_input)
# Working directory for optimization result
path_res = paste(path, 'CoMOLA_results',sep="/")
list.files(path_res)


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


### Upload table with dispersal distances
disp_dist <- read.csv(paste0(path_input,'/disp_distances_07_12.csv'), sep = ";")


### Upload hru list for genome
genome_hru <- read.csv(paste0(path_input,'/measure_location.csv'))


### Upload and clean Best Solutions (=genome)
BestSol <- read.csv(paste0(path_res,'/BS_genomes.csv'), h = F, as.is=T, sep = ";")

# flip rows and column
genome_all <- BestSol


# Table to register the results
BestSol_res <- data.frame(t(BestSol))

BestSol_res <- BestSol_res %>%
  mutate(PC_average = NA) %>%
  mutate(PC_ins = NA) %>%
  mutate(PC_bird = NA) %>%
  mutate(PC_mamm = NA) 




### Code for optimization in COMOLA
# Duplicate lu map to keep an original 
lu_Schoeps_orig <- lu_Schoeps

# Change name hru
lu_Schoeps$name  <- as.numeric(str_remove(lu_Schoeps$name, 'hru'))

lu_Schoeps$lowtill <- NA
lu_Schoeps$field_gras_w <- NA
lu_Schoeps$field_hedge <- NA

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}



# Start loop through the optimization best solutions
for (n in 1:1070) {
  
  ## Select genome column
  genome <- as.data.frame(genome_all[, n])
  
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




  # CALCULATION HABITAT CONNECTIVITY ############################################
  
  ### Identify habitat lu classes
  ## Select semi-natural habitat
  lu_Schoeps_snh <- lu_Schoeps %>%
    filter(lu_1 == 'rnge'| lu_1 == 'rngb' | lu_1 == 'wetl')
  
  # Identify forest edges
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

  # Write output
  # z <- file.path(path, "PC_output2.csv")  
  # write.table(disp_dist, z, append=FALSE ,sep =";",col.names=FALSE ,row.names=FALSE)


  #Average PC values for the considered species
  # The value is between 0 and 1
  PC_average <- mean(disp_dist$PC_norm)
  PC_ins <- mean(disp_dist[1:9, 5])
  PC_bird <- mean(disp_dist[10:18, 5])
  PC_mamm <- mean(disp_dist[11:27, 5])

  BestSol_res[n, 303] <- PC_average
  BestSol_res[n, 304] <- PC_ins
  BestSol_res[n, 305] <- PC_bird
  BestSol_res[n, 306] <- PC_mamm

}




###### PLOTS ######################################
### Plot pareto frontier for each animal class

### Upload and prepare fitness best solutions
BS_fit <- read.csv(paste0(path_res,'/BS_fitness.csv'), h = F, as.is=T, sep = ";")

BS_fit <- data.frame(t(BS_fit))

BS_fit <- BS_fit %>%
  mutate(ID = c(1:1070))
 
 
# Join fitness values with new HabCnt values
BestSol_res <- BestSol_res %>%
  select("HabCnt_av", "HabCnt_ins", "HabCnt_bird", "HabCnt_mamm") %>%
  mutate(ID = c(1:1070))

BS_fit <- BS_fit %>%
  left_join(BestSol_res, by = "ID")


# # Save new cvs file
# z <- file.path(path, "Best_Solutions_fitness.csv")  
# write.table(Best_Sol_fitness, z, append=FALSE ,sep =";",col.names=TRUE ,row.names=FALSE)


# Calculte status quo for each animal class ???
# Create dataframe with status quo value
# StatusQuo <- data.frame(matrix(data=NA, nrow=1, ncol=4))
# names(StatusQuo) <- c("HabCnt", "HabQlt", "WtrQlt", "AgrPrd")
# StatusQuo[1,] <- c(0.00161, 0.0508, -6038.0, 59083.86)


# Plot complete Pareto frontier: insects
p1 <- ggplot() +
  geom_point(data = BS_fit, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt_ins, size = HabQlt), shape = 21, color = "black") +
  #geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Probability of connectivity") +
  scale_size(name = "Habitat quality") + 
  #scale_shape_manual(values = c(21, 21, 21, 21)) +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))


# Plot complete Pareto frontier: birds
p2 <- ggplot() +
  geom_point(data = BS_fit, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt_bird, size = HabQlt), shape = 21, color = "black") +
  #geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Probability of connectivity") +
  scale_size(name = "Habitat quality") + 
  #scale_shape_manual(values = c(21, 21, 21, 21)) +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))


# Plot complete Pareto frontier: small mammals
p3 <- ggplot() +
  geom_point(data = BS_fit, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt_mamm, size = HabQlt), shape = 21, color = "black") +
  #geom_point(data = StatusQuo, aes(x = WtrQlt, y = AgrPrd, fill = HabCnt, size = HabQlt)) +
  # Change names labels
  labs(x = "Phorsphorus load [kg/year]",
       y = "Crop yield [grain unit]") +
  scale_fill_viridis_c(name = "Probability of connectivity") +
  scale_size(name = "Habitat quality") + 
  #scale_shape_manual(values = c(21, 21, 21, 21)) +
  theme(text=element_text(size=18))
#geom_point(data = StatusQuo, aes(x = V304, y = V305, color = V302, size = V303))


# Plot plots close to one another
patchwork <- (p1 | p2 | p3)
patchwork








































































































































































































sink()
