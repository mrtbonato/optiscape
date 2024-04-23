
###################################################

# Title: Land_use_map_preparation
# Purpose: This code is used to prepare the land use map used as input for all the models
# Reference: 
# Author: Marta Bonato 
# Date: last modified on 23 April 2024

###################################################



# load required packages
library(tidyverse)
library(sf) # for vector spatial analysis


# Set working directory
setwd("Y:/Gruppen/cle/MichaS/Marta/Optimization_Analysis")
path = paste(getwd(),'DATA', sep="/")
path_input = paste(path, 'input',sep="/")
list.files(path_input)


# Upload input lu map
# 
lu_Schoeps <- read_sf(paste(path_input,'/lu_join','.shp', sep=""))
# lu_Schoeps <- st_buffer(lu_Schoeps, 0.0) #clean geometry
# lu_Schoeps$OBJECTID <- c(1:dim(lu_Schoeps)[1])


# Select only columns relevant for the analyisis
lu_Schoeps <- lu_Schoeps %>%
  select(c(1:84))


# Save file in input folder
z <- file.path(path_input, "lu_Schoeps_final.shp")  
st_write(lu_Schoeps, z)