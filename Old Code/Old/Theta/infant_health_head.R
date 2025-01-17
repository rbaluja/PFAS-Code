#set working directory
setwd('~/Desktop/PFAS Infants/New Hampshire')

#load in helper functions
source("Code/Primary/env_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal)

#set up environment
meters = 10000 #size of radius
dist = meters
run_cleaning = FALSE
ppt = 1000
huc = 10 #watershed size
domestic = FALSE #include domestic wells
wind_dist = 15000
dist_allow = wind_dist
fs_dem = TRUE

#obtain theta info for Northeastern contamination data
source("Code/Primary/Theta/groundwater_algorithm.R")

#well location and service area data (NHDES)
source("Code/Primary/Theta/source_service_cleaning.R")

#set up wind
source("Code/Primary/wind.R")

#first stage
source("Code/Primary/Theta/first_stage_mult.R")


if (run_cleaning == TRUE){
  #read in (and clean) natality data
  source('Code/Primary/natality_data.R')
  
  #combine the well and natality data
  source("Code/Primary/natality_wells.R")
  
  #bring in demographic data
  source("Code/Primary/demographic_merge.R")
  
  #elevation at the well which provides water
  source("Code/Primary/elev.R")
  
}else{
  load("/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality062623.RData")
}

#set up empirical strategy
source("Code/Primary/Theta/main_strat.R")


