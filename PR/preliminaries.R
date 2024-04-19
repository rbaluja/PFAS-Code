#set working directory
setwd("~/Dropbox/PFAS Infants")
source("PFAS-Code/PR/env_functions.R")

load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             units, tidycensus, ggpattern, forcats)

code_check = TRUE
redo_GIS = FALSE
rerun_fs_clean = FALSE
n_cores = 1
ppt = 1000
wind_dist = 10000
meters = 5000
drop_states = FALSE
census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82"

#clean test well data
if (!file.exists(modify_path("Data_Verify/Contamination/cleaned_contwell.csv")) | rerun_fs_clean){
  source("PFAS-Code/PR/Data/pfas_lab_sites.R")
  #get wind stuff for cont_cleaning
  source("PFAS-Code/PR/Data/wind.R")
  
  source("PFAS-Code/PR/Data/cont_cleaning.R")
}

#create GIS objects
source("PFAS-Code/PR/GIS/gis_head.R")
