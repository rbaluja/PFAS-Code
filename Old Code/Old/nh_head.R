##See README.md for more info on the sourced files

library(sfheaders)
library(lwgeom)
library(dplyr)
library(geosphere)
library(sp)
library(readxl)
library(sf)
library(raster)
library(plyr)
library(pbapply)
library(tigris)
library(terra)
library(readr)
library(data.table)
library(stringr)
library(elevatr)
library(gmodels)
library(rgdal)


setwd('~/Desktop/PFAS Infants/New Hampshire')

#set options
meters = 12000
dist = 12000
gw_dist_allowance = 3000
n_triangles = 6
w_width = 2 * meters * sin(pi/8)
run_cont_reg = FALSE
wind = FALSE
run_cleaning = FALSE
first_stage = TRUE
rerun_gw = FALSE
ppt = 1000
t_option = "Theta"
huc = 12

  
#obtain triangle info for Northeastern contamination data
source(paste0('Code/', t_option, '/groundwater_algorithm.R'))

#well location and service area data (NHDES)
source(paste0('Code/', t_option,'/source_service_cleaning.R'))

if (first_stage == TRUE){
  source(paste0('Code/', t_option,'/first_stage.R'))
}

if (run_cleaning == TRUE){
  #read in (and clean) natality data
  source('Code/natality_data.R')
  
  #combine the well and natality data
  source("Code/natality_wells.R")
  
  #bring in demographic data
  source("Code/demographic_merge.R")
  
  #elevation at the well which provides water
  source("Code/elev.R")
  
}else{
  load("/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality_predem062323.RData")
  df = df %>% 
    left_join(w_elev)
}



if (t_option == "Triangles"){
  source("Code/Triangles/semipar_setup.R")
}







if (wind == TRUE){
  source("Code/wind.R")
}  

#setup triangles
source("code/semipar_setup.R")

#run basic triangle strategy
source("Code/triangle_strat.R")

#get figure function if needed
source("Code/Triangles/figure_fun.R")

#run analysis with state testing data
source("Code/cont_data_df.R")




