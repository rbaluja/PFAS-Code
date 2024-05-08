#set working directory
setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             units, tidycensus, ggpattern, forcats)
options(modelsummary_format_numeric_latex = "mathmode")
options(tigris_use_cache = TRUE)

#set up environment
natality_path = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/" #set path to natality data in Box Health
meters = 5000 #buffer for base spec
wind_dist= dist_allow = 10000 #wind distance cutoff
ppt = 1000 #cutoff for primary contamination site
nat_run_cont_ws = FALSE#recreate national watershed shapes?
nat_reassn = FALSE #reassign national CBGs to release sites?
nb_cbg = FALSE
census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82"
code_check = FALSE
n_cores = 1


#clean raw births data
if (nb_cbg){
  source("PFAS-Code/PR/National Costs/nat_births.R") 
  births = cbg_births
  rm(cbg_births)
}else{
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character"))
}

#get watershed for each cont site
if (nat_run_cont_ws){
  source("PFAS-Code/PR/National Costs/nat_watersheds.R")  
  cont_ws = get(load(modify_path("Data_Verify/GIS/National/nat_cont_watershed.RData")))
  wells_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character"))
}else{
  #load in watersheds
  cont_ws = get(load(modify_path("Data_Verify/GIS/National/nat_cont_watershed.RData")))
  wells_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
}

#assign cbg to sites
if (nat_reassn){
  source("PFAS-Code/PR/National Costs/nat_assn.R")
}else{
  load(modify_path("Data_Verify/National/births_sites_assigned5.RData"))
}

#run national costs (primary - Figure 3 part 1)
source("PFAS-Code/PR/National Costs/nat_costs.R")

#national costs map (figure 3 part 2)
source("PFAS-Code/PR/Figures/figure3_map.R")

#national costs map preterm (figure 3)
source("PFAS-Code/PR/Figures/figure3_map_preterm.R")

#predicted PFAS map (figure 3 part 3)
source("PFAS-Code/PR/Figures/figure3_predicted_pfas_map.R")

#full national costs (figure 3)
source("PFAS-Code/PR/Figures/full_natcost_figure.R")

#map of national release sites and pop density (Figure S4)
source("PFAS-Code/PR/National Costs/nat_map.R")

#national costs (binary - Figure S5)
source("PFAS-Code/PR/National Costs/nat_costs_binary.R")
