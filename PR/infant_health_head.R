#set working directory
setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             units, tidycensus)
options(modelsummary_format_numeric_latex = "mathmode")

#set up environment
natality_path = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/" #set path to natality data in Box Health
meters = 5000 #buffer for base spec
wind_dist= dist_allow = 10000 #wind distance cutoff
ppt = 1000 #cutoff for primary contamination site
run_cleaning = FALSE #clean natality data?
match_wells = FALSE #Re match natality data to wells?
domestic = FALSE #include individuals outside of PWS boundaries?
drop_far_down = TRUE
drop_far_up = FALSE
IV = TRUE #Run IV spec?
rerun_fs_clean = FALSE #clean first stage data?
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
GIS_create = FALSE #recreate watershed shapes?
create_figures = FALSE #output figures?
nat_run_cont_ws = FALSE#recreate national watershed shapes?
nat_reassn = FALSE #reassign national CBGs to release sites?
nat_redo_soil = FALSE #recalculate soil stats for national data?
oster_robust = FALSE #run Oster (2019) selection on unobservables?
false_test = FALSE #run falsification test?
census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82"

if (GIS_create == TRUE){
  source("PFAS-Code/PR/GIS/gis_head.R")
}

#data cleaning
source("PFAS-Code/PR/Data/data_head.R")

#main analysis
source("PFAS-Code/PR/Main Analysis/main_analy_head.R")

#Tables
source("PFAS-Code/PR/Tables/tables.R")

#Oster Coefficient of Proportionality (Table S4)
if (oster_robust == TRUE){
  source("PFAS-Code/PR/Robustness/oster_selection.R")
}

#bootstrap standard errors for all results using first stage - Table 2, Figure 4, Figure S3, Table S9, Figure S5
source("PFAS-Code/PR/Main Analysis/bootstrap_iv.R")

#figures
source("PFAS-Code/PR/Figures/figures_head.R")

#national costs
source("PFAS-Code/PR/National Costs/national_costs_head.R")

#Falsification Test
if (false_test == TRUE){
  source("PFAS-Code/PR/Robustness/Placebo/placebo_head.R") 
}

