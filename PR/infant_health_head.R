#set working directory
setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             rgdal, units, tidycensus, ggpattern, forcats)
options(modelsummary_format_numeric_latex = "mathmode")
options(tigris_use_cache = TRUE)

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
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82"
tables = TRUE
figures = TRUE
code_check = FALSE
n_cores = 1
rob_app_fig = FALSE
bs_cov = TRUE #bootstrap covariance matrix for IV

#data cleaning
source("PFAS-Code/PR/Data/data_head.R")

#main analysis
source("PFAS-Code/PR/Main Analysis/main_analy_head.R")

if (tables){
  source("PFAS-Code/PR/Tables/tables.R")
}

if (figures){
  source("PFAS-Code/PR/Figures/figures_head.R") 
}

