setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             rgdal, units, tidycensus, ggpattern, forcats)
options("modelsummary_format_numeric_latex" = "plain")
config_modelsummary(factory_latex = "kableExtra")
options(tigris_use_cache = TRUE)


census_key = "Enter your Census API key:"
code_check = FALSE
n_cores = 1

#preliminaries environment
redo_GIS = FALSE
rerun_fs_clean = FALSE

#infant_health head environment
natality_path = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/" #set path to natality data in Box Health
meters = 5000 #buffer for base spec
wind_dist= dist_allow = 10000 #wind distance cutoff
ppt = 1000 #cutoff for primary contamination site
run_cleaning = FALSE #clean natality data?
match_wells = FALSE #Re match natality data to wells?
drop_far_down = TRUE
drop_far_up = FALSE
IV = TRUE #Run IV spec?

#national environment
nat_run_cont_ws = FALSE#recreate national watershed shapes?
nat_reassn = FALSE #reassign national CBGs to release sites?
nb_cbg = FALSE