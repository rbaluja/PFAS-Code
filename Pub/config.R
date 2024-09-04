setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/Pub/env_functions.R")
source("PFAS-Code/Pub/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             rgdal, units, tidycensus, ggpattern, forcats, rayshader)
options("modelsummary_format_numeric_latex" = "plain")
config_modelsummary(factory_latex = "kableExtra")
options(tigris_use_cache = TRUE)


census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82" #"Enter your Census API key:"
code_check = FALSE
n_cores = 1
code_verify = TRUE

#preliminaries environment
redo_GIS = FALSE
rerun_fs_clean = FALSE
rerun_placebos = TRUE

#infant_health head environment
natality_path = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/" #set path to natality data in Box Health
meters = 5000 #buffer for base spec
wind_dist= dist_allow = 10000 #wind distance cutoff
ppt = 1000 #cutoff for primary contamination site
run_cleaning = FALSE #clean natality data?
drop_far_down = TRUE
drop_far_up = FALSE
IV = TRUE #Run IV spec?
oster_factor = 1.3 #base for Oster calcs

#national environment
nat_run_cont_ws = FALSE#recreate national watershed shapes?
nat_reassn = FALSE #reassign national CBGs to release sites?
nb_cbg = FALSE
#cost per birth by birth outcome (2023 dollars)
lpre_pc = 36728
mpre_pc = 205041
vpre_pc = 204083
vlbw_pc = 2636968.91356
mlbw_pc = 1767021.261968
mort_pc = 4230796.86

#new york
all_wells = T
population_weighted = T
gw_dist_allowance = 5000
nyc = F
longisland = F
rerun_weather = F
rerun_pollution = F
rerun_birthdata = T