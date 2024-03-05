#clear memory
rm(list = ls())


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
IV = FALSE #Run IV spec?
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
code_check = FALSE

#data cleaning
source("PFAS-Code/PR/Data/data_head.R")

#main analysis
source("PFAS-Code/PR/Main Analysis/binary.R")

figure_s2a = ggplot(wells1, aes(x = dist_near / 1000)) +
  geom_density(aes(fill = "blue"), alpha = 0.4, color = NA) +  # Set fill and remove border
  scale_fill_identity() +  # Use the literal color name
  theme_minimal() +
  xlab("Distance from Nearest Contamination Site (km)") +
  ylab("Density") +
  geom_vline(xintercept = 5, size = 2) + 
  theme(axis.text = element_text(size = 20, face = "bold"), 
        axis.title = element_text(size = 20, face = "bold")) + 
  scale_x_continuous(breaks = c(5, 20, 40, 60))

ggsave("Figures/figure_s2a.png", figure_s2a)
