#clear memory
rm(list = ls())
#restart R
.rs.restartR()

#set working directory
if (file.exists('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH')){
  setwd('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH') 
}else{
  setwd('/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH')
}

#load in helper functions
source("Code/Primary/env_functions.R")
source("Code/Primary/Watersheds/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets)
options(modelsummary_format_numeric_latex = "mathmode")

#set up environment
meters = 5000
wind_dist= dist_allow = 10000
ppt = 1000
run_cleaning = FALSE
match_wells = FALSE
old_wells = FALSE
domestic = FALSE
system = FALSE
drop_dups = TRUE #needs to be false if calculating se on difference in theta
drop_far_down = TRUE
drop_far_up = FALSE
well_fd = test_fd = FALSE #flow line distance?
IV = TRUE
fa_resid = FALSE

#obtain theta info for Northeastern contamination data
source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/Watersheds/groundwater_algorithm.R")

#well location and service area data (NHDES)
source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/Watersheds/source_service_cleaning.R")

#set up wind
source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/wind.R")


if (run_cleaning == TRUE){
  #read in (and clean) natality data
  source('/Users/robert/Documents/GitHub/PFAS_IH/Primary/natality_data.R')
  
  #get covariates for birth records
  source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/birth_covars.R")
  
}else if (match_wells == TRUE){
  load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_wdem122023.RData")
  #combine the well and natality data
  source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/natality_wells.R")
  
  #get elevation at relevant well and residence
  source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/elev_setup.R")
}else if (old_wells == FALSE){
  load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_matched122023.RData") 
}else{
  load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_matched_oldwells122023.RData")
}

if (domestic == FALSE){
  df = df[df$sys_id != "Domestic Well", ] #50874 individuals on domestic water
}

rs_ll = fread("New Hampshire/Data/rs_ll.csv")
rs_ll$index = 1:nrow(rs_ll)
well_dist = function(i, meters){
  w = wells[i, ]
  
  dists = distm(c(w$lng, w$lat), rs_ll[, c("lng", "lat")])
  
  ind = which.min(dists)
  
  #if neither down, nor up, set distance as that to the nearest site
  w$dist_near = dists[ind]
  #if neither down, nor up, set pfas as that at the nearest site
  w$pfas_near = rs_ll$pfas[ind]
  #get number of nearby sites
  w$n_sites_meters = length(which(dists <= meters))
  
  w$site_near = rs_ll$site[ind]
  
  return(w)
  
  
}
wells1 = dplyr::bind_rows(pblapply(1:nrow(wells), well_dist, meters))

df = df %>% left_join(wells1 %>% as_tibble() %>% dplyr::select(!geometry), by = c("sys_id","source"))

ggplot(wells1, aes(x = dist_near / 1000)) +
  geom_density(aes(fill = "blue"), alpha = 0.4, color = NA) +  # Set fill and remove border
  scale_fill_identity() +  # Use the literal color name
  theme_minimal() +
  xlab("Distance from Nearest Contamination Site (km)") +
  ylab("Density") +
  geom_vline(xintercept = 5, size = 1) + 
  theme(axis.text = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 12, face = "bold")) + 
  scale_x_continuous(breaks = c(5, 20, 40, 60))
