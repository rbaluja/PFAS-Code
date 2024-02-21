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
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox)
options(modelsummary_format_numeric_latex = "mathmode")
wbt_verbose(FALSE)

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
fa_resid = TRUE
soil_well = TRUE #get soil properties at well?
drop_states = FALSE
relaxed_up = FALSE

#well location and service area data (NHDES)
source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/Watersheds/source_service_cleaning.R")

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

nh_shape = tigris::states() %>% 
  dplyr::filter(STUSPS == "NH") %>% 
  st_transform(32110)

#read in placebo functions
source("Code/Placebo/placebo_functions.R")

if (rerun_placebos == TRUE){
  placebos_1 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_1, file = "New Hampshire/Data/RData/placebos_1.RData")
  
  placebos_2 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_2, file = "New Hampshire/Data/RData/placebos_2.RData")
  
  placebos_3 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_3, file = "New Hampshire/Data/RData/placebos_3.RData")
  
  placebos_4 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_4, file = "New Hampshire/Data/RData/placebos_4.RData")
  
  placebos_5 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_5, file = "New Hampshire/Data/RData/placebos_5.RData")
  
  placebos_6 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_6, file = "New Hampshire/Data/RData/placebos_6.RData")
  
  placebos_7 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_7, file = "New Hampshire/Data/RData/placebos_7.RData")
  
  placebos_8 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_8, file = "New Hampshire/Data/RData/placebos_8.RData")
  
  placebos_9 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_9, file = "New Hampshire/Data/RData/placebos_9.RData")
  
  placebos_10 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_10, file = "New Hampshire/Data/RData/placebos_10.RData")
  
  plac = NULL
  # Loop through the files
  for (i in 1:10) {
    # Create a new environment for loading the file
    env = new.env()
    
    # Load the file into the new environment
    load(paste0("New Hampshire/Data/RData/placebos_", i, ".RData"), envir = env)
    
    # Dynamically access the object name, assuming it's the only object in the file
    object_name = ls(env)
    
    # Use get() to retrieve the object by name from the environment
    current_placebo = get(object_name, env)
    
    # bind plac with current_placebo
    if(is.null(plac)) {
      plac = current_placebo
    } else {
      plac = rbind(plac, current_placebo)
    }
  }
  
  length(which(is.na(plac$preterm)))
  plac = plac %>%
    tidyr::drop_na()
  #23 missing obs, fill those in
  plac_na = dplyr::bind_rows(pblapply(1:23, placebo, df, wells))
  
  plac = rbind(plac, plac_na)
  save(plac, file = "New Hampshire/Data/RData/placebos.RData") 
}else{
  load("New Hampshire/Data/RData/placebos.RData") 
}

plac$pre_sig = as.numeric(plac$preterm/plac$preterm_se > 1.644854)
sum(plac$pre_sig)#149 false positives
plac$lpre_sig = as.numeric(plac$lpreterm/plac$lpreterm_se > 1.644854)
sum(plac$lpre_sig)#129 false positives
plac$vpre_sig = as.numeric(plac$vpreterm/plac$vpreterm_se > 2.326348)
sum(plac$vpre_sig)#39 false positives at 1%

plac$lbw_sig = as.numeric(plac$lbw/plac$lbw_se > 2.326348) 
sum(plac$lbw_sig)#89 false positives
plac$llbw_sig = as.numeric(plac$llbw/plac$llbw_se > 1.644854)
sum(plac$llbw_sig)#177 false positives
plac$vlbw_sig = as.numeric(plac$vlbw/plac$vlbw_se > 2.326348)#49 false positives
sum(plac$vlbw_sig)

sum(as.numeric(plac$preterm/plac$preterm_se > 1.644854 & 
                 plac$lpreterm/plac$lpreterm_se > 1.644854 & 
                 plac$vpreterm/plac$vpreterm_se > 2.326348 & 
                 plac$lbw/plac$lbw_se > 2.326348 & 
                 plac$llbw/plac$llbw_se > 1.644854 & 
                 plac$vlbw/plac$vlbw_se > 2.326348) &
                  plac$mlbw/plac$mlbw_se < -0.0053/0.0046) # 2 of the 1000 had the same significance (or greater) on all of our significant effects

