#set working directory
setwd("~/Dropbox/PFAS Infants")

set.seed(1)

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             units, tidycensus)
wbt_verbose(FALSE)

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
rerun_placebos = TRUE
code_check = TRUE
n_cores = 1

#data cleaning
source("PFAS-Code/PR/Data/data_head.R")

nh_shape = tigris::states() %>% 
  dplyr::filter(STUSPS == "NH") %>% 
  st_transform(32110)

#read in placebo functions
source("PFAS-Code/PR/Placebo/placebo_functions.R")

if (rerun_placebos == TRUE){
  placebos_1 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_1, file = modify_path("Data_Verify/RData/placebos_1.RData"))
  
  placebos_2 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_2, file = modify_path("Data_Verify/RData/placebos_2.RData"))
  
  placebos_3 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_3, file = modify_path("Data_Verify/RData/placebos_3.RData"))
  
  placebos_4 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_4, file = modify_path("Data_Verify/RData/placebos_4.RData"))
  
  placebos_5 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_5, file = modify_path("Data_Verify/RData/placebos_5.RData"))
  
  placebos_6 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_6, file = modify_path("Data_Verify/RData/placebos_6.RData"))
  
  placebos_7 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_7, file = modify_path("Data_Verify/RData/placebos_7.RData"))
  
  placebos_8 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_8, file = modify_path("Data_Verify/RData/placebos_8.RData"))
  
  placebos_9 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_9, file = modify_path("Data_Verify/RData/placebos_9.RData"))
  
  placebos_10 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_10, file = modify_path("Data_Verify/RData/placebos_10.RData"))
  
  plac = NULL
  # Loop through the files
  for (i in 1:10) {
    # Create a new environment for loading the file
    env = new.env()
    
    # Load the file into the new environment
    load(modify_path(paste0("Data_Verify/RData/placebos_", i, ".RData")), envir = env)
    
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
  
  n_na = length(which(is.na(plac$preterm)))
  plac = plac %>%
    tidyr::drop_na()
  #15 missing obs, fill those in
  plac_na = dplyr::bind_rows(pblapply(1:n_na, placebo, df, wells))
  
  plac = rbind(plac, plac_na)
  save(plac, file = modify_path("Data_Verify/RData/placebos.RData") )
}else{
  load(modify_path("Data_Verify/RData/placebos.RData") )
}

#This is the input to table S-6. It counts the number of runs with sig positive coef estimates
plac$pre_sig = as.numeric(plac$preterm/plac$preterm_se > 1.281552)
sum(plac$pre_sig)#201 false positives
plac$vpre_sig = as.numeric(plac$vpreterm/plac$vpreterm_se > 2.326348)
sum(plac$vpre_sig)#35 false positives at 1%

plac$lbw_sig = as.numeric(plac$lbw/plac$lbw_se > 2.326348) 
sum(plac$lbw_sig)#103 false positives
plac$llbw_sig = as.numeric(plac$llbw/plac$llbw_se > 1.644854)
sum(plac$llbw_sig)#195 false positives
plac$vlbw_sig = as.numeric(plac$vlbw/plac$vlbw_se > 2.326348)#47 false positives
sum(plac$vlbw_sig)

sum(as.numeric(plac$preterm/plac$preterm_se > 1.281552 & 
                 plac$vpreterm/plac$vpreterm_se > 2.326348 & 
                 plac$lbw/plac$lbw_se > 2.326348 & 
                 plac$llbw/plac$llbw_se > 1.644854 & 
                 plac$vlbw/plac$vlbw_se > 2.326348)) # 2 of the 1000 had the same significance (or greater) on all of our significant effects

