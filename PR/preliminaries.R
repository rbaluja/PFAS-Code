#set working directory
setwd("~/Dropbox/PFAS Infants")
source("PFAS-Code/PR/env_functions.R")

code_check = FALSE
redo_GIS = FALSE
rerun_fs_clean = FALSE
rerun_bs = FALSE
n_cores = 1
#create GIS objects
source("PFAS-Code/PR/GIS/gis_head.R")

#clean test well data
if (!file.exists(modify_path("Data_Verify/Contamination/cleaned_contwell.csv")) | rerun_fs_clean){
  source("PFAS-Code/PR/Data/cont_cleaning.R")
}

#calculate bootstrapped standard errors
if (!all(file.exists(modify_path("Data_Verify/RData/bootstrap.RData"),  
                 modify_path("Data_Verify/RData/bootstrap_quant.RData"), 
                 modify_path("Data_Verify/RData/bootstrap_sb.RData"))) | 
    rerun_bs){
  source("PFAS-Code/PR/Bootstrap/bootstrap_iv.R") 
}
