source("PFAS-Code/Pub/config.R")
#clean test well data
if (!file.exists(modify_path("Data_Verify/Contamination/cleaned_contwell.csv")) | rerun_fs_clean){
  drop_states = FALSE
  source("PFAS-Code/Pub/Data/pfas_lab_sites.R")
  #get wind stuff for cont_cleaning
  source("PFAS-Code/Pub/Data/wind.R")
  
  source("PFAS-Code/Pub/Data/cont_cleaning.R")
}

#create GIS objects
source("PFAS-Code/Pub/GIS/gis_head.R")
