#clean test well data
if (!file.exists(modify_path("Data_Verify/Contamination/cleaned_contwell.csv")) | rerun_fs_clean){
  source("PFAS-Code/PR/Data/pfas_lab_sites.R")
  #get wind stuff for cont_cleaning
  source("PFAS-Code/PR/Data/wind.R")
  
  source("PFAS-Code/PR/Data/cont_cleaning.R")
}

#create GIS objects
source("PFAS-Code/PR/GIS/gis_head.R")
