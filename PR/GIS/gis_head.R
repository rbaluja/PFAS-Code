#calculate watersheds
if (!file.exists(modify_path("Data_Verify/GIS/cont_watershed.RData")) | redo_GIS){
  source("PFAS-Code/PR/GIS/cont_watershed.R") #PFAS Lab sites 
}

if (!file.exists(modify_path("Data_Verify/GIS/fs_test_watershed.RData"))| redo_GIS){
  source("PFAS-Code/PR/GIS/test_wells_watershed.R") #first stage test wells- NOTE: This takes a long time to run 
}

if (!file.exists(modify_path("Data_Verify/GIS/wells_watershed.RData"))| redo_GIS){
  source("PFAS-Code/PR/GIS/wells_watershed.R") #PWS wells 
}

if (!file.exists(modify_path("Data_Verify/GIS/cont_fa_sum_buffed.tiff"))| redo_GIS){
  #calculate flow accumulation
  source("PFAS-Code/PR/GIS/cont_flowacc.R") 
}