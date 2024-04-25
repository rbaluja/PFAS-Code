#calculate watersheds
if (!file.exists(modify_path("Data_Verify/GIS/cont_watershed.RData")) | redo_GIS){
  source("PFAS-Code/PR/GIS/cont_watershed.R") #PFAS Lab sites 
}

if (!file.exists(modify_path("Data_Verify/GIS/fs_test_watershed.RData"))| redo_GIS){
  source("PFAS-Code/PR/GIS/test_wells_watershed.R") #first stage test wells
}

if (!file.exists(modify_path("Data_Verify/GIS/wells_watershed.RData"))| redo_GIS){
  source("PFAS-Code/PR/GIS/wells_watershed.R") #PWS wells 
}

if (!file.exists(modify_path("Data_Verify/GIS/cont_fa_sum_buffed.tiff"))| redo_GIS){
  #calculate flow accumulation
  source("PFAS-Code/PR/GIS/cont_flowacc.R") 
}

if (!file.exists(modify_path("Data_Verify/GIS/f1_watershed.RData"))| redo_GIS){
  #calculate watersheds for figure 1
  source("PFAS-Code/PR/GIS/figure1_watershed.R") 
}