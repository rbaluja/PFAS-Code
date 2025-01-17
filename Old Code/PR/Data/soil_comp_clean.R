soil_files = list.files(modify_path("Data_Verify/Supplemental/Soil isric"), full.names = TRUE)

state_abb = c("MI", "MN", "NH", "NY", "CO", "ME", "VT", "CA", "FL", "ND", "WI")
soil_write = function(state, soil_type, soil_files){
  #subset clay_files to only those paths that begin with NH_clay
  s_files = soil_files[grepl(paste0(state, "_", soil_type), soil_files)]
  
  s_rasters = lapply(s_files, terra::rast)
  sr = terra::rast(s_rasters)
  mean_s = mean(sr)
  names(mean_s) = paste0("mean_", soil_type)
  writeRaster(mean_s, modify_path(paste0("Data_Verify/Soil/isric/", state, "_mean_", soil_type, ".tif")), overwrite = TRUE) 
}

pblapply(state_abb, soil_write, soil_type = "clay", soil_files = soil_files)
pblapply(state_abb, soil_write, soil_type = "sand", soil_files = soil_files)
pblapply(state_abb, soil_write, soil_type = "silt", soil_files = soil_files)
