#create necessary directories
dir.create("Data_Verify/GIS/wells")
dir.create("Data_Verify/GIS/wells/wells_pp")
dir.create("Data_Verify/GIS/wells/wells_watershed")
dir.create("Data_Verify/GIS/wells/wells_watershed/Shapes")

#read in wells
source("PFAS-Code/PR/Data/NHDES_PWS.R")
#set index to track wells
wells$index = 1:nrow(wells)
#get spatial dataframe for wells
wells_ll = wells %>% as_tibble() %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) 
#write this mapping to memory so we can know which wells correspond to which indices
fwrite(wells_ll %>% as_tibble() %>% dplyr::select(sys_id, source, index), modify_path("Data_Verify/GIS/wells_ll_ws.csv"))

wells_watershed = function(i){
  
  #get location of test well i
  point_sf = wells_ll[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = modify_path("Data_Verify/GIS/flow_acc.tiff"), 
                       output = modify_path(paste0("Data_Verify/GIS/wells/wells_pp/pp_site_", i, ".shp")), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = modify_path("Data_Verify/GIS/flow_dir.tiff"), 
                pour_pts = modify_path(paste0("Data_Verify/GIS/wells/wells_pp/pp_site_", i, ".shp")), 
                output = modify_path(paste0("Data_Verify/GIS/wells/wells_watershed/watershed_", i, ".tiff")))
  
  #read in watershed
  ws = terra::rast(modify_path(paste0("Data_Verify/GIS/wells/wells_watershed/watershed_", i, ".tiff")))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, modify_path(paste0("Data_Verify/GIS/wells/wells_watershed/Shapes/ws_shape_", i, ".shp")), overwrite = TRUE)
  
}

pblapply(1:nrow(wells_ll), wells_watershed, cl = 4)


files = list.files(modify_path("Data_Verify/GIS/wells/wells_watershed/Shapes"), pattern = "*.shp", recursive = T, full.names = T)

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

wells_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
save(wells_ws, file = modify_path("Data_Verify/GIS/wells_watershed.RData"))

#delete intermediate files
unlink("Data_Verify/GIS/wells/", recursive = TRUE)
