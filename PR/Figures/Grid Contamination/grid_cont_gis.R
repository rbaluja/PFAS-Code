#create necessary directories
dir.create(modify_path("Data_Verify/GIS/grids"))
dir.create(modify_path("Data_Verify/GIS/grids/grids_pp"))
dir.create(modify_path("Data_Verify/GIS/grids/grids_watershed"))
dir.create(modify_path("Data_Verify/GIS/grids/grids_watershed/Shapes"))

grid_ll_watershed = function(i){
  
  #get location of test well i
  point_sf = ll[i, "geometry"] %>% 
    st_transform(32110) %>%
    st_centroid() %>% 
    st_transform(4326)
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = modify_path("Data_Verify/GIS/flow_acc.tiff"), 
                       output = modify_path(paste0("Data_Verify/GIS/grids/grids_pp/pp_site_", i, ".shp")), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = modify_path("Data_Verify/GIS/flow_dir.tiff"), 
                pour_pts = modify_path(paste0("Data_Verify/GIS/grids/grids_pp/pp_site_", i, ".shp")), 
                output = modify_path(paste0("Data_Verify/GIS/grids/grids_watershed/watershed_", i, ".tiff")))
  
  #read in watershed
  ws = terra::rast(modify_path(paste0("Data_Verify/GIS/grids/grids_watershed/watershed_", i, ".tiff")))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, modify_path(paste0("Data_Verify/GIS/grids/grids_watershed/Shapes/ws_shape_", i, ".shp")), overwrite = TRUE)
  
}

pblapply(1:nrow(ll), grid_ll_watershed, cl = n_cores)


files = list.files(modify_path("Data_Verify/GIS/grids/grids_watershed/Shapes"), pattern = "*.shp", recursive = T, full.names = T)

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$ws_file = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

grid_ll_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = n_cores))
save(grid_ll_ws, file = modify_path("Data_Verify/GIS/grid_ll_watershed.RData"))

#delete intermediate files
unlink(modify_path("Data_Verify/GIS/grid/"), recursive = TRUE)
