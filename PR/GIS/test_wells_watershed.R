#create necessary directories
dir.create("Data_Verify/GIS/fs_test")
dir.create("Data_Verify/GIS/fs_test/fs_test_pp")
dir.create("Data_Verify/GIS/fs_test/fs_test_watershed")
dir.create("Data_Verify/GIS/fs_test/fs_test_watershed/Shapes")

#read in test wells
fs_cont = fread("Data_Verify/Contamination/cleaned_contwell_122023.csv")%>% 
  st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326)
#set index to keep track of which well is which
fs_cont$index = 1:nrow(fs_cont)

test_watershed = function(i){
  
  #get location of test well i
  point_sf = fs_cont[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = "Data_Verify/GIS/flow_acc.tiff", 
                       output = paste0("Data_Verify/GIS/fs_test/fs_test_pp/pp_site_", i, ".shp"), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = "Data_Verify/GIS/flow_dir.tiff", 
                pour_pts = paste0("Data_Verify/GIS/fs_test/fs_test_pp/pp_site_", i, ".shp"), 
                output = paste0("Data_Verify/GIS/fs_test/fs_test_watershed/watershed_", i, ".tiff"))
  #read in watershed
  ws = terra::rast(paste0("Data_Verify/GIS/fs_test/fs_test_watershed/watershed_", i, ".tiff"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("Data_Verify/GIS/fs_test/fs_test_watershed/Shapes/ws_shape_", i, ".shp"), overwrite = TRUE)
  
}
#apply above function over all test wells to get their shapes
pblapply(1:nrow(fs_cont), test_watershed, cl = 4)

#list all test well shapes
files = list.files("Data_Verify/GIS/fs_test/fs_test_watershed/Shapes", pattern = "*.shp", recursive = T, full.names = T)

#function to read in test well shapes
well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}
#apply well_ws to each file, resulting in a list of single row dataframes. Then bind rows into a single dataframe
test_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
save(test_ws, file = "Data_Verify/GIS/fs_test_watershed.RData")

#delete intermediate files
unlink("Data_Verify/GIS/fs_test/", recursive = TRUE)
