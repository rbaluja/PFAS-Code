#create necessary directories
dir.create(modify_path("Data_Verify/GIS/figure1"))
dir.create(modify_path("Data_Verify/GIS/figure1/cont_pp"))
dir.create(modify_path("Data_Verify/GIS/figure1/cont_watershed"))
dir.create(modify_path("Data_Verify/GIS/figure1/cont_watershed/Shapes"))

#get interpolated dem and related rasters for smoothed figure
# dem = terra::rast(modify_path("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff")) 
# dem = terra::disagg(dem, 10, method = "bilinear")
# writeRaster(dem, modify_path("Data_Verify/GIS/dem_smoothed.tiff"))

states = tigris::states() %>% 
  dplyr::filter(STUSPS == "NH")
states = states %>% 
  st_crop(xmin = -71.5, xmax = -71.15, ymin = 44.18, ymax = 44.38)
e = get_elev_raster(states, z = 14)
writeRaster(e, modify_path("Data_Verify/GIS/ger_dem.tiff"), overwrite = TRUE)

#fill sinks
wbt_breach_depressions(modify_path("Data_Verify/GIS/ger_dem.tiff"), modify_path("Data_Verify/GIS/filled_ger_smoothed.tiff"))

#flow accumulation
wbt_d8_flow_accumulation(modify_path("Data_Verify/GIS/filled_ger_smoothed.tiff"), modify_path("Data_Verify/GIS/flow_acc_smoothed_ger.tiff"))

#flow direction
wbt_d8_pointer(modify_path("Data_Verify/GIS/filled_ger_smoothed.tiff"), modify_path("Data_Verify/GIS/flow_dir_smoothed_ger.tiff"))


f1 = data.frame(x = c(-71.35, -71.375, -71.365, -71.32), y = c(44.28, 44.232, 44.25, 44.254)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)


f1_watershed = function(i){
  
  #get location of site i
  point_sf = f1[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = modify_path("Data_Verify/GIS/flow_acc_smoothed_ger.tiff"), 
                       output = modify_path(paste0("Data_Verify/GIS/figure1/cont_pp/pp_site_", i, ".shp")), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = modify_path("Data_Verify/GIS/flow_dir_smoothed_ger.tiff"), 
                pour_pts = modify_path(paste0("Data_Verify/GIS/figure1/cont_pp/pp_site_", i, ".shp")), 
                output = modify_path(paste0("Data_Verify/GIS/figure1/cont_watershed/watershed_", i, ".tiff")))
  #read in watershed
  ws = terra::rast(modify_path(paste0("Data_Verify/GIS/figure1/cont_watershed/watershed_", i, ".tiff")))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, modify_path(paste0("Data_Verify/GIS/figure1/cont_watershed/Shapes/ws_shape_", i, ".shp")), overwrite = TRUE)
  
}
#apply watershed function over three points
pblapply(1:4, f1_watershed, cl = 1)

files = list.files(modify_path("Data_Verify/GIS/figure1/cont_watershed/Shapes"), pattern = "*.shp", recursive = T, full.names = T)

#read in all cont watershed shapes, returning a spatial dataframe with one row
f1_ws_fn = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}
#apply cont_ws to each file, resulting in a list of single row dataframes. Then bind rows into a single dataframe
f1_ws = dplyr::bind_rows(pblapply(files, f1_ws_fn, cl = 1))

save(f1_ws, file = modify_path("Data_Verify/GIS/f1_watershed.RData")) 


#delete intermediate files
unlink(modify_path("Data_Verify/GIS/figure1/"), recursive = TRUE)
