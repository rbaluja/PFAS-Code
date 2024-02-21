library(whitebox)

#fill sinks
wbt_breach_depressions("/Users/robert/Library/CloudStorage/Box-Box/New Hampshire/Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff", "New Hampshire/Data/QGIS/filled_dem.tiff")

#flow direction
wbt_d8_pointer("New Hampshire/Data/QGIS/filled_dem.tif", "New Hampshire/Data/QGIS/flow_dir.tiff")

#read in test wells
fs_cont = fread("New Hampshire/Data/Contamination/cleaned_contwell_122023.csv")%>% 
  st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326)

fs_cont$index = 1:nrow(fs_cont)



fs_cont_fa = function(i){
  
  point_sf = fs_cont[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  wbt_trace_downslope_flowpaths(seed_pts = temp_point_path, 
                                d8_pntr = "New Hampshire/Data/QGIS/flow_dir.tiff", 
                                output = paste0("New Hampshire/Data/QGIS/fs_cont_fa/cont_fa_", i, ".tiff"))
  
  fa = terra::rast(paste0("New Hampshire/Data/QGIS/fs_cont_fa/cont_fa_", i, ".tiff"))
  
  #transform watershed to a polygon
  fa_poly = as.polygons(fa)
  #save shapefile of watershed
  writeVector(fa_poly, paste0("New Hampshire/Data/QGIS/fs_cont_fa/Shapes/fa_shape_", i, ".shp"), overwrite = TRUE)
  
}

pblapply(1:nrow(fs_cont), fs_cont_fa, cl = 4)


files = list.files("New Hampshire/Data/QGIS/fs_cont_fa/Shapes", pattern = "*.shp", recursive = T, full.names = T)

cont_fa_read = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

cont_fa = dplyr::bind_rows(pblapply(files, cont_fa_read))
save(cont_fa, file = "New Hampshire/Data/RData/fs_cont_fa.RData")
