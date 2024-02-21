library(whitebox)

#fill sinks
wbt_breach_depressions("/Users/robert/Library/CloudStorage/Box-Box/New Hampshire/Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff", "New Hampshire/Data/QGIS/filled_dem.tiff")

#flow direction
wbt_d8_pointer("New Hampshire/Data/QGIS/filled_dem.tif", "New Hampshire/Data/QGIS/flow_dir.tiff")

#read in wells
source("Code/Primary/Watersheds/source_service_cleaning.R")
wells$index = 1:nrow(wells)

wells_ll = wells %>% as_tibble() %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) 


wells_fa = function(i){
  
  point_sf = wells_ll[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  wbt_trace_downslope_flowpaths(seed_pts = temp_point_path, 
                                d8_pntr = "New Hampshire/Data/QGIS/flow_dir.tiff", 
                                output = paste0("New Hampshire/Data/QGIS/wells_fa/cont_fa_", i, ".tiff"))
  
  fa = terra::rast(paste0("New Hampshire/Data/QGIS/wells_fa/cont_fa_", i, ".tiff"))
  
  #transform watershed to a polygon
  fa_poly = as.polygons(fa)
  #save shapefile of watershed
  writeVector(fa_poly, paste0("New Hampshire/Data/QGIS/wells_fa/Shapes/fa_shape_", i, ".shp"), overwrite = TRUE)
  
}

pblapply(1:nrow(wells), wells_fa)


files = list.files("New Hampshire/Data/QGIS/wells_fa/Shapes", pattern = "*.shp", recursive = T, full.names = T)

wells_fa_read = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

wells_fa = dplyr::bind_rows(pblapply(files, wells_fa_read))
save(wells_fa, file = "New Hampshire/Data/RData/wells_fa.RData")
