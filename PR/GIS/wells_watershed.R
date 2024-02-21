library(whitebox)

#fill sinks
wbt_breach_depressions("/Users/robert/Library/CloudStorage/Box-Box/New Hampshire/Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff", "New Hampshire/Data/QGIS/filled_dem.tiff")

#flow accumulation
wbt_d8_flow_accumulation("New Hampshire/Data/QGIS/filled_dem.tif", "New Hampshire/Data/QGIS/flow_acc.tiff")

#flow direction
wbt_d8_pointer("New Hampshire/Data/QGIS/filled_dem.tif", "New Hampshire/Data/QGIS/flow_dir.tiff")

#read in wells
source("Code/Primary/Watersheds/source_service_cleaning.R")
wells$index = 1:nrow(wells)

wells_ll = wells %>% as_tibble() %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) 

wells_watershed = function(i){
  
  point_sf = wells_ll[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = "New Hampshire/Data/QGIS/flow_acc.tiff", 
                       output = paste0("New Hampshire/Data/QGIS/wells_pp/pp_site_", i, ".shp"), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = "New Hampshire/Data/QGIS/flow_dir.tiff", 
                pour_pts = paste0("New Hampshire/Data/QGIS/wells_pp/pp_site_", i, ".shp"), 
                output = paste0("New Hampshire/Data/QGIS/wells_watershed/watershed_", i, ".tiff"))
  #read in watershed
  ws = terra::rast(paste0("New Hampshire/Data/QGIS/wells_watershed/watershed_", i, ".tiff"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("New Hampshire/Data/QGIS/wells_watershed/Shapes/ws_shape_", i, ".shp"), overwrite = TRUE)
  
}

pblapply(1:nrow(wells_ll), wells_watershed, cl = 4)


files = list.files("New Hampshire/Data/QGIS/wells_watershed/Shapes", pattern = "*.shp", recursive = T, full.names = T)
files = files[!endsWith(files, ".xml")]

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

wells_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
save(wells_ws, file = "New Hampshire/Data/RData/wells_watershed.RData")
