library(whitebox)

#fill sinks
wbt_breach_depressions("/Users/robert/Library/CloudStorage/Box-Box/New Hampshire/Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff", "New Hampshire/Data/QGIS/filled_dem.tiff")

#flow accumulation
wbt_d8_flow_accumulation("New Hampshire/Data/QGIS/filled_dem.tif", "New Hampshire/Data/QGIS/flow_acc.tiff")

#flow direction
wbt_d8_pointer("New Hampshire/Data/QGIS/filled_dem.tif", "New Hampshire/Data/QGIS/flow_dir.tiff")

#read in cont_sites
cont_sites = read_xlsx('New Hampshire/Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(`Site name`, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

cont_sites$index = 1:nrow(cont_sites)
 

cont_watershed = function(i){
  
  point_sf = cont_sites[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = "New Hampshire/Data/QGIS/flow_acc.tiff", 
                       output = paste0("New Hampshire/Data/QGIS/cont_pp/pp_site_", i, ".shp"), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = "New Hampshire/Data/QGIS/flow_dir.tiff", 
                pour_pts = paste0("New Hampshire/Data/QGIS/cont_pp/pp_site_", i, ".shp"), 
                output = paste0("New Hampshire/Data/QGIS/cont_watershed/watershed_", i, ".tiff"))
  #read in watershed
  ws = terra::rast(paste0("New Hampshire/Data/QGIS/cont_watershed/watershed_", i, ".tiff"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("New Hampshire/Data/QGIS/cont_watershed/Shapes/ws_shape_", i, ".shp"), overwrite = TRUE)
  
}

pblapply(1:nrow(cont_sites), cont_watershed, cl = 4)


files = list.files("New Hampshire/Data/QGIS/cont_watershed/Shapes", pattern = "*.shp", recursive = T, full.names = T)
files = files[!endsWith(files, ".xml")]

cont_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

cont_ws = dplyr::bind_rows(pblapply(files, cont_ws, cl = 4))
save(cont_ws, file = "New Hampshire/Data/RData/cont_watershed.RData")
