#create necessary directories
dir.create("Data_Verify/GIS/cont_site")
dir.create("Data_Verify/GIS/cont_site/cont_pp")
dir.create("Data_Verify/GIS/cont_site/cont_watershed")
dir.create("Data_Verify/GIS/cont_site/cont_watershed/Shapes")

#fill sinks
wbt_breach_depressions("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff", "Data_Verify/GIS/filled_dem.tiff")

#flow accumulation
wbt_d8_flow_accumulation("Data_Verify/GIS/filled_dem.tiff", "Data_Verify/GIS/flow_acc.tiff")

#flow direction
wbt_d8_pointer("Data_Verify/GIS/filled_dem.tiff", "Data_Verify/GIS/flow_dir.tiff")

#read in cont_sites
cont_sites = read_xlsx('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>% # transform into a spatial dataframe
  st_set_crs('+proj=longlat +datum=WGS84' )

#two sites are repeated, remove them
cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

#set index to keep track of which site is which
cont_sites$index = 1:nrow(cont_sites)
#write this mapping to memory so we can know which wells correspond to which indices
fwrite(cont_sites %>% as_tibble() %>% dplyr::select(site, lng, lat, pfas = sum_pfoa_pfos, index), "Data_Verify/GIS/rs_ll_ws.csv")
 
#this function iterates over each site, returning a shape of its watershed
cont_watershed = function(i){
  
  #get location of site i
  point_sf = cont_sites[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = "Data_Verify/GIS/flow_acc.tiff", 
                       output = paste0("Data_Verify/GIS/cont_site/cont_pp/pp_site_", i, ".shp"), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = "Data_Verify/GIS/flow_dir.tiff", 
                pour_pts = paste0("Data_Verify/GIS/cont_site/cont_pp/pp_site_", i, ".shp"), 
                output = paste0("Data_Verify/GIS/cont_site/cont_watershed/watershed_", i, ".tiff"))
  #read in watershed
  ws = terra::rast(paste0("Data_Verify/GIS/cont_site/cont_watershed/watershed_", i, ".tiff"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("Data_Verify/GIS/cont_site/cont_watershed/Shapes/ws_shape_", i, ".shp"), overwrite = TRUE)
  
}
#apply watershed function over all sites 
pblapply(1:nrow(cont_sites), cont_watershed, cl = 4)

#list all cont watershed shapes
files = list.files("Data_Verify/GIS/cont_site/cont_watershed/Shapes", pattern = "*.shp", recursive = T, full.names = T)

#read in all cont watershed shapes, returning a spatial dataframe with one row
cont_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}
#apply cont_ws to each file, resulting in a list of single row dataframes. Then bind rows into a single dataframe
cont_ws = dplyr::bind_rows(pblapply(files, cont_ws, cl = 4))
save(cont_ws, file = "Data_Verify/GIS/cont_watershed.RData")

#delete intermediate files
unlink("Data_Verify/GIS/cont_site/", recursive = TRUE)
