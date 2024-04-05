#load in contamination data
cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(`Matrix Type` == 'Groundwater' & State != "Alaska") %>% 
  dplyr::select(`Site name`, State, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, state = State, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

#begin by iterating over states and getting catchment areas for all cont sites in the 11 states
states = tigris::states() %>% 
  dplyr::filter(GEOID %in% c("26", "27", "33", "36", "08", "23", "50", "06", "12", "38", "55"))

#merge cont sites with states to get state number (GEOID)
cont_sites = cont_sites %>% 
  left_join(states %>% 
              as_tibble() %>% 
              dplyr::select(state = NAME, state_num = GEOID))


#subset cbgs (births) to only those within 5km of a cont site
csite_buff = cont_sites %>% 
  st_transform(5070) %>% #get to albers projection for meters
  st_buffer(meters)



#create necessary directories
dir.create(modify_path("Data_Verify/GIS/nat_cont"))
dir.create(modify_path("Data_Verify/GIS/nat_cont/cont_pp"))
dir.create(modify_path("Data_Verify/GIS/nat_cont/cont_watershed"))
dir.create(modify_path("Data_Verify/GIS/nat_cont/cont_watershed/Shapes"))

cont_sites$index = 1:nrow(cont_sites)

states11 = c("Michigan", 
             "Minnesota", 
             "New Hampshire", 
             "New York", 
             "Colorado", 
             "Maine", 
             "Vermont", 
             "California", 
             "Florida", 
             "North Dakota", 
             "Wisconsin")

cont_sites = cont_sites %>% 
  dplyr::filter(state %in% states11)

fwrite(cont_sites %>% as_tibble() %>% dplyr::select(state, site, index), modify_path("Data_Verify/GIS/National/nat_rs_ws.csv"))


cont_ws = function(state, states11){
  
  #get dem for state
  e = get_elev_raster(states[which(states$NAME == state), ]$geometry, z = 9)
  writeRaster(e, modify_path(paste0("Data_Verify/GIS/National/", state, "dem.tif")), overwrite = TRUE)
  
  #fill the dem sinks
  wbt_breach_depressions(modify_path(paste0("Data_Verify/GIS/National/", state, "dem.tif")), modify_path(paste0("Data_Verify/GIS/National/", state, "filled_dem.tif")))
  
  #flow accumulation
  wbt_d8_flow_accumulation(modify_path(paste0("Data_Verify/GIS/National/", state, "filled_dem.tif")), modify_path(paste0("Data_Verify/GIS/National", state, "flow_acc.tif")))
  
  #flow direction
  wbt_d8_pointer(paste0(modify_path("Data_Verify/GIS/National/", state, "filled_dem.tif")), modify_path(paste0("Data_Verify/GIS/National/", state, "flow_dir.tif")))
  
  #########
  ##Calculate watershed
  #iterate through sites 
  state_sites = cont_sites[which(cont_sites$state == state), ]
  for (i in 1:nrow(state_sites)){
    point_sf = state_sites$geometry[i]
    temp_point_path = tempfile(fileext = ".shp")
    st_write(point_sf, temp_point_path, quiet = TRUE) 
    
    # Run snap pour points
    wbt_snap_pour_points(pour_pts = temp_point_path, 
                         flow_accum = modify_path(paste0("Data_Verify/GIS/National", state, "flow_acc.tif")), 
                         output = modify_path(paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "pp.shp")),
                         snap_dist = 0.007569 * 5)
    
    #calculate watershed
    wbt_watershed(d8_pntr = modify_path(paste0("Data_Verify/GIS/National/", state, "flow_dir.tif")), 
                  pour_pts = modify_path(paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "pp.shp")), 
                  output = modify_path(paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "_watershed.tif")))
    
    #read in watershed
    ws = terra::rast(modify_path(paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "_watershed.tif")))
    
    #transform watershed to a polygon
    ws_poly = as.polygons(ws)
    #save shapefile of watershed
    writeVector(ws_poly, modify_path(paste0("Data_Verify/GIS/nat_cont/cont_watershed/Shapes/site_", state_sites$index[i] ,  "ws_shape.shp")), overwrite = TRUE)
  }
}
pblapply(states11, cont_ws, states11, cl = 1) 

files = list.files(modify_path("Data_Verify/GIS/nat_cont/cont_watershed/Shapes"), pattern = "*.shp", recursive = T, full.names = T)

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

n_cont_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
n_cont_ws = n_cont_ws %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(state, site, index))
save(n_cont_ws, file = modify_path("Data_Verify/RData/nat_cont_watershed.RData"))

#delete intermediate files
unlink(modify_path("Data_Verify/GIS/nat_cont/"), recursive = TRUE)