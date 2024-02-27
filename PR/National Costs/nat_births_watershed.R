#only keep cbgs within a buffer
births = st_intersection(births %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(5070), csite_buff)

births$state = stringr::str_sub(births$county, 1, 2)

births = births[births$state %in% c("26", "27", "33", "36", "08", "23", "50", "06", "12", "38", "55"), ]

births$geoid = paste0(births$county, as.numeric(births$tract), births$cbg)

#only need to know which cbgs are relevant here
births = births %>% 
  dplyr::select(county, tract, cbg, births, state, geoid, geometry) %>% 
  unique()

births = births %>% st_transform(4326)

#get state crosswalk
states = tigris::states() %>% 
  dplyr::filter(GEOID %in% c("26", "27", "33", "36", "08", "23", "50", "06", "12", "38", "55"))

#create necessary directories
dir.create("Data_Verify/GIS/nat_births")
dir.create("Data_Verify/GIS/nat_births/cont_pp")
dir.create("Data_Verify/GIS/nat_births/cont_watershed")
dir.create("Data_Verify/GIS/nat_births/cont_watershed/Shapes")
inner_cbg_ws = function(i, state_cbgs, state){
  point_sf = state_cbgs$geometry[i]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE) 
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = paste0("Data_Verify/GIS/National", state, "flow_acc.tif"), 
                       output = paste0("Data_Verify/GIS/nat_births/cbg_", state_cbgs$geoid[i], "pp.shp"),
                       snap_dist = 0.007569 * 5)
  
  #calculate watershed
  wbt_watershed(d8_pntr = paste0("Data_Verify/GIS/National/", state, "flow_dir.tif"), 
                pour_pts = paste0("Data_Verify/GIS/nat_births/cbg_", state_cbgs$geoid[i], "pp.shp"), 
                output = paste0("Data_Verify/GIS/nat_births/cont_watershed/cbg_", state_cbgs$geoid[i], "_watershed.tif"))
  
  #read in watershed
  ws = terra::rast(paste0("Data_Verify/GIS/nat_births/cont_watershed/cbg_", state_cbgs$geoid[i], "_watershed.tif"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("Data_Verify/GIS/nat_births/cont_watershed/Shapes/cbg_", state_cbgs$geoid[i],  "_ws_shape.shp"), overwrite = TRUE)
}

#set watersheds for each cbg
for (sn in 1:length(unique(births$state))){
  state_num = unique(births$state)[sn]
  state = states[which(states$GEOID == state_num), ]$NAME
  
  
  inds = which(births$state == state_num)
  state_cbgs = births[inds, ]
  x = nrow(state_cbgs)
  pblapply(1:x, inner_cbg_ws, state_cbgs, state)
}

#check to make sure all watershed boundaries were made
x = rep(0, nrow(births))
for (i in 1:nrow(births)){
  if (!file.exists(paste0("Data_Verify/GIS/nat_births/cont_watershed/Shapes/cbg_", births$geoid[i],  "_ws_shape.shp"))){
    x[i] = 1
  }
}
sum(x)
#great, they are all saved. 
#now lets read them all in and save them 
files = list.files("Data_Verify/GIS/nat_births/cont_watershed/Shapes/", pattern = "*.shp", recursive = T, full.names = T)
files = files[!endsWith(files, "pp.shp")]

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$geoid = gsub("[^0-9]", "", f)
  return(w_ws1)
}

wells_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
save(wells_ws, file = "Data_Verify/GIS/nat_cbg_watershed.RData")
