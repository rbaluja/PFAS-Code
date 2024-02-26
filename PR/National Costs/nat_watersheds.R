#add state column to cbgs
births$state = stringr::str_sub(births$county, 1, 2)

births = births %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

births$geoid = paste0(births$county, births$tract, births$cbg)

#load in contamination data
cont_sites = read_xlsx('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
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
dir.create("Data_Verify/GIS/nat_cont")
dir.create("Data_Verify/GIS/nat_cont/cont_pp")
dir.create("Data_Verify/GIS/nat_cont/cont_watershed")
dir.create("Data_Verify/GIS/nat_cont/cont_watershed/Shapes")

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

fwrite(cont_sites %>% as_tibble() %>% dplyr::select(state, site, index), "Data_Verify/GIS/National/nat_rs_ws.csv")


cont_ws = function(state, states11){
  
  #get dem for state
  e = get_elev_raster(states[which(states$NAME == state), ]$geometry, z = 9)
  writeRaster(e, paste0("Data_Verify/GIS/National/", state, "dem.tif"), overwrite = TRUE)
  
  #fill the dem sinks
  wbt_breach_depressions(paste0("Data_Verify/GIS/National/", state, "dem.tif"), paste0("Data_Verify/GIS/National/", state, "filled_dem.tif"))
  
  #flow accumulation
  wbt_d8_flow_accumulation(paste0("Data_Verify/GIS/National/", state, "filled_dem.tif"), paste0("Data_Verify/GIS/National", state, "flow_acc.tif"))
  
  #flow direction
  wbt_d8_pointer(paste0("Data_Verify/GIS/National/", state, "filled_dem.tif"), paste0("Data_Verify/GIS/National/", state, "flow_dir.tif"))
  
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
                         flow_accum = paste0("Data_Verify/GIS/National", state, "flow_acc.tif"), 
                         output = paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "pp.shp"),
                         snap_dist = 0.007569 * 5)
    
    #calculate watershed
    wbt_watershed(d8_pntr = paste0("Data_Verify/GIS/National/", state, "flow_dir.tif"), 
                  pour_pts = paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "pp.shp"), 
                  output = paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "_watershed.tif"))
    
    #read in watershed
    ws = terra::rast(paste0("Data_Verify/GIS/nat_cont/site_", state_sites$index[i] ,  "_watershed.tif"))
    
    #transform watershed to a polygon
    ws_poly = as.polygons(ws)
    #save shapefile of watershed
    writeVector(ws_poly, paste0("Data_Verify/GIS/nat_cont/cont_watershed/Shapes/site_", state_sites$index[i] ,  "ws_shape.shp"), overwrite = TRUE)
  }
}
pblapply(states11, cont_ws, states11, cl = 1) 

files = list.files("Data_Verify/GIS/nat_cont/cont_watershed/Shapes", pattern = "*.shp", recursive = T, full.names = T)

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

n_cont_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
n_cont_ws = n_cont_ws %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(state, site, index))
save(n_cont_ws, file = "Data_Verify/RData/nat_cont_watershed.RData")

#delete intermediate files
unlink("Data_Verify/GIS/nat_cont/", recursive = TRUE)


### All above this line has been refactored and cleaned


#####
##CBG-centroid watersheds (births)

#only keep cbgs within a buffer
births = st_intersection(births %>% st_transform(5070), csite_buff)

#only need to know which cbgs are relevant here
births = births %>% 
  dplyr::select(county, tract, cbg, births, state, geoid, geometry) %>% 
  unique()

births = births %>% st_transform(4326)
inner_cbg_ws = function(i){
  point_sf = state_cbgs$geometry[i]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE) 
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = paste0("Nat Data/state watersheds/", state, "flow_acc.tif"), 
                       output = paste0("Nat Data/cbg_watersheds/cbg_", state_cbgs$geoid[i], "pp.shp"),
                       snap_dist = 0.007569 * 5)
  
  #calculate watershed
  wbt_watershed(d8_pntr = paste0("Nat Data/state watersheds/", state, "flow_dir.tif"), 
                pour_pts = paste0("Nat Data/cbg_watersheds/cbg_", state_cbgs$geoid[i], "pp.shp"), 
                output = paste0("Nat Data/cbg_watersheds/cbg_", state_cbgs$geoid[i],  "_watershed.tif"))
  
  #read in watershed
  ws = terra::rast(paste0("Nat Data/cbg_watersheds/cbg_", state_cbgs$geoid[i],  "_watershed.tif"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("Nat Data/cbg_watersheds/cbg_", state_cbgs$geoid[i],  "_ws_shape.shp"), overwrite = TRUE)
}

#set watersheds for each cbg
for (sn in 1:length(unique(births$state))){
  state_num = unique(births$state)[sn]
  state = states[which(states$GEOID == state_num), ]$NAME
  
  
  inds = which(births$state == state_num)
  state_cbgs = births[inds, ]
  x = nrow(state_cbgs)
  pblapply(1:x, inner_cbg_ws, cl = 2)
}

#check to make sure all watershed boundaries were made
x = rep(0, nrow(births))
for (i in 1:nrow(births)){
  if (!file.exists(paste0("Nat Data/cbg_watersheds/cbg_", births$geoid[i],  "_ws_shape.shp"))){
    x[i] = 1
  }
}
#need to fix 4 of these
redo = which(x == 1)
bredo = births[redo, ]

#all of these are for conneticut, I need to rerun the state level tifs 
state_num = "09"
state = states[which(states$GEOID == state_num), ]$NAME
#get dem for state
e = get_elev_raster(states[which(states$NAME == state), ]$geometry, z = 9)
writeRaster(e, paste0("Nat Data/state watersheds/", state, "dem.tif"), overwrite = TRUE)

#fill the dem sinks
wbt_breach_depressions(paste0("Nat Data/state watersheds/", state, "dem.tif"), paste0("Nat Data/state watersheds/", state, "filled_dem.tif"))

#flow accumulation
wbt_d8_flow_accumulation(paste0("Nat Data/state watersheds/", state, "filled_dem.tif"), paste0("Nat Data/state watersheds/", state, "flow_acc.tif"))

#flow direction
wbt_d8_pointer(paste0("Nat Data/state watersheds/", state, "filled_dem.tif"), paste0("Nat Data/state watersheds/", state, "flow_dir.tif"))

inner_cbg_ws_redo = function(i){
  #all of these are for conneticut, I need to rerun the state level tifs 
  state_num = "09"
  state = states[which(states$GEOID == state_num), ]$NAME
  
  
  point_sf = bredo$geometry[i]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE) 
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = paste0("Nat Data/state watersheds/", state, "flow_acc.tif"), 
                       output = paste0("Nat Data/cbg_watersheds/cbg_", bredo$geoid[i], "pp.shp"),
                       snap_dist = 0.007569 * 5)
  
  #calculate watershed
  wbt_watershed(d8_pntr = paste0("Nat Data/state watersheds/", state, "flow_dir.tif"), 
                pour_pts = paste0("Nat Data/cbg_watersheds/cbg_", bredo$geoid[i], "pp.shp"), 
                output = paste0("Nat Data/cbg_watersheds/cbg_", bredo$geoid[i],  "_watershed.tif"))
  
  #read in watershed
  ws = terra::rast(paste0("Nat Data/cbg_watersheds/cbg_", bredo$geoid[i],  "_watershed.tif"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  
  #save shapefile of watershed
  writeVector(ws_poly, paste0("Nat Data/cbg_watersheds/cbg_", bredo$geoid[i],  "_ws_shape.shp"), overwrite = TRUE)
}
for (i in 1:nrow(bredo)){
  inner_cbg_ws_redo(i)
}

#check to make sure all watershed boundaries were made
x = rep(0, nrow(births))
for (i in 1:nrow(births)){
  if (!file.exists(paste0("Nat Data/cbg_watersheds/cbg_", births$geoid[i],  "_ws_shape.shp"))){
    x[i] = 1
  }
}
sum(x)
#great, they are all saved. 
#now lets read them all in and save them 
files = list.files("Nat Data/cbg_watersheds", pattern = "*.shp", recursive = T, full.names = T)
files = files[!endsWith(files, "pp.shp")]

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$geoid = gsub("[^0-9]", "", f)
  return(w_ws1)
}

wells_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
save(wells_ws, file = "New Hampshire/Data/RData/nat_cbg_watershed.RData")
