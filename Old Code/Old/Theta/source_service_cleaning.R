sf_use_s2(F)
#First clean the service area data
sa = st_read('Data/Groundwater/NH_Confidential_Data/Secure_Data/Water_and_Sewer_Lines.shp') 


sa = sa %>% 
  rename_all(tolower) %>% 
  dplyr::filter(pipe_type %in% c('WATER', 'BOTH') & sys_type == 'C') %>% #get rid of only sewer service and non community water systems (think restaurants, schools, etc.)
  dplyr::select(sys_id, sys_name, geometry) %>% #only keep the system name and its shape
  group_by(sys_id) %>% 
  dplyr::summarise(geometry = st_union(geometry)) %>% #most large systems have separate shapes for only water and both water/sewer service. Combine them. 
  ungroup() %>%
  unique()

#bring in well data
wells = st_read('Data/Groundwater/NH_Confidential_Data/Secure_Data/Public_Water_Supply_Wells.shp') 

wells = wells %>% 
  rename_all(tolower) %>% 
  dplyr::filter(system_typ == 'C' & source_typ == 'G') %>% #only keep cws groundwater wells
  as_tibble() %>% 
  dplyr::select(system_id, source, longitude, latitude) %>% 
  dplyr::rename(sys_id = system_id, lng = longitude, lat = latitude)

# wells$elevation = get_elev_point(as.data.frame(wells %>%
#                                                  as_tibble() %>%
#                                                  dplyr::select(c(lng, lat))), prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")$elevation
# w_elev = wells %>%
#   as_tibble() %>%
#   dplyr::select(sys_id, source, elevation) %>%
#   unique()
# 
# fwrite(w_elev, "Data/Supplemental/wells_elev.csv")

wells = wells %>% 
  left_join(sa) %>% 
  arrange(sys_id, source) %>% 
  st_as_sf(sf_column_name = 'geometry', crs = 'EPSG:3437') %>% 
  st_transform(5070) %>%
  dplyr::filter(!st_is_empty(.))


sf_use_s2(T)
well_locs = wells %>% 
  as_tibble() %>% 
  dplyr::select(!geometry) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>% 
  st_buffer(0.1) %>%
  st_transform(st_crs(cont_huc_inter))
wells_theta = st_intersection(well_locs, cont_huc_inter)





#find systems with no wells
sa_nowells = st_read('Data/Groundwater/NH_Confidential_Data/Secure_Data/Water_and_Sewer_Lines.shp') 
systems = sa_nowells %>% 
  as_tibble() %>%
  dplyr::filter(PIPE_TYPE %in% c('WATER', 'BOTH') & SYS_TYPE == 'C') %>% 
  dplyr::select(SYS_ID) %>% 
  unique()

#only keeping cws' with at least one active well
w = st_read('Data/Groundwater/NH_Confidential_Data/Secure_Data/Public_Water_Supply_Wells.shp') %>%
  rename_all(tolower) %>% 
  dplyr::filter(system_act == 'ACTIVE' & system_typ == 'C' & source_typ == 'G') %>% 
  as_tibble()

#classify surface water systems as those that have no active wells
sys_skip = systems %>% 
  dplyr::filter(!(SYS_ID %in% unique(w$system_id))) %>% 
  unique()
sys_skip = as.character(sys_skip$SYS_ID)



#pop we have to drop: 349218
#pop that uses groundwater (not including domestic wells): 1143261