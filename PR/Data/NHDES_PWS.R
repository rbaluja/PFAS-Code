sf_use_s2(F)
#service area data
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

if (old_wells == FALSE){
  wells = wells %>% 
    rename_all(tolower) %>% 
    dplyr::filter(system_act == 'ACTIVE' & source_act == "ACTIVE" & system_typ == 'C' & source_typ == 'G') %>% #only keep cws groundwater wells
    as_tibble() %>% 
    dplyr::select(system_id, source, town, longitude, latitude) %>% 
    dplyr::rename(sys_id = system_id, lng = longitude, lat = latitude) 
}else{
  wells = wells %>% 
    rename_all(tolower) %>% 
    dplyr::filter(system_typ == 'C' & source_typ == 'G') %>% #only keep cws groundwater wells
    as_tibble() %>% 
    dplyr::select(system_id, source, town, longitude, latitude) %>% 
    dplyr::rename(sys_id = system_id, lng = longitude, lat = latitude) 
}

wells = wells %>% 
  left_join(sa) %>% 
  arrange(sys_id, source) %>% 
  st_as_sf(sf_column_name = 'geometry', crs = 'EPSG:3437') %>% 
  st_transform(5070) %>%
  dplyr::filter(!st_is_empty(.))

#return sys_skip (systems with no active groundwater wells)
source("Code/PR/Data/no_wells.R")
