sf_use_s2(F)
#read in service area data
sa = st_read(modify_path('Data_Verify/Groundwater/NH_Confidential_Data/Secure_Data/Water_and_Sewer_Lines.shp')) 

sa = sa %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(pipe_type %in% c('WATER', 'BOTH') & sys_type == 'C') %>% #get rid of only sewer service and non community water systems (think restaurants, schools, etc.)
  dplyr::select(sys_id, sys_name, geometry) %>% #only keep the system name and its shape
  dplyr::group_by(sys_id) %>% 
  dplyr::summarise(geometry = st_union(geometry)) %>% #most large systems have separate shapes for only water and both water/sewer service. Combine them. 
  ungroup() %>%
  unique()

#bring in well location data
wells = st_read(modify_path('Data_Verify/Groundwater/NH_Confidential_Data/Secure_Data/Public_Water_Supply_Wells.shp')) 

wells = wells %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(system_act == 'ACTIVE' & source_act == "ACTIVE" & system_typ == 'C' & source_typ == 'G') %>% #only keep cws groundwater wells
  as_tibble() %>% 
  dplyr::select(sys_id = system_id, source, lng = longitude, lat = latitude) 

#merge wells with sys id to get service areas at the system level
wells = wells %>% 
  left_join(sa) %>% 
  arrange(sys_id, source) %>% 
  st_as_sf(sf_column_name = 'geometry', crs = 'EPSG:3437') %>% 
  st_transform(5070) %>%
  dplyr::filter(!st_is_empty(.))

#number of water systems with active gw wells
length(unique(wells$sys_id))

#return sys_skip (systems with no active groundwater wells)
source("PFAS-Code/PR/Data/no_wells.R")
