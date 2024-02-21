#find systems with no wells
sa_nowells = st_read('Data/Groundwater/NH_Confidential_Data/Secure_Data/Water_and_Sewer_Lines.shp') 

#all cws
systems = sa_nowells %>% 
  as_tibble() %>%
  dplyr::filter(PIPE_TYPE %in% c('WATER', 'BOTH') & SYS_TYPE == 'C') %>% 
  dplyr::select(SYS_ID) %>% 
  unique()

if (old_wells == FALSE){
  #location of all active wells associated with an active cws
  w = st_read('Data/Groundwater/NH_Confidential_Data/Secure_Data/Public_Water_Supply_Wells.shp') %>%
    rename_all(tolower) %>% 
    dplyr::filter(system_act == 'ACTIVE' & source_act == "ACTIVE" & system_typ == 'C' & source_typ == 'G') %>% 
    as_tibble() 
}else{
  #location of all wells (active or not)
  w = st_read('Data/Groundwater/NH_Confidential_Data/Secure_Data/Public_Water_Supply_Wells.shp') %>%
    rename_all(tolower) %>% 
    dplyr::filter( system_typ == 'C' & source_typ == 'G') %>% 
    as_tibble() 
}

#classify surface water systems as those that have no wells
sys_skip = systems %>% 
  dplyr::filter(!(SYS_ID %in% unique(w$system_id))) %>% 
  unique()
sys_skip = as.character(sys_skip$SYS_ID)
