#first figure out who lives in a service district, and which one
df = df %>% 
  st_transform(32110) %>%
  st_join(sa %>% 
            st_transform(32110) %>% 
            st_buffer(250), join = st_within, largest = T)


df = df %>% 
  rename_all(tolower) %>%
  dplyr::mutate(sys_id = as.character(sys_id))


df[which(is.na(df$sys_id)), ]$sys_id = 'Domestic Well' #Assume those living outside the piped areas are on domestic wells

#algorithm for assigning well service (assume that for those in a service area, the nearest well is the one that services them)

#make list of the dataframes by sys_id
serviced_list = df %>% 
  group_by(sys_id) %>% 
  group_split()



well_assigner = function(i, l){
  
  sl = l[[i]]
  sl = sl %>% 
    st_transform(4326)
  
  if (!(sl$sys_id[1] %in% sys_skip) & sl$sys_id[1] != 'Domestic Well'){
    
    sl_coord = sl %>% 
      st_coordinates()
    
    w = wells %>% 
      dplyr::filter(as.character(sys_id) == sl$sys_id[1]) %>%
      as_tibble() %>%
      dplyr::select(lng, lat, source)
    
    distance = distm(sl_coord, w[, c("lng", "lat")])
    
    w$index = 1:nrow(w)
    sl$index = as.vector(apply(distance, 1, FUN = which.min))
    
    sl = sl %>% 
      left_join(w, by = 'index') %>% 
      dplyr::rename(well_lat = lat, well_lng = lng) %>%
      dplyr::select(-c(index))
  }else if (sl$sys_id[1] == 'Domestic Well'){
    sl_coord = sl %>% 
      st_coordinates()
    sl$well_lat = sl_coord[, 2]
    sl$well_lng = sl_coord[, 1]
  }else{
    sl$well_lat = NA
    sl$well_lng = NA
  }
  
  return(sl)
}

df = dplyr::bind_rows(pblapply(1:length(serviced_list), well_assigner, serviced_list, cl = 8))

#this removes individuals who are serviced by surface water (i.e., their supplier is in sys_skip)
df = df %>% 
  tidyr::drop_na(well_lat) %>% 
  unique() #lose 31871 individuals who receive their water from a source without groundwater wells

#save(df, file = "/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality_wdist.RData")
