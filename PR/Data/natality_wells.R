#first figure out who lives in a service district, and which one
df = df %>% 
  st_transform(32110) %>%
  st_join(sa %>% 
            st_transform(32110), join = st_within, largest = T)


df = df %>% 
  dplyr::mutate(sys_id = as.character(sys_id))


df[which(is.na(df$sys_id)), ]$sys_id = 'Domestic Well' #Assume those living outside the piped areas are on domestic wells


#make list of the dataframes by sys_id
serviced_list = df %>% 
  dplyr::group_by(sys_id) %>% 
  dplyr::group_split()


#algorithm for assigning well service (assume that for those in a service area, the nearest well is the one that services them)
well_assigner = function(i, l){
  
  #get all birth records corresponding to the ith group (system)
  sl = l[[i]]
  #transform to lat long data
  sl = sl %>% 
    st_transform(4326)
  
  #if the assigned system is not in sys_skip (created in NHDES_PWS.R, no gw well systems) and not domestic well
  if (!(sl$sys_id[1] %in% sys_skip) & sl$sys_id[1] != 'Domestic Well'){
    
    #grab lat long for each birth record
    sl_coord = sl %>% 
      st_coordinates()
    
    #subset wells to only those in the right system
    w = wells %>% 
      dplyr::filter(as.character(sys_id) == sl$sys_id[1]) %>%
      as_tibble() %>%
      dplyr::select(lng, lat, source)
    
    #get distance from each residence to the wells in the assigned system
    distance = distm(sl_coord, w[, c("lng", "lat")])
    
    w$index = 1:nrow(w)
    #for each row in distance, obtain the column index with the minimum valye
    #assign this index to sl (so we can match the right well)
    sl$index = as.vector(apply(distance, 1, FUN = which.min))
    
    #join sl with w by index to get the nearest well in the right service area
    sl = sl %>% 
      left_join(w %>% dplyr::rename(well_lat = lat, well_lng = lng), by = 'index') %>% 
      dplyr::select(-c(index))
  }else if (sl$sys_id[1] == 'Domestic Well'){
    #if sl is outside a service area, then assign its well as its residence
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

#apply the above well assigning function to unique service area by birth record list, bind rows to get df back
df = dplyr::bind_rows(pblapply(1:length(serviced_list), well_assigner, serviced_list))

#this removes individuals who are serviced by surface water (i.e., their supplier is in sys_skip)
df = df %>% 
  tidyr::drop_na(well_lat) %>% 
  unique() #lose 31543 individuals who receive their water from a source without groundwater wells
