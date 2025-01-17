################
#Read in Data###
################
sf_use_s2(TRUE)
#read in cont_sites
cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>% # transform into a spatial dataframe
  st_set_crs('+proj=longlat +datum=WGS84')

#two sites are repeated, remove them
cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

#buffer cont sites by 'meters' meters (5000 in base spec), then transform back to lat long coordinate system
cont_sites_buff = cont_sites %>% 
  st_transform(32110) %>% #NH meters projected CRS
  st_buffer(meters) %>% 
  st_transform(4326)

#If running robustness spec where dropping sites near state borders
if (drop_states == TRUE){
  states = tigris::states() %>% 
    dplyr::filter(STUSPS %in% c("VT", "MA", "ME")) #get shapes of bordering states
  
  #this function takes as input the cont site index and returns the distance to the neatesy state border
  state_dist = function(i){
    cs = cont_sites[i, ]
    cs = cs %>% st_transform(32110)
    
    dist = as.numeric(st_distance(cs, states %>% st_transform(32110)))
    
    cs$dist_nstate = min(dist)
    return(cs)
  }
  
  #find nearest state border for each cont site
  cont_sites = dplyr::bind_rows(pblapply(1:nrow(cont_sites), state_dist))
  
  #subset cont sites to only those further than 'meters' from a state border
  cont_sites = cont_sites[which(cont_sites$dist_nstate >= meters), ]
  #do the same for the buffered dataframe
  cont_sites_buff = cont_sites_buff %>% 
    dplyr::filter(site %in% unique(cont_sites$site))
  
}

