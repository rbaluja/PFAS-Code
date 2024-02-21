################
#Read in Data###
################
sf_use_s2(TRUE)
cont_sites = read_xlsx('Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(`Site name`, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)


cont_sites_buff = cont_sites %>% 
  st_transform(32110) %>% #NH meters projected CRS
  st_buffer(meters) %>% 
  st_transform(4326)

#there are two sites repeated in the data
if (drop_dups == TRUE){
  cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]
  cont_sites_buff = cont_sites_buff[which(!cont_sites_buff$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ] 
}

if (drop_states == TRUE){
  states = tigris::states() %>% 
    dplyr::filter(STUSPS %in% c("VT", "MA", "ME"))
  
  state_dist = function(i){
    cs = cont_sites[i, ]
    cs = cs %>% st_transform(32110)
    
    dist = as.numeric(st_distance(cs, states %>% st_transform(32110)))
    
    cs$dist_nstate = min(dist)
    return(cs)
  }
  
  cont_sites = dplyr::bind_rows(pblapply(1:nrow(cont_sites), state_dist))
  
  cont_sites = cont_sites[which(cont_sites$dist_nstate >= meters), ]
  cont_sites_buff = cont_sites_buff %>% 
    dplyr::filter(site %in% unique(cont_sites$site))
  
}

