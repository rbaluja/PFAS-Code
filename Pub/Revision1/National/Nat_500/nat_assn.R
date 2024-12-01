###Prepare main datasets to be merged with births_ws and cont_ws
births$state = stringr::str_sub(births$county, 1, 2)

births$geoid = paste0(births$county, as.numeric(births$tract), births$cbg)
fwrite(births %>% dplyr::select(geoid, lng, lat), modify_path("Data_Verify/National/cbg_ll.csv"))

births = births %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

#load in contamination data
cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
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
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

#subset cbgs (births) to only those within 5km of a cont site
csite_buff = cont_sites %>% 
  st_transform(5070) %>% #get to albers projection for meters
  st_buffer(meters)

#only keep cbgs within a buffer
births = st_intersection(births %>% st_transform(5070), csite_buff)

#only keep unique birth observations within 'meters' of a cont site
births = births %>% 
  dplyr::select(county, tract, cbg, births, state, geoid, geometry) %>% 
  unique()

#only keep births in 11 states
births = births %>% 
  dplyr::filter(state %in% c("26", "27", "33", "36", "08", "23", "50", "06", "12", "38", "55"))



#merge watersheds
#cont sites
cont_ws = cont_ws %>% 
  left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, state, pfas = sum_pfoa_pfos))

#births
births_ws = births_ws %>%  #this is how the original object was named. It isnt actually wells
  left_join(births %>% as_tibble() %>% dplyr::select(geoid, births))


source("PFAS-Code/Pub/Revision1/National/Nat_500/well_assn.R") 
