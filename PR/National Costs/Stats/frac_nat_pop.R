cbg_pop = fread("New Hampshire/Data/Supplemental/cbg_pop.csv")
cbg_births = fread("New Hampshire/Data/Supplemental/births_cbg_cleaned_2010.csv")

c = cbg_pop %>% left_join(cbg_births %>% dplyr::select(county, tract, cbg, births))

#population from cdc wonder births:
sum(c[!is.na(c$births), ]$pop)/sum(c$pop)
#cdc wonder covers 76% of US population

#now check which fraction are within 5km of a site
#load in births data
births = read.csv("New Hampshire/Data/Supplemental/births_cbg_cleaned_2010.csv")
#add state column to cbgs
births$county = stringr::str_pad(as.character(births$county), 5, "left", "0")
births$state = stringr::str_sub(births$county, 1, 2)

births$geoid = paste0(births$county, births$tract, births$cbg)
#fwrite(births %>% dplyr::select(geoid, lng, lat), "NH/New Hampshire/Data/Supplemental/cbg_ll.csv")

births = births %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

#load in contamination data
cont_sites = read_xlsx('New Hampshire/Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
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

#subset cbgs (births) to only those within 5km of a cont site
csite_buff = cont_sites %>% 
  st_transform(5070) %>% #get to albers projection for meters
  st_buffer(meters)

#only keep cbgs within a buffer
births = st_intersection(births %>% st_transform(5070), csite_buff)

#some cbgs are within the buffer of more than one site. Only need to know which are close, so keep unique
births = births %>% 
  dplyr::select(county, tract, cbg, births, state, geoid, geometry) %>% 
  unique()

#merge with cbg_pop
c2 = cbg_pop %>% left_join(births %>% as_tibble() %>% dplyr::mutate(county = as.numeric(county)) %>% dplyr::select(county, tract, cbg, births))

sum(c2[!is.na(c2$births), ]$pop)/sum(c2$pop)
                           