#get birth cbg's within 5km of a cont site
births = read.csv("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/Nat Data/births_cbg_cleaned_2010.csv")
#add state column to cbgs
births$county = stringr::str_pad(as.character(births$county), 5, "left", "0")
births$state = stringr::str_sub(births$county, 1, 2)

births$geoid = paste0(births$county, births$tract, births$cbg)

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

#Now, get the cbg's within 5km of a cont site
cbg_ll = fread("New Hampshire/Data/Supplemental/cbg_pop.csv")
cont_cbg = st_intersection(cbg_ll %>% 
                             st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                             st_transform(st_crs(csite_buff)), 
                           csite_buff) %>% 
  as_tibble() %>% 
  dplyr::select(state, county, tract, cbg, state.1) %>% 
  unique()

#join with births cbg's within 5km
cc = cont_cbg %>% 
  left_join(births %>% dplyr::mutate(state = as.numeric(state), 
                                     county = as.numeric(county)))

#missing from wonder
mw = cc[is.na(cc$births), ]
unique(mw$state)
mw = mw %>% 
  dplyr::filter(state.1 %in% c("Michigan", 
                             "Minnesota", 
                             "New Hampshire", 
                             "New York", 
                             "Colorado", 
                             "Maine", 
                             "Vermont", 
                             "California", 
                             "Florida", 
                             "North Dakota", 
                             "Wisconsin"))


#bring in population of cbg
cbg_pop = fread("New Hampshire/Data/Supplemental/cbg_pop.csv")

#missing population
mw = mw %>% left_join(cbg_pop)

#full population
cc = cc %>% left_join(cbg_pop)

#prop missing pop
sum(mw$pop)/sum(cc$pop)
