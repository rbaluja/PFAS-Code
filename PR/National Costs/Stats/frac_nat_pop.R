cbg_pop = fread(modify_path("Data_Verify/National/cbg_pop.csv"))  
cbg_births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"))

c = cbg_pop %>% left_join(cbg_births %>% dplyr::select(county, tract, cbg, births))

#population from cdc wonder births:
sum(c[!is.na(c$births), ]$pop)/sum(c$pop)
#cdc wonder covers 76% of total US population

##########
##Fraction of US pop living in 11 states who tested
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
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

#subset cbgs (births) to only those within 5km of a cont site
csite_buff = cont_sites %>% 
  st_transform(5070) %>% #get to albers projection for meters
  st_buffer(meters)

#bring in cbg pops
cbg_pop = fread(modify_path("Data_Verify/Supplemental/cbg_pop.csv"), colClasses = c("state" = "character"))  

#remove Hawaii and Alaska
cbg_pop = cbg_pop %>% 
  dplyr::filter(!(state %in% c("02", "15")))

states = tigris::states() %>% 
  as_tibble() %>% 
  dplyr::select(state_name = NAME, state = GEOID) %>% 
  dplyr::filter(state_name %in% c("Michigan", 
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

cbg_pop_sub = cbg_pop %>% 
  left_join(states) %>% 
  dplyr::filter(state_name %in% c("Michigan", 
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



#only keep cbgs within a buffer
cbg_pops_close = st_intersection(cbg_pop_sub %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(5070), csite_buff)

#subset cbg_pop to not these states
cbg_pop2 = cbg_pop %>% 
  dplyr::filter(!state %in% unique(cbg_pops_close$state))

prop_close11 = sum(cbg_pops_close$pop)/sum(cbg_pop_sub$pop) #this is the prop of pop in 11 states within 5km of a site (0.0598043)


prop = sum(cbg_pop_sub$pop)/sum(cbg_pop$pop) #this is the prop of pop living in 11 states compared to lower 48 (0.3409937)
1/prop #this is the factor to multiply by to get the lower 48 US pop 