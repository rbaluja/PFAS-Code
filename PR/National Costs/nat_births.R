#read in birth data (this is the raw file from CDC Wonder)
births = fread(modify_path("Data_Verify/National/nat_births10.txt"), sep = "\t") #only for counties with at least 100,000 people

births =births %>% 
  dplyr::select(!Notes) %>% 
  dplyr::mutate(Births = as.numeric(Births)) %>% 
  dplyr::rename(county = `County Code`, n_birth = Births) %>% 
  dplyr::mutate(county = stringr::str_pad(county, 5, "left", "0"))

##########################
#get population at block group

#This is a crosswalk for state name to state number to state abbreviation 
states = fread('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv')
sc = as.character(states$st)
sc = stringr::str_pad(sc, 2, "left", "0")
#This downloads the cbg-level populations from the census, iterating over state number
cbg_ll = fread(paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG", sc[1], ".txt"))
for (i in 2:length(sc)){
  cll = fread(paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG", sc[i], ".txt"))
  cbg_ll = rbind(cbg_ll, cll)
}

cbg_ll = cbg_ll %>% 
  dplyr::rename(state = STATEFP, 
                county = COUNTYFP, 
                pop = POPULATION, 
                lat = LATITUDE, 
                lng = LONGITUDE, 
                tract = TRACTCE, 
                cbg = BLKGRPCE) %>%
  mutate(state = stringr::str_pad(state, 2, "left", "0"), 
         county = stringr::str_pad(county, 3, "left", "0"))

cbg_ll$county = paste0(cbg_ll$state, cbg_ll$county)

# calculate county level populations and the number of cbgs in each
cbg_births = cbg_ll %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(n_cbg_county = n(), #number of cbgs within a county
                pop_county = sum(pop, na.rm = T)) %>% #population within each county
  ungroup() %>%
  left_join(births) %>% #merge this with birth data to add county-level population and n cbgs in each birth row 
  tidyr::drop_na(n_birth) %>% #remove rows with missing birth counts
  dplyr::mutate(births = (pop/pop_county) * n_birth) #set births in each cbg as the number of births in the county, weighted by the prop of the population in that cbg


cbg_births = cbg_births %>% 
  dplyr::select(county, tract, cbg, lat, lng, births) %>% 
  dplyr::mutate(tract = stringr::str_pad(tract, 6, "left", "0"))


#save cbg-level birth information
fwrite(cbg_births, modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"))
