#read in birth data
births =  fread("New Hampshire/Data/Supplemental/nat_births10.txt", sep = "\t") #only for counties with at least 100,000 people
births =births %>% 
  dplyr::select(!Notes) %>% 
  dplyr::mutate(Births = as.numeric(Births))

births = births %>% dplyr::rename(county = `County Code`, n_birth = Births)

births = births %>% 
  mutate(county = as.character(county)) %>% 
  mutate(county = ifelse(nchar(county) == 4, paste0("0", county), county))


#get population at block group
states = fread('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv')
sc = as.character(states$st)
sc = ifelse(nchar(sc) < 2, paste0("0", sc), sc)
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
  mutate(state = as.character(state), 
         county = as.character(county)) %>% 
  mutate(state = ifelse(nchar(state) < 2, paste0("0", state), state), 
         county = ifelse(nchar(county) == 1, paste0("00", county), 
                         ifelse(nchar(county) == 2, paste0("0", county), county)))
cbg_ll$county = paste0(cbg_ll$state, cbg_ll$county)

cbg_births = cbg_ll %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(n_cbg_county = n(), 
                pop_county = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  left_join(births) %>% 
  tidyr::drop_na(n_birth) %>% 
  dplyr::mutate(births = (pop/pop_county) * n_birth)


cbg_births = cbg_births %>% 
  dplyr::select(county, tract, cbg, lat, lng, births)
fwrite(cbg_births, "New Hampshire/Data/Supplemental/births_cbg_cleaned_2010.csv")
