#set working directory
if (file.exists('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH')){
  setwd('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH') 
}else{
  setwd('/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH')
}

#load in helper functions
source("Code/Primary/env_functions.R")
source("Code/Primary/Watersheds/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox)
options(modelsummary_format_numeric_latex = "mathmode")

#set up environment
meters = 5000
wind_dist= dist_allow = 10000
ppt = 1000
run_cleaning = FALSE
match_wells = FALSE
old_wells = FALSE
domestic = FALSE
system = FALSE
drop_dups = TRUE #needs to be false if calculating se on difference in theta
drop_far_down = TRUE
drop_far_up = FALSE
well_fd = test_fd = FALSE #flow line distance?
IV = TRUE
fa_resid = TRUE
soil_well = TRUE #get soil properties at well?
drop_states = FALSE

#obtain theta info for Northeastern contamination data
source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/Watersheds/groundwater_algorithm.R")

#read in and set cont site watersheds 
load("New Hampshire/Data/RData/cont_watershed.RData")

#read in sites_ll to get right site number
rs_ll = fread("New Hampshire/Data/rs_ll.csv")
rs_ll$index = 1:nrow(rs_ll)

cont_ws = cont_ws %>% 
  left_join(rs_ll %>% dplyr::select(site, index))


#well data
gw_level = read.delim('Data/Groundwater/nh_gwlevels') %>%
  dplyr::select(agency = agency_cd, 
                site = site_no, 
                gw_level = sl_lev_va, 
                type = site_tp_cd, 
                datum = sl_datum_cd ) %>% 
  dplyr::mutate(gw_level = as.numeric(gw_level)) %>% 
  tidyr::drop_na(gw_level) %>% 
  dplyr::filter(datum == "NAVD88") %>% 
  dplyr::group_by(agency, site) %>% 
  dplyr::summarise(gw_level = mean(gw_level))


#load latitude/longitude information for well id
gw_loc = read_delim('Data/Groundwater/nh_gwloc', 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
gw_loc = gw_loc[-1, ]

gw_loc = gw_loc %>% 
  dplyr::mutate(site_no = as.character(site_no)) %>% 
  dplyr::select(agency = agency_cd, 
                site = site_no, 
                lng = dec_long_va, 
                lat = dec_lat_va)

#merge lat-long onto depth data by well id and site number
gw = gw_level %>% 
  left_join(gw_loc) %>% 
  dplyr::mutate(lng = as.numeric(lng), 
                lat = as.numeric(lat)) %>%
  tidyr::drop_na() %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326, remove = FALSE) 


#get distance from each cont site to the nearest well measurement
dists = st_distance(cont_sites %>% st_transform(32110), 
                    gw %>% st_transform(32110))

cont_sites$min_dist = apply(dists, 1, min)
cont_sites$wmin_dist = apply(dists, 1, which.min)

cont_sites$index = 1:nrow(dists)

#subset cont_sites to only those with a well measurement within 250 meters
cont_sites = cont_sites[which(cont_sites$min_dist <= 250), ]

#assign gw level as that of the nearest measurement
cont_sites$gw_level = gw$gw_level[cont_sites$wmin_dist]

#subset cont_ws to only these sites
cont_ws = cont_ws[which(cont_ws$site %in% cont_sites$site), ]

#function which grabs the gw elevations within 5km

ver_reg = function(i){
  
  ind = cont_ws$index[i]
  cm = which(drop_units(dists[ind, ]) < 5000)
  #now, check whether each measurement location is in the catchment area of i
  csi = st_intersection(cont_ws[i, ] %>% st_transform(32210), gw[cm, ] %>% st_transform(32210))
  csi$cs_ws = 1
  
  #get close wells
  cw = gw[cm, ]
  
  #merge csi with cw to see which measruements are in catchment area
  cw = cw %>% left_join(csi %>% as_tibble() %>% dplyr::select(agency, site = site.1, cs_ws))
  cw[which(is.na(cw$cs_ws)), ]$cs_ws = 0
  
  gw_rs = cont_sites[cont_sites$index == ind, ]$gw_level
  
  reg = fixest::feols(I(gw_level > gw_rs) ~ cs_ws, cw)
  
  return(reg)
  
  

}


v = pblapply(1:nrow(cont_ws), ver_reg)
v[[1]]
v[[2]]
v[[3]]
v[[4]]
v[[5]]
v[[6]]
v[[7]]
v[[8]]
v[[9]]
v[[10]]
v[[11]]
v[[13]]

x = rep(0, length(v))
for (i in 1:length(v)){
  x[i] = v[[i]]$coefficients["cs_ws"]
}
mean(x)
median(x)
