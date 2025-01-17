library(sfheaders)
library(lwgeom)
library(tidyverse)
library(geosphere)
library(sp)
library(readxl)
library(sf)
library(raster)
library(plyr)
library(pbapply)
library(tigris)


setwd('/Users/robert/Dropbox/PFAS Infants/New Hampshire/')

#desired specification
meters = 16093.4 #10 miles
dist = 16093.4
gw_dist_allowance = 3028
n_triangles = 8

#get contamination dataset with the triangles
source('Code/groundwater_algorithm.R')

#build fake dataset
n_state = 20000
n_towns = 30000
n = n_state + n_towns

#lat long
nh_state_shape = states() %>% 
  dplyr::filter(GEOID == 33)

sf_use_s2(T)
df_state = st_sample(nh_state_shape$geometry, size = n_state)
df_state = do.call(rbind, st_geometry(df_state)) %>%
  as_tibble() %>% 
  setNames(c("lng","lat"))

nh_town_shape = places(state = 'NH')

df_town = st_sample(nh_town_shape$geometry, size = n_towns)
df_town = do.call(rbind, st_geometry(df_town)) %>%
  as_tibble() %>% 
  setNames(c("lng","lat"))

df = rbind(df_town, df_state)
rm(df_town, df_state)

#mortality
deaths = rep(0, n)
death_index = sample(1:n, ceiling(4.3 * n/1000))
deaths[death_index] = 1
df$deceased = deaths


#child sex
df$female = sample(0:1, n, replace=TRUE)

#month and year of birth
df$year = sample(2010:2019, n, replace = T)
df$month = sample(1:12, n, replace = T)

#maternal age
df$age = round(rnorm(n, mean = 30, sd = 4), 0)

#marital status
df$married = sample(0:1, n, replace=TRUE, prob = c(.4, .6))

#gestation
df$preterm = sample(0:1, n, replace=TRUE, prob = c(.9, .1))
df$early_preterm = 0
df[which(df$preterm == 1), ]$early_preterm = sample(0:1, length(which(df$preterm == 1)), replace = TRUE, prob = c(.8, .2))

#number of prenatal visits
df$num_prenatal = round(rnorm(n, mean = 8, sd = 2), 0)
low_visits = which(df$num_prenatal <= 4)
zeros = sample(low_visits, size = length(low_visits)/2)
df[zeros, ]$num_prenatal = 0


#birthweight
df$low_wgt = sample(0:1, n, replace=TRUE, prob = c(.91, .09))
df$vlow_wgt = 0
df[which(df$low_wgt== 1), ]$vlow_wgt = sample(0:1, length(which(df$low_wgt == 1)), replace = TRUE, prob = c(.89, .11))

#turn df spatial
df = df %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)


##########################
###Bring in Well Data#####
##########################

source('Code/source_service_cleaning.R')



###################
#Merge Data#######
###################

#change crs of df to match that of the water data
df = df %>% 
  st_transform('EPSG:3437')


#first figure out who lives in a service district, and which one
df = df %>% 
  st_join(sa, join = st_within)

df = df %>% 
  dplyr::mutate(sys_id = as.character(sys_id))
  

df[which(is.na(df$sys_id)), ]$sys_id = 'Domestic Well' #Assume those living outside the piped areas are on domestic wells

#algorithm for assigning well service (assume that for those in a service area, the nearest well is the one that services them)

#make list of the dataframes by sys_id
serviced_list = df %>% 
  group_by(sys_id) %>% 
  group_split()



well_assigner = function(i, list){

  sl = list[[i]]
  
  if (!(sl$sys_id[1] %in% sys_skip) & sl$sys_id[1] != 'Domestic Well'){
    sl = sl %>% 
      st_transform(4326)
    
    sl_coord = sl %>% 
      st_coordinates()
    
    w = wells %>% 
      dplyr::filter(as.character(sys_id) == sl$sys_id[1]) %>%
      as_tibble() %>%
      dplyr::select(lng, lat)
    
    distance = distm(sl_coord, w)
    
    w$index = 1:nrow(w)
    sl$index = as.vector(apply(distance, 1, FUN = which.min))
    
    sl = sl %>% 
      left_join(w, by = 'index') %>% 
      dplyr::rename(well_lat = lat, well_lng = lng) %>%
      dplyr::select(-index)
  }else if (sl$sys_id[1] == 'Domestic Well'){
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

df = dplyr::bind_rows(pblapply(1:length(serviced_list), well_assigner, serviced_list))

df = df %>% 
  drop_na(well_lat) %>% 
  unique()


