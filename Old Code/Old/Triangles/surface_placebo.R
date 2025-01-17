library(sfheaders)
library(lwgeom)
library(dplyr)
library(geosphere)
library(sp)
library(readxl)
library(sf)
library(raster)
library(plyr)
library(pbapply)
library(tigris)
library(terra)
library(readr)
library(data.table)
library(stringr)
library(elevatr)
library(gmodels)


setwd('~/Desktop/PFAS Infants/New Hampshire')

#set options
meters = 10000 
dist = 10000
gw_dist_allowance = 3000
n_triangles = 6
w_width =10000
run_cont_reg = FALSE
wind = FALSE
run_cleaning = FALSE
first_stage = TRUE


#obtain triangle info for Northeastern contamination data
source('Code/groundwater_algorithm.R')

#well location and service area data (NHDES)
source('Code/source_service_cleaning.R')

source('Code/natality_data.R')

#change crs of df to match that of the water data
df = df %>% 
  st_transform('EPSG:3437')


#first figure out who lives in a service district, and which one
df = df %>% 
  st_join(sa, join = st_within, largest = T)

#subset to only those individuals on surface water sources
df = df %>% 
  dplyr::filter(sys_id %in% sys_skip)


#bring in demographic data

w = fread("Data/Supplemental/nh_cbg_weather.csv") %>% 
  dplyr::rename(geoid = location)
w$year = as.numeric(str_sub(w$date, 1, 4))

w = w %>% 
  group_by(geoid, year) %>% 
  dplyr::mutate(temp = (tmin + tmax)/2) %>% 
  dplyr::summarize(temp = mean(temp, na.rm = T))


#read in and bind pollution data
pm = fread("Data/Supplemental/nh_cbg_pm25.csv")

env = left_join(w, pm)

#read in census vars (at tract level)
dem = fread("Data/Supplemental/tract_stats.csv")
dem = dem %>% 
  dplyr::mutate(tract = as.character(tract)) %>%
  dplyr::mutate(county = dplyr::case_when(
    county < 10 ~ paste0("00", county), 
    county >= 10 ~ paste0("0", county)), 
    tract = dplyr::case_when(
      nchar(as.character(tract)) == 3 ~ paste0("000", tract), 
      nchar(as.character(tract)) == 4 ~ paste0("00", tract), 
      nchar(as.character(tract)) == 5 ~ paste0("0", tract), 
      nchar(as.character(tract)) == 6 ~ tract
    )) %>% 
  dplyr::mutate(geoid = paste0("33", county, tract))

#join environmental and demographic data
env$t_geoid = str_sub(env$geoid, 1, 11)

d = env %>% 
  left_join(dem, by = c("t_geoid" = "geoid"))

#####
##assign block group by lat long

#read in blocks shapefile
load("/Users/nhdata/Library/CloudStorage/Box-Box/NH Supplemental Data/cbg_tigris.RData")

#assign block of residence
df = df %>% 
  st_transform(st_crs(cbg_shape)) %>%
  st_join(cbg_shape, join = st_within, largest = T)

df = df %>% 
  dplyr::rename(geoid = GEOID) %>%
  left_join(d %>% dplyr::mutate(year = as.character(year), 
                                geoid = as.character(geoid)))





#main strategy
df$index = 1:nrow(df)



df_triangle = st_intersection(df  %>% st_transform(st_crs(cont_sites)),
                                cont_sites %>% dplyr::select(industry, triangle, lng, lat, total_pfas) %>% dplyr::rename(cont_lng = lng, cont_lat = lat))

#add in distance to the site
df_ll = df %>% 
  st_transform(4326) %>%
  mutate(lng = unlist(purrr::map(geometry,1)),
         lat = unlist(purrr::map(geometry,2))) %>%
  as_tibble() %>%
  dplyr::select(c(lng, lat, index))
tll = as_tibble(df_triangle) %>% dplyr::select(c(cont_lng, cont_lat)) %>% dplyr::rename(lng = cont_lng, lat = cont_lat)


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(df_ll[which(df_ll$index == df_triangle$index[i]), c("lng", "lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
df_triangle$dist = pbsapply(1:nrow(df_triangle), dist_filler)
df_triangle$dist = as.numeric(df_triangle$dist)

df_triangle = df_triangle %>% 
  dplyr::mutate(km = dplyr::case_when(
    dist <= 1000 ~ 1, 
    dist > 1000 & dist <= 3000 ~ 3, 
    dist > 3000 ~ 5))

wt_summary = df_triangle %>% 
  dplyr::group_by(index, triangle, km) %>% 
  dplyr::summarise(nsites = n(), 
                   pfas = sum(total_pfas)) %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)

wt = wt_summary %>% 
  tidyr::pivot_wider(id_cols = index, names_from = c(triangle, km),
                     names_glue = sprintf('t{triangle}_km{km}_{.value}'), 
                     values_from = c(nsites, pfas))
wt[is.na(wt)] = 0

df = df %>% 
  left_join(wt)
df = df %>% 
  st_transform(4326) %>%
  mutate(well_lng = unlist(purrr::map(geometry,1)),
         well_lat = unlist(purrr::map(geometry,2)))
df = df %>% dplyr::select(!index)

r = fixest::feols(as.formula(paste("gestation", "~",
                                   paste(colnames(df)[endsWith(colnames(df), "nsites") | 
                                                        endsWith(colnames(df), "pfas") ], collapse = "+"), 
                                   " + m_age + gestation + m_married + white + private_insurance + cig + n_prenatal + ",
                                   "pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ +month ",
                                   sep = "")), data = df)
summary(r)
