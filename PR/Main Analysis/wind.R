source("Code/PR/Main Analysis/wind_functions.R")
#redownload contamination site data (twice)
c_sites = read_xlsx('Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
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
  dplyr::filter(industry == 'Industry' ) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326) %>% 
  dplyr::filter(site == "Saint Gobain Performance Plastics")

sites = read_xlsx('Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
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
  dplyr::filter(industry == 'Industry') %>% 
  dplyr::filter(site == "Saint Gobain Performance Plastics") %>%
  dplyr::select(lng, lat) %>% 
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>%
  terra::vect()

#bring in the average daily wind direction from GRIDMET
wind = brick('Data/Wind/agg_met_th_1979_CurrentYear_CONUS.nc')
wind = as(wind, 'SpatRaster')

#get mean raster value at SG
wind = terra::extract(wind, sites, fun = mean, na.rm = T)
colnames(wind)[2:length(wind)] = str_sub(colnames(wind)[2:length(wind)], 2, -1)
c = colnames(wind)[2:(length(wind))]
wind = wind %>% 
  tidyr::pivot_longer(cols = all_of(c), 
               names_to = 'date', 
               values_to = 'd_degrees')
wind$d_radians = pi/180 * wind$d_degrees
#map radians from 0, 2pi to -pi, pi
wind$norm_radians = ifelse(wind$d_radians < pi, wind$d_radians, wind$d_radians - 2*pi)

#change date to actual date
wind$date = as.Date(as.POSIXct(as.numeric(wind$date)*24*60*60, 
                               origin = "1900-01-01", tz="UTC"))
wind$year = lubridate::year(wind$date)

wind = wind %>% 
  dplyr::filter(year == 2015)

