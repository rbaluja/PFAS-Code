source("PFAS-Code/Pub/Revision2/Surface Water/Data/wind_functions.R")

#first, subset contamination dataset to only Saint Gobain
c_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>% # transform into a spatial dataframe
  st_set_crs('+proj=longlat +datum=WGS84') %>%
  dplyr::filter(site == "Saint Gobain Performance Plastics")

#turn cont_sites into sf object (allows you to use terra package)
sites = cont_sites %>%
  dplyr::filter(site == "Saint Gobain Performance Plastics") %>%
  terra::vect()

#bring in the average daily wind direction from GRIDMET
wind = raster::brick(modify_path('Data_Verify/Wind/agg_met_th_1979_CurrentYear_CONUS.nc'))
wind = as(wind, 'SpatRaster')

#get mean wind direction value at SG
wind = terra::extract(wind, sites, fun = mean, na.rm = T)
#column names(from 2 on) are the date (After X). Remove leading X
colnames(wind)[2:length(wind)] = str_sub(colnames(wind)[2:length(wind)], 2, -1)
c = colnames(wind)[2:(length(wind))]
#pivot wind to long around the date
wind = wind %>% 
  tidyr::pivot_longer(cols = all_of(c), 
               names_to = 'date', 
               values_to = 'd_degrees')
#transform wind direction from degrees to radians
wind$d_radians = pi/180 * wind$d_degrees
#map radians from 0, 2pi to -pi, pi
wind$norm_radians = ifelse(wind$d_radians < pi, wind$d_radians, wind$d_radians - 2*pi)

#change date to actual date
wind$date = as.Date(as.POSIXct(as.numeric(wind$date)*24*60*60, 
                               origin = "1900-01-01", tz="UTC"))
wind$year = lubridate::year(wind$date)

wind = wind %>% 
  dplyr::filter(year == 2015)

