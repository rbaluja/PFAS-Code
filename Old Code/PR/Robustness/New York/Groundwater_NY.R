#set working directory
setwd("~/Dropbox/PFAS Infants/")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             units, tidycensus, ggpattern, forcats, zipcodeR)

options("modelsummary_format_numeric_latex" = "plain")
config_modelsummary(factory_latex = "kableExtra")

#set options
all_wells = T
population_weighted = T
gw_dist_allowance = 5000
nyc = F
longisland = F
rerun_weather = F
rerun_pollution = F
rerun_birthdata = F
code_check = F
census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82"
n_cores = 1


#read in the water measurement data
gw_level = read.delim(modify_path4('New York/Data/Groundwater/gwlevels_ny'))
gw_level$sl_lev_va = as.numeric(gw_level$sl_lev_va)
gw_level = gw_level %>% tidyr::drop_na(sl_lev_va) #gets rid of missing data
gw_level = gw_level %>% 
  dplyr::filter(sl_datum_cd == 'NAVD88' & site_tp_cd != 'ST') #At this point, only use well measurements. Also set the reference datum. 

#find average depth for each well
gw_level = gw_level %>% 
  dplyr::group_by(agency_cd, site_no) %>% 
  dplyr::summarize(gw_depth = mean(sl_lev_va))

#if all_wells == False, then only use well measurements from 2000. When doing this, the majority of measurements drop out. (Bo said we don't need to do this.)
if (all_wells == F){
  gw_level$lev_dt = as.Date(gw_level$lev_dt)
  gw_level = subset(gw_level, gw_level$lev_dt >= as.Date('2000-01-01'))
}

#load latitude/longitude information for well id
gw_loc = read_delim(modify_path4('New York/Data/Groundwater/gwlevels_tabbed'), 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
gw_loc = gw_loc[-1, ] #drop first row
gw_loc$site_no = as.character(gw_loc$site_no)

gw = gw_loc %>% 
  dplyr::select(agency_cd, site_no, lng = dec_long_va, lat = dec_lat_va) %>% 
  dplyr::mutate(lng = as.numeric(lng), lat = as.numeric(lat)) %>%
  left_join(gw_level) %>% 
  tidyr::drop_na(gw_depth) %>% 
  st_as_sf(coords = c("lng", "lat"), remove = F, crs = 4326) %>% 
  st_transform(32118)


#load and combine vitality data 2010-1018
if (rerun_birthdata == T){
  gw_ny = read_csv(modify_path4('New York/Data/Births/ny_birth10-12.csv'), 
                   col_types = cols(...1 = col_skip(), `Out of Wedlock` = col_skip(), 
                                    `Medicaid or Self-pay` = col_skip(),
                                    `Infant Deaths Rate` = col_skip(), 
                                    `Neonatal Deaths Rate` = col_skip(), 
                                    `Teen Birth Rate` = col_skip(), `Teen Pregnancy Rate` = col_skip()))
  colnames(gw_ny) = c('zip', 'total_birth', 'percent_premature', 'percent_all_low','percent_late_care', 'infant_deaths', 'neonatal_deaths')
  gw_ny$year = '2010-2012'
  
  df = read_csv(modify_path4('New York/Data/Births/ny_birth13-15.csv'), 
                col_types = cols(...1 = col_skip(), 
                                 `Medicaid or Self-pay` = col_skip(), 
                                 `Infant Deaths Rate` = col_skip(), 
                                 `Neonatal Deaths Rate` = col_skip(), 
                                 `Teen Birth Rate` = col_skip(), `Teen Pregnancy Rate` = col_skip(), `Unmarried Parent` = col_skip()))
  colnames(df) = c('zip', 'total_birth', 'percent_premature', 'percent_all_low','percent_late_care', 'infant_deaths', 'neonatal_deaths')
  df$year = '2013-2015'
  
  gw_ny = rbind(gw_ny, df)
  
  
  df = read_csv(modify_path4('New York/Data/Births/ny_birth16-18.csv'), 
                col_types = cols(...1 = col_skip(),  
                                 `Medicaid or Self-pay` = col_skip(), 
                                 `Infant Deaths Rate` = col_skip(), 
                                 `Neonatal Deaths Rate` = col_skip(), 
                                 `Teen Birth Rate` = col_skip(), `Teen Pregnancy Rate` = col_skip(), `Unmarried Parent` = col_skip()))
  colnames(df) = c('zip', 'total_birth', 'percent_premature', 'percent_all_low','percent_late_care', 'infant_deaths', 'neonatal_deaths')
  df$year = '2016-2018'
  
  gw_ny = rbind(gw_ny, df)
}else{
  gw_ny = read.csv(modify_path4('New York/Data/Births/ny_with_nearest_well_pop_full_run.csv'))
  gw_ny = subset(gw_ny, select = - X)
}


gw_ny$percent_infant_deaths = gw_ny$infant_deaths/gw_ny$total_birth * 100 
gw_ny$percent_neonatal_deaths = gw_ny$neonatal_deaths/gw_ny$total_birth * 100

if(nyc == F){
  #getting rid of nyc(some of this is likely redundant, but I believe that it gets the job done.)
  staten= search_city('Staten Island', 'NY')
  bronx = search_city('Bronx', 'NY')
  yonkers = search_city('Yonkers', 'NY')
  brooklyn = search_city('Brooklyn', 'NY')
  queens = search_county('Queens', 'NY')
  kings = search_county('Kings', 'NY')
  nassau = search_county('Nassau', 'NY')
  suffolk = search_county('Suffolk', 'NY')
  new_york = search_city('New York', 'NY')
  
  staten = as.numeric(staten$zipcode)
  bronx = as.numeric(bronx$zipcode)
  yonkers = as.numeric(yonkers$zipcode)
  brooklyn = as.numeric(brooklyn$zipcode)
  queens = as.numeric(queens$zipcode)
  kings = as.numeric(kings$zipcode)
  nassau = as.numeric(nassau$zipcode)
  suffolk = as.numeric(suffolk$zipcode)
  new_york = as.numeric(new_york$zipcode)
  
  
  gw_ny = subset(gw_ny, !(gw_ny$zip %in% staten))
  gw_ny = subset(gw_ny, !(gw_ny$zip %in% bronx))
  gw_ny = subset(gw_ny, !(gw_ny$zip %in% yonkers))
  gw_ny = subset(gw_ny, !(gw_ny$zip %in% brooklyn))
  gw_ny = subset(gw_ny, !(gw_ny$zip %in% queens))
  gw_ny = subset(gw_ny, !(gw_ny$zip %in% kings))
  gw_ny = subset(gw_ny, !(gw_ny$zip %in% new_york))
  if (longisland == F){
    gw_ny = subset(gw_ny, !(gw_ny$zip %in% suffolk))
    gw_ny = subset(gw_ny, !(gw_ny$zip %in% nassau))
  }
}

#geocode the data

if(rerun_birthdata == T){
  if (population_weighted == F){
    lat_long = geocode_zip(gw_ny$zip)
    gw_ny= merge(gw_ny, lat_long, by.x = 'zip', by.y = 'zipcode', all.x = T)
    #13611 (Belleville) is Missing, so I use Google to fill it in.
    gw_ny$lat[is.na(gw_ny$lat)] = 43.800
    gw_ny$lng[is.na(gw_ny$lng)] = -76.12
  }else{
    pop_weighted_location = read_csv(modify_path4('New York/Data/Supplemental/ZIP_Code_Population_Weighted_Centroids.csv')) %>% #this dataset comes from https://hudgis-hud.opendata.arcgis.com/datasets/d032efff520b4bf0aa620a54a477c70e/explore
      dplyr::select(zip = STD_ZIP5, state = USPS_ZIP_PREF_STATE_1221, lng = LGT, lat = LAT) %>% 
      dplyr::filter(state == "NY") %>% 
      dplyr::select(!state)

    gw_ny = gw_ny %>% 
      left_join(pop_weighted_location %>% 
                  dplyr::mutate(zip = as.numeric(zip)))
    
    rm(pop_weighted_location)
  }
}



gw_ny_contaminated = read_excel(modify_path4("New York/Data/Pollution/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx"), sheet = 2) %>% 
  dplyr::filter(State == "New York" & `Matrix Type` == "Groundwater") %>% 
  dplyr::select(name = `Site name`, 
                lng = Longitude,
                lat = Latitude, 
                industry = Industry, 
                total_pfas = `PFAS Level (ppt)`
                )


#add elevation data 
ny = tigris::states() %>% 
  dplyr::filter(STUSPS == "NY")
ny_dem = get_elev_raster(ny, z = 10)

rs_ll = gw_ny_contaminated %>% 
  dplyr::select(name, lng, lat) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

e1 = raster::extract(ny_dem, rs_ll)
gw_ny_contaminated$elevation = e1

b_ll = gw_ny %>% 
  dplyr::select(zip, year, lng, lat) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

e1 = raster::extract(ny_dem, b_ll)
gw_ny$elevation = e1
  

#water level algorithms

gw_ny = gw_ny %>% 
  st_as_sf(coords = c("lng", "lat"), remove = F, crs = 4326) %>% 
  st_transform(32118)

gw_assn = function(i, df, gw){
  d = df[i, ]
  #get distance from row i to each groundwater measurement
  dists = as.numeric(st_distance(d, gw))
  
  dmin_ind = which.min(dists)
  dmin = dists[dmin_ind]
  
  if (dmin > gw_dist_allowance){ #if nearest gw measurement is further than allowance, set gw at NA
    d$gw_level = NA
    d$dist_gw = NA
    return(d)
  }
  
  if (dmin <= 500){ #if minimum distance is less than 500 meters away, set groundwater level at that 
    d$gw_level = mean(gw$gw_depth[dmin_ind]) #take mean of all wells at same, nearest, distance
    d$dist_gw = dmin
    return(d)
  }
  
  #if we get to this point, then 500  < dmin <= gw_dist_allowance
  #take distance weighted average of nearby well measurements
  g = gw
  g$dist = dists
  g = g[g$dist <= 5000, ]
  #set weight at inverse squared distance
  g$weight = 1/(g$dist^2)
  #take gw_level as weighted average 
  d$gw_level = as.numeric((g$weight %*% g$gw_depth)/sum(g$weight))
  d$dist_gw = dmin
  
  return(d)
}

gw_ny = dplyr::bind_rows(pblapply(1:nrow(gw_ny), gw_assn, gw_ny, gw, cl = n_cores))

gw_ny_contaminated = gw_ny_contaminated %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(32118)
gw_ny_contaminated = dplyr::bind_rows(pblapply(1:nrow(gw_ny_contaminated), gw_assn, gw_ny_contaminated, gw))


#assigning ZCTA to dataset
ZiptoZcta_Crosswalk_2021 = read_excel(modify_path4('New York/Data/Supplemental/ZiptoZcta_Crosswalk_2021.xlsx'), #These data come from udsmapper.org
                                      col_types = c("text", "skip", "text", 
                                                    "skip", "text", "skip")) %>% 
  dplyr::select(zip = ZIP_CODE, 
                state = STATE, 
                zcta = ZCTA)
gw_ny = gw_ny %>% 
  left_join(ZiptoZcta_Crosswalk_2021 %>% dplyr::mutate(zip = as.numeric(zip)))


#####################################
####Bringing in Demographic Data#####
#####################################
census_api_key(census_key) #probably shouldn't share this...

#race
race_variables = c('total' = 'B02001_001', 'white' = 'B02001_002',
                   'black' = 'B02001_003', 'native_amer' = 'B02001_004',
                   'asian' = 'B02001_005', 'pac_isl' = 'B02001_006'
)

race = get_acs(geography =  'zcta', 
               variables = race_variables,
               year = 2013,
               state = 'NY')
race = subset(race, select = -moe)
race = tidyr::spread(race, variable, estimate)

#calculate demographic data
race$percent_white = race$white/race$total
race$percent_black = race$black/race$total
race$percent_asian = race$asian/race$total
race[is.na(race)] = NA

gw_ny = gw_ny %>% 
  left_join(race %>% dplyr::select(zcta = GEOID, percent_white, percent_black, percent_asian))

#employment and income variables
demographic_variables = c('median_age' = 'B01002_001', 'median_income' = 'B06011_001',
                          'labor_force' = 'B23025_003', 'employed' = 'B23025_004'
)

demographic = get_acs(geography =  'zcta', 
                      variables = demographic_variables,
                      year = 2013,
                      state = 'NY')

demographic = subset(demographic, select = -moe)
demographic = tidyr::spread(demographic, variable, estimate)

demographic$percent_employed = demographic$employed/demographic$labor_force
demographic[is.na(demographic)] = NA

gw_ny = gw_ny %>% 
  left_join(demographic %>% dplyr::select(zcta = GEOID, median_age, median_income, percent_employed))

#merging in insurance status
insurance_variables = c('total_under18' = 'B27010_002', 'under18_one' = 'B27010_003',
                        'under18_two' = 'B27010_010', 'total_1834' = 'B27010_018',
                        '1834_one' = 'B27010_019', '1834_two' = 'B27010_026',
                        'total_3564' = 'B27010_034', '3564_one' = 'B27010_035',
                        '3564_two' = 'B27010_042')
insurance= get_acs(geography =  'zcta', 
                   variables = insurance_variables,
                   year = 2013,
                   state = 'NY')
insurance = subset(insurance, select = -moe)
insurance = tidyr::spread(insurance, variable, estimate)

insurance$percent_insured_u18 = insurance$under18_one/insurance$total_under18 + insurance$under18_two/insurance$total_under18
insurance$percent_insured_1834 = insurance$`1834_one`/insurance$total_1834 + insurance$`1834_two`/insurance$total_1834
insurance$percent_insured_3564 = insurance$`3564_one`/insurance$total_3564 + insurance$`3564_two`/insurance$total_3564
insurance[is.na(insurance)] = NA

gw_ny = gw_ny %>% 
  left_join(insurance %>% dplyr::select(zcta = GEOID, percent_insured_u18, percent_insured_1834, percent_insured_3564))

#bringing in industrial share 
industry_data = get_acs(
  geography =  'zcta', 
  variables = c( manufacturing = "C24050_004",
                 total = "C24050_001", 
                 farming = 'C24050_002'),
  year = 2013,
  state = 'NY')
industry_data = subset(industry_data, select = -c(moe, NAME))
industry_data = tidyr::spread(industry_data, variable, estimate)
industry_data$percent_manufacturing = industry_data$manufacturing/industry_data$total 
industry_data$percent_farming = industry_data$farming/industry_data$total

gw_ny = gw_ny %>% 
  left_join(industry_data %>% 
              dplyr::select(zcta = GEOID, percent_manufacturing, percent_farming))


#bringing in median housing value
housing = get_acs(geography =  'zcta',
                  variables = c(median_price = 'B25077_001'),
                  year = 2013, 
                  state = 'NY')
housing = subset(housing, select = -c(moe, variable))
colnames(housing)[3] = 'median_price'
housing = subset(housing, select = -NAME)
gw_ny = gw_ny %>% 
  left_join(housing %>% dplyr::select(zcta = GEOID, median_price))

#bringing in number of housing units
housing_units = get_acs(geography =  'zcta',
                        variables = c(housing_units = 'B25001_001'),
                        year = 2013, 
                        state = 'NY')
housing_units = subset(housing_units, select = -c(moe, variable, NAME))
colnames(housing_units)[2] = 'housing_units'

gw_ny = gw_ny %>% 
  left_join(housing_units %>% dplyr::select(zcta = GEOID, housing_units))

#bringing in pollution data
if (rerun_pollution == T){
  ny_shapefile = zctas(year = 2010, state = 'NY')
  ny_shapefile$geometry = st_transform(ny_shapefile$geometry, crs = '+proj=longlat +datum=WGS84')
  space_pm25 = raster(modify_path4('New York/Data/Pollution/2010.tif'))
  space_pm25.value = raster::extract(space_pm25, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
  d = as.data.frame(space_pm25.value)
  d = subset(d, select = c(ZCTA5CE10, X2010))
  colnames(d) = c('zcta', 'mean_pm25_2010')
  for (i in c('2011', '2012', '2013', '2014', '2015', '2016')){
    path = modify_path4(paste0('New York/Data/Pollution/' , i, '.tif'))
    space_pm25_i = raster(path)
    space_pm25_i.value = raster::extract(space_pm25_i, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
    d_i = as.data.frame(space_pm25_i.value)
    d_i = d_i[, c(2, ncol(d_i))]
    colnames(d_i) = c('zcta', paste0('pm25_', i)) 
    d = merge(d, d_i, by = 'zcta')
  }
  pollution = d
  write.csv(pollution, modify_path4('New York/Data/Pollution/ny_pop_weighted_pm25.csv'))
}else{
  pollution = read.csv(modify_path4('New York/Data/Pollution/ny_pop_weighted_pm25.csv'))
  pollution = pollution[, -1] #removes X
}
pollution$mean_pm25_12 = rowMeans(pollution[, c(2, 3, 4)])
pollution$mean_pm25_15 = rowMeans(pollution[, c(5, 6, 7)])
pollution$mean_pm25_18 = pollution$pm25_2016 #the SEDAC dataset only goes to 2016. 
pollution = subset(pollution, select = c(zcta, mean_pm25_12, mean_pm25_15, mean_pm25_18))
gw_ny = merge(gw_ny, pollution, by = 'zcta')
gw_ny$mean_pm25 = ifelse(gw_ny$year == "2010-2012", gw_ny$mean_pm25_12, ifelse(gw_ny$year == "2013-2015", gw_ny$mean_pm25_15, gw_ny$mean_pm25_18))

#bringing in temperature data
if (rerun_weather == T){
  weather = raster(modify_path4('New York/Data/Temperature/PRISM_tmean_stable_4kmM3_2010_bil.bil'))
  weather.value = raster::extract(weather, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
  d = as.data.frame(weather.value)
  d = d[, c(2, ncol(d))]
  colnames(d) = c('zcta', 'temp_2010') 
  for (i in c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')){
    path = modify_path4(paste0('New York/Data/Temperature/PRISM_tmean_stable_4kmM3_' , i, '_bil.bil'))
    weather_i = raster(path)
    weather_i.value = raster::extract(weather_i, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
    d_i = as.data.frame(weather_i.value)
    d_i = d_i[, c(2, ncol(d_i))]
    colnames(d_i) = c('zcta', paste0('temp_', i)) 
    d = merge(d, d_i, by = 'zcta')
  }
  weather = d
  write.csv(weather,  modify_path4('New York/Data/Temperature/ny_pop_weighted_temp.csv'))
}else{
  weather = read.csv(modify_path4('New York/Data/Temperature/ny_pop_weighted_temp.csv'))
  weather = weather[, -1]
}
weather$mean_temp_13 = rowMeans(weather[, c(2, 3, 4)])
weather$mean_temp_15 = rowMeans(weather[, c(5, 6, 7)])
weather$mean_temp_18 = rowMeans(weather[, c(8, 9, 10)])
weather = subset(weather, select = c(zcta, mean_temp_13, mean_temp_15, mean_temp_18))

gw_ny = merge(gw_ny, weather, by = 'zcta')
gw_ny$temp = ifelse(gw_ny$year == "2010-2012", gw_ny$mean_temp_13, ifelse(gw_ny$year == "2013-2015", gw_ny$mean_temp_15, gw_ny$mean_temp_18))




################
####main spec####
################
meters =  15000
differential = 0
gw_ny$dist = 0
gw_ny$dummy = 0
gw_ny$contaminated_amount = 0
gw_ny$site= NA_character_
gw_ny$treatment = NA
gw_ny$close = NA
gw_ny$diff_gw = NA
for (i in 1:nrow(gw_ny)){
  #copy dataframe so original data is preserved
  gw_ny_c = gw_ny_contaminated 
  
  #set x as distance between well and contamination site
  gw_ny_c$x = as.numeric(st_distance(gw_ny[i, ], gw_ny_c))
  y = gw_ny_c 
  gw_ny_c = subset(gw_ny_c, gw_ny_c$x <= meters) #subsetting to the contaminated locations within meters
  
  #only run this section if there are sufficiently close water measurements (within gw_dist_allowance)
  if(!is.na(gw_ny$gw_level[i])){
    #if there are "close by" contaminated sites 
    if (nrow(gw_ny_c) > 0){
      islow = F
      min_dist = meters
      for (k in 1:nrow(gw_ny_c)){
        #if the particular site is lower in elevation, set islow to T 
        if(gw_ny$gw_level[i] < gw_ny_c$gw_level[k] - differential){
          islow = T
          #set new minimum distance as the closer site
          if (gw_ny_c$x[k]<= min_dist){
            min_dist = gw_ny_c$x[k] 
            contaminated = as.numeric(gw_ny_c$total_pfas[k])
            site = gw_ny_c$name[k]
            diff_gw = gw_ny_c$gw_level[k] - gw_ny$gw_level[i]
          }
        }
      }
      
      #if there are some sites within meters, but none are at higher elevation, set the distance to the closest one. 
      if (islow == F){ 
        gw_ny$dist[i] = min(gw_ny_c$x)
        a = which(gw_ny_c$x == min(gw_ny_c$x))
        gw_ny$site[i] = gw_ny_c$name[a]
        gw_ny$treatment[i] = 0
        gw_ny$contaminated_amount[i] = gw_ny_c$total_pfas[a]
        gw_ny$close[i] = 1
        a = which(gw_ny_c$gw_level == max(gw_ny_c$gw_level))
        if (length(a) > 1){
          a = a[1]
        }
        gw_ny$diff_gw[i] = gw_ny_c$gw_level[a] - gw_ny$gw_level[i]
        
      }else{
        gw_ny$dist[i] = min_dist 
        gw_ny$contaminated_amount[i] = contaminated
        gw_ny$site[i] = site
        gw_ny$treatment[i] = 1
        gw_ny$close[i] = 1
        gw_ny$diff_gw[i] = diff_gw
      }
      #set dummy indicator for whether the zip code has a lower elevation
      gw_ny$dummy[i] = as.numeric(islow) 
    }else{ #no close sites
      l = which(y$x == min(y$x))
      gw_ny$dummy[i] = ifelse(gw_ny$gw_level[i] < y$gw_level[l], 1, 0) #set dummy (the zip codes affected here are not close to a contaminated site)
      gw_ny$dist[i] = y$x[l] #set distance
      gw_ny$treatment[i] = 0
      gw_ny$close[i] = 0
    }
  }
}
gw_ny$contaminated_amount = as.numeric(gw_ny$contaminated_amount)
gw_ny$treatment = as.factor(gw_ny$treatment)



#regressions
reg_prim1 = fixest::feols(percent_premature  ~ 
                    treatment + close + log(median_income) + median_age + I(percent_white * 100)  + elevation + 
                    I(percent_insured_u18 * 100) + I(percent_insured_1834 * 100) + I(percent_insured_3564 * 100)  +
                    percent_late_care + percent_manufacturing + percent_employed + percent_late_care + log(median_price) + temp + mean_pm25 + log(housing_units)|year, data = gw_ny, cluster = "zip")
summary(reg_prim1)

reg_prim2 = fixest::feols(percent_all_low  ~ 
                    treatment + close + log(median_income) + median_age + I(percent_white * 100)  + elevation + 
                    I(percent_insured_u18 * 100) + I(percent_insured_1834 * 100) + I(percent_insured_3564 * 100)  +
                    percent_late_care + percent_manufacturing + percent_employed + percent_late_care + log(median_price) + temp + mean_pm25 + log(housing_units)|year, data = gw_ny, cluster = "zip")
summary(reg_prim2)

reg1_nd = fixest::feols(percent_infant_deaths  ~ 
                          treatment + close + log(median_income) + median_age + I(percent_white * 100)  + elevation + 
                          I(percent_insured_u18 * 100) + I(percent_insured_1834 * 100) + I(percent_insured_3564 * 100)  +
                          percent_late_care + percent_manufacturing + percent_employed + percent_late_care + log(median_price) + temp + mean_pm25 + log(housing_units)|year, data = gw_ny, cluster = "zip")


modelsummary::modelsummary(list(reg1_nd, reg_prim1, reg_prim2), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #one-sided stars 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("treatment1" = "Downgradient", 
                                        "treatment1:I(contaminated_amount/10^3)" = "Downgradient x PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_NY.tex"))
