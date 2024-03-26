#first read in and aggregate weather data
w = fread("Data_Verify/Supplemental/nh_cbg_weather.csv", colClasses=c("location" = "character")) %>% 
  dplyr::rename(geoid = location)
w$year = as.numeric(str_sub(w$date, 1, 4))
#calculate mean temp and average over daily average temps
w = w %>% 
  dplyr::mutate(temp = (as.numeric(tmin) + as.numeric(tmax))/2) %>% 
  group_by(geoid, year) %>% 
  dplyr::summarize(temp = mean(temp, na.rm = T))


#read in and bind pollution data
pm = fread("Data_Verify/Supplemental/nh_cbg_pm25.csv", colClasses=c("geoid" = "character"))

env = left_join(w, pm)

#read in census vars (at tract level)
dem_vars = fread("Data_Verify/Supplemental/tract_stats.csv", colClasses = c("tract" = "character"))
dem_vars = dem_vars %>% 
  dplyr::mutate(county = stringr::str_pad(county, 3, "left", "0"), #county and tract were read in as numeric, fix that
                tract = stringr::str_pad(tract, 6, "left", "0")) %>%
  dplyr::mutate(geoid = paste0("33", county, tract)) #create census geoid
#get tract-level geoid
env$t_geoid = str_sub(env$geoid, 1, 11)
#join environmental and demographic data
dem_vars = env %>% 
  left_join(dem_vars, by = c("t_geoid" = "geoid"))

df = df %>% 
  dplyr::mutate(year = as.character(year)) %>%
  left_join(dem_vars %>% 
              dplyr::mutate(year = as.character(year), 
                                geoid = as.character(geoid)))

df$county = paste0("33", df$COUNTYFP)

#bringing in tri facilities
tri = fread("Data_Verify/Supplemental/tri_nh.csv") %>% 
  dplyr::select(tri_lat = `12. LATITUDE`, tri_lng = `13. LONGITUDE`)
tri$index = 1:nrow(tri)

#get distance from each row in df and tri
tri_dist =  st_distance(df %>% st_transform(32110), tri %>% st_as_sf(coords = c("tri_lng", "tri_lat"), crs = 4326) %>% st_transform(32110))
df$tri5 = apply(tri_dist, 1, function(x) sum(x <= 5000))

#distance from each mother's residence and the nearest cont site
rc_dist= st_distance(df %>% st_transform(32110), cont_sites %>% st_transform(32110))
df$csite_dist = apply(rc_dist, 1, min)
#save(df, file= paste0(natality_path, "[UA Box Health] birth_records_wdem_prematch.RData"))
