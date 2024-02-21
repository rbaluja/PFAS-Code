#first read in and aggregate weather data
w = fread("New Hampshire/Data/Supplemental/nh_cbg_weather.csv", colClasses=c("location" = "character")) %>% 
  dplyr::rename(geoid = location)
w$year = as.numeric(str_sub(w$date, 1, 4))

w = w %>% 
  group_by(geoid, year) %>% 
  dplyr::mutate(temp = (as.numeric(tmin) + as.numeric(tmax))/2) %>% 
  dplyr::summarize(temp = mean(temp, na.rm = T))


#read in and bind pollution data
pm = fread("New Hampshire/Data/Supplemental/nh_cbg_pm25.csv", colClasses=c("geoid" = "character"))

env = left_join(w, pm)

#read in census vars (at tract level)
dem_vars = fread("New Hampshire/Data/Supplemental/tract_stats.csv", colClasses = c("tract" = "character"))
dem_vars = dem_vars %>% 
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

dem_vars = env %>% 
  left_join(dem_vars, by = c("t_geoid" = "geoid"))

df = df %>% 
  left_join(dem_vars %>% dplyr::mutate(year = as.character(year), 
                                geoid = as.character(geoid)))

df$county = paste0("33", df$COUNTYFP)

#bringing in tri facilities
tri = fread("New Hampshire/Data/Supplemental/tri_nh.csv") %>% 
  dplyr::select(tri_lat = `12. LATITUDE`, tri_lng = `13. LONGITUDE`)
tri$index = 1:nrow(tri)


tri_dists = distm(df %>% 
                    as_tibble() %>% 
                    dplyr::select(lng, lat), tri %>% 
                    dplyr::select(tri_lng, tri_lat))
t_dist = function(i){
  d = df[i, ]
  dists = tri_dists[i, ]
  d$tri1 = length(which(dists <= 1000))
  d$tri3 = length(which(dists <= 3000))
  d$tri5 = length(which(dists <= 5000))
  return(d)
  
}
df = dplyr::bind_rows(pblapply(1:nrow(df), t_dist, cl = 4))

#distance to nearest site
#get distance from each mother's residence and the nearest cont site
cont_dists = distm(df %>% 
                     as_tibble() %>% 
                     dplyr::select(lng, lat), cont_sites %>% 
                     as_tibble() %>% 
                     dplyr::select(lng, lat))
dc_dist = function(i){
  d = df[i, ]
  dists = cont_dists[i, ]
  d$csite_dist = min(dists)
  return(d)
  
}
df = dplyr::bind_rows(pblapply(1:nrow(df), dc_dist, cl = 4))

#save(df, file= "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_wdem122023.RData")
