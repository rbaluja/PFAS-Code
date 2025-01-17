#create 12 triangles corresponding to the prevailing wind direction each month of the year
#in the empirical strategy, I figure we can see how many of these triangles a water source lies in 
#as a means to proxy for the amount of pfas traveled in this fashion
#key: Right now I have the distance of air transport to be the same as via ground water, dist
# w_width is the desired width of the individual triangles 

#need to use rwind library for converting u and v to distance (not strictly necessary, but makes my life a bit easier)
library(rWind)


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
  dplyr::filter(industry == 'Industry' & total_pfas >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)
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
  dplyr::filter(industry == 'Industry' & total_pfas >= ppt) %>% 
  dplyr::select(lng, lat) %>% 
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  terra::vect()

#bring in the average daily wind direction from GRIDMET
wind = brick('Data/Wind/agg_met_th_1979_CurrentYear_CONUS.nc')
wind = as(wind, 'SpatRaster')


wind = terra::extract(wind, sites, fun = mean, na.rm = T)
colnames(wind)[2:length(wind)] = str_sub(colnames(wind)[2:length(wind)], 2, -1)
c = colnames(wind)[2:(length(wind))]
wind = wind %>% 
  tidyr::pivot_longer(cols = all_of(c), 
               names_to = 'date', 
               values_to = 'd_degrees')
wind$d_radians = pi/180 * wind$d_degrees

#change date to actual date
wind$date = as.Date(as.POSIXct(as.numeric(wind$date)*24*60*60, 
                               origin = "1900-01-01", tz="UTC"))
wind$year = lubridate::year(wind$date)


wind_triangle = function(i, dist, w_width){
  
  #first figure out correct angle. this is an application of the law of cosines, since we know all of the side lengths (dist) and the width (w_width)
  #we can find the angle opposite of the width with the below formula
  angle = acos(1 - (w_width^2)/(2 * dist^2))
    
    
  d = wind %>% 
    dplyr::filter(ID == i)
  
  
  c = c_sites[i, ]
  cont_point = c %>% 
    st_coordinates()
  
  #building the individual triangles
  l = vector(mode = 'list', length = 12)
  for (j in 1:12){
    
    lat = d[j, ]$v + c$lat[1]
    lng = d[j, ]$u + c$lng[1]
    
    point_df = data.frame(x = c(c$lng[1], lng), y = c(c$lat[1], lat), cont = c(1, 0), linestring_id = c(1, 1))
    
    
    line = sf_line(obj = point_df) %>%
      st_set_crs(4326) %>%
      st_transform(3488)
    
    sf_use_s2(FALSE)
    ratio = dist/st_length(line)
    
    #this is a point dist meters away in the direction of fastest decrease
    m1 = st_linesubstring(line, from = 0, to = ratio) %>%
      st_endpoint() 
    
    m1 = m1 %>%
      st_transform(4326) %>% 
      st_coordinates()
    
    #use m1 to figure out the amount to correct for in theta
    theta = atan2(m1[2] - cont_point[2], m1[1] - cont_point[1])
    
    
    #getting edge points
    cp_3488 = c %>% 
      st_transform(3488) %>% 
      st_coordinates()
    
    a = list('x' = (cp_3488[1] + dist * cos(theta + angle)), 'y' = cp_3488[2] + dist * sin(theta + angle)) %>%
      as.data.frame() %>% 
      st_as_sf(coords = c('x', 'y'), crs = 3488) %>% 
      st_transform(4326)
    
    b = list('x' = (cp_3488[1] + dist * cos(theta - angle)), 'y' = cp_3488[2] + dist * sin(theta - angle)) %>%
      as.data.frame() %>% 
      st_as_sf(coords = c('x', 'y'), crs = 3488) %>% 
      st_transform(4326)
    
    cp = c$geometry
    tri = rbind(cp, a, b, cp)
    
    tri = tri %>%
      st_set_crs(4326) %>%
      st_transform(32632) %>% #helps with casting
      dplyr::summarise() %>%
      st_cast('POLYGON') %>% 
      st_transform(4326)
    
    l[[j]] = tri
    
  }
  
  l = bind_rows(setNames(l, seq_along(l)), .id = "month")
  l$id = i
  
  return(l)
}

w_triangles = pblapply(1:nrow(c_sites), wind_triangle, dist, w_width)
w_triangles = dplyr::bind_rows(w_triangles)

#merge these triangles with the contamination data
c_sites$id = 1:nrow(c_sites)
c_sites = c_sites %>% 
  as_tibble() %>% 
  dplyr::select(!geometry) %>%
  left_join(w_triangles) %>% 
  dplyr::select(-id)

c_sites = st_as_sf(c_sites, crs = 4326) 

####Merge this with the well data
wells$id = 1:nrow(wells)
wells_wind = st_intersection(wells %>% 
                                   as_tibble() %>%  
                                   dplyr::select(!geometry) %>% 
                                   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                                   st_transform(st_crs(c_sites)), 
                                 c_sites %>% 
                                   dplyr::rename(wind_month = month) %>%
                                   dplyr::select(site, wind_month, lng, lat, total_pfas)) %>% 
            st_transform("EPSG:32110")

#add in distance to the site
well_ll = as_tibble(wells) %>% dplyr::select(c(lng, lat, id))
tll = as_tibble(wells_wind) %>% dplyr::select(c(lng, lat))


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(well_ll[which(well_ll$id == wells_wind$id[i]), c("lng", "lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
wells_wind$dist = pbsapply(1:nrow(wells_wind), dist_filler, cl = 6)
wells_wind$dist = as.numeric(wells_wind$dist)


wt_summary = wells_wind %>% 
  dplyr::group_by(sys_id, source) %>% 
  dplyr::summarise(n_months_wind = n(), 
                   n_sites_wind = length(unique(site)), 
                   sum_pfas_wind = sum(total_pfas/dist)) %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)

#add this to the natality dataframe
df = df %>% left_join(wt_summary)



###
#do the same for the domestic wells
###
dom_well = df %>% 
  dplyr::filter(sys_id == "Domestic Well") %>% 
  dplyr::select(!c(n_months_wind, n_sites_wind, sum_pfas_wind) & !c(well_lat, well_lng))
dom_well$id = 1:nrow(dom_well)



domw_triangle = st_intersection(dom_well  %>% st_transform(st_crs(c_sites)),
                                c_sites %>% 
                                  dplyr::rename(wind_month = month) %>%
                                  dplyr::select(site, wind_month, lng, lat, total_pfas) %>% dplyr::rename(cont_lng = lng, cont_lat = lat))

#add in distance to the site
dwell_ll = dom_well %>% 
  st_transform(4326) %>%
  mutate(lng = unlist(purrr::map(geometry,1)),
         lat = unlist(purrr::map(geometry,2))) %>%
  as_tibble() %>%
  dplyr::select(c(lng, lat, id))
tll = as_tibble(domw_triangle) %>% dplyr::select(c(cont_lng, cont_lat)) %>% dplyr::rename(lng = cont_lng, lat = cont_lat)


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(dwell_ll[which(dwell_ll$id == domw_triangle$id[i]), c("lng", "lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
domw_triangle$dist = pbsapply(1:nrow(domw_triangle), dist_filler, cl = 6)
domw_triangle$dist = as.numeric(domw_triangle$dist)




domw_summary = domw_triangle %>% 
  st_transform("EPSG:32110") %>%
  dplyr::group_by(id) %>% 
  dplyr::summarise(n_months_wind = n(), 
                   n_sites_wind = length(unique(site)), 
                   sum_pfas_wind = sum(total_pfas/dist)) %>% 
  as_tibble() %>% 
  ungroup() %>%
  dplyr::select(!geometry)


#add this to dom_well
dom_well = dom_well %>% 
  left_join(domw_summary)
dom_well = dom_well %>% 
  st_transform(4326) %>%
  mutate(well_lng = unlist(purrr::map(geometry,1)),
         well_lat = unlist(purrr::map(geometry,2)))
dom_well = dom_well %>% dplyr::select(!id)

#remove domestic wells from df and add them back
df = df %>% 
  st_transform(4326) %>% 
  dplyr::filter(sys_id != "Domestic Well") %>% 
  rbind(dom_well)
