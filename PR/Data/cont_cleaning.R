fs_cont = fread("New Hampshire/Data/Contamination/NH_tests.csv") 

#translate all results to ppt
fs_cont[fs_cont$DETLIMU == "UG/KG" | fs_cont$DETLIMU == "UG/KG DRY", ]$NUMRESULT1 = 1000 * fs_cont[fs_cont$DETLIMU == "UG/KG" | fs_cont$DETLIMU == "UG/KG DRY", ]$NUMRESULT1
fs_cont[fs_cont$DETLIMU == "UG/KG" | fs_cont$DETLIMU == "UG/KG DRY", ]$DETLIM = 1000 * fs_cont[fs_cont$DETLIMU == "UG/KG" | fs_cont$DETLIMU == "UG/KG DRY", ]$DETLIM

fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1 = 1000 * fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1
fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$DETLIM = 1000 * fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$DETLIM

fs_cont[fs_cont$DETLIMU == "UG/L" | fs_cont$DETLIMU == "UG/L DRY", ]$NUMRESULT1 = 1000 * fs_cont[fs_cont$DETLIMU == "UG/L" | fs_cont$DETLIMU == "UG/L DRY", ]$NUMRESULT1
fs_cont[fs_cont$DETLIMU == "UG/L" | fs_cont$DETLIMU == "UG/L DRY", ]$DETLIM = 1000 * fs_cont[fs_cont$DETLIMU == "UG/L" | fs_cont$DETLIMU == "UG/L DRY", ]$DETLIM

fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1 = 1000 * fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1
fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$DETLIM = 1000 * fs_cont[fs_cont$DETLIMU == "NG/G" | fs_cont$DETLIMU == "NG/G DRY", ]$DETLIM

fs_cont[fs_cont$DETLIMU == "MG/KG" | fs_cont$DETLIMU == "MG/KG DRY", ]$NUMRESULT1 = 1000000 * fs_cont[fs_cont$DETLIMU == "MG/KG" | fs_cont$DETLIMU == "MG/KG DRY", ]$NUMRESULT1
fs_cont[fs_cont$DETLIMU == "MG/KG" | fs_cont$DETLIMU == "MG/KG DRY", ]$DETLIM = 1000000 * fs_cont[fs_cont$DETLIMU == "MG/KG" | fs_cont$DETLIMU == "MG/KG DRY", ]$DETLIM

fs_cont[fs_cont$DETLIMU == "MG/L" | fs_cont$DETLIMU == "MG/L DRY", ]$NUMRESULT1 = 1000000 * fs_cont[fs_cont$DETLIMU == "MG/L" | fs_cont$DETLIMU == "MG/L DRY", ]$NUMRESULT1
fs_cont[fs_cont$DETLIMU == "MG/L" | fs_cont$DETLIMU == "MG/L DRY", ]$DETLIM = 1000000 * fs_cont[fs_cont$DETLIMU == "MG/L" | fs_cont$DETLIMU == "MG/L DRY", ]$DETLIM

#put any tests with a result that was lower than the detection limit to the detection limit/sqrt(2). Based on my reading, this detection limit it that which has α = 0.01
#where α is in relation to a test for a value strictly positive. 
fs_cont[fs_cont$QUALIFIER == "<", ]$NUMRESULT1 = fs_cont[fs_cont$QUALIFIER == "<", ]$DETLIM/sqrt(2)
#for remaining values, NA is either because there is no detection limit recorded (and it was less than the true one), or it was a non detect
fs_cont[is.na(fs_cont$NUMRESULT1), ]$NUMRESULT1 = 0 


fs_cont = fs_cont %>% 
  rename_all(tolower) %>% 
  dplyr::group_by(activityid, wshedparmname, wellusage, startdate, stationid1, latdecdeg, longdecdeg) %>%
  dplyr::mutate(numresult1 = max(numresult1, na.rm = T)) %>% #take max of tested levels (within same activity, chemical, well etc.)
  ungroup() %>%
  dplyr::distinct(activityid, wshedparmname, startdate, stationid1, latdecdeg, longdecdeg, numresult1, .keep_all = TRUE) %>% 
  tidyr::pivot_wider(id_cols = c(activityid, startdate, town, stationid1, latdecdeg, longdecdeg, watervapusage), 
                     names_from = wshedparmname, 
                     values_from = numresult1) #pivots dataset to create frame fs_containing each test for all chemicals

fs_cont = fs_cont %>%
  dplyr::rename(genx = `2,3,3,3-TETRAFLUORO-2-(HEPTAFLUOROPROPOXY)PROPANOIC ACID - HFPO-DA - GENXACID`, 
                pfna = `PERFLUORONONANOIC ACID - PFNA`, 
                pfhxs = `PERFLUOROHEXANE SULFONIC ACID - PFHXS`, 
                pfbs = `PERFLUOROBUTANE SULFONIC ACID - PFBS`, 
                pfoa = `PERFLUOROOCTANOIC ACID - PFOA`,
                pfos = `PERFLUOROOCTANE SULFONIC ACID - PFOS`, 
                lat = latdecdeg, 
                lng = longdecdeg)

fs_cont = fs_cont %>% 
  dplyr::mutate(
    pfos = dplyr::coalesce(pfos, `PERFLUOROOCTANE SULFONATE - PFOS`))

#set date
fs_cont$date = as.Date(sapply(strsplit(fs_cont$startdate, " "), "[", 1), format = "%m/%d/%Y")
fs_cont$year = lubridate::year(fs_cont$date)

#extract well and source id
fs_cont$source_id = sapply(strsplit(fs_cont$stationid1, "_"), "[", 1)
fs_cont$well_id = sapply(strsplit(fs_cont$stationid1, "_"), "[", 2)

fs_cont = fs_cont %>% 
  tidyr::drop_na(lng)
fs_cont$wind_exposure = pbmapply(wind_function, fs_cont$lng, fs_cont$lat, rep(wind_dist, nrow(fs_cont)))

#select relevant test
fs_cont = fs_cont %>% 
  dplyr::group_by(source_id, well_id, lat, lng) %>% 
  dplyr::filter(sum(pfoa, pfos, na.rm = T) == max(sum(pfoa, pfos, na.rm = T), na.rm = T)) %>% #if multiple tests, select that with the max level
  dplyr::filter(date == min(date)) %>% #if multiple tests with same tested level, select that with the earliest date
  dplyr::select(!c(date)) 


fs_cont$index = 1:nrow(fs_cont)
fs_cont = fs_cont %>%  
  dplyr::rename(well_lat = lat, well_lng = lng)


#############
##Demographics

#bring in geoid
load("/Users/robert/Library/CloudStorage/Box-Box/NH Supplemental Data/cbg_tigris.RData")

#change cbg and df projections to planar for intersection 
cbg_shape = cbg_shape %>%
  st_transform(32110)

fs_cont = fs_cont %>% 
  st_as_sf(coords = c("well_lng", "well_lat"), remove = FALSE, crs = 4326) %>%
  st_transform(32110)

#assign cbg to fs_cont
fs_cont = fs_cont %>% 
  st_join(cbg_shape, join = st_within, largest = T)

save(fs_cont, file = "Data/Contamination/cont_well_geo.RData")

#read in weather and pm25 data at cbg level
w = fread("Data/Supplemental/nh_cbg_weather.csv", colClasses=c("location" = "character")) %>% 
  dplyr::rename(geoid = location)
w$year = as.numeric(str_sub(w$date, 1, 4))

w = w %>% 
  group_by(geoid, year) %>% 
  dplyr::mutate(temp = (as.numeric(tmin) + as.numeric(tmax))/2) %>% 
  dplyr::summarize(temp = mean(temp, na.rm = T))

w = w %>% 
  group_by(geoid) %>% 
  dplyr::summarise(temp = mean(temp, na.rm = T))


#read in and bind pollution data
pm = fread("Data/Supplemental/nh_cbg_pm25.csv", colClasses=c("geoid" = "character"))
pm = pm %>% 
  group_by(geoid) %>% 
  dplyr::summarise(pm25 = mean(pm25, na.rm = T))

env = left_join(w, pm)

fs_cont$cbg = str_sub(fs_cont$GEOID, 1, 12)
fs_cont = fs_cont %>%
  left_join(env, by = c("cbg" = "geoid"))


#read in census vars
library(tidycensus)
census_api_key("9f59b9fec9cffa85b5740734df3d81e7b617cf82") #probably shouldn't share this...

tract_vars = c(manufacturing = "C24050_004", 
               total_pop = "C24050_001", 
               med_inc = "B06011_001", 
               med_hprice = "B25077_001", 
               n_hunits = "B25001_001", 
               employed = 'B23025_004', 
               lf = 'B23025_003')


#get tract info
td = get_acs(geography = "tract", 
             variables = tract_vars, 
             state = "NH")

td = td %>% 
  dplyr::select(!c(NAME, moe)) %>%
  tidyr::pivot_wider(id_cols = GEOID, names_from = variable,
                     values_from = estimate) 

td = td %>% 
  dplyr::mutate(county = str_sub(GEOID, 3, 5), 
                tract = str_sub(GEOID, 6, 11)) %>% 
  dplyr::mutate(p_manuf = manufacturing/lf, 
                em_rate = employed/lf) %>% 
  dplyr::select(county, tract, med_inc, p_manuf, n_hunits, med_hprice, em_rate)

td = td %>% 
  dplyr::mutate(tract = as.character(tract)) %>% 
  dplyr::mutate(geoid = paste0("33", county, tract))

#merge this in with cont data
fs_cont = fs_cont %>% 
  dplyr::mutate(t_geoid = str_sub(GEOID, 1, 11)) %>% 
  left_join(td, by = c("t_geoid" = "geoid"))

#bring in elevation 
dem_elev = raster("Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff")
fs_cont$row = 1:nrow(fs_cont)
fs_cont_ll = fs_cont %>% 
  as_tibble() %>% 
  dplyr::select(row, well_lng, well_lat) %>% 
  st_as_sf(coords = c("well_lng", "well_lat"))

e1 = raster::extract(dem_elev, fs_cont_ll)
fs_cont$elevation = e1


#bringing in tri facilities
tri = fread("Data/Supplemental/tri_nh.csv") %>% 
  dplyr::select(tri_lat = `12. LATITUDE`, tri_lng = `13. LONGITUDE`)
tri$index = 1:nrow(tri)


tri_dists = distm(fs_cont %>% 
                    as_tibble() %>% 
                    dplyr::select(well_lng, well_lat), tri %>% 
                    dplyr::select(tri_lng, tri_lat))
t_dist = function(i){
  d = fs_cont[i, ]
  dists = tri_dists[i, ]
  d$tri1 = length(which(dists <= 1000))
  d$tri3 = length(which(dists <= 3000))
  d$tri5 = length(which(dists <= 5000))
  return(d)
  
}
fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), t_dist, cl = 4))

fwrite(fs_cont %>% 
         as_tibble() %>% 
         dplyr::select(!geometry), "Data/Contamination/cleaned_contwell_122023.csv")
