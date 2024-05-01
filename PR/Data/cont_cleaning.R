fs_cont = fread(modify_path("Data_Verify/Contamination/NH_tests.csv"))

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

#put any tests with a result that was lower than the detection limit to the detection as limit/sqrt(2). Based on my reading, this detection limit it that which has α = 0.01
#where α is in relation to a test for a value strictly positive. 
fs_cont[fs_cont$QUALIFIER == "<", ]$NUMRESULT1 = fs_cont[fs_cont$QUALIFIER == "<", ]$DETLIM/sqrt(2)
#for remaining values, NA is either because there is no detection limit recorded (and it was less than the true one), it was a non detect, or it was just left blank
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
cbg_shape = tigris::block_groups("NH")

#change cbg and df projections to planar for intersection 
cbg_shape = cbg_shape %>%
  st_transform(32110)

#turn fs_cont spatial
fs_cont = fs_cont %>% 
  st_as_sf(coords = c("well_lng", "well_lat"), remove = FALSE, crs = 4326) %>%
  st_transform(32110)

#assign cbg to fs_cont
fs_cont = fs_cont %>% 
  st_join(cbg_shape, join = st_within, largest = T)
fs_cont$cbg = str_sub(fs_cont$GEOID, 1, 12)


#read in weather and pm25 data at cbg level
w = fread(modify_path("Data_Verify/Supplemental/nh_cbg_weather.csv"), colClasses=c("location" = "character")) %>% 
  dplyr::rename(geoid = location)
w$year = as.numeric(str_sub(w$date, 1, 4))

#take mean annual temp as average over daily mean temperatures
w = w %>% 
  dplyr::mutate(temp = (as.numeric(tmin) + as.numeric(tmax))/2) %>% 
  group_by(geoid, year) %>% 
  dplyr::summarize(temp = mean(temp, na.rm = T))
#summarize temperature to a constant average, by geoid (average over years)
w = w %>% 
  dplyr::group_by(geoid) %>% 
  dplyr::summarise(temp = mean(temp, na.rm = T))


#read in and bind pollution data
pm = fread(modify_path("Data_Verify/Supplemental/nh_cbg_pm25.csv"), colClasses=c("geoid" = "character"))
#summarize pm25 to a constant average, by geoid (average over years)
pm = pm %>% 
  dplyr::group_by(geoid) %>% 
  dplyr::summarise(pm25 = mean(pm25, na.rm = T))

env = left_join(w, pm)
#join fs_cont and environmental covariates
fs_cont = fs_cont %>%
  left_join(env, by = c("cbg" = "geoid"))


#read in census vars
census_api_key(census_key)

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
dem_elev = raster(modify_path("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff"))
fs_cont$row = 1:nrow(fs_cont)
fs_cont_ll = fs_cont %>% 
  as_tibble() %>% 
  dplyr::select(row, well_lng, well_lat) %>% 
  st_as_sf(coords = c("well_lng", "well_lat"))

e1 = raster::extract(dem_elev, fs_cont_ll)
fs_cont$elevation = e1


#bringing in tri facilities
tri = fread(modify_path("Data_Verify/Supplemental/tri_nh.csv")) %>% 
  dplyr::select(tri_lat = `12. LATITUDE`, tri_lng = `13. LONGITUDE`)
tri$index = 1:nrow(tri)

#get distance from each test well and tri site
tri_dist =  st_distance(fs_cont %>% st_transform(32110), tri %>% st_as_sf(coords = c("tri_lng", "tri_lat"), crs = 4326) %>% st_transform(32110))
fs_cont$tri5 = apply(tri_dist, 1, function(x) sum(x <= 5000))

fwrite(fs_cont %>% 
         as_tibble() %>% 
         dplyr::select(!geometry), modify_path("Data_Verify/Contamination/cleaned_contwell.csv"))

#get distance between each test well and cont site (for SI discussion)
cf_dist = st_distance(fs_cont %>% 
                        st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
                        st_transform(32110), cont_sites %>% st_transform(32110))
#percent of test wells within 10km of a cont site
mean(apply(cf_dist, 1, function(x) sum(x <= 10000) > 0))
#percent of test wells within 5km of a cont site
mean(apply(cf_dist, 1, function(x) sum(x <= 5000) > 0))
#median number of sites within 5km, among wells within 5km
nw = apply(cf_dist, 1, function(x) sum(x <= 5000))
mean(nw[nw >= 1])
rm(nw)

#number of wells with non-missing PFOA/PFOS levels
sum(!is.na(fs_cont$pfoa) & !is.na(fs_cont$pfos))
#number of wells with missing PFOS but not PFOA levels
sum(is.na(fs_cont$pfos) & !is.na(fs_cont$pfoa))
#number of wells with missing PFOA but not PFOS levels
sum(is.na(fs_cont$pfoa) & !is.na(fs_cont$pfos))
#number of wells with missing PFOA and PFOS levels
sum(is.na(fs_cont$pfoa) & is.na(fs_cont$pfos))
