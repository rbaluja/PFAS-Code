cont = fread("Data/Contamination/NH_tests.csv") 

#translate all results to ppt
cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$DETLIM

cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM

cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$DETLIM

cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM

cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$NUMRESULT1 = 1000000 * cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$DETLIM = 1000000 * cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$DETLIM

cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$NUMRESULT1 = 1000000 * cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$DETLIM = 1000000 * cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$DETLIM

#put any tests with a result that was lower than the detection limit to zero. Based on my reading, this detection limit it that which has α = 0.01
#where α is in relation to a test for a value strictly positive. 
cont[cont$QUALIFIER == "<", ]$NUMRESULT1 = cont[cont$QUALIFIER == "<", ]$DETLIM/sqrt(2) #NEED TO FIX
cont[is.na(cont$NUMRESULT1), ]$NUMRESULT1 = 0


cont = cont %>% 
  rename_all(tolower) %>% 
  tidyr::drop_na(numresult1) %>%
  dplyr::group_by(activityid, wshedparmname, startdate, stationid1, latdecdeg, longdecdeg) %>%
  dplyr::mutate(numresult1 = max(numresult1, na.rm = T)) %>% #if more than one test at the same time, take max
  ungroup() %>%
  dplyr::distinct(activityid, wshedparmname, startdate, stationid1, latdecdeg, longdecdeg, numresult1, .keep_all = TRUE) %>% 
  tidyr::pivot_wider(id_cols = c(activityid, startdate, town, stationid1, latdecdeg, longdecdeg, watervapusage), 
              names_from = wshedparmname, 
              values_from = numresult1) #pivots dataset to create frame containing each test for all chemicals

#turn NA to 0 (they didnt test for that chemical)
cont[, 8:ncol(cont)][is.na(cont[, 8:ncol(cont)])] = 0


cont = cont %>%
  dplyr::rename(genx = `2,3,3,3-TETRAFLUORO-2-(HEPTAFLUOROPROPOXY)PROPANOIC ACID - HFPO-DA - GENXACID`, 
                pfna = `PERFLUORONONANOIC ACID - PFNA`, 
                pfhxs = `PERFLUOROHEXANE SULFONIC ACID - PFHXS`, 
                pfbs = `PERFLUOROBUTANE SULFONIC ACID - PFBS`, 
                pfoa = `PERFLUOROOCTANOIC ACID - PFOA`,
                pfos = `PERFLUOROOCTANE SULFONIC ACID - PFOS`, 
                lat = latdecdeg, 
                lng = longdecdeg)

#set date
cont$date = as.Date(sapply(strsplit(cont$startdate, " "), "[", 1), format = "%m/%d/%Y")
cont$year = lubridate::year(cont$date)
cont$total_pfoa_pfos = cont$pfoa + cont$pfos


#determine contaminated wells
c1 = cont %>% 
  tidyr::drop_na(lat) %>% 
  dplyr::group_by(stationid1) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)%>% 
  st_transform(3488) %>% 
  st_buffer(1) %>%
  st_transform(st_crs(cont_sites))
c1$id = 1:nrow(c1)

c1 = c1 %>% 
  dplyr::select(stationid1, lat, lng, watervapusage, total_pfoa_pfos, id) %>% 
  dplyr::rename(well_id = stationid1, well_type = watervapusage)

cont_wells_triangle = st_intersection(c1 %>% dplyr::select(!c("lng", "lat")), cont_sites %>% dplyr::select(industry, triangle, lng, lat, total_pfas))

#add in distance to the site
c = as_tibble(c1) %>% dplyr::select(!geometry)
cll = as_tibble(cont_wells_triangle) %>% dplyr::select(c(lng, lat))


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(c[which(c$id == cont_wells_triangle$id[i]), c("lng", "lat")])), 
                                     as.vector(unlist(cll[i, ]))))
}
cont_wells_triangle$dist = pbsapply(1:nrow(cont_wells_triangle), dist_filler, cl = 6)
cont_wells_triangle$dist = as.numeric(cont_wells_triangle$dist)


#great, now I just need to aggregate these info up by site 
cwt = cont_wells_triangle %>% 
  tidyr::drop_na(well_id) %>% 
  dplyr::group_by(well_id, triangle) %>%
  dplyr::summarize(site_cont_sum = sum(total_pfas), 
                   site_dist = sum(dist), 
                   site_interaction = sum(dist * total_pfas))

cwt = cwt %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)

cwt = cwt %>%
  tidyr::pivot_wider(id_cols = well_id, names_from = triangle,
                     names_glue = '{.value}_{triangle}', 
                     values_from = c(site_cont_sum, site_dist, site_interaction))

cwt.1 = left_join(cwt, cont %>% dplyr::select(stationid1, total_pfoa_pfos), by = c("well_id" = "stationid1"))

cwt.1 = cwt.1 %>% 
  dplyr::group_by(well_id) %>% 
  dplyr::mutate(total_pfoa_pfos = mean(total_pfoa_pfos)) %>% 
  unique()

cwt.1[is.na(cwt.1)] = 0


