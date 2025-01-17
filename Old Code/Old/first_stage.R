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

#put any tests with a result that was lower than the detection limit to the detection limit/sqrt(2). Based on my reading, this detection limit it that which has α = 0.01
#where α is in relation to a test for a value strictly positive. 
cont[cont$QUALIFIER == "<", ]$NUMRESULT1 = cont[cont$QUALIFIER == "<", ]$DETLIM/sqrt(2) #NEED TO FIX
cont[is.na(cont$NUMRESULT1), ]$NUMRESULT1 = 0


#first test pfoa
cont = cont %>% dplyr::filter(WSHEDPARMNAME == "PERFLUOROOCTANOIC ACID - PFOA")
cont = cont %>% 
  dplyr::rename(pfoa = NUMRESULT1) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::select(stationid1, latdecdeg, watervapusage, startdate, longdecdeg, pfoa) %>% 
  dplyr::rename(lat = latdecdeg, lng = longdecdeg)


#extract well and source id
cont$source_id = sapply(strsplit(cont$stationid1, "_"), "[", 1)
cont$well_id = sapply(strsplit(cont$stationid1, "_"), "[", 2)

#set date
cont$date = as.Date(sapply(strsplit(cont$startdate, " "), "[", 1), format = "%m/%d/%Y")
cont$year = lubridate::year(cont$date)

c = cont %>% 
  dplyr::group_by(source_id, well_id) %>% 
  dplyr::filter(pfoa == max(pfoa))

#bring in the triangles that each well is a member of from the nearby cont sites
c$id = 1:nrow(c)
c_triangle = st_intersection(c %>% 
                               as_tibble() %>%  
                               tidyr::drop_na(lng) %>%
                               st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                               st_transform(st_crs(cont_sites)), 
                             cont_sites %>% dplyr::select( industry, triangle, lng, lat, pfoa) %>% dplyr::rename(pfas = pfoa))


#add in distance to the site
c_ll = as_tibble(c) %>% dplyr::select(c(lng, lat, id))
tll = as_tibble(c_triangle) %>% dplyr::select(c(lng, lat))


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(c_ll[which(c_ll$id == c_triangle$id[i]), c("lng", "lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
c_triangle$dist = pbsapply(1:nrow(c_triangle), dist_filler)
c_triangle$dist = as.numeric(c_triangle$dist)

if (dist == 8000){
  c_triangle = c_triangle %>%
    dplyr::mutate(km = dplyr::case_when(
      dist <= 1000 ~ 1,
      dist <= 4000 & dist > 1000~ 4,
      dist <= 6000 & dist > 4000~ 6,
      dist <= 8000 & dist > 6000~ 8,
      dist > 8000 ~ NA)) %>%
    tidyr::drop_na(km)  
}else{
  c_triangle$km = round(c_triangle$dist/1000)
}



c_summary = c_triangle %>% 
  dplyr::group_by(id, triangle, km) %>% 
  dplyr::summarise(nsites = n(), 
                   pfas = sum(pfas), 
                   year = min(year)) %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)

c_summary = c_summary %>% 
  tidyr::pivot_wider(id_cols = id, names_from = c(triangle, km),
                     names_glue = sprintf('t{triangle}_km{km}_{.value}'), 
                     values_from = c(nsites, pfas))
c_summary[is.na(c_summary)] = 0


#add this to the contaminmation  dataframe

c = c %>% left_join(c_summary)
c$domestic = ifelse(c$watervapusage == "DOMESTIC", 1, 0)

c1 = c %>% 
  ungroup() %>%
  dplyr::filter(pfoa <= quantile(pfoa, probs = 0.95, na.rm = T))

fs_reg = fixest::feols(as.formula(paste("pfoa", "~",
                                        paste(paste0(colnames(c)[endsWith(colnames(c), "pfas") | 
                                                                   endsWith(colnames(c), "nsites")], sep = ""), collapse = " + "), 
                                        
                                        " + ", 
                                        paste(paste0(colnames(c)[endsWith(colnames(c), "pfas")], "^2", sep = ""), collapse = " + "),
                                        
                                        " + domestic",
                                        
                                        sep = "")), data = c1)
summary(fs_reg)




