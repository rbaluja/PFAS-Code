source("Code/Theta/theta_functions.R")
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
  dplyr::select(stationid1, latdecdeg, longdecdeg, watervapusage, startdate, pfoa) %>% 
  dplyr::rename(lat = latdecdeg, lng = longdecdeg)


#extract well and source id
cont$source_id = sapply(strsplit(cont$stationid1, "_"), "[", 1)
cont$well_id = sapply(strsplit(cont$stationid1, "_"), "[", 2)

#set date
cont$date = as.Date(sapply(strsplit(cont$startdate, " "), "[", 1), format = "%m/%d/%Y")
cont$year = lubridate::year(cont$date)

c = cont %>% 
  dplyr::group_by(source_id, well_id) %>% 
  dplyr::filter(pfoa == max(pfoa)) %>% 
  dplyr::select(!c(date))

c$index = 1:nrow(c)
c = c %>%  
  as_tibble() %>%  
  tidyr::drop_na(lng) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F) %>%
  st_transform(32110) %>% 
  st_buffer(0.001) %>%
  st_transform(st_crs(cont_huc_inter)) %>% 
  dplyr::rename(well_lat = lat, well_lng = lng)

ct = st_intersection(c, cont_huc_inter %>% 
                       dplyr::select( site, industry, lng, lat, pfoa, theta_gw) %>% dplyr::rename(pfas = pfoa))

#add in distance to the site
c_ll = as_tibble(c) %>% dplyr::select(c(well_lng, well_lat, index))
tll = as_tibble(ct) %>% dplyr::select(c(lng, lat))


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(c_ll[which(c_ll$index == ct$index[i]), c("well_lng", "well_lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
ct$dist = pbsapply(1:nrow(ct), dist_filler)
ct$dist = as.numeric(ct$dist)


#get reprojected lat long for wells and cont_sites
wrp = c %>%
  as_tibble() %>%
  dplyr::select(!geometry) %>%
  st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>%
  st_transform(32110) %>%
  dplyr::mutate(well_lng = unlist(purrr::map(geometry,1)),
                well_lat = unlist(purrr::map(geometry,2))) %>% 
  as_tibble() %>%
  dplyr::select(index, well_lng, well_lat)

crp = cont_huc_inter %>% 
  as_tibble() %>%
  dplyr::select(!geometry) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(32110) %>%
  dplyr::mutate(lng = unlist(purrr::map(geometry,1)),
                lat = unlist(purrr::map(geometry,2))) %>% 
  as_tibble() %>% 
  dplyr::select(site, lng, lat)

#join these back to ct
ct = ct %>% 
  dplyr::select(!c(well_lat, well_lng, lng, lat)) %>% 
  left_join(wrp) %>% 
  left_join(crp)

#drop anyone with singular theta_gw for now
# ct = ct %>% 
#   dplyr::group_by(index) %>% 
#   dplyr::filter(sum(theta_gw == "singular") == 0)

ct$theta = atan2(ct$well_lat - ct$lat, ct$well_lng - ct$lng)
ct$theta_gw = as.numeric(ct$theta_gw)


ct$tdist = atan2(sin(ct$theta - ct$theta_gw), cos(ct$theta - ct$theta_gw))

ct$downstream = ifelse(abs(ct$tdist) <= pi/4 & !is.na(ct$tdist), 1, 0)
ct$close = ifelse(ct$dist <= 1000, 1, 0)
ct$domestic = ifelse(ct$watervapusage == "DOMESTIC", 1, 0)

ct$distf = as.factor(floor(ct$dist/5000))

#move to ppb
ct$pfas = ct$pfas/1000
ct$pfoa = ct$pfoa/1000

ct2 = ct %>% 
  group_by(index, downstream, distf, domestic, pfoa) %>% 
  dplyr::summarize(pfas = sum(pfas))

ct2$dd =paste("down", ct2$downstream, "_km", ct2$distf , sep = "")

ct2 = ct2 %>%
  tidyr::pivot_wider(id_cols =  c("index", "domestic", "pfoa") , names_from = "dd", values_from = "pfas")
ct2[is.na(ct2)] = 0

# 
# ct1 = ct %>% 
#   dplyr::group_by(index, domestic, pfoa) %>% 
#   dplyr::summarize(close_pfas = sum(pfas[close == 1]), 
#                    down_pfas = sum(pfas[downstream == 1 & close == 0]), 
#                    tot_down = sum(pfas[downstream == 1]), 
#                    tot_up = sum(pfas[downstream == 0]), 
#                    other_pfas = sum(pfas[close == 0 & downstream == 0]), 
#                    far_pfas = sum(pfas[close == 0]), 
#                    dist = min(dist))
# 
# 
# c1 = ct1 %>% 
#   dplyr::filter(pfoa <= quantile(pfoa, probs = c(0.95)))




 # fs1 = fixest::feols(asinh(pfoa) ~ down0_km0 + down1_km0 + down1_km1 + down0_km1 +
 #                     domestic, data = ct2)
 # summary(fs1)
 # 
 # tobit_fs1 = vglm(asinh(pfoa) ~ down0_km0 + down1_km0 + down1_km1 + down0_km1 +
 #                    domestic, tobit(Lower = 0), data = ct2)
 # summary(tobit_fs1)
# car::linearHypothesis(fs1, c("asinh(down1_km1) = asinh(down0_km1)"))
# car::linearHypothesis(fs1, c("asinh(down1_km0) = asinh(down0_km1)"))


