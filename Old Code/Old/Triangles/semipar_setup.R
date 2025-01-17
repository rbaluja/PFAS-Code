#####
##Dist Method
wells$id = 1:nrow(wells)
wells_triangle = st_intersection(wells %>% 
                                   as_tibble() %>%  
                                   dplyr::select(!geometry) %>% 
                                   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                                   st_transform(st_crs(cont_sites)), 
                                 cont_sites %>% dplyr::select(industry, triangle, lng, lat, pfoa) %>% dplyr::rename(pfas = pfoa))

#add in distance to the site
well_ll = as_tibble(wells) %>% dplyr::select(c(lng, lat, id))
tll = as_tibble(wells_triangle) %>% dplyr::select(c(lng, lat))


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(well_ll[which(well_ll$id == wells_triangle$id[i]), c("lng", "lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
wells_triangle$dist = pbsapply(1:nrow(wells_triangle), dist_filler)
wells_triangle$dist = as.numeric(wells_triangle$dist)

if (dist == 8000){
  wells_triangle = wells_triangle %>%
    dplyr::mutate(km = dplyr::case_when(
      dist <= 2000 ~ 2,
      dist <= 4000 & dist > 2000~ 4,
      dist <= 6000 & dist > 4000~ 6,
      dist <= 8000 & dist > 6000~ 8,
      dist > 8000 ~ NA)) %>%
    tidyr::drop_na(km)   
}else{
  wells_triangle$km = round(wells_triangle$dist/1000)
}

wells_triangle[wells_triangle$km == 2, ]$triangle = "close"

wt_summary = wells_triangle %>% 
  dplyr::group_by(sys_id, source, triangle, km) %>% 
  dplyr::summarise(nsites = n(), 
                   pfas = sum(pfas)) %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)


wt_summary = wt_summary %>% 
  tidyr::pivot_wider(id_cols = c(sys_id, source), names_from = c(triangle, km),
                     names_glue = sprintf('t{triangle}_km{km}_{.value}'), 
                     values_from = c(nsites, pfas))
wt_summary[is.na(wt_summary)] = 0


#add this to the natality dataframe
df = df %>% left_join(wt_summary)



#do the same for the domestic wells
dom_well = df %>% 
  dplyr::filter(sys_id == "Domestic Well") %>% 
  dplyr::select(!ends_with("nsites") & 
                  !ends_with("pfas") &
                  !c(well_lat, well_lng)) #NOTE: Change any names that start with t, or change triangles
dom_well$index = 1:nrow(dom_well)



domw_triangle = st_intersection(dom_well  %>% st_transform(st_crs(cont_sites)),
                                cont_sites %>% dplyr::select(industry, triangle, lng, lat, pfoa) %>% dplyr::rename(cont_lng = lng, cont_lat = lat, pfas = pfoa))

#add in distance to the site
dwell_ll = dom_well %>% 
  st_transform(4326) %>%
  mutate(lng = unlist(purrr::map(geometry,1)),
         lat = unlist(purrr::map(geometry,2))) %>%
  as_tibble() %>%
  dplyr::select(c(lng, lat, index))
tll = as_tibble(domw_triangle) %>% dplyr::select(c(cont_lng, cont_lat)) %>% dplyr::rename(lng = cont_lng, lat = cont_lat)


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(dwell_ll[which(dwell_ll$index == domw_triangle$index[i]), c("lng", "lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
domw_triangle$dist = pbsapply(1:nrow(domw_triangle), dist_filler)
domw_triangle$dist = as.numeric(domw_triangle$dist)

if (dist == 8000){
  domw_triangle = domw_triangle %>%
    dplyr::mutate(km = dplyr::case_when(
      dist <= 2000 ~ 2,
      dist <= 4000 & dist > 2000~ 4,
      dist <= 6000 & dist > 4000~ 6,
      dist <= 8000 & dist > 6000~ 8,
      dist > 8000 ~ NA)) %>%
    tidyr::drop_na(km)    
}else{
  domw_triangle$km = round(domw_triangle$dist/1000)
}

domw_triangle[domw_triangle$km == 2, ]$triangle = "close"

wt_summary = domw_triangle %>% 
  dplyr::group_by(index, triangle, km) %>% 
  dplyr::summarise(nsites = n(), 
                   pfas = sum(pfas)) %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)

wt = wt_summary %>% 
  tidyr::pivot_wider(id_cols = index, names_from = c(triangle, km),
                     names_glue = sprintf('t{triangle}_km{km}_{.value}'), 
                     values_from = c(nsites, pfas))
wt[is.na(wt)] = 0

dom_well = dom_well %>% 
  left_join(wt)
dom_well = dom_well %>% 
  st_transform(4326) %>%
  mutate(well_lng = unlist(purrr::map(geometry,1)),
         well_lat = unlist(purrr::map(geometry,2)))
dom_well = dom_well %>% dplyr::select(!index)

#remove domestic wells from df and add them back
df = df %>% 
  st_transform(4326) %>% 
  dplyr::filter(sys_id != "Domestic Well") %>% 
  plyr::rbind.fill(dom_well)

#check if close to Pease AFB
df$dist_pease = distm(df %>% 
                        as_tibble() %>% 
                        dplyr::select(c(well_lng, well_lat)), 
                      cont_sites %>% 
                        as_tibble() %>% 
                        dplyr::filter(site == "Pease Air Force Base") %>%
                        dplyr::select(c(lng, lat)) %>% 
                        dplyr::slice(1))[, 1]
df$close_pease = ifelse(df$dist_pease <= meters, 1, 0)


#make all 

#fill in predicted pfas exposure from first stage
df_t = df %>% 
  dplyr::filter(!is.na(t1_km4_nsites))
df_t$domestic = ifelse(df_t$sys_id == "Domestic Well", 1, 0)
df_t$pred_pfas = predict(fs_reg, df_t)
df_t$pred_pfas = ifelse(df_t$pred_pfas < 0, 0, df_t$pred_pfas)
