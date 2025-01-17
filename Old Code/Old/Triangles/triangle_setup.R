
wells$id = 1:nrow(wells)
wells_triangle = st_intersection(wells %>%
                                   as_tibble() %>%
                                   dplyr::select(!geometry) %>%
                                   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                                   st_transform(st_crs(cont_sites)),
                                 cont_sites %>% dplyr::select(industry, triangle, lng, lat, total_pfas))

#add in distance to the site
well_ll = as_tibble(wells) %>% dplyr::select(c(lng, lat, id))
tll = as_tibble(wells_triangle) %>% dplyr::select(c(lng, lat))


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(well_ll[which(well_ll$id == wells_triangle$id[i]), c("lng", "lat")])),
                                     as.vector(unlist(tll[i, ]))))
}
wells_triangle$dist = pbsapply(1:nrow(wells_triangle), dist_filler, cl = 8)
wells_triangle$dist = as.numeric(wells_triangle$dist)




wt_summary = wells_triangle %>%
  dplyr::group_by(sys_id, source, triangle) %>%
  dplyr::summarise(n_sites = n(),
                   sum_pfas = sum(total_pfas)) %>%
  as_tibble() %>%
  dplyr::select(!geometry)

#reshape to wide format
wt_summary = wt_summary %>%
  tidyr::pivot_wider(id_cols = c(sys_id, source), names_from = triangle,
                     names_glue = 't{triangle}_{.value}',
                     values_from = c(n_sites, sum_pfas))
wt_summary[is.na(wt_summary)] = 0


#add this to the natality dataframe
df = df %>% left_join(wt_summary)

#do the same for the domestic wells
dom_well = df %>%
  dplyr::filter(sys_id == "Domestic Well") %>%
  dplyr::select(!ends_with("sites") &
                  !ends_with("sum_pfas") &
                  !c(well_lat, well_lng)) #NOTE: Change any names that start with t, or change triangles
dom_well$index = 1:nrow(dom_well)



domw_triangle = st_intersection(dom_well  %>% st_transform(st_crs(cont_sites)),
                                cont_sites %>% dplyr::select(industry, triangle, lng, lat, total_pfas) %>% dplyr::rename(cont_lng = lng, cont_lat = lat))

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
domw_triangle$dist = pbsapply(1:nrow(domw_triangle), dist_filler, cl = 4)
domw_triangle$dist = as.numeric(domw_triangle$dist)




domw_summary = domw_triangle %>%
  dplyr::group_by(index, triangle) %>%
  dplyr::summarise(n_sites = n(),
                   sum_pfas = sum(total_pfas/dist)) %>%
  as_tibble() %>%
  dplyr::select(!geometry)

#reshape to wide format
domw_summary = domw_summary %>%
  tidyr::pivot_wider(id_cols = index, names_from = triangle,
                     names_glue = 't{triangle}_{.value}',
                     values_from = c(n_sites, sum_pfas))
domw_summary[is.na(domw_summary)] = 0

#add this to dom_well
dom_well = dom_well %>%
  left_join(domw_summary)
dom_well = dom_well %>%
  st_transform(4326) %>%
  mutate(well_lng = unlist(purrr::map(geometry,1)),
         well_lat = unlist(purrr::map(geometry,2)))
dom_well = dom_well %>% dplyr::select(!index)

#remove domestic wells from df and add them back
df = df %>%
  st_transform(4326) %>%
  dplyr::filter(sys_id != "Domestic Well") %>%
  rbind(dom_well)