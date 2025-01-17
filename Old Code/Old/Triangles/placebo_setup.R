#main strategy
df$index = 1:nrow(df)



df_triangle = st_intersection(df  %>% st_transform(st_crs(cont_sites)),
                              cont_sites %>% dplyr::select(industry, triangle, lng, lat, total_pfas) %>% dplyr::rename(cont_lng = lng, cont_lat = lat))

#add in distance to the site
df_ll = df %>% 
  st_transform(4326) %>%
  mutate(lng = unlist(purrr::map(geometry,1)),
         lat = unlist(purrr::map(geometry,2))) %>%
  as_tibble() %>%
  dplyr::select(c(lng, lat, index))
tll = as_tibble(df_triangle) %>% dplyr::select(c(cont_lng, cont_lat)) %>% dplyr::rename(lng = cont_lng, lat = cont_lat)


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(df_ll[which(df_ll$index == df_triangle$index[i]), c("lng", "lat")])), 
                                     as.vector(unlist(tll[i, ]))))
}
df_triangle$dist = pbsapply(1:nrow(df_triangle), dist_filler)
df_triangle$dist = as.numeric(df_triangle$dist)

df_triangle = df_triangle %>% 
  dplyr::mutate(km = dplyr::case_when(
    dist <= 1000 ~ 1,
    dist <= 4000 & dist > 1000~ 4,
    dist <= 6000 & dist > 4000~ 6,
    dist <= 8000 & dist > 6000~ 8,
    dist > 8000 ~ NA)) %>%
  tidyr::drop_na(km)   

wt_summary = df_triangle %>% 
  dplyr::group_by(index, triangle, km) %>% 
  dplyr::summarise(nsites = n(), 
                   pfas = sum(total_pfas)) %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)

wt = wt_summary %>% 
  tidyr::pivot_wider(id_cols = index, names_from = c(triangle, km),
                     names_glue = sprintf('t{triangle}_km{km}_{.value}'), 
                     values_from = c(nsites, pfas))
wt[is.na(wt)] = 0

df = df %>% 
  left_join(wt)
df = df %>% 
  st_transform(4326) %>%
  mutate(well_lng = unlist(purrr::map(geometry,1)),
         well_lat = unlist(purrr::map(geometry,2)))
df = df %>% dplyr::select(!index)