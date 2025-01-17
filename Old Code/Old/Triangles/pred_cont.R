#merge testing data 
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
c1$total_pfoa_pfos = c1$pfoa + c1$pfos

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


cont_reg = fixest::feols(as.formula(paste(colnames(cwt.1)[ncol(cwt.1)], "~",
                                          paste(colnames(cwt.1)[2:(ncol(cwt.1) - 1)], collapse = "+"),
                                          sep = "")), data = cwt.1)

