wells$id = 1:nrow(wells)
wells_theta = st_intersection(wells %>%
                                as_tibble() %>%
                                dplyr::select(!geometry) %>%
                                st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                                st_transform(st_crs(cont_sites)),
                              cont_huc_inter %>% dplyr::select(site, industry, lng, lat, total_pfas, theta_gw))

#add in distance to the site
well_ll = as_tibble(wells) %>% dplyr::select(c(lng, lat, id))
tll = as_tibble(wells_theta) %>% dplyr::select(c(lng, lat))


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(well_ll[which(well_ll$id == wells_theta$id[i]), c("lng", "lat")])),
                                     as.vector(unlist(tll[i, ]))))
}
wells_theta$dist = pbsapply(1:nrow(wells_theta), dist_filler, cl = 8)
wells_theta$dist = as.numeric(wells_theta$dist)


#get reprojected lat long for wells and cont_sites
wrp = wells %>%
  as_tibble() %>%
  dplyr::select(!geometry) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  dplyr::mutate(well_lng = unlist(purrr::map(geometry,1)),
                well_lat = unlist(purrr::map(geometry,2))) %>% 
  as_tibble() %>%
  dplyr::select(id, well_lng, well_lat)

crp = cont_sites_buff %>% 
  as_tibble() %>%
  dplyr::select(!geometry) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  dplyr::mutate(lng = unlist(purrr::map(geometry,1)),
                lat = unlist(purrr::map(geometry,2))) %>% 
  as_tibble() %>% 
  dplyr::select(site, lng, lat)

#join these back to wells_triangle
wells_theta = wells_theta %>% 
  dplyr::select(!c(lat, lng)) %>% 
  left_join(wrp) %>% 
  left_join(crp)

wells_theta$theta = atan2(wells_theta$well_lat - wells_theta$lat, 
                          wells_theta$well_lng - wells_theta$lng)



####IMPORTANT: I AM TAKING THE SITE WITH THE MAX AMOUNT OF PFAS
wells_theta$tdist = atan2(sin(wells_theta$theta - wells_theta$theta_gw), cos(wells_theta$theta - wells_theta$theta_gw))

wells_theta = wells_theta %>% 
  dplyr::mutate(triangle = dplyr::case_when(
    tdist > -pi/4 & tdist <= pi/4 ~ 1, 
    tdist > pi/4 & tdist <= 3 * pi/4 ~ 2, 
    tdist > -3 * pi/4 & tdist <= -pi/4 ~ 4, 
    tdist > 3 * pi/4 | tdist < -3 * pi/4 ~ 3
  ))

wells_theta = wells_theta %>% 
  dplyr::mutate(km = dplyr::case_when(
    dist <= 2000 ~ 2, 
    dist > 2000 & dist <= 4000 ~ 4, 
    dist > 4000 & dist <= 6000 ~ 6, 
    dist > 6000 & dist <= 8050 ~ 8, #giving 50 meter buffer before dropping it
    dist > 8050 ~ NA
  ))

wells_theta = wells_theta %>% 
  dplyr::group_by(sys_id, source, km, triangle) %>% 
  dplyr::summarize(nsites = n(), 
                   pfas = sum(total_pfas)) %>% 
  as_tibble() %>% dplyr::select(!geometry)

wt = wells_theta %>% 
  tidyr::pivot_wider(id_cols = c(sys_id, source), names_from = c(triangle, km),
                     names_glue = sprintf('t{triangle}_km{km}_{.value}'), 
                     values_from = c(nsites, pfas))
wt[is.na(wt)] = 0




#add this to the natality dataframe
df = df %>% left_join(wt %>% 
                        as_tibble())

#do the same for the domestic wells
dom_well = df %>%
  dplyr::filter(sys_id == "Domestic Well") %>%
  dplyr::select(!ends_with("nsites") & 
                  !ends_with("pfas")) #NOTE: Change any names that start with t, or change triangles
dom_well$index = 1:nrow(dom_well)



domw_triangle = st_intersection(dom_well  %>% 
                                  st_transform(st_crs(cont_sites)) %>% 
                                  dplyr::select(index),
                                cont_huc_inter %>% dplyr::select(site, industry, lng, lat, total_pfas, theta_gw))

#add in distance to the site
dwell_ll = dom_well %>%
  st_transform(4326) %>%
  mutate(lng = unlist(purrr::map(geometry,1)),
         lat = unlist(purrr::map(geometry,2))) %>%
  as_tibble() %>%
  dplyr::select(c(lng, lat, index))
tll = as_tibble(domw_triangle) %>% dplyr::select(c(lng, lat)) 


dist_filler = function(i){
  dist = as.numeric(geosphere::distm(as.vector(unlist(dwell_ll[which(dwell_ll$index == domw_triangle$index[i]), c("lng", "lat")])),
                                     as.vector(unlist(tll[i, ]))))
}
domw_triangle$dist = pbsapply(1:nrow(domw_triangle), dist_filler, cl = 4)
domw_triangle$dist = as.numeric(domw_triangle$dist)

wrp = dom_well %>%
  as_tibble() %>%
  dplyr::select(!geometry) %>%
  st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>%
  st_transform(5070) %>%
  dplyr::mutate(well_lng = unlist(purrr::map(geometry,1)),
                well_lat = unlist(purrr::map(geometry,2))) %>% 
  as_tibble() %>%
  dplyr::select(index, well_lng, well_lat)


#join these back to domw_triangle
domw_triangle = domw_triangle %>% 
  dplyr::select(!c(lat, lng)) %>% 
  left_join(wrp) %>% 
  left_join(crp)

domw_triangle$theta = atan2(domw_triangle$well_lat - domw_triangle$lat, 
                            domw_triangle$well_lng - domw_triangle$lng)
domw_triangle$tdist = atan2(sin(domw_triangle$theta - domw_triangle$theta_gw), cos(domw_triangle$theta - domw_triangle$theta_gw))


domw_triangle = domw_triangle %>% 
  dplyr::mutate(triangle = dplyr::case_when(
    tdist > -pi/4 & tdist <= pi/4 ~ 1, 
    tdist > pi/4 & tdist <= 3 * pi/4 ~ 2, 
    tdist > -3 * pi/4 & tdist <= -pi/4 ~ 4, 
    tdist > 3 * pi/4 | tdist < -3 * pi/4 ~ 3
  ))

domw_triangle = domw_triangle %>% 
  dplyr::mutate(km = dplyr::case_when(
    dist <= 2000 ~ 2, 
    dist > 2000 & dist <= 4000 ~ 4, 
    dist > 4000 & dist <= 6000 ~ 6, 
    dist > 6000 & dist <= 8050 ~ 8, #giving 50 meter buffer before dropping it
    dist > 8050 ~ NA
  ))


domw_triangle = domw_triangle %>% 
  dplyr::group_by(index, km, triangle) %>% 
  dplyr::summarize(nsites = n(), 
                   pfas = sum(total_pfas)) %>% 
  as_tibble() %>%
  dplyr::select(!geometry)

wt = domw_triangle %>% 
  tidyr::pivot_wider(id_cols = index, names_from = c(triangle, km),
                     names_glue = sprintf('t{triangle}_km{km}_{.value}'), 
                     values_from = c(nsites, pfas))
wt[is.na(wt)] = 0


dom_well = dom_well %>% 
  left_join(wt)


#remove domestic wells from df and add them back
df = df %>%
  dplyr::filter(sys_id != "Domestic Well") %>%
  rbind(dom_well %>% dplyr::select(!index))

#find distance from pease afb
df$dist_pease = distm(df %>% 
                        as_tibble() %>% 
                        dplyr::select(c(well_lng, well_lat)), 
                      cont_sites %>% 
                        as_tibble() %>% 
                        dplyr::filter(site == "Pease Air Force Base") %>%
                        dplyr::select(c(lng, lat)))[, 1]
df$close_pease = ifelse(df$dist_pease <= meters, 1, 0)
