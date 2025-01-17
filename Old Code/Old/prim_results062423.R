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
  st_transform(32110) %>%
  dplyr::mutate(well_lng = unlist(purrr::map(geometry,1)),
                well_lat = unlist(purrr::map(geometry,2))) %>% 
  as_tibble() %>%
  dplyr::select(id, well_lng, well_lat)

crp = cont_sites_buff %>% 
  as_tibble() %>%
  dplyr::select(!geometry) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(32110) %>%
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


wells_theta$theta_gw = as.numeric(wells_theta$theta_gw)



####IMPORTANT: I AM TAKING THE SITE WITH THE MAX AMOUNT OF PFAS
wells_theta$tdist = atan2(sin(wells_theta$theta - wells_theta$theta_gw), cos(wells_theta$theta - wells_theta$theta_gw))

wells_theta$downstream = ifelse(abs(wells_theta$tdist) <= pi/8 & !is.na(wells_theta$tdist), 1, 0)
wells_theta$close = ifelse(wells_theta$dist <= 1000, 1, 0)


wells_theta$distf = as.factor(floor(wells_theta$dist/4000))
wells_theta = wells_theta %>% 
  group_by(sys_id, source, downstream, distf) %>% 
  dplyr::summarize(pfas = sum(total_pfas))

wells_theta$dd =paste("down", wells_theta$downstream, "_km", wells_theta$distf , sep = "")
wells_theta = wells_theta %>%
  tidyr::pivot_wider(id_cols =  c("sys_id", "source"), names_from = "dd", values_from = "pfas")
wells_theta[is.na(wells_theta)] = 0

df = df %>% 
  left_join(wells_theta)



summary(fixest::feols(gestation ~ down0_km0 + down1_km0 + down1_km1 + m_age + 
          m_married + white + private_insurance  + cig  + n_prenatal  + 
          pm25 + temp +med_inc+ p_manuf + n_hunits + elevation |tract + year^month, data = df, warn = F, notes = F))
