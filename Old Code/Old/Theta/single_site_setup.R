
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
  


####IMPORTANT: I AM TAKING THE SITE closest to the flow direction
wells_theta$tdist = atan2(sin(wells_theta$theta - wells_theta$theta_gw), cos(wells_theta$theta - wells_theta$theta_gw))

wells_theta = wells_theta %>% 
  dplyr::group_by(sys_id, source) %>% 
  dplyr::filter(total_pfas == max(total_pfas)) 




#add this to the natality dataframe
df = df %>% left_join(wells_theta %>% 
                        as_tibble() %>%
                        dplyr::select(sys_id, source, total_pfas, dist, theta, theta_gw, site,  industry))

#do the same for the domestic wells
dom_well = df %>%
  dplyr::filter(sys_id == "Domestic Well") %>%
  dplyr::select(!c(total_pfas, dist, theta, theta_gw, site,  industry)) #NOTE: Change any names that start with t, or change triangles
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




####IMPORTANT: I AM TAKING THE SITE closest to the flow direction
domw_triangle = domw_triangle %>% 
  dplyr::group_by(index) %>% 
  dplyr::filter(total_pfas == max(total_pfas)) 

dom_well = dom_well %>% left_join(domw_triangle %>% 
                        as_tibble() %>%
                        dplyr::select(index, total_pfas, dist, theta, theta_gw, site,  industry)) %>% 
  dplyr::select(!index)


#remove domestic wells from df and add them back
df = df %>%
  dplyr::filter(sys_id != "Domestic Well") %>%
  rbind(dom_well)

df$tdist = atan2(sin(df$theta - df$theta_gw), cos(df$theta - df$theta_gw))



df$dist_pease = distm(df %>% 
                        as_tibble() %>% 
                        dplyr::select(c(well_lng, well_lat)), 
                      cont_sites %>% 
                        as_tibble() %>% 
                        dplyr::filter(site == "Pease Air Force Base") %>%
                        dplyr::select(c(lng, lat)))
df$close_pease = ifelse(df$dist_pease <= meters, 1, 0)

#run analysis
summary(fixest::feols(gestation~ abs(tdist)*I(dist/1000) +
                      + m_age + m_married + white + private_insurance  + cig  + n_prenatal  + 
                        pm25 + temp +med_inc+ p_manuf + n_hunits + elevation  + em_rate + man_rgdp|year^month, 
                      data = df ))

summary(fixest::feols(gestation ~  abs(tdist)*I(dist/1000)|year^month, data = df ))




#great, they are very similar
df$ldist = log(df$dist)
df$year_month = paste0(df$year, "/", df$month)

r = fixest::feols(gestation ~ I(log(dist)) + I(abs(theta_gw - theta))|year^month, data = df[df$gestation <= 60, ] )

b = fixest::feols(bweight ~ I(log(dist))*I(abs(theta_gw - theta)), data = df )







