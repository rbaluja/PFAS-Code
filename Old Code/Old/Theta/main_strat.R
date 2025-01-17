wells$id = 1:nrow(wells)
wells_theta = st_intersection(wells %>%
                                as_tibble() %>%
                                dplyr::select(!geometry) %>%
                                st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                                st_transform(st_crs(cont_sites)),
                              cont_huc_inter %>% dplyr::select(site, industry, lng, lat, sum_pfoa_pfos, theta_gw))

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
  dplyr::mutate(cont_lng = unlist(purrr::map(geometry,1)),
                cont_lat = unlist(purrr::map(geometry,2))) %>% 
  as_tibble() %>% 
  dplyr::select(site, cont_lng, cont_lat)

#join these back to wells_triangle
wells_theta = wells_theta %>% 
  dplyr::select(!c(lat, lng)) %>% 
  left_join(wrp) %>% 
  left_join(crp)

wells_theta$theta = atan2(wells_theta$well_lat - wells_theta$cont_lat, 
                          wells_theta$well_lng - wells_theta$cont_lng)


wells_theta$theta_gw = as.numeric(wells_theta$theta_gw)



#gives distance between theta_gw and theta in theta space
wells_theta$tdist = atan2(sin(wells_theta$theta - wells_theta$theta_gw), cos(wells_theta$theta - wells_theta$theta_gw))

wells_theta$downstream = ifelse(abs(wells_theta$tdist) <= pi/4 & !is.na(wells_theta$tdist), 1, 0)


wells_theta$distf = as.factor(floor(wells_theta$dist/5000))
#transform to ppb
wells_theta$total_pfas = wells_theta$sum_pfoa_pfos/1000

wells_theta = wells_theta %>% 
  group_by(sys_id, source, downstream, distf) %>% 
  dplyr::summarize(pfas = sum(total_pfas))

#add grouping variable
wells_theta$dd = paste("down", wells_theta$downstream, "_km", wells_theta$distf , sep = "")

#pivot around the grouping variable to get amount of pfas in each block
wells_theta = wells_theta %>%
  tidyr::pivot_wider(id_cols =  c("sys_id", "source"), names_from = "dd", values_from = "pfas")

wells_theta[is.na(wells_theta)] = 0

df = df %>% 
  left_join(wells_theta)



# summary(fixest::feols(preterm ~ down0_km0 + down1_km0 + down1_km1 + 
#                         m_age + m_married + white + private_insurance  + cig  + n_prenatal  + 
#                         pm25 + temp +med_inc+ p_manuf + n_hunits + elevation
#                       |county^year + year^month + max_educ, data = df, warn = F, notes = F))


if (domestic == TRUE){
  #do the same for the domestic wells
  dom_well = df %>%
    dplyr::filter(sys_id == "Domestic Well") %>%
    dplyr::select(!starts_with("down"))
  dom_well$index = 1:nrow(dom_well)



  domw_triangle = st_intersection(dom_well  %>%
                                    st_transform(st_crs(cont_sites)) %>%
                                    dplyr::select(index),
                                  cont_huc_inter %>% dplyr::select(site, industry, lng, lat, sum_pfoa_pfos, theta_gw))

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
    st_transform(32110) %>%
    dplyr::mutate(well_lng = unlist(purrr::map(geometry,1)),
                  well_lat = unlist(purrr::map(geometry,2))) %>%
    as_tibble() %>%
    dplyr::select(index, well_lng, well_lat)


  #join these back to domw_triangle
  domw_triangle = domw_triangle %>%
    dplyr::select(!c(lat, lng)) %>%
    left_join(wrp) %>%
    left_join(crp)

  #takes care of flat sites
  domw_triangle$theta_gw = as.numeric(domw_triangle$theta_gw)


  domw_triangle$theta = atan2(domw_triangle$well_lat - domw_triangle$cont_lat,
                              domw_triangle$well_lng - domw_triangle$cont_lng)
  
  #gives distance between theta_gw and theta in theta space
  domw_triangle$tdist = atan2(sin(domw_triangle$theta - domw_triangle$theta_gw), cos(domw_triangle$theta - domw_triangle$theta_gw))

  domw_triangle$downstream = ifelse(abs(domw_triangle$tdist) <= pi/4 & !is.na(domw_triangle$theta_gw), 1, 0)

  domw_triangle$distf = as.factor(floor(domw_triangle$dist/5000))
  
  #transform to ppb
  domw_triangle$total_pfas = domw_triangle$sum_pfoa_pfos/1000
  
  domw_triangle = domw_triangle %>% 
    group_by(index, downstream, distf) %>% 
    dplyr::summarize(pfas = sum(total_pfas))
  
  #add grouping variable
  domw_triangle$dd = paste("down", domw_triangle$downstream, "_km", domw_triangle$distf , sep = "")
  
  #pivot around the grouping variable to get amount of pfas in each block
  domw_triangle = domw_triangle %>%
    tidyr::pivot_wider(id_cols = "index", names_from = "dd", values_from = "pfas")
  
  domw_triangle[is.na(domw_triangle)] = 0
  
  dom_well = dom_well %>%
    left_join(domw_triangle)


  #remove domestic wells from df and add them back
  df = df %>%
    dplyr::filter(sys_id != "Domestic Well") %>%
    rbind(dom_well %>% 
            dplyr::select(!index))
  
}

# #find distance from pease afb
df$dist_pease = distm(df %>%
                        as_tibble() %>%
                        dplyr::select(c(well_lng, well_lat)),
                      cont_sites %>%
                        as_tibble() %>%
                        dplyr::filter(site == "Pease Air Force Base") %>%
                        dplyr::select(c(lng, lat)))
df$close_pease = ifelse(df$dist_pease <= meters, 1, 0)



df$preterm = ifelse(df$gestation <= 36, 1, 0)
df$low_apgar = ifelse(df$apgar5 <= 5, 1, 0)
df$domestic = ifelse(df$sys_id == "Domestic Well", 1, 0)
# summary(fixest::feols(gestation~ down0_km0 + down1_km0 + down1_km1 + down0_km1 + 
# 
#                         m_age + m_married + white + private_insurance  + cig  + n_prenatal  +
#                         pm25 + temp +med_inc+ p_manuf + n_hunits  + domestic + elevation|
#                       year^month + year^geoid + max_educ, data = df, warn = F, notes = F))


