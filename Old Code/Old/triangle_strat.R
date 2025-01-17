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
                   sum_pfas = max(total_pfas)) %>% 
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
domw_triangle$dist = pbsapply(1:nrow(domw_triangle), dist_filler)
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

df$lbweight = ifelse(df$bweight < 1500, 1, 0)
df$preterm = ifelse(df$gestation <= 36, 1, 0)

df$triangle = ifelse(!is.na(df$t1_n_sites), 1, 0)
df$max_educ = ifelse((!is.na(df$m_educ) & !is.na(df$f_educ) & df$m_educ >= df$f_educ) | 
                       (is.na(df$f_educ) & !is.na(df$m_educ)), df$m_educ,
                     ifelse((!is.na(df$m_educ) & !is.na(df$f_educ) & df$m_educ < df$f_educ) | 
                              (!is.na(df$f_educ) & is.na(df$m_educ)), df$f_educ, NA))

#na's on the amounts are zeros, i.e. they are not close enough to any sites. For now I remove them

r = fixest::feols(as.formula(paste("gestation", "~",
                               paste(colnames(df)[endsWith(colnames(df), "nsites") | 
                                                    endsWith(colnames(df), "pfas") ], collapse = "+"), 
                               " + m_age + gestation + m_married + white + private_insurance + cig + n_prenatal + ",
                               "pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ +month ",
                               sep = "")), data = df)
summary(r)


r = fixest::feols(as.formula(paste("bweight", "~",
                                   paste(colnames(df)[endsWith(colnames(df), "nsites") | 
                                                        endsWith(colnames(df), "pfas") ], collapse = "+"), 
                                   " + m_age + gestation + m_married + white + private_insurance + cig + n_prenatal + ",
                                   "pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^tract + max_educ +month ",
                                   sep = "")), data = df)
summary(r)























# 
# 
# summary(fixest::feols(gestation ~ t4_n_sites  + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ, data = df))
# summary(fixest::feols(gestation ~ ifelse(t1_n_sites > 0, 1, 0)  + m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(preterm ~ ifelse(t4_n_sites > 0, 1, 0) +
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(gestation ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(bweight ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(preterm ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(lbweight ~ triangle + 
#                         m_age + m_married + white + private_insurance + cig + n_prenatal|year^month + max_educ, data = df))
# 
# summary(fixest::feols(lbweight ~ t1_n_sites  + m_age + m_married + white + private_insurance + cig + n_prenatal + gestation|year + month, data = df_nazero))
# 
# 
# df_c = df %>% 
#   dplyr::filter(!is.na(t1_n_sites))
# 
# summary(fixest::feols(gestation ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ, data = df_c))
# 
# summary(fixest::feols(bweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ, data = df_c))
# 
# summary(fixest::feols(preterm ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ, data = df_c))
# 
# summary(fixest::feols(lbweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0) +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ, data = df_c))
# 
# 
# #adding county fe
# summary(fixest::feols(gestation ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  + 
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(bweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(preterm ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0)  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(lbweight ~ ifelse(t1_n_sites > 0, 1, 0) + ifelse(t2_n_sites > 0, 1, 0)  + ifelse(t3_n_sites > 0, 1, 0)  + 
#                         ifelse(t4_n_sites > 0, 1, 0)  + ifelse(t5_n_sites > 0, 1, 0)  + ifelse(t6_n_sites > 0, 1, 0) +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
# #adding cont amounts
# summary(fixest::feols(gestation ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas+
#                         m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(bweight ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas  +
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(preterm ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas+
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice|year^month + max_educ + county, data = df_c))
# 
# summary(fixest::feols(lbweight ~ t1_sum_pfas + t2_sum_pfas + t3_sum_pfas + t4_sum_pfas + t5_sum_pfas + t6_sum_pfas+
#                         + m_age + m_married + white + private_insurance + cig + n_prenatal + 
#                         pm25 + temp + med_inc + p_manuf + n_hunits + med_hprice + gestation|year^month + max_educ + county, data = df_c))
# 
