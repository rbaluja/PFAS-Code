bll = births %>% 
  as_tibble() %>% 
  dplyr::select(geoid, lng, lat)

flowacc = function(i, d, w, option){
  d2 = d[[i]]
  w2 = w[i, ]
  
  if (option == "well"){
    w2$fa_well = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "resid"){
    w2$fa_resid = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "sp"){
    w2$sp = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "awc"){
    w2$awc = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "clay"){
    w2$clay = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "sand"){
    w2$sand = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "silt"){
    w2$silt = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "fc"){
    w2$fc = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }
  
  return(w2)
}

state_soil = function(s_abb, soil_var){
  b = births %>% 
    dplyr::filter(abb == s_abb)
  
  b_fa = births_fa[births_fa$geoid %in% b$geoid, ]
  
  sv = terra::rast(modify_path(paste0("Data_Verify/Soil/isric/", s_abb, "_mean_", soil_var, ".tif")))
  
  b_sv = exactextractr::exact_extract(sv, b_fa)
  b = dplyr::bind_rows(lapply(1:nrow(b), flowacc, b_sv, b, soil_var))
  
  return(b)
}

births_fa = bll %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(32110) %>% 
  st_buffer(1) %>% 
  st_transform(4326)

#soil porosity
sp = terra::rast(modify_path("Data_Verify/Soil/por_gNATSGO/por_gNATSGO_US.tif"))
b_sp = exactextractr::exact_extract(sp, births_fa)
births = dplyr::bind_rows(pblapply(1:nrow(births), flowacc, b_sp, births, "sp"))

#available water capacity
awc = terra::rast(modify_path("Data_Verify/Soil/awc_gNATSGO/awc_gNATSGO_US.tif"))
b_awc = exactextractr::exact_extract(awc, births_fa)
births = dplyr::bind_rows(pblapply(1:nrow(births), flowacc, b_awc, births, "awc"))

#clay content
births = dplyr::bind_rows(pblapply(unique(state_cross$abb), state_soil, "clay"))

#sand content
births = dplyr::bind_rows(pblapply(unique(state_cross$abb), state_soil, "sand"))

#silt content
births = dplyr::bind_rows(pblapply(unique(state_cross$abb), state_soil, "silt"))
