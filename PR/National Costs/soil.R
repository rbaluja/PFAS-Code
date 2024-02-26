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
  }else if (option == "fc"){
    w2$fc = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }
  
  return(w2)
}

#soil porosity
births_fa = bll %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(32110) %>% 
  st_buffer(1) %>% 
  st_transform(4326)

sp = terra::rast("Data_Verify/Soil/por_gNATSGO/por_gNATSGO_US.tif")

b_sp = exactextractr::exact_extract(sp, births_fa)

births = dplyr::bind_rows(pblapply(1:nrow(births), flowacc, b_sp, births, "sp"))


#available water capacity
awc = terra::rast("Data_Verify/Soil/awc_gNATSGO/awc_gNATSGO_US.tif")

b_awc = exactextractr::exact_extract(awc, births_fa)

births = dplyr::bind_rows(pblapply(1:nrow(births), flowacc, b_awc, births, "awc"))

fwrite(births, "Data_Verify/National/nat_births_fcleaned5.csv")
