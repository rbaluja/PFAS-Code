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

sp = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/por_gNATSGO/por_gNATSGO_US.tif")
# sp = terra::focal(sp, 3, "modal", na.policy = "only")
# terra::writeRaster(sp, "/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/por_gNATSGO/por_gNATSGO_US_fna.tif")

b_sp = exactextractr::exact_extract(sp, births_fa)

births = dplyr::bind_rows(pblapply(1:nrow(births), flowacc, b_sp, births, "sp"))


#available water capacity
births_fa = bll %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(32110) %>% 
  st_buffer(1) %>% 
  st_transform(4326)

awc = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/awc_gNATSGO/awc_gNATSGO_US.tif")
# awc = terra::focal(awc, 3, "modal", na.policy = "only")
# terra::writeRaster(awc, "/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/awc_gNATSGO/awc_gNATSGO_US_fna.tif")


b_awc = exactextractr::exact_extract(awc, births_fa)

births = dplyr::bind_rows(pblapply(1:nrow(births), flowacc, b_awc, births, "awc"))

fwrite(births, "Nat Data/nat_births_fcleaned5.csv")
