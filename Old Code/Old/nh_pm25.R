dyn.load("/opt/ohpc/pub/apps/proj/7.2.1/lib/libproj.so.19")
dyn.load("/opt/ohpc/pub/apps/gdal/3.3.2/lib/libgdal.so.29")
dyn.load("/opt/ohpc/pub/libs/gnu8/openmpi3/hdf5/1.10.5/lib/libhdf5_hl.so.100")

library(dplyr)
library(raster)
library(ncdf4)
library(sf)
library(tigris)
library(exactextractr)

files = list.files("/groups/plan/rbaluja/PM25", full.names = T)

b = block_groups(state = "NH")

inner_pm25_fun = function(i, nc, file_num){
  di = exact_extract(nc, b[i, ])[[1]]
  
  #get weights
  weight = di$coverage_fraction
  di = di %>% 
    dplyr::select(!coverage_fraction)

  
  #get which columns are NA
  n = which(!is.na(di[ , 1]))
  di = di[n, ]
  di = t(di)
  
  #remove problem weights too
  weight = weight[n]
  
  #weighted average by hour where the weight is determined by how much of that cell is covered by the shape
  d = as.data.frame((di %*% weight)/sum(weight))
  
  #assign the date
  d$year = str_sub(files[file_num], -8, -5)
  colnames(d)[1] = "pm25"
  
  #bring in mun and state 
  d$geoid = b$GEOID[i]
  
  rownames(d) = NULL
  
  return(d)
}

pm25_fun = function(file_num){
  w = brick(files[file_num])
  w_mx = dplyr::bind_rows(pblapply(1:nrow(b), inner_pm25_fun, w, file_num, cl = 94))
  return(w_mx)
}

weather = dplyr::bind_rows(pblapply(1:length(files), pm25_fun))

#get the cbg's with NaNs and buffer them by 1.5 km and rerun 
a = unique(weather[is.na(weather$pm25), ]$geoid)
 b_m = b %>% 
   dplyr::filter(GEOID %in% a) %>%
   st_transform(3488) %>% 
   st_buffer(1500) %>% 
   st_transform(4326)

 
 inner_pm25_fun = function(i, nc, file_num){
   di = exact_extract(nc, b_m[i, ])[[1]]
   
   #get weights
   weight = di$coverage_fraction
   di = di %>% 
     dplyr::select(!coverage_fraction)
   
   
   #get which columns are NA
   n = which(!is.na(di[ , 1]))
   di = di[n, ]
   di = t(di)
   
   #remove problem weights too
   weight = weight[n]
   
   #weighted average by hour where the weight is determined by how much of that cell is covered by the shape
   d = as.data.frame((di %*% weight)/sum(weight))
   
   #assign the date
   d$year = str_sub(files[file_num], -8, -5)
   colnames(d)[1] = "pm25"
   
   #bring in mun and state 
   d$geoid = b_m$GEOID[i]
   
   rownames(d) = NULL
   
   return(d)
 }
 
 pm25_fun = function(file_num){
   w = brick(files[file_num])
   w_mx = dplyr::bind_rows(pblapply(1:nrow(b_m), inner_pm25_fun, w, file_num))
   return(w_mx)
 }
 
 w_m = dplyr::bind_rows(pblapply(1:length(files), pm25_fun))
 
 #bind them together and save to file
 w = rbind(weather %>% dplyr::filter(!(geoid %in% a)), w_m)
 fwrite(w, "nh_cbg_pm25.csv")
 