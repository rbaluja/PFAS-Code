dyn.load("/opt/ohpc/pub/apps/proj/7.2.1/lib/libproj.so.19")
dyn.load("/opt/ohpc/pub/apps/gdal/3.3.2/lib/libgdal.so.29")
dyn.load("/opt/ohpc/pub/libs/gnu8/openmpi3/hdf5/1.10.5/lib/libhdf5_hl.so.100")
library(tigris)
library(stringr)
library(raster)
library(ncdf4)
library(exactextractr)


b = block_groups(state = "NH")

years = 2009:2019
files = list.files('/xdisk/alanger/rbaluja/Files/Daymet', full.names = T, pattern = '*.nc')
files = files[str_sub(files, -7, -4) %in% as.character(years) & str_sub(files, -12, -9) != "prcp"]
w1 = brick(files[1])

inner_weather_fun = function(i, nc, file_num){
  di = exact_extract(nc, b[i, ])[[1]]
  
  #get weights
  weight = di$coverage_fraction
  di = di[ , -length(di)]
  #change column names to date
  colnames(di)[1:(length(di))] = str_sub(colnames(di)[1:(length(di))], 2, 11)
  
  #get which columns are NA
  n = which(!is.na(di[ , 1]))
  di = di[n, ]
  di = t(di)
  
  #remove problem weights too
  weight = weight[n]
  
  #weighted average by hour where the weight is determined by how much of that cell is covered by the shape
  d = as.data.frame((di %*% weight)/sum(weight))
  
  #assign the date
  d$date = unlist(rownames(d))
  colnames(d)[1] = str_sub(files[file_num], -12, -9)
  
  
  #bring in mun and state 
  d$geoid = b$GEOID[i]
  
  return(d)
}

weather_fun = function(file_num){
  w = brick(files[file_num])
  w_mx = dplyr::bind_rows(pblapply(1:nrow(b), inner_weather_fun, w, file_num, cl = 94))
  return(w_mx)
}

weather = dplyr::bind_rows(pblapply(1:length(files), weather_fun))

w_max = weather %>% 
    dplyr::filter(!is.na(tmax)) %>%
    dplyr::select(!c(tmin))

w_min = weather %>%
  dplyr::filter(!is.na(tmin)) %>%
  dplyr::select(!c(tmax))

rownames(w_max) = NULL
rownames(w_min) = NULL

w = left_join(w_max, w_min)
colnames(w) = c('tmax', 'date','location', 'tmin')
fwrite(w, "nh_block_weather.csv")
