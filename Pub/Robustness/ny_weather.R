weather = raster(modify_path4('New York/Data/Temperature/PRISM_tmean_stable_4kmM3_2010_bil.bil'))
weather.value = raster::extract(weather, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
d = as.data.frame(weather.value)
d = d[, c(2, ncol(d))]
colnames(d) = c('zcta', 'temp_2010') 
for (i in c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')){
  path = modify_path4(paste0('New York/Data/Temperature/PRISM_tmean_stable_4kmM3_' , i, '_bil.bil'))
  weather_i = raster(path)
  weather_i.value = raster::extract(weather_i, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
  d_i = as.data.frame(weather_i.value)
  d_i = d_i[, c(2, ncol(d_i))]
  colnames(d_i) = c('zcta', paste0('temp_', i)) 
  d = merge(d, d_i, by = 'zcta')
}
weather = d
write.csv(weather,  modify_path4('New York/Data/Temperature/ny_pop_weighted_temp.csv'))