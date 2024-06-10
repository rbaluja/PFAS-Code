ny_shapefile = zctas(year = 2010, state = 'NY')
ny_shapefile$geometry = st_transform(ny_shapefile$geometry, crs = '+proj=longlat +datum=WGS84')
space_pm25 = raster(modify_path4('New York/Data/Pollution/2010.tif'))
space_pm25.value = raster::extract(space_pm25, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
d = as.data.frame(space_pm25.value)
d = subset(d, select = c(ZCTA5CE10, X2010))
colnames(d) = c('zcta', 'mean_pm25_2010')
for (i in c('2011', '2012', '2013', '2014', '2015', '2016')){
  path = modify_path4(paste0('New York/Data/Pollution/' , i, '.tif'))
  space_pm25_i = raster(path)
  space_pm25_i.value = raster::extract(space_pm25_i, ny_shapefile, method = 'simple', fun = mean, na.rm = T, sp= T)
  d_i = as.data.frame(space_pm25_i.value)
  d_i = d_i[, c(2, ncol(d_i))]
  colnames(d_i) = c('zcta', paste0('pm25_', i)) 
  d = merge(d, d_i, by = 'zcta')
}
pollution = d
write.csv(pollution, modify_path4('New York/Data/Pollution/ny_pop_weighted_pm25.csv'))