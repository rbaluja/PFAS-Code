library(whitebox)

#fill sinks
wbt_breach_depressions("/Users/robert/Library/CloudStorage/Box-Box/New Hampshire/Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff", "New Hampshire/Data/QGIS/filled_dem.tiff")

#flow direction
wbt_d8_pointer("New Hampshire/Data/QGIS/filled_dem.tif", "New Hampshire/Data/QGIS/flow_dir.tiff")

#read in cont_sites
cont_sites = read_xlsx('New Hampshire/Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(`Site name`, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

cont_sites$index = 1:nrow(cont_sites)

c_pfas = cont_sites %>% as_tibble() %>% dplyr::select(sum_pfoa_pfos)

cont_fa = function(i){
  
  point_sf = cont_sites[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  wbt_trace_downslope_flowpaths(seed_pts = temp_point_path, 
                                d8_pntr = "New Hampshire/Data/QGIS/flow_dir.tiff", 
                                output = paste0("New Hampshire/Data/QGIS/cont_fa/cont_fa_", i, ".tiff"))
  
  fa = terra::rast(paste0("New Hampshire/Data/QGIS/cont_fa/cont_fa_", i, ".tiff"))
  
  #buffer non NA values 500 meters
  fa = terra::buffer(fa, 500, background = NA)
  
  #set all non NA values to pfas at site
  values(fa)[!is.na(values(fa))] =  rep(c_pfas$sum_pfoa_pfos[i], length(which(!is.na(values(fa)))))
  #write raster
  writeRaster(fa, paste0("New Hampshire/Data/QGIS/cont_fa/cont_fa_raster", i, ".tiff"), overwrite = TRUE)
  
 
  }

pblapply(1:nrow(cont_sites), cont_fa)

#initiate blank raster with all zeros
dem = terra::rast("/Users/robert/Library/CloudStorage/Box-Box/New Hampshire/Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff")
z_raster = terra::init(dem, fun=0)


files = list.files("New Hampshire/Data/QGIS/cont_fa/", pattern = "cont_fa_raster", recursive = T, full.names = T)

cont_fa_read = function(f, z_raster){
  w_ws1 = terra::rast(f)
  w_ws1 = cover(w_ws1, z_raster)
  return(w_ws1)
}

rl = pblapply(files, cont_fa_read, z_raster)
rl = rast(rl)
rl = sum(rl)

writeRaster(rl, "New Hampshire/Data/QGIS/cont_fa/cont_fa_sum_buffed.tiff")

  