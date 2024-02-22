#create necessary directory
dir.create("Data_Verify/GIS/cont_fa")
#read in cont_sites
cont_sites = read_xlsx('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>% # transform into a spatial dataframe
  st_set_crs('+proj=longlat +datum=WGS84' )

#two sites are repeated, remove them
cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

#set index to keep track of which site is which
cont_sites$index = 1:nrow(cont_sites)

c_pfas = cont_sites %>% as_tibble() %>% dplyr::select(sum_pfoa_pfos)

cont_fa = function(i){
  
  #get location of site i
  point_sf = cont_sites[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  #get flow path from site i as a raster (non flow path cells are NA)
  wbt_trace_downslope_flowpaths(seed_pts = temp_point_path, 
                                d8_pntr = "Data_Verify/GIS/flow_dir.tiff", 
                                output = paste0("Data_Verify/GIS/cont_fa/cont_fa_", i, ".tiff"))
  
  #read in flow path 
  fa = terra::rast(paste0("Data_Verify/GIS/cont_fa/cont_fa_", i, ".tiff"))
  
  #buffer non NA values 500 meters (so, buffer flow path by 500 meters)
  fa = terra::buffer(fa, 500, background = NA)
  
  #set all non NA values to pfas at site (set flow path valyes to pfas at the site)
  values(fa)[!is.na(values(fa))] =  rep(c_pfas$sum_pfoa_pfos[i], length(which(!is.na(values(fa)))))
  #write raster
  writeRaster(fa, paste0("Data_Verify/GIS/cont_fa/cont_fa_raster", i, ".tiff"), overwrite = TRUE)
  
 
  }

pblapply(1:nrow(cont_sites), cont_fa)

#initiate blank raster with same cells as DEM with all zeros
dem = terra::rast("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff")
z_raster = terra::init(dem, fun=0)


files = list.files("Data_Verify/GIS/cont_fa/", pattern = "cont_fa_raster", recursive = T, full.names = T)

cont_fa_read = function(f, z_raster){
  w_ws1 = terra::rast(f) #read in file f
  w_ws1 = terra::cover(w_ws1, z_raster) #replace NA cells with 0
  return(w_ws1)
}
#get list of all rasters, where NAs are replaced with 0
rl = pblapply(files, cont_fa_read, z_raster)
rl = rast(rl) #turn the list of rasters into one raster with multiple layers (same number as n sites)
rl = sum(rl) #sum across layers

#save buffer flow accumulation raster
writeRaster(rl, "Data_Verify/GIS/cont_fa_sum_buffed.tiff")

#delete intermediate files
unlink("Data_Verify/GIS/cont_fa/", recursive = TRUE)
  