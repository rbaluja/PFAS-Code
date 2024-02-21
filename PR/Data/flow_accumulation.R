# #read in flow accumulation raster
# cont_fa = terra::rast("New Hampshire/Data/QGIS/cont_fa/cont_fa_sum.tiff")
# #in cont sites flow acc raster, turn all zeros to NA, and everything else to 1
# nz_rast = ifel(cont_fa != 0, 1, NA)
# #get a raster with the min distance to a non-NA value
# dist_rast = terra::distance(nz_rast, unit = "m")
# 
# #start with wells
# #get distance from each well to nearest point on a flow line
# w_inter_fa= wells %>% as_tibble() %>%  
#   dplyr::select(!geometry) %>% 
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
#   st_transform(32110) %>% 
#   st_buffer(10) %>% 
#   st_transform(4326)
# 
# wells_dist = exactextractr::exact_extract(dist_rast, w_inter_fa)
# 
# wells = dplyr::bind_rows(pblapply(1:nrow(wells), distNAfun, wells_dist, wells, "well"))
# 
# df = df %>% left_join(wells %>% as_tibble() %>% dplyr::select(source, sys_id, well_fd))
# #move to test wells
# #get distance from each well to nearest point on a flow line
# fs_cont = fread("New Hampshire/Data/Contamination/cleaned_contwell_122023.csv")
# fs_cont$index = 1:nrow(fs_cont)
# fs_cont_fa= fs_cont %>% 
#   st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
#   st_transform(32110) %>% 
#   st_buffer(10) %>% 
#   st_transform(4326)
# 
# fs_cont_dist = exactextractr::exact_extract(dist_rast, fs_cont_fa)
# 
# fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), distNAfun, fs_cont_dist, fs_cont, "well"))

#get fa at well
# #read in flow accumulation raster
cont_fa = terra::rast("New Hampshire/Data/QGIS/cont_fa/cont_fa_sum_buffed.tiff")
w_inter_fa= wells %>% as_tibble() %>%  
  dplyr::select(!geometry) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(32110) %>% 
  st_buffer(10) %>% 
  st_transform(4326)

wells_fa = exactextractr::exact_extract(cont_fa, w_inter_fa)

w_fa = dplyr::bind_rows(pblapply(1:nrow(wells), flowacc, wells_fa, wells, "well"))

df = df %>% left_join(w_fa %>% as_tibble() %>% dplyr::select(sys_id, source, fa_well))

if (fa_resid == TRUE){
  #get flow accumulation at residence
  df_inter_fa= df %>% as_tibble() %>%  
    dplyr::select(!geometry) %>% 
    st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F) %>% 
    st_transform(32110) %>% 
    st_buffer(10) %>% 
    st_transform(4326)
  
  df_fa = exactextractr::exact_extract(cont_fa, df_inter_fa)
  
  df = dplyr::bind_rows(pblapply(1:nrow(df), flowacc, df_fa, df, "resid")) 
}
