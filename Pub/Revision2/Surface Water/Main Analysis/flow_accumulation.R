#read in flow accumulation raster
cont_fa = terra::rast(modify_path("Data_Verify/GIS/cont_fa_sum_buffed.tiff"))

#get flow accumulation at residence
#turns df spatial, and buffers its geometry by 10meters
df_inter_fa= df %>% as_tibble() %>%  
  dplyr::select(!geometry) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F) %>% 
  st_transform(32110) %>% 
  st_buffer(10) %>% 
  st_transform(4326)

#get area weights and cell values for all cells that each row of df_inter_fa falls into
df_fa = exactextractr::exact_extract(cont_fa, df_inter_fa)
#for each residence, take the area weighted sum of flow accumulation cells 
df = dplyr::bind_rows(pblapply(1:nrow(df), flowacc, df_fa, df, "resid", cl = n_cores)) 
