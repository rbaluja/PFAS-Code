#read in DEM
dem_elev = terra::rast(modify_path("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff"))
#subset wells to lat long and turn spatial
well_elev = wells %>% 
  as_tibble() %>% 
  dplyr::select(source, sys_id, lng, lat) %>% 
  st_as_sf(coords = c("lng", "lat"))
#for each well, obtain the elevation in its cell of dem_elev
e1 = raster::extract(dem_elev, well_elev)

well_elev = well_elev %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)
well_elev$well_elev = e1$`LiDAR-Derived Bare Earth DEM - NH`

df = df %>% 
  left_join(well_elev)

#get elevation for location of residence
df$row = 1:nrow(df)
df_ll = df %>% 
  as_tibble() %>% 
  dplyr::select(row, lng, lat) %>% 
  st_as_sf(coords = c("lng", "lat"))

e1 = raster::extract(dem_elev, df_ll)
df$resid_elev = e1$`LiDAR-Derived Bare Earth DEM - NH`

#save(df, file = paste0(natality_path, "[UA Box Health] birth_records_matched.RData"))
