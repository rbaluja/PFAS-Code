dem_elev = raster("New Hampshire/Data/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff")

well_elev = wells %>% 
  as_tibble() %>% 
  dplyr::select(source, sys_id, lng, lat) %>% 
  st_as_sf(coords = c("lng", "lat"))
e1 = raster::extract(dem_elev, well_elev)

well_elev = well_elev %>% 
  as_tibble() %>% 
  dplyr::select(!geometry)
well_elev$well_elev = e1

df = df %>% 
  left_join(well_elev)

#get elevation for location of residence
df$row = 1:nrow(df)
df_ll = df %>% 
  as_tibble() %>% 
  dplyr::select(row, lng, lat) %>% 
  st_as_sf(coords = c("lng", "lat"))

e1 = raster::extract(dem_elev, df_ll)
df$resid_elev = e1

#save(df, file = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_matched_oldwells122023.RData")
