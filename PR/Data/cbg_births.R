#read in blocks shapefile
cbg_shape = tigris::block_groups("NH")

#change cbg and df projections to planar for intersection 
cbg_shape = cbg_shape %>%
  st_transform(32110)

df = df %>% 
  st_transform(32110)

#assign block of residence as that which has the largest intersection
#point in polygon means that there is a unique intersection
df = df %>% 
  st_join(cbg_shape, join = st_within, largest = T)

df = df %>% 
  dplyr::rename(geoid = GEOID)
