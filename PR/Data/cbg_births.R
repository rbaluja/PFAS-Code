#read in blocks shapefile
load("/Users/robert/Library/CloudStorage/Box-Box/NH Supplemental Data/cbg_tigris.RData")

#change cbg and df projections to planar for intersection 
cbg_shape = cbg_shape %>%
  st_transform(32110)

df = df %>% 
  st_transform(32110)

#assign block of residence
df = df %>% 
  st_join(cbg_shape, join = st_within, largest = T)

df = df %>% 
  dplyr::rename(geoid = GEOID)