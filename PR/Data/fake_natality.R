#sample lat long from union of service areas
saf = st_read('Data_Verify/Groundwater/NH_Confidential_Data/Secure_Data/Water_and_Sewer_Lines.shp') %>%
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(pipe_type %in% c('WATER', 'BOTH') & sys_type == 'C') %>% #get rid of only sewer service and non community water systems (think restaurants, schools, etc.)
  dplyr::select(sys_id, sys_name, geometry) %>% #only keep the system name and its shape
  dplyr::group_by(sys_id) %>% 
  dplyr::summarise(geometry = st_union(geometry)) %>% #most large systems have separate shapes for only water and both water/sewer service. Combine them. 
  ungroup() %>%
  unique()

f_resid = st_sample(st_union(saf$geometry), 20000) %>% 
  st_transform(4326)

rm(saf)

df = do.call(rbind, st_geometry(f_resid)) %>% 
  as_tibble() %>% setNames(c("lng","lat")) 

df$year = sample(2010:2019, nrow(df), replace = T)
df$month = sample(stringr::str_pad(as.character(1:12), 2, "left", "0"), nrow(df), replace = T)

df$m_age = sample(16:45, nrow(df), replace = T)
df$m_married = sample(c(0, 1), nrow(df), replace = T)
df$private_insurance = sample(c(0, 1), nrow(df), replace = T)
df$nbr_cgrtt = sample(1:10, nrow(df), replace = T)
df$m_educ = sample(1:9, nrow(df), replace = T)
df$f_educ = sample(1:9, nrow(df), replace = T)
df$mr_04 = sample(c(0, 1), nrow(df), replace = T)
df$mr_08 = sample(c(0, 1), nrow(df), replace = T)
df$mr_18 = sample(c(0, 1), nrow(df), replace = T)
df$mr_21 = sample(c(0, 1), nrow(df), replace = T)
df$mr_26 = sample(c(0, 1), nrow(df), replace = T)
df$mr_27 = sample(c(0, 1), nrow(df), replace = T)
df$mthr_wgt_dlv = sample(100:300, nrow(df), replace = T)
df$mthr_pre_preg_wgt = sample(100:300, nrow(df), replace = T)
df$birth_race_dsc_1 = sample(c("White", "Chinese", "Other (Specify)", 
                               "NA", "Black or African American", "Asian Indian", 
                               "Other Asian (Specify)", "Vietnamese", 
                               "American Indian or Alaska Native", "Filipino", 
                               "Korean", "Other Pacific Islander (Specify)", 
                               "Other Asian", "Japanese", "Other", "Guamanian or Chomorro", 
                               "Native Hawaiian", "Other Asian(Specify)", "Other Pacific Islander", 
                               "Samoan", "Guamanian or Chamorro", "Other(Specify)"), nrow(df), replace = T)

#read in blocks shapefile
cbg_shape = tigris::block_groups("NH")

#change cbg and df projections to planar for intersection 
cbg_shape = cbg_shape %>%
  st_transform(32110)

df = df %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F) %>%
  st_transform(32110)

#assign block of residence as that which has the largest intersection
#point in polygon means that there is a unique intersection
df = df %>% 
  st_join(cbg_shape, join = st_within, largest = T)

df = df %>% 
  dplyr::rename(geoid = GEOID)

save(df, file = "Data_Verify/fake_natality.RData")
