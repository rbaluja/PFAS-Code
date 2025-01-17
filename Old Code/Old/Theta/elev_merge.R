load("/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality_predem062323.RData")
df = df %>% 
  left_join(fread("Data/Supplemental/wells_elev.csv", 
                  colClasses = c(sys_id = "character", source = "character")))

df_dom = df %>% 
  dplyr::filter(sys_id == 'Domestic Well')

df_dom$elevation = get_elev_point(as.data.frame(df_dom %>%
                                                    as_tibble() %>%
                                                    dplyr::select(c(well_lng, well_lat))), 
                                    prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")$elevation
df = df %>% 
  dplyr::filter(sys_id != "Domestic Well") %>% 
  rbind(df_dom)

save(df, file = "/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality062623.RData")
