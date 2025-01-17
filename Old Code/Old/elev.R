df_pub = df %>% 
  dplyr::filter(sys_id != "Domestic Well") %>% 
  left_join(wells %>% 
              as_tibble() %>% 
              dplyr::select(c(sys_id, source, elevation)))

df_dom = df %>% 
  dplyr::filter(sys_id == "Domestic Well")

df_dom$elevation = get_elev_point(as.data.frame(df_dom %>% 
                                                  as_tibble() %>%
                                                  dplyr::select(c(well_lng, well_lat))), prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")$elevation

df = rbind(df_pub, df_dom)

#save(df, file = "/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality_wdem_elev.RData")
