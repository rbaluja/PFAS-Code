dir.create(modify_path("Data_Verify/GIS/cs_down"))
dir.create(modify_path("Data_Verify/GIS/cs_down/cont_pp"))
dir.create(modify_path("Data_Verify/GIS/cs_down/cont_watershed"))
dir.create(modify_path("Data_Verify/GIS/cs_down/cont_watershed/Shapes"))

#fill sinks
wbt_breach_depressions(modify_path("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff"), modify_path("Data_Verify/GIS/filled_dem.tiff"))

#flow accumulation
wbt_d8_flow_accumulation(modify_path("Data_Verify/GIS/filled_dem.tiff"), modify_path("Data_Verify/GIS/flow_acc.tiff"))

#flow direction
wbt_d8_pointer(modify_path("Data_Verify/GIS/filled_dem.tiff"), modify_path("Data_Verify/GIS/flow_dir.tiff"))


#read in cont_sites
cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>% # transform into a spatial dataframe
  st_set_crs('+proj=longlat +datum=WGS84' )

#two sites are repeated, remove them
cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

cont_sites$index = 1:nrow(cont_sites)

cont_sites_buff = cont_sites %>% 
  st_transform(32110) %>% #NH meters projected CRS
  st_buffer(10000) %>% 
  st_transform(4326)

inner_ws = function(j, spoints, site, cs){
  
  spo = spoints$x[j]
  
  temp_point_path = tempfile(fileext = ".shp")
  st_write(spo, temp_point_path, quiet = TRUE)
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = modify_path("Data_Verify/GIS/flow_acc.tiff"), 
                       output = modify_path(paste0("Data_Verify/GIS/cs_down/cont_pp/pp_site_", j, ".shp")), 
                       snap_dist = 0.007569 * 5) #grid size by 5
  #calculate watershed
  wbt_watershed(d8_pntr = modify_path("Data_Verify/GIS/flow_dir.tiff"), 
                pour_pts = modify_path(paste0("Data_Verify/GIS/cs_down/cont_pp/pp_site_", j, ".shp")), 
                output = modify_path(paste0("Data_Verify/GIS/cs_down/cont_watershed/watershed_", j, ".tiff")))
  #read in watershed
  ws = terra::rast(modify_path(paste0("Data_Verify/GIS/cs_down/cont_watershed/watershed_", j, ".tiff")))
  
  #transform watershed to a polygon
  ws_poly = st_as_sf(as.polygons(ws)) %>% 
    st_transform(3437) %>% 
    dplyr::summarise(geometry = st_union(geometry))
  
  
  bin = st_contains(ws_poly, cs %>% st_transform(3437))
  
  coords = st_coordinates(spo)
  
  lng = as.numeric(coords[1,1])
  lat = as.numeric(coords[1,2])
  
  return(data.frame("lng" = lng, "lat" = lat, "site" = site, d_ws = any(lengths(bin) > 0)))
  
  
}


watershed = function(i, n){
  
  csb = cont_sites_buff[i, ]
  cs = cont_sites[i, ]
  
  spoints2 = st_as_sf(st_sample(csb$geometry[1], n))
  
  #get binary classification points for watershed
  ws = dplyr::bind_rows(lapply(1:n, inner_ws, spoints2, cs$site[i], cs))
  
  #get cont_sites without geometry
  cs_dws = cs %>% 
    as_tibble() %>% 
    dplyr::select(!geometry)
  
  #turn watershed points spatial
  cs_dws_sf = st_as_sf(ws, coords = c("lng", "lat"), crs = 4326)
  #subset to down points
  dws_points = cs_dws_sf[cs_dws_sf$d_ws == TRUE, ]
  
  # Compute the convex hull of the true points as the down watershed
  ws_hull = st_convex_hull(st_union(dws_points %>% st_transform(32110)))
  
  cs_dws$geometry = ws_hull
  
  cs_dws = cs_dws %>% 
    st_as_sf(sf_column_name = "geometry", crs = 32110)
  
  return(cs_dws)
}

cs_dws = pblapply(1:nrow(cont_sites), watershed, 10000)
cs_dws = dplyr::bind_rows(cs_dws)


save(cs_dws, file = modify_path("Data_Verify/RData/cont_sites_downstream10000.RData"))

# load("New Hampshire/Data/RData/cont_sites_downstream10000_1_20.RData")
# cs_dws1 = dplyr::bind_rows(cs_dws)
# load("New Hampshire/Data/RData/cont_sites_downstream10000_21_41.RData")
# cs_dws = rbind(cs_dws1, cs_dws)

nh_map_data = map_data("state", region = "new hampshire")

# Create the NH map plot
nh_map_plot = ggplot() +
  geom_polygon(data = nh_map_data, aes(x = long, y = lat, group = group), 
               color = 'black', fill = "transparent") +
  coord_fixed(1.3) + # Adjust aspect ratio if needed
  theme_minimal()

# Add site points and arrows to the map
fig_s1 = nh_map_plot +
  geom_point(data = cont_sites, aes(x = lng, y = lat)) +
  geom_sf(data = cs_dws %>% st_transform(4326), alpha = 0.2, fill = "blue", color = NA) + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        text=element_text(family="Helvetica"))

ggsave(fig_s1, file = modify_path3("Figures/figure_s1.png"))


unlink(modify_path("Data_Verify/GIS/cs_down"))