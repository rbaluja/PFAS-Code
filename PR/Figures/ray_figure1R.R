load(modify_path("Data_Verify/GIS/f1_watershed.RData"))
f1_ws = f1_ws %>% 
  st_transform(4326) 
f1_ws$site = c("C", "omega1", "omega2", "omega3")

f1_ws = f1_ws %>% st_as_sf()

states = tigris::states() %>% 
  dplyr::filter(STUSPS == "NH")

states = states %>% 
  st_crop(xmin = -71.5, xmax = -71.15, ymin = 44.18, ymax = 44.38)

pd = data.frame(x = c(-71.37, -71.313, -71.42, -71.34), y = c(44.268, 44.236, 44.263, 44.27)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
pd$data_column_fill = c("C", "omega3", "omega1", "omega2")

e = get_elev_raster(states, z = 13)
elev_mat = terra::extract(rast(e), pd)

elmat = raster_to_matrix(e)
#2d figure
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_overlay(generate_point_overlay(pd %>% st_transform(st_crs(e)), extent = e, heightmap = elmat, size = 80, 
                                     color =c("C" = "black", "omega1" = "dodgerblue4", "omega2" = "white", "omega3" = "firebrick"), pch = 19)) %>%
  add_overlay(generate_polygon_overlay(f1_ws%>% st_transform(st_crs(e)), extent = e, heightmap = elmat, linewidth = 10, 
                                       palette = "transparent", linecolor = c("C" = "black", "omega1" = "dodgerblue4", "omega2" = "white", "omega3" = "firebrick"))) %>%
  plot_map()

#3d figure
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_overlay(generate_point_overlay(pd %>% st_transform(st_crs(e)), extent = e, heightmap = elmat, size = 80, 
                                     color =c("C" = "black", "omega1" = "dodgerblue4", "omega2" = "white", "omega3" = "firebrick"), pch = 19)) %>%
  add_overlay(generate_polygon_overlay(f1_ws%>% st_transform(st_crs(e)), extent = e, heightmap = elmat, linewidth = 10, 
                                       palette = "transparent", linecolor = c("C" = "black", "omega1" = "dodgerblue4", "omega2" = "white", "omega3" = "firebrick"))) %>%
  plot_3d(elmat, zscale = 3, theta = -75, phi = 45, zoom = 0.8)
render_snapshot()
