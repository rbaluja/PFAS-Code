if (!file.exists(modify_path("Data_Verify/GIS/f1_watershed.RData"))){
  stop("watersheds for this figure are created in preliminaries.R")
}
load(modify_path("Data_Verify/GIS/f1_watershed.RData"))
f1_ws = f1_ws %>% 
  st_transform(4326) 
f1_ws$site = c("C", "omega1", "omega2", "omega3")

f1_ws = f1_ws %>% st_as_sf()

states = tigris::states() %>% 
  dplyr::filter(STUSPS == "NH")

states = states %>% 
  st_crop(xmin = -71.42, xmax = -71.28, ymin = 44.23, ymax = 44.3)

pd = data.frame(x = c(-71.37, -71.385, -71.385, -71.34), y = c(44.268, 44.251, 44.2635, 44.27)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)
pd$data_column_fill = c("C", "omega3", "omega1", "omega2")

e = get_elev_raster(states, z = 14)
elev_mat = terra::extract(rast(e), pd)

elmat = raster_to_matrix(e)

#3d figure
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_overlay(generate_point_overlay(pd %>% st_transform(st_crs(e)), extent = e, heightmap = elmat, size = 80, 
                                     color =c("C" = "black", "omega1" = "dodgerblue4", "omega2" = "white", "omega3" = "firebrick"), pch = 19)) %>%
  add_overlay(generate_polygon_overlay(f1_ws%>% st_transform(st_crs(e)), extent = e, heightmap = elmat, linewidth = 10, 
                                       palette = "transparent", linecolor = c("C" = "black", "omega1" = "dodgerblue4", "omega2" = "white", "omega3" = "firebrick"))) %>%
  plot_3d(elmat, zscale = 3, theta = -90, phi = 45, zoom = 0.4, fov = 60, baseshape = "circle", solid = FALSE)
render_snapshot(filename = modify_path3("Figures/Figure1/spatmap_output.png"))
