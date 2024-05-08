nh_map_data = map_data("state", region = "new hampshire")
nh_map_plot = ggplot() +
  geom_polygon(data = nh_map_data, aes(x = long, y = lat, group = group), 
               color = 'black', fill = "lightgrey") +
  coord_fixed(1.3) + 
  theme_minimal()

dem = terra::rast(modify_path("Data_Verify/GIS/dem_smoothed.tiff")) 
dem =  terra::crop(dem, extent(nh_map_data %>% summarise(x = c(min(long), max(long)), y = c(min(lat), max(lat)))))

dem = dem %>%
  as.data.frame(xy=TRUE)

colnames(dem) = c("x", "y", "elev")

plot_data = data.frame(x = c(-71.35, -71.25, -71.4, -71.32), y = c(44.25, 44.3, 44.26, 44.254), label = c("C", "\\omega_1", "\\omega_2", "\\omega_3"))
#read in calculated watersheds for plot_data points
if (!file.exists(modify_path("Data_Verify/GIS/f1_watershed.RData"))){
  stop("Data not found. Please run PR/GIS/figure1_watershed.R first.")
}
load(modify_path("Data_Verify/GIS/f1_watershed.RData"))
f1_ws = f1_ws %>% 
  st_transform(4326) 
f1_ws$site = c("C", "omega1", "omega2", "omega3")

f1_ws = f1_ws %>% st_as_sf()

f1 = nh_map_plot + 
  geom_raster(data = dem, aes(x = x, y = y, fill = elev)) +
  scale_fill_viridis_c(guide = guide_colorbar(barwidth = 63, barheight = 1,
                                              title = "",
                                              title.hjust = 0.5,
                                              label.hjust = .5,
                                              label.position = "bottom"),
                       breaks = c(0.0, 500, 1000, 1500),
                       labels = c("0", "500m", "1000m", "1500m")) +
  theme(legend.position = "bottom") +
  ylim(44.18, 44.38) + xlim(-71.5, -71.15) +
  annotate("point", x = -71.37, y = 44.27, color = "black", size = 12) +
  annotate("text", x = -71.35, y = 44.28, label = "C", vjust = 1.3, color = "black", size = 10, family = "arial") +
  annotate("point", x = -71.265, y = 44.25, color = "grey77", size = 8) +
  annotate("text", x = -71.232, y = 44.3, label = "ω[3]", parse = TRUE, hjust = -.25, vjust = -0.45, color = "grey77", size = 10, family = "arial") +
  annotate("point", x = -71.415, y = 44.247, color = "white", size = 8) +
  annotate("text", x = -71.4, y = 44.26, label = "ω[1]", parse = TRUE, vjust = -.5, hjust = 1, color = "white", size = 10)  + 
  annotate("point", x = -71.335, y = 44.27, color = "firebrick", size = 8) +
  annotate("text", x = -71.33, y = 44.26, label = "ω[2]", parse = TRUE, vjust = -0.3, hjust = -.3, color = "firebrick4", size = 10) + 
  # geom_segment(aes(x = -71.3, y = 44.27, xend = -71.26, yend = 44.25),
  #              arrow = grid::arrow(type = "closed", length = unit(0.2, "inches")),
  #              color = "white", size = 1, lineend = "round") +
  # geom_segment(aes(x = -71.3, y = 44.27, xend = -71.36, yend = 44.26),
  #              arrow = grid::arrow(type = "closed", length = unit(0.2, "inches")),
  #              color = "white", size = 1, lineend = "round") +
  theme_void() +
  theme(legend.position = "bottom",  
        legend.key = element_blank(),  
        legend.title = element_blank(),  
        legend.text = element_text(size = 20),
        axis.text = element_blank(),
        axis.title = element_blank())

f1 + geom_sf(data = f1_ws, aes(color = as.factor(site)), 
                  alpha = 0, fill = "transparent", linewidth = ifelse(f1_ws$site == "C", 7, 2)) +
  scale_color_manual(values = c("C" = "black", "omega1" = "grey77", "omega2" = "white", "omega3" = "firebrick"), guide = FALSE)



ggsave(modify_path3("Figures/Figure1/figure1_dem.png"), f1, height = 4800, width = 4800, units = "px")


