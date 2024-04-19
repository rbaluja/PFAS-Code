nh_map_data = map_data("state", region = "new hampshire")
nh_map_plot = ggplot() +
  geom_polygon(data = nh_map_data, aes(x = long, y = lat, group = group), 
               color = 'black', fill = "lightgrey") +
  coord_fixed(1.3) + 
  theme_minimal()

dem = terra::rast(modify_path("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff")) 
dem =  terra::crop(dem, extent(nh_map_data %>% summarise(x = c(min(long), max(long)), y = c(min(lat), max(lat)))))

dem = dem %>%
  as.data.frame(xy=TRUE)

colnames(dem) = c("x", "y", "elev")

plot_data = data.frame(x = c(-71.35, -71.25, -71.4), y = c(44.28, 44.3, 44.26), label = c("C", "\\omega_1", "\\omega_2"))

nh_map_plot + 
  geom_raster(data = dem, aes(x = x, y = y, fill = elev)) +
  scale_fill_viridis_c(guide = guide_colorbar(barwidth = 25, barheight = 1,
                                              title = "",
                                              title.hjust = 0.5,
                                              label.hjust = .5,
                                              label.position = "bottom"),
                       breaks = c(0.0, 500, 1000, 1500),
                       labels = c("0", "500m", "1000m", "1500m")) +
  theme(legend.position = "bottom") +
  ylim(44.1, 44.4) + xlim(-71.5, -71.1) +
  annotate("point", x = -71.35, y = 44.28, color = "white", size = 4) +
  annotate("text", x = -71.35, y = 44.28, label = "C", vjust = -0.5, color = "white", size = 10, family = "arial") +
  annotate("point", x = -71.25, y = 44.3, color = "white", size = 4) +
  annotate("text", x = -71.25, y = 44.3, label = "ω[1]", parse = TRUE, vjust = 1.5, color = "white", size = 10, family = "arial") +
  annotate("point", x = -71.4, y = 44.26, color = "white", size = 4) +
  annotate("text", x = -71.4, y = 44.26, label = "ω[2]", parse = TRUE, vjust = 1.5, color = "white", size = 10) + 
  geom_segment(aes(x = -71.3, y = 44.27, xend = -71.26, yend = 44.25),
               arrow = grid::arrow(type = "closed", length = unit(0.2, "inches")),
               color = "white", size = 1, lineend = "round") + 
  geom_segment(aes(x = -71.3, y = 44.27, xend = -71.36, yend = 44.26),
               arrow = grid::arrow(type = "closed", length = unit(0.2, "inches")),
               color = "white", size = 1, lineend = "round") + 
  theme_void() +
  theme(legend.position = "bottom",  # Add legend below the plot
        legend.key = element_blank(),  # Optional: remove the legend key background
        legend.title = element_blank(),  # Optional: style the legend title
        legend.text = element_text(size = 20), 
        axis.text = element_blank(), 
        axis.title = element_blank())

ggsave(modify_path3("Figures/Figure1/figure1_dem.png"))
