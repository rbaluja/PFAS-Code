wells1 = wells %>% 
  as_tibble() %>% 
  dplyr::select(!geometry) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

w_dist = st_distance(wells1 %>% st_transform(5070), cont_sites %>% st_transform(5070))

wells1$dist_near = apply(w_dist, 1, min)

figure_s2a = ggplot(wells1, aes(x = dist_near / 1000)) +
  geom_density(aes(fill = "blue"), alpha = 0.4, color = NA) +  # Set fill and remove border
  scale_fill_identity() +  # Use the literal color name
  theme_minimal() +
  xlab("Distance from Nearest Contamination Site (km)") +
  ylab("Density") +
  geom_vline(xintercept = 5, size = 2) + 
  theme(axis.text = element_text(size = 50, face = "bold"), 
        axis.title = element_text(size = 50, face = "bold")) + 
  scale_x_continuous(breaks = c(5, 20, 40, 60))

ggsave(modify_path3("Figures/figure_s2a.png"), figure_s2a, scale = 3)
