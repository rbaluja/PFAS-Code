wells_f = wells %>% 
  as_tibble() %>% 
  dplyr::select(!geometry) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

w_dist = st_distance(wells_f %>% st_transform(5070), cont_sites %>% st_transform(5070))

wells_f$dist_near = apply(w_dist, 1, min)

#prop of wells within 10km of a contamination site
mean(wells_f$dist_near <= 10000)
#prop of wells within 5km of a contamination site
mean(wells_f$dist_near <= 5000)
#median number of sites within 5km, among wells within 5km
nw = apply(w_dist, 1, function(x) sum(x <= 5000))
median(nw[nw >= 1])
rm(nw)

figure_s2a = ggplot(wells_f, aes(x = dist_near / 1000)) +
  geom_density(aes(fill = "blue"), alpha = 0.4, color = NA) +  # Set fill and remove border
  scale_fill_identity() +  # Use the literal color name
  theme_minimal() +
  xlab("Distance from Nearest Contamination Site (km)") +
  ylab("Density") +
  geom_vline(xintercept = 5, size = 2) + 
  theme(axis.text = element_text(size = 35), 
        axis.title = element_text(size = 35)) + 
  scale_x_continuous(breaks = c(5, 20, 40, 60))

ggsave(modify_path3("Figures/figure_s2a.png"), figure_s2a, scale = 1)
