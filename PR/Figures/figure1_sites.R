cont_sites = read_xlsx(modify_path('Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(`Site name`, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

cont_sites$index = 1:nrow(cont_sites)

cont_sites_bufff = cont_sites %>% 
  st_transform(32110) %>% #NH meters projected CRS
  st_buffer(5000) %>% 
  st_transform(4326)

nh_map_data = map_data("state", region = "new hampshire")

# Create the NH map plot
nh_map_plot = ggplot() +
  geom_polygon(data = nh_map_data, aes(x = long, y = lat, group = group), 
               color = 'black', fill = "transparent") +
  coord_fixed(1.3) + 
  theme_minimal()

figure1_sites = nh_map_plot +
  geom_point(data = cont_sites, aes(x = lng, y = lat), size = 0.5) +
  geom_sf(data = cont_sites_buff, aes(fill = log(sum_pfoa_pfos)), alpha = 0.5, color = NA) + 
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_void() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(family = "Helvetica")) + 
  guides(fill = "none")

ggsave(modify_path3("Figures/Figure1/figure1_sites.png"), figure1_sites, scale = 2)
