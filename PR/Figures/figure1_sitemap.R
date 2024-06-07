cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
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


us_shape = tigris::states() %>% 
  dplyr::filter(!GEOID %in% c("15", "78", "69", "66", "02", "60", "72"))

mainbox = st_bbox(us_shape[us_shape$STUSPS == "NH", ])
nh_bb = st_as_sfc(mainbox)

inset = ggplot() + 
  geom_sf(data = us_shape, fill = "white") + 
  geom_sf(data = nh_bb, fill = NA, color = "grey", size = 1.5) +
  theme_void()


main = ggplot() + 
  geom_polygon(data = nh_map_data, aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  geom_sf(data = cont_sites_buff, aes(fill = log(sum_pfoa_pfos)), alpha = 0.7, color = "transparent") +
  scale_fill_gradientn(colors = c("yellow3", "firebrick4"),
                       guide = guide_colorbar(barwidth = 20, barheight = 1,
                                              title = "Log PFOA + PFOS (ppt)",
                                              title.position = "top",
                                              title.hjust = 0.5,
                                              label.hjust = .5)) +
  theme_void() + 
  theme(legend.position = "bottom") + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(family = "Helvetica"), 
        legend.position = "bottom", 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 26), 
        #legend.box.spacing = unit(-100, "pt"), 
        plot.margin = unit(c(0, -2, 0, -2), "cm")) + 
  geom_sf(data = nh_bb, fill = NA, color = "grey", size = 1.2) 


d = cowplot::ggdraw() +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(inset), xmin = 0, xmax = 7, ymin = 10, ymax = 20) +
  annotation_custom(ggplotGrob(main), xmin = 0, xmax = 20, ymin = 0, ymax = 15) +
  geom_segment(aes(x = 6.2, xend = 12.9, y = 15.8, yend = 14.45), color = "grey", size = 1.2) +
  geom_segment(aes(x = 6, xend = 7.15, y = 15.8, yend = 14.45), color = "grey", size = 1.2)

ggsave(modify_path3("Figures/Figure1/site_map.png"), d)
rm(d)
