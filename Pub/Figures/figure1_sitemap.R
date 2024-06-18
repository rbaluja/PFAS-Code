cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(`Matrix Type` == 'Groundwater') %>% 
  dplyr::select(`Site name`, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`, 
                state = State) %>% 
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

c_max = cont_sites %>% 
  dplyr::group_by(state) %>% 
  dplyr::filter(sum_pfoa_pfos == max(sum_pfoa_pfos, na.rm = T))

cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]

cont_sites$index = 1:nrow(cont_sites)

cont_sites_buff = cont_sites %>% 
  st_transform(32110) %>% #NH meters projected CRS
  st_buffer(5000) %>% 
  st_transform(4326)

nh_map_data = map_data("state", region = "new hampshire")


us_shape = tigris::states() %>% 
  dplyr::filter(!GEOID %in% c("15", "78", "69", "66", "02", "60", "72"))

mainbox = st_bbox(us_shape[us_shape$STUSPS == "NH", ])
nh_bb = st_as_sfc(mainbox) %>% st_transform(4326) %>% st_buffer(0.025)

common_scale = scale_fill_gradientn(
  colors = c("yellow3", "firebrick4"),
  limits = c(6, 17),
  breaks = seq(from = 6, to = 17, by = 2),
  guide = guide_colorbar(
    barwidth = 30, barheight = 1,
    title = "Log PFOA + PFOS (ppt)",
    title.position = "top",
    title.hjust = 0.5,
    label.hjust = 0.5
  )
)

inset = ggplot() + 
  geom_sf(data = us_shape %>% 
            left_join(c_max %>% as_tibble() %>% dplyr::select(!geometry), by = c("NAME" = "state")), aes(fill = log(sum_pfoa_pfos))) + 
  common_scale  +   
  geom_sf(data = nh_bb, fill = NA, color = "black", size = 22) +
  theme_void() + 
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 26))


main = ggplot() + 
  geom_polygon(data = nh_map_data, aes(x = long, y = lat, group = group), fill = 'grey95', color = 'black') +
  geom_sf(data = cont_sites_buff[cont_sites_buff$state == "New Hampshire", ], aes(fill = log(sum_pfoa_pfos)), alpha = 0.7, color = "transparent") +
  common_scale + 
  theme_void() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(family = "Helvetica"), 
        legend.position = "none", 
        legend.text = element_text(size = 20), 
        legend.title = element_text(size = 26), 
        #legend.box.spacing = unit(-100, "pt"), 
        plot.margin = unit(c(0, -2, 0, -2), "cm")) + 
  geom_sf(data = nh_bb, fill = NA, color = "black", size = 10) 


l =  ggpubr::get_legend(inset)
inset = inset + theme(legend.position = "none")

d = cowplot::ggdraw() +
  coord_equal(xlim = c(0, 20), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(inset), xmin = 0, xmax = 7, ymin = 10, ymax = 20) +
  annotation_custom(ggplotGrob(main), xmin = 0, xmax = 20, ymin = 0, ymax = 15) +
  geom_segment(aes(x = 6.2, xend = 13.7, y = 16.2, yend = 14.29), color = "black", size = 0.2) +
  geom_segment(aes(x = 6.05, xend = 6.3, y = 16.2, yend = 14.29), color = "black", size = 0.2) + 
  geom_segment(aes(x = 6.05, xend = 6.26, y = 15.8, yend = 0.65), color = "black", size = 0.2)

site_map = cowplot::plot_grid(d, l, ncol = 1, rel_heights = c(1, 0.1))

ggsave(modify_path3("Figures/Figure1/site_map.svg"), site_map)

