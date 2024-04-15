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

#get raster of predicted levels
source("PFAS-Code/PR/Figures/Grid Contamination/cont_raster.R")

# Create the NH map plot
nh_map_plot = ggplot() +
  geom_polygon(data = nh_map_data, aes(x = long, y = lat, group = group), 
               color = 'black', fill = "lightgrey") +
  coord_fixed(1.3) + 
  theme_minimal()

#load in cont buffer raster
cont_rdf = as.data.frame(z_raster, xy=TRUE)
names(cont_rdf) <- c("x", "y", "pfas")
cont_rdf$pfas = sinh(cont_rdf$pfas)

cont_rdf2 = cont_rdf
cont_rdf2$high = 0
cont_rdf2[cont_rdf2$pfas > 0.15 * 1000,  ]$high = 1
cont_rdf2[cont_rdf2$pfas > 0.15* 1000,  ]$pfas = 0.15* 1000


figure1_sites = nh_map_plot +
  geom_tile(data = cont_rdf2, aes(x = x, y = y, fill = pfas)) + 
  scale_fill_gradientn(colors = c("transparent", "yellow", "red"),
                       values = scales::rescale(c(0, 0.01, 1)),
                       guide = guide_colorbar(barwidth = 50, barheight = 1,
                                               title = "Predicted PFAS (ppt)",
                                               title.position = "top",
                                               title.hjust = 0.5,
                                               label.hjust = .5,
                                               label.position = "bottom"),
                       breaks = c(0.0, 50, 100, 150),
                       labels = c("0", "50", "100", expression("\u2265 150"))) +
  theme_void() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        text = element_text(family = "Helvetica"), 
        legend.position = "bottom", 
        legend.text = element_text(size = 44), 
        legend.title = element_text(size = 50), 
        legend.box.spacing = unit(-150, "pt"), 
        plot.margin = unit(c(0, -2, 0, -2), "cm"))  + 
  labs(fill = "Predicted PFAS") 
ggsave(modify_path3("Figures/Figure1/figure1_sites.png"), figure1_sites,  limitsize = F, scale = 2)
