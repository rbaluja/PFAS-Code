library(ggplot2)
library(ggpubr)

nh_map = map_data("state", region = "new hampshire")
#outline of map
nh_map = ggplot(nh_map, aes(x=long, y=lat)) + 
  geom_polygon(color = 'white', fill = "transparent") + 
  coord_map()

#plot huc10 & contamination sites
huc_shape = st_read('Data/Groundwater/WBD_01_HU2_Shape/Shape/WBDHU10.shp') %>% 
  st_transform(4326)

cont_sites = read_xlsx('Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
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
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)


huc_map1 = nh_map + geom_sf(huc_shape, mapping = aes(), inherit.aes = F) + 
  geom_point(data = cont_sites, aes(x = lng, y = lat)) + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        text=element_text(family="Helvetica"))

huc_map2 = nh_map + geom_sf(huc_shape, mapping = aes(), inherit.aes = F) + 
  geom_point(data = cont_sites, aes(x = lng, y = lat)) + 
  ylim(c(42.6, 44.75)) + xlim(c(-73, -70.5)) + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        text=element_text(family="Helvetica"))

ggarrange(huc_map1, huc_map2, widths = c(1, 2), heights = c(1, 2))

