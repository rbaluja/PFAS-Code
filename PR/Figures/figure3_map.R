state_abb = c("MI", "MN", "NH", "NY", "CO", "ME", "VT", "CA", "FL", "ND", "WI")
states_keep = c("Michigan", 
                "Minnesota", 
                "New Hampshire", 
                "New York", 
                "Colorado", 
                "Maine", 
                "Vermont", 
                "California", 
                "Florida", 
                "North Dakota", 
                "Wisconsin")

cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(`Matrix Type` == 'Groundwater' & State != "Alaska") %>% 
  dplyr::select(`Site name`, State, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, state = State, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)


bs_c = bs %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarise(add_vlbw = sum(add_vlbw), 
                   add_mlbw = sum(add_mlbw), 
                   add_still = sum(add_still))

bs_c$cost = (bs_c$add_vlbw * 5133739.83 + bs_c$add_mlbw * 1634411.22 + bs_c$add_still * 6925374.8993)/10^6


cs = tigris::states() %>% 
  dplyr::filter(STATEFP %in% c("26", "27", "33", "36", "08", "23", "50", "06", "12", "38", "55")) %>% 
  left_join(bs_c %>% as_tibble() %>% dplyr::select(!geometry), by = c("GEOID" = "state"))

states = tigris::states() %>% 
  dplyr::filter(STUSPS %in% state_abb)


states[states$STUSPS == "CO", ]$geometry = states[states$STUSPS == "CO", ]$geometry + matrix(data = c(5, 0), ncol = 2)
states[states$STUSPS == "CA", ]$geometry = states[states$STUSPS == "CA", ]$geometry + matrix(data = c(9, 0), ncol = 2)
states[states$STUSPS == "FL", ]$geometry = states[states$STUSPS == "FL", ]$geometry + matrix(data = c(1, 4), ncol = 2)

cs[cs$STATEFP == "08", ]$geometry = cs[cs$STATEFP == "08", ]$geometry + matrix(data = c(5, 0), ncol = 2)
cs[cs$STATEFP == "06", ]$geometry = cs[cs$STATEFP == "06", ]$geometry + matrix(data = c(9, 0), ncol = 2)
cs[cs$STATEFP == "12", ]$geometry = cs[cs$STATEFP == "12", ]$geometry + matrix(data = c(1, 4), ncol = 2)

cont_sites[cont_sites$state == "Colorado", ]$lng = cont_sites[cont_sites$state == "Colorado", ]$lng + 5
cont_sites[cont_sites$state == "California", ]$lng = cont_sites[cont_sites$state == "California", ]$lng + 9
cont_sites[cont_sites$state == "Florida", ]$lat = cont_sites[cont_sites$state == "Florida", ]$lat + 4
cont_sites[cont_sites$state == "Florida", ]$lng = cont_sites[cont_sites$state == "Florida", ]$lng + 1

cont_sites = cont_sites %>% 
  as_tibble() %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(3395) %>% 
  dplyr::mutate(lng = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])
states = states %>% st_transform(3395)
cs = cs %>% st_transform(3395)

#translate a few states through space
#get x/y coordinates for label on each shape
states$x_lab = 0
states$y_lab = 0

#Above CA
states[states$STUSPS == "CA", ]$x_lab = 0.8 * st_bbox(states[states$STUSPS == "CA", ]$geometry)$xmin + 0.2 * st_bbox(states[states$STUSPS == "CA", ]$geometry)$xmax
states[states$STUSPS == "CA", ]$y_lab = st_bbox(states[states$STUSPS == "CA", ]$geometry)$ymax + 50000
#Above CO
states[states$STUSPS == "CO", ]$x_lab = 0.7 * st_bbox(states[states$STUSPS == "CO", ]$geometry)$xmin + 0.3 * st_bbox(states[states$STUSPS == "CO", ]$geometry)$xmax
states[states$STUSPS == "CO", ]$y_lab = st_bbox(states[states$STUSPS == "CO", ]$geometry)$ymax + 50000
#Above ND
states[states$STUSPS == "ND", ]$x_lab = 0.75 * st_bbox(states[states$STUSPS == "ND", ]$geometry)$xmin + 0.25 * st_bbox(states[states$STUSPS == "ND", ]$geometry)$xmax
states[states$STUSPS == "ND", ]$y_lab = st_bbox(states[states$STUSPS == "ND", ]$geometry)$ymax +50000
#Above MN
states[states$STUSPS == "MN", ]$x_lab = 0.35 * st_bbox(states[states$STUSPS == "MN", ]$geometry)$xmin + 0.65 * st_bbox(states[states$STUSPS == "MN", ]$geometry)$xmax
states[states$STUSPS == "MN", ]$y_lab = st_bbox(states[states$STUSPS == "MN", ]$geometry)$ymax - 50000
#Inside WI
states[states$STUSPS == "WI", ]$x_lab = 0.45 * st_bbox(states[states$STUSPS == "WI", ]$geometry)$xmin + 0.55 * st_bbox(states[states$STUSPS == "WI", ]$geometry)$xmax
states[states$STUSPS == "WI", ]$y_lab = st_bbox(states[states$STUSPS == "WI", ]$geometry)$ymin - 100000
#Inside MI
states[states$STUSPS == "MI", ]$x_lab = 0.5 * st_bbox(states[states$STUSPS == "MI", ]$geometry)$xmin + 0.5 * st_bbox(states[states$STUSPS == "MI", ]$geometry)$xmax
states[states$STUSPS == "MI", ]$y_lab = st_bbox(states[states$STUSPS == "MI", ]$geometry)$ymax + 75000
#Inside NY
states[states$STUSPS == "NY", ]$x_lab = 0.7 * st_bbox(states[states$STUSPS == "NY", ]$geometry)$xmin + 0.3 * st_bbox(states[states$STUSPS == "NY", ]$geometry)$xmax
states[states$STUSPS == "NY", ]$y_lab = st_bbox(states[states$STUSPS == "NY", ]$geometry)$ymin + 150000
#Above VT
states[states$STUSPS == "VT", ]$x_lab = 0.7 * st_bbox(states[states$STUSPS == "VT", ]$geometry)$xmin + 0.3 * st_bbox(states[states$STUSPS == "VT", ]$geometry)$xmax
states[states$STUSPS == "VT", ]$y_lab = st_bbox(states[states$STUSPS == "VT", ]$geometry)$ymax + 50000
#Below NH
states[states$STUSPS == "NH", ]$x_lab = 0.5 * st_bbox(states[states$STUSPS == "NH", ]$geometry)$xmin + 0.5 * st_bbox(states[states$STUSPS == "NH", ]$geometry)$xmax
states[states$STUSPS == "NH", ]$y_lab = st_bbox(states[states$STUSPS == "NH", ]$geometry)$ymin - 50000
#Inside ME
states[states$STUSPS == "ME", ]$x_lab = 0.5 * st_bbox(states[states$STUSPS == "ME", ]$geometry)$xmin + 0.5 * st_bbox(states[states$STUSPS == "ME", ]$geometry)$xmax
states[states$STUSPS == "ME", ]$y_lab = st_bbox(states[states$STUSPS == "ME", ]$geometry)$ymax + 75000
#Above FL
states[states$STUSPS == "FL", ]$x_lab = 0.7 * st_bbox(states[states$STUSPS == "FL", ]$geometry)$xmin + 0.3 * st_bbox(states[states$STUSPS == "FL", ]$geometry)$xmax
states[states$STUSPS == "FL", ]$y_lab = st_bbox(states[states$STUSPS == "FL", ]$geometry)$ymax+ 50000


s_lab = states %>% as_tibble() %>% 
  dplyr::select(!geometry) %>% 
  left_join(cs %>% as_tibble() %>% dplyr::select(!geometry)) %>%
  st_as_sf(coords = c("x_lab", "y_lab"), crs = 3395)
s_lab$l = paste0(s_lab$STUSPS, ":", round(s_lab$cost, 0))



cost_map = ggplot() +
  geom_sf(data = cs, aes(fill = cost), color = NA, alpha = 0.8, lwd = 0) +
  geom_sf(data = states, color = "black", fill = "transparent", lwd = 1) +
  scale_fill_gradient(low = "white", high = "firebrick4",
                      limits =c(0, 3000),
                      breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000),
                      labels = c("0", "$0.5B", "$1B", "$1.5B", "$2B", "$2.5B", "$3B"),
                      name = "Annual Low-Birthweight + Stillbirth Costs",
                      guide = guide_colorbar(barwidth = 100, barheight = 1,
                                             title.position = "top",
                                             title.hjust = 0.5,
                                             label.hjust = .5,
                                             label.position = "bottom")) +
  geom_point(data = cont_sites %>% filter(state %in% states_keep), 
             aes(x = lng, y = lat, color = "Confirmed Site"), 
             alpha = 0.4, size = 3) +
  scale_color_manual(values = "black",
                     name = "Site with Confirmed Groundwater Contamination above 1000 ppt") +
  guides(color = guide_legend(override.aes = list(size = 10),
                              title.position = "left")) + 
  theme_void() +
  theme(legend.title = element_text(size = 60),
        legend.text = element_text(size = 58),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.margin = margin(2, 2, 2, 2, "cm"),
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm")) +
  geom_sf_text(data = s_lab, aes(label = l), size = 14)
ggsave(modify_path3("Figures/Figure3/costs_map.png"), cost_map,  width = 9166, height = 5875, units = "px", device = "png", limitsize = FALSE)
