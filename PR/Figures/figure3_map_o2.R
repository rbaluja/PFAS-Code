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

states = tigris::states()

bs_c = bs %>% 
  dplyr::group_by(county) %>% 
  dplyr::summarise(add_vlbw = sum(add_vlbw), 
                   add_mlbw = sum(add_mlbw))

bs_c$cost = (bs_c$add_vlbw * 5133739.83 + bs_c$add_mlbw * 1634411.22)/10^6

bs_c[which(bs_c$cost > 150), ]$cost = 150

cs = tigris::counties() %>% 
  dplyr::filter(STATEFP %in% c("26", "27", "33", "36", "08", "23", "50", "06", "12", "38", "55")) %>% 
  left_join(bs_c %>% as_tibble() %>% dplyr::select(!geometry), by = c("GEOID" = "county"))

states = tigris::states() %>% 
  dplyr::filter(STUSPS %in% state_abb)

cs[is.na(cs$cost), ]$cost = 0

ggplot() +
  geom_sf(data = states, color = "black", fill = "transparent", size = 10) + 
  geom_sf(data = cs, aes(fill = cost), color = NA, alpha = 0.8) +
  scale_fill_gradient(low = "white", high = "firebrick4", limits = c(0, 150), 
                      breaks = c(0, 50, 100, 150),
                      labels = c("$0", "$50M", "$100M", expression("> $150M")),
                      guide = guide_colorbar(barwidth = 60, barheight = 1,
                                             title = "Annual Low-Birthweight Costs",
                                             title.position = "top",
                                             title.hjust = 0.5,
                                             label.hjust = .5,
                                             label.position = "bottom")) +
  geom_point(data = cont_sites %>% dplyr::filter(state %in% states_keep), aes(x = lng, y = lat), alpha = 0.4, size = 2) + theme_void() + 
  theme(legend.title = element_text(size = 26), 
        legend.text = element_text(size = 26), 
        legend.position = "bottom", 
        legend.key.height = unit(2, "cm"),
        legend.key.width = unit(2, "cm")) 

ggsave(modify_path3("Figures/Figure3/costs_map2.png"),  scale= 4, device = "png", limitsize = FALSE)
