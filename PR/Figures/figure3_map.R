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

bs_c$cost = (bs_c$add_vlbw * 5133739.83 + bs_c$add_mlbw * 1634411.22)/10^9

cmap = function(i, state_abb, states_keep, bs_c){
  cs_counties = tigris::counties(state = state_abb[i])
  
  
  cs_counties = cs_counties %>% left_join(bs_c %>% as_tibble() %>% dplyr::select(!geometry), by = c("GEOID" = "county"))
  cs_counties[is.na(cs_counties$cost), ]$cost = 0
  
  
  smap = ggplot() +
    geom_sf(data = states[states$NAME == states_keep[i], ], color = "black") + 
    geom_sf(data = cs_counties, aes(fill = cost), color = NA, alpha = 0.6) +
    scale_fill_gradient(low = "white", high = "red", limits = c(0, 0.4), 
                        guide = guide_colorbar(barwidth = 60, barheight = 1,
                                               title = "Low-Birthweight Costs ($B)",
                                               title.position = "top",
                                               title.hjust = 0.5,
                                               label.hjust = .5,
                                               label.position = "bottom")) +
    geom_point(data = cont_sites %>% dplyr::filter(state == states_keep[i]), aes(x = lng, y = lat), alpha = 0.6, size = 8) + theme_void() + 
    theme(legend.title = element_text(size = 54), 
          legend.text = element_text(size = 60), 
          legend.position = "bottom", 
          legend.key.height = unit(3, "cm"),
          legend.key.width = unit(3, "cm"))
  
  return(smap)
}

state_maps = list()
for (i in 1:length(state_abb)) {
  state_maps[[state_abb[i]]] = cmap(i, state_abb, states_keep)
}

layout = c(
  area(t = 0, b = 10, l = 16, r = 30), 
  area(t = 11, b = 20, l = 5, r = 30), 
  area(t = 21, b = 30, l = 0, r = 33), 
  area(t = 31, b = 45, l = 0, r = 30)
)

figure3_map = ((state_maps[["ME"]])/
  ( state_maps[["ND"]] + state_maps[["MN"]] + state_maps[["WI"]] + state_maps[["MI"]]  + plot_layout(nrow = 1))/
  (state_maps[["CO"]] + state_maps[["VT"]] + state_maps[["NH"]] + state_maps[["NY"]]+ plot_layout(nrow = 1))/
  (state_maps[["CA"]] +plot_spacer() + state_maps[["FL"]] + plot_layout(nrow = 1))) + plot_layout(design = layout, guides = "collect")

figure3_map = figure3_map + plot_annotation(theme = theme(legend.position = "bottom"))

ggsave(modify_path3("Figures/Figure3/costs_map.png"), figure3_map, scale = 3)
 
