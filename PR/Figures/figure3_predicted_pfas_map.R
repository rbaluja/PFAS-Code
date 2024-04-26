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
  dplyr::summarise(pred_pfas = max(pred_pfas, na.rm = T))

bs_c$pred_pfas = sinh(bs_c$pred_pfas)

bs_c[bs_c$pred_pfas > 1000, "pred_pfas"] = 1000


cmap = function(i, state_abb, states_keep, bs_c){
  cs_counties = tigris::counties(state = state_abb[i])
  
  
  cs_counties = cs_counties %>% left_join(bs_c %>% as_tibble() %>% dplyr::select(!geometry), by = c("GEOID" = "county"))
  cs_counties[is.na(cs_counties$pred_pfas), ]$pred_pfas = 0
  
  
  smap = ggplot() +
    geom_sf(data = states[states$NAME == states_keep[i], ], color = "black", fill = "transparent") + 
    geom_sf(data = cs_counties, aes(fill = pred_pfas), color = NA, alpha = 0.8) +
    scale_fill_gradient(low = "white", high = "firebrick4", limits = c(0, 1000), 
                        breaks = c(0, 250, 500, 750, 1000),
                        labels = c("0", "250", "500", "750", expression(">1000")),
                        guide = guide_colorbar(barwidth = 40, barheight = 1,
                                               title = "Predicted PFAS Level (ppt)",
                                               title.position = "top",
                                               title.hjust = 0.5,
                                               label.hjust = .5,
                                               label.position = "bottom")) +
    geom_point(data = cont_sites %>% dplyr::filter(state == states_keep[i]), aes(x = lng, y = lat), alpha = 0.4, size = 2) + theme_void() + 
    theme(legend.title = element_text(size = 26), 
          legend.text = element_text(size = 26), 
          legend.position = "bottom", 
          legend.key.height = unit(2, "cm"),
          legend.key.width = unit(2, "cm")) 
  
  if (state_abb[i] == "CA"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.1,  y=0.9, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "NH"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.1,  y=0.9, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "ME"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.35,  y=0.8, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "NY"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.58,  y=0.83, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "FL"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.5,  y=0.85, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "WI"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.4,  y=0.68, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "MI"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.2,  y=0.8, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "CO"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.5,  y=0.85, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "ND"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.5,  y=0.85, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else if (state_abb[i] == "VT"){
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.5,  y=0.85, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))
  }else{
    grob = grid::grobTree(grid::textGrob(state_abb[i], x=0.5,  y=0.9, hjust=0,
                                         gp=grid::gpar(col="black", fontsize=18)))  
  }
  
  
  smap = smap + annotation_custom(grob)
  
  return(smap)
}

state_maps = list()
for (i in 1:length(state_abb)) {
  state_maps[[state_abb[i]]] = cmap(i, state_abb, states_keep, bs_c)
}



plot_for_legend = state_maps$FL
g = ggplotGrob(plot_for_legend)
legend = gtable::gtable_filter(g, "guide-box")

convert = function(p) p / 100

viewports = list(
  grid::viewport(x = convert(71+7),  y = convert(100-12.5), width = convert(14), height = convert(25)),  # MI
  grid::viewport(x = convert(41+7),  y = convert(100-12.5), width = convert(14), height = convert(25)),  # MN
  grid::viewport(x = convert(86+7),   y = convert(100-34.5), width = convert(14), height = convert(20)), # NH
  grid::viewport(x = convert(50+10),  y = convert(100-35), width = convert(20), height = convert(20)),   # NY
  grid::viewport(x = convert(26+7),   y = convert(100-40), width = convert(14), height = convert(20)),   # CO
  grid::viewport(x = convert(86+7),  y = convert(100-12.5), width = convert(14), height = convert(25)),  # ME
  grid::viewport(x = convert(71+7),   y = convert(100-35), width = convert(14), height = convert(20)),   # VT
  grid::viewport(x = convert(0+12.5), y = convert(100-50), width = convert(25), height = convert(50)),   # CA
  grid::viewport(x = convert(75+12.5), y = convert(100-70), width = convert(25), height = convert(25)), # FL
  grid::viewport(x = convert(26+7), y = convert(100-12.5), width = convert(14), height = convert(25)),  # ND
  grid::viewport(x = convert(56+7),  y = convert(100-12.5), width = convert(14), height = convert(25))  # WI
)

# Now plot each map in its respective viewport
pdf(modify_path3("Figures/Figure3/pred_pfas_map_2.pdf"), width = 12, height = 8)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 1)))
for (i in seq_along(viewports)) {
  state_maps[[i]] = state_maps[[i]] + guides(fill = "none")
  grid::pushViewport(viewports[[i]])
  print(state_maps[[names(state_maps)[i]]], newpage = FALSE)
  grid::popViewport()
}

# Draw the legend at the bottom
bottom_vp = grid::viewport(x = 0.5, y = 0.05, width = 0.6, height = 0.05, just = c("center", "bottom"))
grid::pushViewport(bottom_vp)
grid::grid.draw(legend)
grid::popViewport()

dev.off()
