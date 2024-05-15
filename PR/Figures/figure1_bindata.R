df_c = df %>% 
  dplyr::filter(dist <= meters) %>% 
  dplyr::filter(!is.na(gestation) & 
                  !is.na(m_age) & 
                  !is.na(m_married) & 
                  !is.na(private_insurance) & 
                  !is.na(nbr_cgrtt) & 
                  !is.na(m_educ) & 
                  !is.na(f_educ) & 
                  !is.na(pm25) & 
                  !is.na(temp) & 
                  !is.na(p_manuf) & 
                  !is.na(n_hunits) & 
                  !is.na(med_hprice) & 
                  !is.na(well_elev) & 
                  !is.na(resid_elev) & 
                  !is.na(mr_04) & 
                  !is.na(mr_18) & 
                  !is.na(mr_21) & 
                  !is.na(mr_26) & 
                  !is.na(mr_27) & 
                  !is.na(mthr_wgt_dlv) & 
                  !is.na(mthr_pre_preg_wgt) & 
                  !is.na(m_height) & 
                  !is.na(tri5) & 
                  !is.na(county) & 
                  !is.na(year) & 
                  !is.na(month) & 
                  !is.na(birth_race_dsc_1) & 
                  !is.na(wic))

df_c$vlbw = as.numeric(df_c$bweight < 1000)

df_c$dist_bin = as.integer(cut(df_c$dist, breaks = c(0, 3000, 5000), include.lowest = TRUE, labels =FALSE))

df_c$dus = ifelse(df_c$down == 1, "down", ifelse(df_c$up == 1, "up", "side"))

dfc_vlbw = df_c %>% 
  as_tibble() %>%
  dplyr::group_by(dist_bin, dus, vlbw) %>% 
  dplyr::summarise(n = n())

dfc_vlbw$middle = as.numeric(dfc_vlbw$dist_bin == 2)
#dfc_vlbw$outer = as.numeric(dfc_vlbw$dist_bin == 3)
dfc_vlbw$triangle = ifelse(dfc_vlbw$dus == "up", 1, ifelse(dfc_vlbw$dus == "down", 3, 4))

# dfc_side = dfc_vlbw[dfc_vlbw$triangle == 4, ]
# dfc_side3 = dfc_side
# dfc_side3$triangle = 2
# dfc_side3$n = floor(dfc_side3$n/2)
# dfc_side$n = ceiling(dfc_side$n/2)
# 
# dfc_side = rbind(dfc_side, dfc_side3)
# 
# dfc_vlbw = dfc_vlbw %>% 
#   dplyr::filter(triangle != 4) %>% 
#   rbind(dfc_side) %>% 
#   rbind(dfc_side3) %>% 
#   ungroup()

dfc_vlbw = dfc_vlbw %>% 
  ungroup()

for (l in 0:1){
  for (t in 1:4){
    for (d in 1:2){
      d2 = dfc_vlbw %>% 
        dplyr::filter(vlbw == l, 
                      triangle == t, 
                      dist_bin == d)
      if (nrow(d2) == 0){
        nr = c(dist_bin = d, 
               dus = "", 
               vlbw = l, 
               n = 0, 
               middle = ifelse(d == 2, 1, 0), 
               triangle = t)
        dfc_vlbw = rbind(dfc_vlbw, nr)
      }
    }
  }
}

#build triangles figure ("fig")
source("PFAS-Code/PR/Figures/figure1_triangles.R")

fig = fig %>% right_join(dfc_vlbw %>% 
                           dplyr::mutate(middle = as.numeric(middle), 
                                         
                                         triangle = as.numeric(triangle)))



fig$nb = ifelse(fig$vlbw == "1", as.numeric(fig$n), ceiling(as.numeric(fig$n)/100))



points_list = list()
for (i in 1:nrow(fig)) {
  current_polygon = fig[i, ]
  nb_points = current_polygon$nb
  
  if (nb_points > 0) {
    # Generate random points within the polygon
    random_points = st_sample(current_polygon, size = nb_points)
    
    # Create a dataframe for these points with the vlbw value
    points_df = data.frame(vlbw = as.numeric(current_polygon$vlbw))
    
    # Convert the dataframe to an sf object, combining with random_points as geometry
    points_sf = st_sf(points_df, geometry = st_sfc(random_points))
    
    # Append the generated sf object to the list
    points_list[[i]] = points_sf 
  }
}

points_sf = do.call(rbind, points_list)

points_sf$vlbw = ifelse(points_sf$vlbw == "1", "1 Very Low-Weight Birth", "100 Non-Very Low-Weight Births")
points_sf$vlbw = factor(points_sf$vlbw, levels = c("1 Very Low-Weight Birth", "100 Non-Very Low-Weight Births"))

fig1_bin = ggplot() +
  geom_sf(data = fig, fill = NA, color = "black", size = 2) +  
  geom_sf(data = points_sf, aes(color = as.factor(vlbw)), alpha = 0.4, size = ifelse(points_sf$vlbw == "1 Very Low-Weight Birth", 8, 12)) + 
  scale_color_manual(values = c("100 Non-Very Low-Weight Births" = "dodgerblue3", "1 Very Low-Weight Birth" = "firebrick4"), 
                     guide = guide_colorsteps(title.position = "top", 
                                              title = "")) + 
  theme_void() + labs(color = "") + 
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = -5), arrow = arrow(type = "closed", length = unit(0.5, "inches")), color = "black", size = 1) +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size= 40), 
        legend.title = element_text(size= 35), 
        legend.position = "bottom") + 
  guides(color = guide_legend(override.aes = list(size = 10))) + 
  geom_segment(aes(x = 0, y = 0, xend = -5, yend = 0), 
               linetype = "dotted", 
               color = "black", size = 1) + 
  annotate("text", x = -1.5, y = 0.08, label = "3km", hjust = 0.5, vjust = 0, size = 20) + 
  annotate("text", x = -4, y = 0.08, label = "5km", hjust = 0.5, vjust = 0, size = 20)

ggsave(modify_path3("Figures/Figure1/figure1_bindata.png"), fig1_bin, width = 6000, height = 4000, units = "px")
