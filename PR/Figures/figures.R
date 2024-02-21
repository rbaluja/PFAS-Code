library(ggplot2)
library(ggpubr)

nh_map = map_data("state", region = "new hampshire")
#outline of map
nh_map = ggplot(nh_map, aes(x=long, y=lat)) + 
  geom_polygon(color = 'black', fill = "transparent") + 
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

#map with 
nh_map +
  geom_point(data = cont_sites, aes(x = lng, y = lat, color = log(sum_pfoa_pfos))) + 
  theme_minimal() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        text=element_text(family="Helvetica")) + 
  scale_color_gradient(low = "orange", high = "red") + 
  labs(color = "log(PFAS)")




#histograms
library(ggplot2)
ggplot(ct2, aes(x = asinh(down1_km1), fill = "Total PFAS Upstream & 5-10km"), size = 0) +
  geom_density(alpha = 0.5) +
  geom_density(aes(x = asinh(down1_km0), fill = "Total PFAS Upstream & 0-5km"), alpha = 0.5, size = 0) +
  geom_density(aes(x = asinh(down0_km0), fill = "Other PFAS 0-5km"), alpha = 0.5, size = 0) +
  geom_density(aes(x = asinh(down0_km1), fill = "Other PFAS 5-10km"), alpha = 0.5, size = 0) +
  scale_fill_manual(values = c("Total PFAS Upstream & 0-5km" = "firebrick2", 
                               "Total PFAS Upstream & 5-10km" = "darkorange4", 
                               "Other PFAS 0-5km" = "slateblue4", 
                               "Other PFAS 5-10km" = "seagreen4")) +
  theme_minimal() + xlim(0, 6) + ylim(0, 1) + guides(fill=guide_legend(title= "")) + 
  xlab("asinh(PFAS)")


library(ggplot2)
df$d0 = as.factor(ifelse(df$down0_km0 + df$down0_km1 > 0 & (!is.na(df$down0_km0) & !is.na(df$down0_km1)), 1, 0))
df$d1 = as.factor(ifelse(df$down1_km0 + df$down1_km1 > 0 & (!is.na(df$down1_km0) & !is.na(df$down1_km1)), 1, 0))
df$group = as.factor(ifelse(df$d0 == 0 & df$d1 == 0, 0, ifelse(df$d0 == 1 & df$d1 == 0, 1, 2)))

ggplot(df %>% 
         dplyr::filter(t_bin3 == 1 | t_bin3 == 3 & !is.na(t_bin3)), aes(x = gestation, colour = as.factor(t_bin3))) + 
  geom_density(adjust = 2) + 
  xlim(c(36, 42))


ggplot(df %>% 
         dplyr::filter(t_bin3 == 1 | t_bin3 == 3 & !is.na(t_bin3)), aes(x = gestation, colour = as.factor(t_bin3))) + 
  geom_histogram(aes(y=..count../sum(..count..))) + 
  xlim(c(25, 42))



d1 = df %>% 
  dplyr::filter(t_bin3 == 1 & !is.na(t_bin3))
d3 = df %>% 
  dplyr::filter(t_bin3 == 3 & !is.na(t_bin3))

ggplot() +
  geom_histogram(data = d1, aes(x = gestation, y=..count../sum(..count..)), fill = "blue", alpha = 0.5)+
  geom_histogram(data = d3, aes(x = gestation, y=..count../sum(..count..)), fill = "green", alpha  = 0.5) +
  xlim(c(25, 36))

#dummy for less than 32, 36
df$early = ifelse(df$gestation <= 36, 1, 0)
df$vearly = ifelse(df$gestation <= 32, 1, 0)

gmodels::CrossTable(df$early, df$t_bin3)

df$updown = ifelse(df$up == 1 | df$down == 1, 1, 0)
r_coefs = data.frame(matrix(ncol = 3, nrow = 0))
colnames(r_coefs) = c("weeks", "coef", "se")
index = 1
for (w in seq(from = 20, to = 40, by = 4)){
  r1 = fixest::feols(I(gestation < w & gestation >= w - 4) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  r_coefs[index, "weeks"] = w
  r_coefs[index, "coef"] = r1$coeftable["down", 1]
  r_coefs[index, "se"] = r1$coeftable["down", 2]

  index = index + 1
  
}
pre_s = loess(coef ~ weeks, data=r_coefs)
r_coefs$pre_s = predict(pre_s, r_coefs)
r_coefs$pre_lower = r_coefs$pre_s - 1.96*r_coefs$se
r_coefs$pre_upper = r_coefs$pre_s + 1.96*r_coefs$se

ggplot(r_coefs, aes(x = weeks)) + 
  geom_line(aes(y = pre_s), color = "blue") +
  geom_ribbon(aes(ymin = pre_lower, ymax = pre_upper, x = weeks, fill = 'Confidence Band' ), alpha = .3, fill = 'blue') +
  ylab('Estimated Impact of Downgradient') + xlab('Preterm Cutoff (Weeks)') + theme_minimal() 

r_coefs$pre_lower = r_coefs$coef - 1.96*r_coefs$se
r_coefs$pre_upper = r_coefs$coef + 1.96*r_coefs$se

r_coefs$week_range = cut(r_coefs$weeks,
                          breaks = c(16, 20, 24, 28, 32, 36, 40),
                          labels = c("[17, 20]", "[21, 24]", "[25, 28]", "[29, 32]", "[33, 36]", "[37, 40]"),
                          include.lowest = TRUE)

# Use the new variable for the x-axis
pre_bin = ggplot(r_coefs, aes(x=week_range, y=coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=pre_lower , ymax=pre_upper), width=0.1, alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  ylab('Estimated Impact of Downgradient') + xlab('Gestation (Weeks)') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"), 
        axis.title.x = element_text(face = "bold", size = 12), 
        axis.title.y = element_text(face = "bold", size = 12), 
        axis.text.y = element_text(face = "bold", size = 12))


r_coefs = data.frame(matrix(ncol = 3, nrow = 0))
colnames(r_coefs) = c("weeks", "coef", "se")
index = 1
for (w in seq(from = 500, to = 2500, by = 500)){
  r1 = fixest::feols(I(bweight < w & bweight >= w - 500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  r_coefs[index, "weeks"] = w
  r_coefs[index, "coef"] = r1$coeftable["down", 1]
  r_coefs[index, "se"] = r1$coeftable["down", 2]
  
  index = index + 1
  
}
pre_s = loess(coef ~ weeks, data=r_coefs)
r_coefs$pre_s = predict(pre_s, r_coefs)
r_coefs$pre_lower = r_coefs$pre_s - 1.96*r_coefs$se
r_coefs$pre_upper = r_coefs$pre_s + 1.96*r_coefs$se

ggplot(r_coefs, aes(x = weeks)) + 
  geom_line(aes(y = pre_s), color = "blue") +
  geom_ribbon(aes(ymin = pre_lower, ymax = pre_upper, x = weeks, fill = 'Confidence Band' ), alpha = .3, fill = 'blue') +
  ylab('Estimated Impact of Downgradient') + xlab('Preterm Cutoff (Weeks)') + theme_minimal() 

r_coefs$pre_lower = r_coefs$coef - 1.96*r_coefs$se
r_coefs$pre_upper = r_coefs$coef + 1.96*r_coefs$se

r_coefs$week_range = cut(r_coefs$weeks,
                         breaks = c(0, 500, 1000, 1500, 2000, 2500),
                         labels = c("[0, 500)", "[500, 1000)", "[1000, 1500)", "[1500, 2000)", "[2000, 2500)"),
                         include.lowest = TRUE)

# Use the new variable for the x-axis
lbw_bin = ggplot(r_coefs, aes(x=week_range, y=coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=pre_lower , ymax=pre_upper), width=0.1, alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  ylab('Estimated Impact of Downgradient') + xlab('Birthweight (grams)') + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"), 
        axis.title.x = element_text(face = "bold", size = 12), 
        axis.title.y = element_text(face = "bold", size = 12), 
        axis.text.y = element_text(face = "bold", size = 12)) 

pre_bin | lbw_bin







r1 = fixest::feols(I(gestation < 36 & gestation >= 32) ~ pred_pfas + asinh(pfas) +
                                      dist  + n_sites + wind_exposure +
                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height + tri5 + fa_resid|county + year^month, data = df, cluster = "county" )

r2 = fixest::feols(I(gestation < 32 & gestation >= 29) ~ pred_pfas + asinh(pfas) +
                                                 dist  + n_sites +wind_exposure +
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month, data = df, cluster = "county" )

r3 = fixest::feols(I(gestation < 29) ~ pred_pfas + asinh(pfas) +
                                           dist  + n_sites +wind_exposure +
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid|county + year^month, data = df, cluster = "county" )


r4 = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                                              wind_exposure + dist  + n_sites +
                                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                              m_height + tri5 + fa_resid|county + year^month, data = df, cluster = "county" )

r5 = fixest::feols(I(bweight < 1500) ~ pred_pfas + asinh(pfas) +
                                                   wind_exposure + dist  + n_sites +
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid|county + year^month, data = df, cluster = "county" )

r_coefs = data.frame(matrix(ncol = 3, nrow = 0))
colnames(r_coefs) = c("weeks", "coef", "se")
r_coefs[1, "weeks"] = 28
r_coefs[1, "coef"] = r3$coeftable["pred_pfas", 1]
r_coefs[1, "se"] = r3$coeftable["pred_pfas", 2]

r_coefs[2, "weeks"] = 31
r_coefs[2, "coef"] = r2$coefficients["pred_pfas"]
r_coefs[2, "se"] = r2$coeftable["pred_pfas", 2]

r_coefs[3, "weeks"] = 36
r_coefs[3, "coef"] = r1$coefficients["pred_pfas"]
r_coefs[3, "se"] = r1$coeftable["pred_pfas", 2]

r_coefs$pre_lower = r_coefs$coef - 1.96*r_coefs$se
r_coefs$pre_upper = r_coefs$coef + 1.96*r_coefs$se

r_coefs$weeks_factor = factor(r_coefs$weeks, levels = c(28, 31, 36))

ggplot(r_coefs, aes(x=weeks_factor, y=coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=pre_lower, ymax=pre_upper), width=0.1, alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  xlab("Length of Gestation (Weeks)") + 
  ylab("Estimated Effect of PFAS")

r_coefs = data.frame(matrix(ncol = 3, nrow = 0))
colnames(r_coefs) = c("grams", "coef", "se")
r_coefs[1, "grams"] = 1500
r_coefs[1, "coef"] = r5$coeftable["pred_pfas", 1]
r_coefs[1, "se"] = r5$coeftable["pred_pfas", 2]

r_coefs[2, "grams"] = 2500
r_coefs[2, "coef"] = r4$coefficients["pred_pfas"]
r_coefs[2, "se"] = r4$coeftable["pred_pfas", 2]

r_coefs$lbw_lower = r_coefs$coef - 1.96*r_coefs$se
r_coefs$lbw_upper = r_coefs$coef + 1.96*r_coefs$se

r_coefs$grams_factor = factor(r_coefs$grams, levels = c(1500, 2500))

ggplot(r_coefs, aes(x=grams_factor, y=coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=lbw_lower, ymax=lbw_upper), width=0.1, alpha = 0.5) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  xlab("Birthweight (grams)") + 
  ylab("Estimated Effect of PFAS")


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("<= 28 Weeks", "28 < Weeks <= 31", "31 < Weeks <= 36"), 2), 
                 levels = c("<= 28 Weeks", "28 < Weeks <= 31", "31 < Weeks <= 36")),
  Value = c(3460, 2162, 19027, 1191, 403, 540), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")) # Axis assignment
)

# Scaling factor for right axis values
scale_factor = max(data$Value[data$Axis == "Left"]) / max(data$Value[data$Axis == "Right"])

data$Axis = factor(data$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data$Weeks = factor(data$Weeks, 
                     levels = c("<= 28 Weeks", "28 < Weeks <= 31", "31 < Weeks <= 36"),
                     labels = c("≤ 28 Weeks", "28 < Weeks ≤ 31", "31 < Weeks ≤ 36"))


# Updated ggplot code
ggplot(data, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar(stat="identity", position=position_dodge(), aes(y=ifelse(Axis=="↑ Births", Value, Value * scale_factor), alpha = 0.5)) +
  scale_fill_manual(values=c("↑ Births" = "blue", "Cost" = "red")) +
  scale_y_continuous(
    "Additional Births",
    sec.axis = sec_axis(~./scale_factor, name="Cost ($ Million)") # Adjusting the secondary axis
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), axis.title.x = element_blank()) + 
  guides(alpha = "none")

