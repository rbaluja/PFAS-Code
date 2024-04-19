#soil stuff
source("PFAS-Code/PR/National Costs/soil.R")

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

bs = births


bs$updown = ifelse(bs$up == 1 | bs$down == 1, 1, 0)
#taken from national first stage. These coefficients come from w_reg_nat and w_reg_nos in first_stage.R
bs$pred_pfas = 0.017041 + 6.886192 * bs$down + 0.004021 * bs$sp + 
  -0.005266 * bs$awc + 0.665858 * asinh(bs$pfas) + -0.568659 * log(bs$dist) + 
  -0.270853 * bs$updown + -0.002472 * bs$sp * bs$down +  0.001074 * bs$awc * bs$down + 
  -0.625525 * log(bs$dist) * bs$down

#for those with missing soil data, use modified regression for imputation
nind = which(is.na(bs$pred_pfas))
bs[nind, ]$pred_pfas = 1.767553+ 5.543289 * bs[nind, ]$down + 
  0.671526 * asinh(bs[nind, ]$pfas) + -0.630132* log(bs[nind, ]$dist) + 
  -0.308053 * bs[nind, ]$updown + 
  -0.583208  * log(bs[nind, ]$dist) * bs[nind, ]$down


#getting impacts in states with initiatives
#vpre
bs$add_vpre = bs$pred_pfas * bs$births * 0.0027
vpre_births = sum(bs$add_vpre) #664.3684
vpre_cost = (vpre_births * 204083)/10^9
bs$add_vpre_se = bs$pred_pfas * bs$births * 0.001
vpre_births_se = sum(bs$add_vpre_se)
vpre_cost_se = (vpre_births_se * 204083)/10^9

#mpre
bs$add_mpre = bs$pred_pfas * bs$births * 0.00138
mpre_births = sum(bs$add_mpre) #339.5661
mpre_cost = (mpre_births * 205041)/10^9
bs$add_mpre_se = bs$pred_pfas * bs$births * 0.0004
mpre_births_se = sum(bs$add_mpre_se)# 98.42495  births se
mpre_cost_se = (mpre_births_se *  205041)/10^9

#lpre
bs$add_lpre = bs$pred_pfas * bs$births * 0.006
lpre_births = sum(bs$add_lpre)
lpre_cost = (lpre_births * 36728)/10^9
bs$add_lpre_se = bs$pred_pfas * bs$births *  0.002
lpre_births_se = sum(bs$add_lpre_se)
lpre_cost_se = (lpre_births_se * 36728)/10^9


#birthweight
#vlbw
bs$add_vlbw = bs$pred_pfas * bs$births * 0.0035
vlbw_births = sum(bs$add_vlbw)
vlbw_cost = (vlbw_births * 5133739.83)/10^9
bs$add_vlbw_se = bs$pred_pfas * bs$births * 0.001
vlbw_births_se = sum(bs$add_vlbw_se)
vlbw_cost_se = (vlbw_births_se * 5133739.83)/10^9

#mlbw
bs$add_mlbw = bs$pred_pfas * bs$births * 0.00133
mlbw_births = sum(bs$add_mlbw) 
mlbw_cost = (mlbw_births * 1634411.22)/10^9
bs$add_mlbw_se = bs$pred_pfas * bs$births *0.0005
mlbw_births_se = sum(bs$add_mlbw_se)
mlbw_cost_se = (mlbw_births_se * 1634411.22)/10^9


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Very", "Moderately", "Slightly"), 2), 
                 levels = c("Very", "Moderately", "Slightly")),
  Value = c(round(vpre_births), round(mpre_births), round(lpre_births), round(vpre_cost, digits = 2), round(mpre_cost, digits = 2), round(lpre_cost, digits = 2)), 
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")),
  se = c(paste0("(", round(vpre_births_se), ")"), paste0("(", round(mpre_births_se), ")"), paste0("(", round(lpre_births_se), ")"), 
         paste0("(", round(vpre_cost_se, digits = 2), ")"), paste0("(", round(mpre_cost_se, digits = 2), ")"), paste0("(", round(lpre_cost_se, digits = 2), ")"))
)

# Scaling factor for right axis values
scale_factor = 3000/8

data$Axis = factor(data$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data$Weeks = factor(data$Weeks, 
                    levels = c("Very", "Moderately", "Slightly"),
                    labels = c("Very", "Moderately", "Slightly"))


# Updated ggplot code
p_costs = ggplot(data, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern( 
    stat="identity", 
    position=position_dodge(), 
    aes(y=ifelse(Axis=="↑ Births", Value, Value * scale_factor), alpha = 0.5, pattern = Axis),
    pattern_fill = "white", 
    pattern_density = 0.1, 
    pattern_spacing = 0.02, 
    pattern_key_scale_factor = 0.9 
  ) +
  scale_fill_manual(values=c("↑ Births" = "dodgerblue3", "Cost" = "firebrick4")) +
  scale_y_continuous(
    "Annual Additional Preterm Births",
    sec.axis = sec_axis(~./scale_factor, name="Annual Cost ($ Billion)"), 
    limits = c(NA, 3000) 
  )  +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 60), 
        axis.text.y = element_text(size = 60), 
        legend.text = element_text(size = 60), 
        axis.text.x = element_text(size = 60),
        plot.title = element_text(hjust = 0.5, size = 22), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25)) +
  guides(alpha = "none", fill = "none", pattern = "none") +
  scale_pattern_manual(values = c("none", "stripe"))

p_costs = p_costs + geom_text(aes(label=round(Value, digits=2), 
                                  y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 200),
                              position=position_dodge(width=0.9), 
                              vjust=0, 
                              size=20)
p_costs = p_costs + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 80),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=18)



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Very", "Moderately"), 2), 
                 levels = c("Very", "Moderately")),
  Value = c(round(vlbw_births), round(mlbw_births), round(vlbw_cost, digits = 2), round(mlbw_cost, digits = 2)), 
  Axis = factor(c("Left", "Left", "Right", "Right")),
  se = c(paste0("(", round(vlbw_births_se), ")"), paste0("(", round(mlbw_births_se), ")"), 
         paste0("(", round(vlbw_cost_se, digits = 2), ")"), paste0("(", round(mlbw_cost_se, digits = 2), ")"))
)

# Scaling factor
scale_factor_bw = 3000/8

data_bw$Axis = factor(data_bw$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data_bw$Weeks = factor(data_bw$Weeks, 
                       levels = c("Very", "Moderately"),
                       labels = c("Very", "Moderately"))


# Updated ggplot code
lbw_cost = ggplot(data_bw, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern(
    stat="identity", 
    position=position_dodge(),
    aes(y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw), alpha = 0.5, pattern = Axis),
    pattern_fill = "black", 
    pattern_density = 0.1, 
    pattern_spacing = 0.02, 
    pattern_key_scale_factor = 0.9 
  ) +
  scale_fill_manual(values=c("↑ Births" = "dodgerblue3", "Cost" = "firebrick4")) +
  scale_y_continuous(
    "Annual Additional Low-Birthweight Births",
    sec.axis = sec_axis(~./scale_factor_bw, name="Annual Cost ($ Billion)"), 
    limits = c(NA, 3000) 
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(4, "lines"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 60),
        axis.text.y = element_text(size = 60),
        legend.text = element_text(size = 60),
        axis.text.x = element_text(size = 50),
        plot.title = element_text(hjust = 0.5, size = 22), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25)) + 
  guides(alpha = "none") + # Adjust guide for patterns
  scale_pattern_manual(values = c("none", "stripe"))
lbw_cost = lbw_cost + geom_text(aes(label=round(Value, digits=2), 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 200),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=18)

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 80),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=16)


p_costs = p_costs + guides(pattern = "none")
figure_3 = p_costs / lbw_cost
ggsave(modify_path3("Figures/Figure3/costs_bar.png"), figure_3, width = 10416, height = 11291, device = "png", units = "px")
