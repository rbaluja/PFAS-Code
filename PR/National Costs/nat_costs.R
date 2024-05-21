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

#read in standard errors
if (!file.exists(modify_path("Data_Verify/RData/preterm_sd.RData")) | 
    !file.exists(modify_path("Data_Verify/RData/lbw_sd.RData")) | 
    !file.exists(modify_path("Data_Verify/RData/mort_sd.RData"))){
  stop("Run main analysis through tables.R before calculating national cost.")
}
load(modify_path("Data_Verify/RData/preterm_sd.RData"))
load(modify_path("Data_Verify/RData/lbw_sd.RData"))
load(modify_path("Data_Verify/RData/mort_sd.RData"))
#getting impacts in states with initiatives
#vpre
bs$add_vpre = bs$pred_pfas * bs$births * 0.0027
vpre_births = sum(bs$add_vpre) #664.3684
vpre_cost = (vpre_births * 204083)/10^9
bs$add_vpre_se = bs$pred_pfas * bs$births * vpreterm_sd
vpre_births_se = sum(bs$add_vpre_se)
vpre_cost_se = (vpre_births_se * 204083)/10^9

#mpre
bs$add_mpre = bs$pred_pfas * bs$births * 0.00128
mpre_births = sum(bs$add_mpre) #339.5661
mpre_cost = (mpre_births * 205041)/10^9
bs$add_mpre_se = bs$pred_pfas * bs$births * mpreterm_sd
mpre_births_se = sum(bs$add_mpre_se)# 98.42495  births se
mpre_cost_se = (mpre_births_se *  205041)/10^9

#lpre
bs$add_lpre = bs$pred_pfas * bs$births * 0.0059
lpre_births = sum(bs$add_lpre)
lpre_cost = (lpre_births * 36728)/10^9
bs$add_lpre_se = bs$pred_pfas * bs$births *  lpreterm_sd
lpre_births_se = sum(bs$add_lpre_se)
lpre_cost_se = (lpre_births_se * 36728)/10^9


#birthweight
#vlbw
bs$add_vlbw = bs$pred_pfas * bs$births * 0.0035
vlbw_births = sum(bs$add_vlbw)
vlbw_cost = (vlbw_births * 5133739.83)/10^9
bs$add_vlbw_se = bs$pred_pfas * bs$births * vlbw_sd
vlbw_births_se = sum(bs$add_vlbw_se)
vlbw_cost_se = (vlbw_births_se * 5133739.83)/10^9

#mlbw
bs$add_mlbw = bs$pred_pfas * bs$births * 0.00133
mlbw_births = sum(bs$add_mlbw) 
mlbw_cost = (mlbw_births * 1634411.22)/10^9
bs$add_mlbw_se = bs$pred_pfas * bs$births * mlbw_sd
mlbw_births_se = sum(bs$add_mlbw_se)
mlbw_cost_se = (mlbw_births_se * 1634411.22)/10^9

#lbw 
bs$add_lbw = bs$pred_pfas * bs$births * 0.0051
lbw_births = sum(bs$add_lbw)
bs$add_lbw_se = bs$pred_pfas * bs$births * llbw_sd
lbw_births_se = sum(bs$add_lbw_se)


#infant mortality
bs$add_mort = bs$pred_pfas * bs$births * 0.0016
mort_births = sum(bs$add_mort)
mort_cost = (mort_births * 6581967.8795)/10^9
bs$add_mort_se = bs$pred_pfas * bs$births * mort_sd
mort_births_se = sum(bs$add_mort_se)
mort_cost_se = (mort_births_se * 6581967.8795)/10^9


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Slightly", "Moderately", "Very"), 2), 
                 levels = c("Slightly", "Moderately", "Very")),
  Value = c(round(lpre_births), round(mpre_births), round(vpre_births), round(lpre_cost, digits = 2), round(mpre_cost, digits = 2), round(vpre_cost, digits = 2)), 
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")),
  se = c(paste0("(", round(lpre_births_se), ")"), paste0("(", round(mpre_births_se), ")"), paste0("(", round(vpre_births_se), ")"), 
         paste0("(", round(lpre_cost_se, digits = 2), ")"), paste0("(", round(mpre_cost_se, digits = 2), ")"), paste0("(", round(vpre_cost_se, digits = 2), ")"))
)



# Scaling factor for right axis values
scale_factor = 2000/6

data$Axis = factor(data$Axis, levels = c("Left", "Right"), labels = c("↑ Births (Left Axis)", "Costs (Right Axis)"))
data$Weeks = factor(data$Weeks, 
                    levels = c("Slightly", "Moderately", "Very"),
                    labels = c("Slightly", "Moderately", "Very"))


p_costs = ggplot(data, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern( 
    stat="identity", 
    position=position_dodge(), 
    aes(y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor), alpha = 0.5, pattern = Axis),
    pattern_fill = "black", 
    pattern_density = 0.1, 
    pattern_spacing = 0.02, 
    pattern_key_scale_factor = 0.9 
  ) +
  scale_fill_manual(values=c("↑ Births (Left Axis)" = "dodgerblue3", "Costs (Right Axis)" = "firebrick4")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor, name=""), 
    limits = c(NA, 2000) 
  )  +
  theme_minimal() + 
  ggtitle("Preterm Births") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        legend.text = element_text(size = 60), 
        axis.text.x = element_text(size = 60),
        plot.title = element_text(hjust = 0.5, size = 70), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25), 
        axis.text.y = element_blank()) +
  guides(alpha = "none", fill = "none", pattern = "none") +
  scale_pattern_manual(values = c("none", "stripe")) 

p_costs = p_costs + geom_text(aes(label=round(Value, digits=2), 
                                  y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor) + 120),
                              position=position_dodge(width=0.9), 
                              vjust=0, 
                              size=20)
p_costs = p_costs + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor) + 60),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=18)



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Slightly", "Moderately", "Very"), 2), 
                 levels = c("Slightly", "Moderately", "Very")),
  Value = c(round(lbw_births), round(mlbw_births), round(vlbw_births), 0, round(mlbw_cost, digits = 2), round(vlbw_cost, digits = 2)), 
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")),
  se = c(paste0("(", round(lbw_births_se), ")"), paste0("(", round(mlbw_births_se), ")"), paste0("(", round(vlbw_births_se), ")"), 
         "", paste0("(", round(mlbw_cost_se, digits = 2), ")"), paste0("(", round(vlbw_cost_se, digits = 2), ")"))
)

# Scaling factor
scale_factor_bw = 2000/6

data_bw$Axis = factor(data_bw$Axis, levels = c("Left", "Right"), labels = c("↑ Births (Left Axis)", "Costs (Right Axis)"))
data_bw$Weeks = factor(data_bw$Weeks, 
                       levels = c("Slightly", "Moderately", "Very"),
                       labels = c("Slightly","Moderately", "Very"))


# Updated ggplot code
lbw_cost = ggplot(data_bw, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern(
    stat="identity", 
    position=position_dodge(),
    aes(y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw), alpha = 0.5, pattern = Axis),
    pattern_fill = "black", 
    pattern_density = 0.1, 
    pattern_spacing = 0.02, 
    pattern_key_scale_factor = 0.9 
  ) +
  scale_fill_manual(values=c("↑ Births (Left Axis)" = "dodgerblue3", "Costs (Right Axis)" = "firebrick4")) +
  scale_y_continuous(
    "",
    sec.axis = sec_axis(~./scale_factor_bw, name="Annual Cost ($ Billion)"), 
    limits = c(NA, 2000) 
  ) +
  ggtitle("Low-Weight Births") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(4, "lines"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 60),
        axis.text.x = element_text(size = 50),
        plot.title = element_text(hjust = 0.5, size = 70), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25),
        axis.text.y.right = element_text(size = 60), 
        axis.title.y.right = element_text(size = 60),
        legend.spacing.x = unit(1.5, 'cm')) + 
  guides(alpha = "none") + 
  scale_pattern_manual(values = c("none", "stripe")) 
lbw_cost = lbw_cost + geom_text(aes(label=ifelse(Weeks != "Slightly" | Axis != "Costs (Right Axis)", round(Value, digits=2), ""), 
                                    y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw) + 120),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=18)

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw) +60),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=16)


#Infant Mortality
data_mort = data.frame(
  Weeks = factor(rep(c("Infant Mortality"), 2), 
                 levels = c("Infant Mortality")),
  Value = c(round(mort_births), round(mort_cost, digits = 2)), 
  Axis = factor(c("Left", "Right")),
  se = c(paste0("(", round(mort_births_se), ")"), 
         paste0("(", round(mort_cost_se, digits = 2), ")"))
)

# Scaling factor
scale_factor_mort = 2000/6

data_mort$Axis = factor(data_mort$Axis, levels = c("Left", "Right"), labels = c("↑ Births (Left Axis)", "Costs (Right Axis)"))
data_mort$Weeks = factor(data_mort$Weeks, 
                       levels = c("Infant Mortality"),
                       labels = c("Infant Mortality"))


# Updated ggplot code
mort_cost_fig = ggplot(data_mort, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern(
    stat="identity", 
    position=position_dodge(),
    aes(y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw), alpha = 0.5, pattern = Axis),
    pattern_fill = "black", 
    pattern_density = 0.1, 
    pattern_spacing = 0.05, 
    pattern_key_scale_factor = 0.9 
  ) +
  scale_fill_manual(values=c("↑ Births (Left Axis)" = "dodgerblue3", "Costs (Right Axis)" = "firebrick4")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor_bw, name=""), 
    limits = c(NA, 2000) 
  ) +
  ggtitle("Infant Mortalities") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 60), 
        axis.text.y = element_text(size = 60), 
        legend.text = element_text(size = 60), 
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 70), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25), 
        axis.text.y.right = element_blank()) + 
  scale_pattern_manual(values = c("none", "stripe")) + 
  guides(alpha = "none", fill = "none", pattern = "none")
mort_cost_fig = mort_cost_fig + geom_text(aes(label=round(Value, digits=2), 
                                    y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_mort) + 120),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=18)

mort_cost_fig = mort_cost_fig + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw) +60),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=16)






p_costs = p_costs + guides(pattern = "none")
mort_cost_fig = mort_cost_fig + guides(pattern = "none")
figure_3 = (mort_cost_fig | p_costs | lbw_cost) + plot_layout(widths = c(1, 3, 3), guides = "collect")& 
  theme(legend.position = 'bottom')

ggsave(modify_path3("Figures/Figure3/costs_bar.png"), figure_3, width = 12000, height = 9541, units = "px", device = "png", limitsize = FALSE)
