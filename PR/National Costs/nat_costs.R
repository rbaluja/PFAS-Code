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
bs$add_vpre = bs$pred_pfas * bs$births * 0.0039
sum(bs$add_vpre) #959.6433
#very preterm cost: 959.6433 * 204083 = 195846884/10^9
#standard error: 
bs$add_vpre_se = bs$pred_pfas * bs$births * 0.001
sum(bs$add_vpre_se)#246.0624 births se
#cost se: 246.0624  * 204083 = 50217153/10^9

bs$add_mpre = bs$pred_pfas * bs$births * 0.00019
sum(bs$add_mpre) #46.75185
#moderately preterm cost: 46.75185 * 205041 =  9586046/10^9
#standard error: 
bs$add_mpre_se = bs$pred_pfas * bs$births * 0.002
sum(bs$add_mpre_se)# 492.1248births se
#cost se:  492.1248* 205041 = 100905761/10^9


bs$add_lpre = bs$pred_pfas * bs$births * 0.006
sum(bs$add_lpre) #1476.374 * 36728 =  54224264/10^9
#standard error: 
bs$add_lpre_se = bs$pred_pfas * bs$births *  0.002
sum(bs$add_lpre_se)# 492.1248 births se
#cost se: 492.1248 * 36728 = 18074760/10^9


#birthweight
bs$add_vlbw = bs$pred_pfas * bs$births * 0.0035
sum(bs$add_vlbw) #861.2183
#very low birthweight cost: 861.2183 * 5133739.83 =  4421270689/10^9
#standard error: 
bs$add_vlbw_se = bs$pred_pfas * bs$births * 0.001
sum(bs$add_vlbw_se)# 246.0624 births se
#cost se:  246.0624 * 5133739.83 =   1263220344/10^9

bs$add_mlbw = bs$pred_pfas * bs$births * 0.00133
sum(bs$add_mlbw) # 327.263
#moderately low birthweight cost:  327.263 * 1634411.22 = 534882319/10^9
#standard error: 
bs$add_mlbw_se = bs$pred_pfas * bs$births *0.0005
sum(bs$add_mlbw_se) #123.0312 births se
#cost se: 123.0312 * 1634411.22 = 201083574/10^9


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Very Preterm", "Mod. Preterm", "Late Preterm"), 2), 
                 levels = c("Very Preterm", "Mod. Preterm", "Late Preterm")),
  Value = c(960, 47, 1476, 0.20, 0.01, 0.05), 
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")),
  se = c("(246)", "(492)", "(492)", "(0.05)", "(0.10)", "(0.02)")
)

# Scaling factor for right axis values
scale_factor = 3000/8

data$Axis = factor(data$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data$Weeks = factor(data$Weeks, 
                    levels = c("Very Preterm", "Mod. Preterm", "Late Preterm"),
                    labels = c("Very Preterm", "Mod. Preterm", "Late Preterm"))


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
  scale_fill_manual(values=c("↑ Births" = "blue", "Cost" = "red")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor, name="Annual Cost ($ Billion)"), 
    limits = c(NA, 3000) 
  )  +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"), 
        legend.text = element_text(size = 20, face = "bold"), 
        axis.text.x = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25)) +
  guides(alpha = "none", fill = "none", pattern = "none") +
  scale_pattern_manual(values = c("none", "stripe"))

p_costs = p_costs + geom_text(aes(label=round(Value, digits=2), 
                                  y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 200),
                              position=position_dodge(width=0.9), 
                              vjust=0, 
                              size=7, 
                              fontface = "bold")
p_costs = p_costs + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 100),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=5, 
                                fontface = "bold")
p_costs



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Very Low Birthweight", "Mod. Low Birthweight"), 2), 
                 levels = c("Very Low Birthweight", "Mod. Low Birthweight")),
  Value = c(861, 327, 4.42, 0.53), 
  Axis = factor(c("Left", "Left", "Right", "Right")),
  se = c("(246)", "(123)", "(1.26)", "(0.20)")
)

# Scaling factor
scale_factor_bw = 3000/8

data_bw$Axis = factor(data_bw$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data_bw$Weeks = factor(data_bw$Weeks, 
                       levels = c("Very Low Birthweight", "Mod. Low Birthweight"),
                       labels = c("Very Low Birthweight", "Mod. Low Birthweight"))


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
  scale_fill_manual(values=c("↑ Births" = "blue", "Cost" = "red")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor_bw, name="Annual Cost ($ Billion)"), 
    limits = c(NA, 3000) 
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25)) + 
  guides(alpha = "none") + # Adjust guide for patterns
  scale_pattern_manual(values = c("none", "stripe"))
lbw_cost = lbw_cost + geom_text(aes(label=round(Value, digits=2), 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 200),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=7, 
                                fontface = "bold")

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 100),
                                position=position_dodge(width=0.9), 
                                vjust=0, 
                                size=5, 
                                fontface = "bold")

lbw_cost

p_costs = p_costs + guides(pattern = "none")
figure_3 = p_costs / lbw_cost
ggsave("Figures/National Costs/figure_3.png", figure_3)
