#soil stuff
source("PFAS-Code/PR/National Costs/soil.R")

cont_sites = read_xlsx('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
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

#subsetting to states with initiatives
bs = births %>% 
  left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, state)) %>% 
  dplyr::filter(state %in% c("Michigan", 
                             "Minnesota", 
                             "New Hampshire", 
                             "New York", 
                             "Colorado", 
                             "Maine", 
                             "Vermont", 
                             "California", 
                             "Florida", 
                             "North Dakota", 
                             "Wisconsin"))



bs$updown = ifelse(bs$up == 1 | bs$down == 1, 1, 0)
#taken from national first stage. See daily note (01/23/23) for why these are only vars
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
sum(bs$add_vpre) #961.1678
#very preterm cost: 961.1678 * 204083 = 196158008/10^9
#standard error: 
bs$add_vpre_se = bs$pred_pfas * bs$births * 0.001
sum(bs$add_vpre_se)#246.4533 births se
#cost se: 246.4533  * 204083 = 50296929/10^9

bs$add_mpre = bs$pred_pfas * bs$births * 0.00019
sum(bs$add_mpre) #46.82612
#moderately preterm cost: 46.82612 * 205041 =  9601274/10^9
#standard error: 
bs$add_mpre_se = bs$pred_pfas * bs$births * 0.002
sum(bs$add_mpre_se)#492.9066 births se
#cost se: 492.9066 * 205041 = 101066062/10^9


bs$add_lpre = bs$pred_pfas * bs$births * 0.006
sum(bs$add_lpre) #1478.72 * 36728 = 54310428/10^9
#standard error: 
bs$add_lpre_se = bs$pred_pfas * bs$births *  0.002
sum(bs$add_lpre_se)# 492.9066 births se
#cost se: 492.90662 * 36728 = 18103474/10^9


#birthweight
bs$add_vlbw = bs$pred_pfas * bs$births * 0.0035
sum(bs$add_vlbw) #862.5865
#very low birthweight cost: 862.5865 * 5133739.83 = 4428294672/10^9
#standard error: 
bs$add_vlbw_se = bs$pred_pfas * bs$births * 0.001
sum(bs$add_vlbw_se)#246.4533 births se
#cost se: 246.4533 * 5133739.83 =  1265227122/10^9

bs$add_mlbw = bs$pred_pfas * bs$births * 0.00133
sum(bs$add_mlbw) #327.7829
#moderately low birthweight cost: 327.7829 * 1634411.22 = 535732049/10^9
#standard error: 
bs$add_mlbw_se = bs$pred_pfas * bs$births *0.0005
sum(bs$add_mlbw_se) #123.2266births se
#cost se: 123.2266 * 1634411.22 = 201402938/10^9


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Very Preterm", "Mod. Preterm", "Late Preterm"), 2), 
                 levels = c("Very Preterm", "Mod. Preterm", "Late Preterm")),
  Value = c(961, 47, 1479, 0.20, 0.01, 0.05), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")), # Axis assignment
  se = c("(246)", "(493)", "(493)", "(0.05)", "(0.10)", "(0.02)")
)

# Scaling factor for right axis values
#scale_factor = max(data$Value[data$Axis == "Left"]) / max(data$Value[data$Axis == "Right"])
scale_factor = 3000/8

data$Axis = factor(data$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data$Weeks = factor(data$Weeks, 
                    levels = c("Very Preterm", "Mod. Preterm", "Late Preterm"),
                    labels = c("Very Preterm", "Mod. Preterm", "Late Preterm"))


# Updated ggplot code
p_costs = ggplot(data, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern( # Use geom_bar_pattern for patterns
    stat="identity", 
    position=position_dodge(), 
    aes(y=ifelse(Axis=="↑ Births", Value, Value * scale_factor), alpha = 0.5, pattern = Axis),
    pattern_fill = "white", # Set the color of the pattern
    pattern_density = 0.1, # Adjust density of the pattern lines
    pattern_spacing = 0.02, # Adjust spacing of the pattern lines
    pattern_key_scale_factor = 0.9 # Adjust the scale of the pattern in the legend
  ) +
  scale_fill_manual(values=c("↑ Births" = "blue", "Cost" = "red")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor, name="Annual Cost ($ Billion)"), # Adjusting secondary axis
    limits = c(NA, 3000) # Set the upper limit to a higher value
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
                                  y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 200), # Adjust the offset as needed
                              position=position_dodge(width=0.9), 
                              vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                              size=7, 
                              fontface = "bold")
p_costs = p_costs + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 100), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=5, 
                                fontface = "bold")
p_costs



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Very Low Birthweight", "Mod. Low Birthweight"), 2), 
                 levels = c("Very Low Birthweight", "Mod. Low Birthweight")),
  Value = c(863, 328, 4.43, 0.54), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Right", "Right")), # Axis assignment
  se = c("(246)", "(123)", "(1.27)", "(0.20)")
)

# Scaling factor for right axis values
#scale_factor_bw = max(data_bw$Value[data_bw$Axis == "Left"]) / max(data_bw$Value[data_bw$Axis == "Right"])
scale_factor_bw = 3000/8

data_bw$Axis = factor(data_bw$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data_bw$Weeks = factor(data_bw$Weeks, 
                       levels = c("Very Low Birthweight", "Mod. Low Birthweight"),
                       labels = c("Very Low Birthweight", "Mod. Low Birthweight"))


# Updated ggplot code
lbw_cost = ggplot(data_bw, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern( # Use geom_bar_pattern to apply patterns
    stat="identity", 
    position=position_dodge(),
    aes(y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw), alpha = 0.5, pattern = Axis),
    pattern_fill = "black", # Set color of the pattern
    pattern_density = 0.1, # Adjust density of pattern lines
    pattern_spacing = 0.02, # Adjust spacing of pattern lines
    pattern_key_scale_factor = 0.9 # Adjust scale of pattern in legend
  ) +
  scale_fill_manual(values=c("↑ Births" = "blue", "Cost" = "red")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor_bw, name="Annual Cost ($ Billion)"), # Adjusting secondary axis
    limits = c(NA, 3000) # Set the upper limit to a higher value
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
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 200), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=7, 
                                fontface = "bold")

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 100), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=5, 
                                fontface = "bold")

lbw_cost

p_costs = p_costs + guides(pattern = "none")
p_costs / lbw_cost
