#number of births for each health outcome
#getting impacts in states with initiatives
vpre_births = sum(births$births_pfas, na.rm = T) * 0.0047 # 92.33542
vpre_cost = (vpre_births * vpre_pc)/10^9
#standard error: 
vpre_births_se =sum(births$births_pfas, na.rm = T) * 0.0018 #35.3625
vpre_cost_se = (vpre_births_se * vpre_pc)/10^9

mpre_births = sum(births$births_pfas, na.rm = T) * 0.0025 #49.11458
mpre_cost = (mpre_births * mpre_pc)/10^9
#standard error: 
mpre_births_se =sum(births$births_pfas, na.rm = T) * 0.0027 #53.04375
mpre_cost_se = (mpre_births_se *  mpre_pc)/10^9

lpre_births = sum(births$births_pfas, na.rm = T) * 0.0076 # 149.3083
lpre_cost = (lpre_births * lpre_pc)/10^9
#standard error: 
lpre_births_se =sum(births$births_pfas, na.rm = T) * 0.0079 #155.2021
lpre_cost_se = (lpre_births_se * lpre_pc)/10^9


#birthweight
vlbw_births = sum(births$births_pfas, na.rm = T) * 0.0061 #119.8396
vlbw_cost = (vlbw_births * vlbw_pc)/10^9
#standard error: 
vlbw_births_se =sum(births$births_pfas, na.rm = T) * 0.0021 #41.25625
vlbw_cost_se = (vlbw_births_se * vlbw_pc)/10^9

mlbw_births = sum(births$births_pfas, na.rm = T) * -0.00005 #-0.9822917
mlbw_cost = (mlbw_births * mlbw_pc)/10^9
#standard error: 
mlbw_births_se =sum(births$births_pfas, na.rm = T) * 0.00185 #36.34479
mlbw_cost_se = (mlbw_births_se * mlbw_pc)/10^9

lbw_births = sum(births$births_pfas, na.rm = T) * 0.0204
lbw_births_se = sum(births$births_pfas, na.rm = T) * 0.0086

#infant mortality
mort_births = sum(births$births_pfas, na.rm = T) * 0.0061
mort_cost = (mort_births * mort_pc)/10^9
mort_births_se = sum(births$births_pfas, na.rm = T) * 0.0018
mort_cost_se = (mort_births_se * mort_pc)/10^9


#make binary cost figure
#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Moderately", "Very", "Extremely"), 2), 
                 levels = c("Moderately", "Very", "Extremely")),
  Value = c(round(lpre_births), round(mpre_births), round(vpre_births), round(lpre_cost, digits = 2), round(mpre_cost, digits = 2), round(vpre_cost, digits = 2)), 
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")),
  se = c(paste0("(", round(lpre_births_se), ")"), paste0("(", round(mpre_births_se), ")"), paste0("(", round(vpre_births_se), ")"), 
         paste0("(", round(lpre_cost_se, digits = 2), ")"), paste0("(", round(mpre_cost_se, digits = 2), ")"), paste0("(", round(vpre_cost_se, digits = 2), ")"))
)

# Scaling factor for right axis values
#scale_factor = max(data$Value[data$Axis == "Left"]) / max(data$Value[data$Axis == "Right"])
scale_factor = 45000/60

data$Axis = factor(data$Axis, levels = c("Left", "Right"), labels = c("↑ Births (Left Axis)", "Costs (Right Axis)"))
data$Weeks = factor(data$Weeks, 
                    levels = c("Moderately", "Very", "Extremely"),
                    labels = c("Moderately", "Very", "Extremely"))

# Updated ggplot code
p_costs = 
  ggplot(data, aes(x=Weeks, y=Value, fill=Axis)) +
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
    limits = c(NA, 45000) 
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

p_costs = 
  p_costs + geom_text(aes(label=round(Value, digits=3), 
                          y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor) + 1000), 
                      position=position_dodge(width=0.9), 
                      vjust=0, 
                      size=16)
p_costs = 
  p_costs + geom_text(aes(label=se, 
                          y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor) + 100), 
                      position=position_dodge(width=0.9), 
                      vjust=0, 
                      size=16)




#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Moderately", "Very", "Extremely"), 2), 
                 levels = c("Moderately", "Very", "Extremely")),
  Value = c(round(lbw_births), round(mlbw_births), round(vlbw_births), 0, round(mlbw_cost, digits = 2), round(vlbw_cost, digits = 2)), 
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")),
  se = c(paste0("(", round(lbw_births_se), ")"), paste0("(", round(mlbw_births_se), ")"), paste0("(", round(vlbw_births_se), ")"), 
         "", paste0("(", round(mlbw_cost_se, digits = 2), ")"), paste0("(", round(vlbw_cost_se, digits = 2), ")"))
)

# Scaling factor for right axis values
scale_factor_bw = 45000/60

data_bw$Axis = factor(data_bw$Axis, levels = c("Left", "Right"), labels = c("↑ Births (Left Axis)", "Costs (Right Axis)"))
data_bw$Weeks = factor(data_bw$Weeks, 
                       levels = c("Moderately", "Very", "Extremely"),
                       labels = c("Moderately","Very", "Extremely"))


lbw_cost =
  ggplot(data_bw, aes(x=Weeks, y=Value, fill=Axis)) +
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
    limits = c(NA, 45000) 
  ) +
  ggtitle("Low-Weight Births") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(4, "lines"),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 60),
        axis.text.x = element_text(size = 60),
        plot.title = element_text(hjust = 0.5, size = 70), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25),
        axis.text.y.right = element_text(size = 60), 
        axis.title.y.right = element_text(size = 60),
        legend.spacing.x = unit(1.5, 'cm')) + 
  guides(alpha = "none") + 
  scale_pattern_manual(values = c("none", "stripe")) 

lbw_cost = 
  lbw_cost + geom_text(aes(label=ifelse(Weeks != "Moderately" | Axis != "Costs (Right Axis)", round(Value, digits=2), ""), 
                           y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw) + 1000),
                       position=position_dodge(width=0.9), 
                       vjust=0,
                       size=16)

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw) + 100),
                                position=position_dodge(width=0.9), 
                                vjust=0,
                                size=16)




data_mort = data.frame(
  Weeks = factor(rep(c("Infant Mortalities"), 2), 
                 levels = c("Infant Mortalities")),
  Value = c(round(mort_births), round(mort_cost, digits = 2)), 
  Axis = factor(c("Left", "Right")),
  se = c(paste0("(", round(mort_births_se), ")"), 
         paste0("(", round(mort_cost_se, digits = 2), ")"))
)

# Scaling factor
scale_factor_mort = 45000/60

data_mort$Axis = factor(data_mort$Axis, levels = c("Left", "Right"), labels = c("↑ Births (Left Axis)", "Costs (Right Axis)"))
data_mort$Weeks = factor(data_mort$Weeks, 
                         levels = c("Infant Mortalities"),
                         labels = c("Infant Mortalities"))


# Updated ggplot code
mort_cost_fig = 
  ggplot(data_mort, aes(x=Weeks, y=Value, fill=Axis)) +
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
    "Annual Additional Births (1,000)",
    sec.axis = sec_axis(~./scale_factor_bw, name=""), 
    limits = c(NA, 45000), 
    labels = scales::label_number(scale = 0.001)
  ) +
  ggtitle("Infant Mortality") +
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

mort_cost_fig = 
  mort_cost_fig + geom_text(aes(label=round(Value, digits=2), 
                                y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_mort) + 1000),
                            position=position_dodge(width=0.9), 
                            vjust=0, 
                            size=16)

mort_cost_fig = mort_cost_fig + geom_text(aes(label=se, 
                                              y=ifelse(Axis=="↑ Births (Left Axis)", Value, Value * scale_factor_bw) + 10),
                                          position=position_dodge(width=0.9), 
                                          vjust=0, 
                                          size=16)




p_costs = p_costs + guides(pattern = "none")
mort_cost_fig = mort_cost_fig + guides(pattern = "none")
figure_s7 = (mort_cost_fig | p_costs | lbw_cost) + plot_layout(widths = c(1, 3, 3), guides = "collect")& 
  theme(legend.position = 'bottom')
ggsave("Figures Revision/bin_cost_xgboost.png", figure_s7, width = 10416, height = 11291, units = "px", device = "png")
