#getting impacts in states with initiatives
add_vpre = sum(bs[which(bs$down == 1), ]$births) * 0.0047 # 92.33542
add_vpre
#very preterm cost:  92.33542 * 204083 =  18844090/10^9
#standard error: 
add_vpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0018 #35.3625
add_vpre_se
#cost se: 35.3625 *204083 = 7216885/10^9

add_mpre = sum(bs[which(bs$down == 1), ]$births) * 0.0025 #49.11458
add_mpre
#m preterm cost se: 49.11458 * 205041  =  10070503/10^9
#standard error: 
add_mpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0027 #53.04375
add_mpre_se
#cost se: 53.04375 * 205041  = 10876144/10^9

add_lpre = sum(bs[which(bs$down == 1), ]$births) * 0.0076 # 149.3083
add_lpre
#late preterm cost:  149.3083 * 36728.05 = 5483803/10^9
#standard error: 
add_lpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0079 #155.2021
add_lpre_se
#cost se:155.2021 * 36728.05 = 5700270/10^9



#birthweight
add_vlbw = sum(bs[which(bs$down == 1), ]$births) * 0.0061 #119.8396
add_vlbw
#very lbw cost:119.8396 * 5133739.83 = 615225328/10^9
#standard error: 
add_vlbw_se =sum(bs[which(bs$down == 1), ]$births) * 0.0021 #41.25625
add_vlbw_se
#cost se: 41.25625 * 5133739.83 = 211798854/10^9

add_mlbw = sum(bs[which(bs$down == 1), ]$births) * -0.00005 #-0.9822917
add_mlbw
#mod lbw cost: -0.9822917* 1634411.22 = -1605469/10^9
#standard error: 
add_mlbw_se =sum(bs[which(bs$down == 1), ]$births) * 0.00185 #36.34479
add_mlbw_se
#cost se: 36.34479 * 1634411.22 = 59402333/10^9


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Very Preterm", "Mod. Preterm", "Late Preterm"), 2), 
                 levels = c("Very Preterm", "Mod. Preterm", "Late Preterm")),
  Value = c(92, 49, 149, 0.02, 0.01, 0.005), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")), # Axis assignment
  se = c("(35)", "(53)", "(155)", "(0.007)", "(0.01)", "(0.006)")
)

# Scaling factor for right axis values
#scale_factor = max(data$Value[data$Axis == "Left"]) / max(data$Value[data$Axis == "Right"])
scale_factor = 500/1

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
    limits = c(NA, 500)
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

p_costs = p_costs + geom_text(aes(label=round(Value, digits=3), 
                                 y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 25), 
                             position=position_dodge(width=0.9), 
                             vjust=0, 
                             size=7, 
                             fontface = "bold")
p_costs = p_costs + geom_text(aes(label=se, 
                                  y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 5), 
                              position=position_dodge(width=0.9), 
                              vjust=0, 
                              size=5, 
                              fontface = "bold")
p_costs



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Very Low Birthweight", "Mod. Low Birthweight"), 2), 
                 levels = c("Very Low Birthweight", "Mod. Low Birthweight")),
  Value = c(120, -1, 0.62, -0.002),
  Axis = factor(c("Left", "Left", "Right", "Right")), 
  se = c("(41)", "(36)", "(0.21)", "(0.06)")
)

# Scaling factor for right axis values
scale_factor_bw = 500/1

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
    limits = c(NA, 500) 
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
  guides(alpha = "none") +
  scale_pattern_manual(values = c("none", "stripe"))
lbw_cost = lbw_cost + geom_text(aes(label=round(Value, digits=3), 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 25),
                                position=position_dodge(width=0.9), 
                                vjust=0,
                                size=7, 
                                fontface = "bold")

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 5),
                                position=position_dodge(width=0.9), 
                                vjust=0,
                                size=5, 
                                fontface = "bold")

lbw_cost

p_costs = p_costs + guides(pattern = "none")
figure_s7 = p_costs / lbw_cost
ggsave("Figures/National Costs/figure_s7.png", figure_s7)