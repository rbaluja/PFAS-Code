#getting impacts in states with initiatives
add_vpre = sum(bs[which(bs$down == 1), ]$births) * 0.0047 # 92.50363
#very preterm cost:  92.50363 * 204083 =  18878418/10^9
#standard error: 
add_vpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0018 #35.42692
#cost se: 35.42692 *204083 = 7230032/10^9

add_mpre = sum(bs[which(bs$down == 1), ]$births) * 0.0025 #49.20406
#m preterm cost se: 49.20406 * 205041  =  1887210/10^9
#standard error: 
add_mpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0027 #53.14038
#cost se: 53.14038 * 205041  = 10895957/10^9

add_lpre = sum(bs[which(bs$down == 1), ]$births) * 0.0076 # 149.5803
#late preterm cost:  149.5803 * 36728.05 = 5493793/10^9
#standard error: 
add_lpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0079 # 155.4848
#cost se: 155.4848 * 36728.05 = 5710654/10^9



#birthweight
add_vlbw = sum(bs[which(bs$down == 1), ]$births) * 0.0061 #120.0579
#very lbw cost:120.0579 * 5133739.83 = 616346023/10^9
#standard error: 
add_vlbw_se =sum(bs[which(bs$down == 1), ]$births) * 0.0021 #41.33141
#cost se: 41.33141 * 5133739.83 = 212184706/10^9

add_mlbw = sum(bs[which(bs$down == 1), ]$births) * -0.00005 #-0.9840812
#mod lbw cost: -0.9840812* 1634411.22 = -1608393/10^9
#standard error: 
add_mlbw_se =sum(bs[which(bs$down == 1), ]$births) * 0.00185 #36.411
#cost se: 36.411 * 1634411.22 = 59510547/10^9


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Very Preterm", "Mod. Preterm", "Late Preterm"), 2), 
                 levels = c("Very Preterm", "Mod. Preterm", "Late Preterm")),
  Value = c(93, 49, 150, 0.02, 0.002, 0.005), # Combined values for both axes
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
    limits = c(NA, 500) # Set the upper limit to a higher value
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
                                 y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 25), # Adjust the offset as needed
                             position=position_dodge(width=0.9), 
                             vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                             size=7, 
                             fontface = "bold")
p_costs = p_costs + geom_text(aes(label=se, 
                                  y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 5), # Adjust the offset as needed
                              position=position_dodge(width=0.9), 
                              vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                              size=5, 
                              fontface = "bold")
p_costs



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Very Low Birthweight", "Mod. Low Birthweight"), 2), 
                 levels = c("Very Low Birthweight", "Mod. Low Birthweight")),
  Value = c(120, -1, 0.61, -0.002), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Right", "Right")), # Axis assignment
  se = c("(41)", "(36)", "(0.21)", "(0.06)")
)

# Scaling factor for right axis values
#scale_factor_bw = max(data_bw$Value[data_bw$Axis == "Left"]) / max(data_bw$Value[data_bw$Axis == "Right"])
scale_factor_bw = 500/1

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
    limits = c(NA, 500) # Set the upper limit to a higher value
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
lbw_cost = lbw_cost + geom_text(aes(label=round(Value, digits=3), 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 25), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=7, 
                                fontface = "bold")

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 5), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=5, 
                                fontface = "bold")

lbw_cost

p_costs = p_costs + guides(pattern = "none")
p_costs / lbw_cost
