reg_data$pre_dlower = reg_data$pre_down - 1.96 * reg_data$pre_dse
reg_data$pre_dupper = reg_data$pre_down + 1.96 * reg_data$pre_dse

reg_data$pre_udlower = reg_data$pre_updown - 1.96 * reg_data$pre_udse
reg_data$pre_udupper = reg_data$pre_updown + 1.96 * reg_data$pre_udse


reg_data$lpre_dlower = reg_data$lpre_down - 1.96 * reg_data$lpre_dse
reg_data$lpre_dupper = reg_data$lpre_down + 1.96 * reg_data$lpre_dse

reg_data$lpre_udlower = reg_data$lpre_updown - 1.96 * reg_data$lpre_udse
reg_data$lpre_udupper = reg_data$lpre_updown + 1.96 * reg_data$lpre_udse

reg_data$mpre_dlower = reg_data$mpre_down - 1.96 * reg_data$mpre_dse
reg_data$mpre_dupper = reg_data$mpre_down + 1.96 * reg_data$mpre_dse

reg_data$mpre_udlower = reg_data$mpre_updown - 1.96 * reg_data$mpre_udse
reg_data$mpre_udupper = reg_data$mpre_updown + 1.96 * reg_data$mpre_udse

reg_data$vpre_dlower = reg_data$vpre_down - 1.96 * reg_data$vpre_dse
reg_data$vpre_dupper = reg_data$vpre_down + 1.96 * reg_data$vpre_dse

reg_data$vpre_udlower = reg_data$vpre_updown - 1.96 * reg_data$vpre_udse
reg_data$vpre_udupper = reg_data$vpre_updown + 1.96 * reg_data$vpre_udse

# Create adjusted datasets
reg_data_upgradient = reg_data

reg_data_upgradient$pre_down_p_label = ifelse(reg_data_upgradient$pre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$pre_down_p))
reg_data_upgradient$lpre_down_p_label = ifelse(reg_data_upgradient$lpre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$lpre_down_p))
reg_data_upgradient$mpre_down_p_label =  ifelse(reg_data_upgradient$mpre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$mpre_down_p))
reg_data_upgradient$vpre_down_p_label =  ifelse(reg_data_upgradient$vpre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$vpre_down_p))


p_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = pre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = pre_dlower, ymax = pre_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = pre_dupper, label = pre_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Any (<37 Weeks)") + 
  xlab("") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24), 
        plot.title = element_text(hjust = 0.5, size = 34)) +
  guides(color = "none", fill = "none")+ ggtitle("Preterm") + 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE) + 
  ylim(-0.06, 0.065)


lp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = lpre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = lpre_dlower, ymax = lpre_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = lpre_dupper, label = lpre_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Moderately (32-36 Weeks)") + 
  xlab("") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25)+  guides(color = FALSE)+ 
  ylim(-0.06, 0.065)

mp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = mpre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = mpre_dlower, ymax = mpre_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = mpre_dupper, label = mpre_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Very (28-31 Weeks)") + 
  xlab("") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25)+  guides(color = FALSE)+ 
  ylim(-0.06, 0.065)


vp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = vpre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = vpre_dlower, ymax = vpre_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = vpre_dupper, label = vpre_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Extremely (<28 Weeks)") + 
  theme_minimal() + 
  xlab("Threshold (ppt)") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 26), 
        axis.title.x = element_text(size = 28), 
        legend.text = element_text(size = 24),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + 
  ylim(-0.06, 0.065)

reg_data$lbw_dlower = reg_data$lbw_down - 1.96 * reg_data$lbw_dse
reg_data$lbw_dupper = reg_data$lbw_down + 1.96 * reg_data$lbw_dse

reg_data$lbw_udlower = reg_data$lbw_updown - 1.96 * reg_data$lbw_udse
reg_data$lbw_udupper = reg_data$lbw_updown + 1.96 * reg_data$lbw_udse

reg_data$llbw_dlower = reg_data$llbw_down - 1.96 * reg_data$llbw_dse
reg_data$llbw_dupper = reg_data$llbw_down + 1.96 * reg_data$llbw_dse

reg_data$llbw_udlower = reg_data$llbw_updown - 1.96 * reg_data$llbw_udse
reg_data$llbw_udupper = reg_data$llbw_updown + 1.96 * reg_data$llbw_udse

reg_data$vlbw_dlower = reg_data$vlbw_down - 1.96 * reg_data$vlbw_dse
reg_data$vlbw_dupper = reg_data$vlbw_down + 1.96 * reg_data$vlbw_dse

reg_data$vlbw_udlower = reg_data$vlbw_updown - 1.96 * reg_data$vlbw_udse
reg_data$vlbw_udupper = reg_data$vlbw_updown + 1.96 * reg_data$vlbw_udse

reg_data$elbw_dlower = reg_data$elbw_down - 1.96 * reg_data$elbw_dse
reg_data$elbw_dupper = reg_data$elbw_down + 1.96 * reg_data$elbw_dse

reg_data$elbw_udlower = reg_data$elbw_updown - 1.96 * reg_data$elbw_udse
reg_data$elbw_udupper = reg_data$elbw_updown + 1.96 * reg_data$elbw_udse



reg_data_upgradient$lbw_down_p_label = ifelse(reg_data_upgradient$lbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$lbw_down_p))
reg_data_upgradient$llbw_down_p_label = ifelse(reg_data_upgradient$llbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$llbw_down_p))
reg_data_upgradient$vlbw_down_p_label = ifelse(reg_data_upgradient$vlbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$vlbw_down_p))
reg_data_upgradient$elbw_down_p_label = ifelse(reg_data_upgradient$elbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$elbw_down_p))

lbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = lbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = lbw_dlower, ymax = lbw_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = lbw_dupper, label = lbw_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Any (<2500g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24), 
        plot.title = element_text(hjust = 0.5, size = 34)) +
  guides(color = "none", fill = "none")+ ggtitle("Low Birthweight") + 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+ 
  ylim(-0.06, 0.065)


llbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = llbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = llbw_dlower, ymax = llbw_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = llbw_dupper, label = llbw_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Moderately (1500-2499g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+ 
  ylim(-0.06, 0.065)

vlbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = vlbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = vlbw_dlower, ymax = vlbw_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = vlbw_dupper, label = vlbw_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Very (1000-1499g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+ 
  ylim(-0.06, 0.065)


elbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = elbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = elbw_dlower, ymax = elbw_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = elbw_dupper, 
                                            label = elbw_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Extremely (<1000g)") + 
  theme_minimal() + 
  xlab("Threshold (ppt)") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 26), 
        axis.title.x = element_text(size = 28), 
        legend.text = element_text(size = 24),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + 
  ylim(-0.06, 0.065)

#infant mortality
reg_data$mort_dlower = reg_data$mort_down - 1.96 * reg_data$mort_dse
reg_data$mort_dupper = reg_data$mort_down + 1.96 * reg_data$mort_dse


reg_data_upgradient$mort_down_p_label = ifelse(reg_data_upgradient$mort_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$mort_down_p))

mort_fig = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = mort_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = mort_dlower, ymax = mort_dupper), width = 0.3) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = mort_dupper, 
                                            label = mort_down_p_label), nudge_y = 0.01, size = 6) +
  ylab("Infant Mortality") + 
  theme_minimal() + 
  xlab("Threshold (ppt)") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 26), 
        axis.title.x = element_text(size = 28), 
        legend.text = element_text(size = 24),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + 
  ylim(-0.06, 0.065)



figure_s3 = (p_combined | lbw_combined) / (lp_combined | llbw_combined) / (mp_combined | vlbw_combined) / (vp_combined | elbw_combined)
ggsave("Figures Revision/threshold_fig.png", figure_s3, width = 10000, height = 7000, units = "px", dpi = 300)
ggsave("Figures Revision/threshold_fig_mort.png", mort_fig, width = 7500, height = 1750, units = "px")


