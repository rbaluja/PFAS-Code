load("Data Revisions/RData/reg_data_thresh.RData")
load(paste0(natality_path, "[UA Box Health] birth_records_matched_cv_", code_verify, ".RData")) 

#get death rate
path = paste0(natality_path, "[UA Box Health] VR2210_Deliverable/dr_6264_deliverable.xlsx")
df_d = read_excel(path, sheet = 3)
df = df %>% 
  left_join(df_d %>% 
              dplyr::rename(age_death = DECD_AGE_YR, 
                            manner_death = CERTFR_MANNER_DTH_CD, 
                            id = BRTH_CERT_FILE_NBR))

df$death = as.numeric(!is.na(df$age_death))

#subset to every 200 ppt
#reg_data = reg_data %>% dplyr::filter(threshold %% 200 == 0)

#subset to above 0 and below 1000
reg_data = reg_data %>% dplyr::filter(threshold > 0 & threshold <= 1000)
reg_data$pre_down = reg_data$pre_down/mean(df$gestation < 37) * 100
reg_data$lpre_down = reg_data$lpre_down/mean(df$gestation < 37 & df$gestation >= 32) * 100
reg_data$mpre_down = reg_data$mpre_down/mean(df$gestation < 32 & df$gestation >= 28) * 100
reg_data$vpre_down = reg_data$vpre_down/mean(df$gestation < 28) * 100

reg_data$lbw_down = reg_data$lbw_down/mean(df$bweight < 2500) * 100
reg_data$llbw_down = reg_data$llbw_down/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
reg_data$vlbw_down = reg_data$vlbw_down/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
reg_data$elbw_down = reg_data$elbw_down/mean(df$bweight < 1000) * 100

reg_data$mort_down = reg_data$mort_down/mean(df$death) * 100

reg_data$pre_dse = reg_data$pre_dse/mean(df$gestation < 37) * 100
reg_data$lpre_dse = reg_data$lpre_dse/mean(df$gestation < 37 & df$gestation >= 32) * 100
reg_data$mpre_dse = reg_data$mpre_dse/mean(df$gestation < 32 & df$gestation >= 28) * 100
reg_data$vpre_dse = reg_data$vpre_dse/mean(df$gestation < 28) * 100

reg_data$lbw_dse = reg_data$lbw_dse/mean(df$bweight < 2500) * 100
reg_data$llbw_dse = reg_data$llbw_dse/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
reg_data$vlbw_dse = reg_data$vlbw_dse/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
reg_data$elbw_dse = reg_data$elbw_dse/mean(df$bweight < 1000) * 100

reg_data$mort_dse = reg_data$mort_dse/mean(df$death) * 100

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

reg_data_upgradient$pre_down_p_label = ifelse(reg_data_upgradient$pre_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$pre_down_p))
reg_data_upgradient$lpre_down_p_label = ifelse(reg_data_upgradient$lpre_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$lpre_down_p))
reg_data_upgradient$mpre_down_p_label =  ifelse(reg_data_upgradient$mpre_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$mpre_down_p))
reg_data_upgradient$vpre_down_p_label =  ifelse(reg_data_upgradient$vpre_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$vpre_down_p))


p_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = pre_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = pre_dlower, ymax = pre_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = pre_dupper, label = pre_down_p_label), nudge_y = 50, size = 10) +
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
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500))+ 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))


lp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = lpre_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = lpre_dlower, ymax = lpre_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = lpre_dupper, label = lpre_down_p_label), nudge_y = 50, size = 10) +
  ylab("Moderately (32-36 Weeks)") + 
  xlab("") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25)+  guides(color = FALSE)+
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))

mp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = mpre_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = mpre_dlower, ymax = mpre_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = mpre_dupper, label = mpre_down_p_label), nudge_y = 50, size = 10) +
  ylab("Very (28-31 Weeks)") + 
  xlab("") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25)+  guides(color = FALSE)+
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))


vp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = vpre_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = vpre_dlower, ymax = vpre_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = vpre_dupper, label = vpre_down_p_label), nudge_y = 50, size = 10) +
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
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))

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


reg_data_upgradient = reg_data
reg_data_upgradient$lbw_down_p_label = ifelse(reg_data_upgradient$lbw_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$lbw_down_p))
reg_data_upgradient$llbw_down_p_label = ifelse(reg_data_upgradient$llbw_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$llbw_down_p))
reg_data_upgradient$vlbw_down_p_label = ifelse(reg_data_upgradient$vlbw_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$vlbw_down_p))
reg_data_upgradient$elbw_down_p_label = ifelse(reg_data_upgradient$elbw_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$elbw_down_p))

lbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = lbw_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = lbw_dlower, ymax = lbw_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = lbw_dupper, label = lbw_down_p_label), nudge_y = 50, size = 10) +
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
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))


llbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = llbw_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = llbw_dlower, ymax = llbw_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = llbw_dupper, label = llbw_down_p_label), nudge_y = 50, size = 10) +
  ylab("Moderately (1500-2499g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))

vlbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = vlbw_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = vlbw_dlower, ymax = vlbw_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = vlbw_dupper, label = vlbw_down_p_label), nudge_y = 50, size = 10) +
  ylab("Very (1000-1499g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))


elbw_combined = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = elbw_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = elbw_dlower, ymax = elbw_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = elbw_dupper, 
                                            label = elbw_down_p_label), nudge_y = 50, size = 10) +
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
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))

#infant mortality
reg_data$mort_dlower = reg_data$mort_down - 1.96 * reg_data$mort_dse
reg_data$mort_dupper = reg_data$mort_down + 1.96 * reg_data$mort_dse

reg_data_upgradient = reg_data
reg_data_upgradient$mort_down_p_label = ifelse(reg_data_upgradient$mort_down_p < 0.01, "<0.01", sprintf("%.2f", reg_data_upgradient$mort_down_p))

mort_fig = ggplot(reg_data, aes(x = threshold)) + 
  geom_point(data = reg_data_upgradient, aes(x = threshold, y = mort_down), size = 2) +
  geom_errorbar(data = reg_data_upgradient, aes(x = threshold, ymin = mort_dlower, ymax = mort_dupper), width = 20) +
  geom_text(data = reg_data_upgradient, aes(x = threshold, y = mort_dupper, 
                                            label = mort_down_p_label), nudge_y = 50, size = 10) +
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
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(-400, 500)) + 
  scale_x_continuous(breaks = seq(from = 100, to = 1000, by = 100))



figure_s3 = (p_combined | lbw_combined) / (lp_combined | llbw_combined) / (mp_combined | vlbw_combined) / (vp_combined | elbw_combined)
ggsave("Figures Revision/threshold_fig.png", figure_s3, width = 10000, height = 7000, units = "px", dpi = 300)
ggsave("Figures Revision/threshold_fig_mort.png", mort_fig, width = 7500, height = 1750, units = "px")


