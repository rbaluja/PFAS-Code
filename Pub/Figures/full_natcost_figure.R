#preterm costs in 11 states
pre_mort_c11 = vpre_cost + mpre_cost + lpre_cost + mort_cost
pre_c11 = vpre_cost + mpre_cost + lpre_cost
#read in covariance terms
load(modify_path("Data_Verify/RData/cov_preterm.RData"))
load(modify_path("Data_Verify/RData/cov_mort.RData"))
#read in se terms
load(modify_path("Data_Verify/RData/preterm_sd.RData"))
load(modify_path("Data_Verify/RData/mort_sd.RData"))
#variance of preterm costs in 11 states
var_pre11 = sum(bs$pred_pfas * bs$births)^2 * 
  ((lpre_pc/10^9)^2 * lpreterm_sd^2 + (mpre_pc/10^9)^2 * mpreterm_sd^2 + (vpre_pc/10^9)^2 * vpreterm_sd^2 + (mort_pc/10^9)^2 * mort_sd^2 + #variance terms
     2 * lpre_pc/10^9 * mpre_pc/10^9 * cov_pre_lm + 2 * lpre_pc/10^9 * vpre_pc/10^9 * cov_pre_lv + 2 * lpre_pc/10^9 * mort_pc/10^9 * cov_mort_pl + #slightly covariances
     2 * mpre_pc/10^9 * vpre_pc/10^9 * cov_pre_mv + 2 * mpre_pc/10^9 * mort_pc/10^9 * cov_mort_pm + #moderately covariances
     2 * vpre_pc/10^9 * mort_pc/10^9 * cov_mort_pv) 
sd_pre11 = sqrt(var_pre11)

#low birthweight costs in 11 states
lbw_c11 = vlbw_cost + mlbw_cost
#read in covariance terms
load(modify_path("Data_Verify/RData/cov_lbw.RData"))
#read in se terms
load(modify_path("Data_Verify/RData/lbw_sd.RData"))
#variance of low birthweight costs in 11 states
var_lbw11 = sum(bs$pred_pfas * bs$births)^2 *
  ((vlbw_pc/10^9)^2 * vlbw_sd^2 + (mlbw_pc/10^9)^2 * mlbw_sd^2 + #variance terms
     2 * vlbw_pc/10^9 * mlbw_pc/10^9 * cov_lbw_mv) #covariance term
sd_lbw11 = sqrt(var_lbw11) 

#total costs (multiply by 2.91)
pre_ct = pre_c11 * 2.91
pre_mort_ct = pre_mort_c11 * 2.91
pre_ct_sd = sd_pre11 * 2.91

lbw_ct = lbw_c11 * 2.91
lbw_ct_sd = sd_lbw11 * 2.91

cost_d = data.frame(costs = c(round(pre_mort_c11, digits = 2), round(lbw_c11, digits = 2), round(pre_mort_ct, digits = 2), round(lbw_ct, digits = 2)),
                    se = c(paste0("(", round(sd_pre11, digits = 2), ")"), paste0("(", round(sd_lbw11, digits = 2), ")"), 
                           paste0("(", round(pre_ct_sd, digits = 2), ")"), paste0("(", format(round(lbw_ct_sd, digits = 2), nsmall = 2), ")")) , 
                    bout = c("Preterm (Lower) + Mortality (Upper)", "Low-Birthweight", "Preterm (Lower) + Mortality (Upper)", "Low-Birthweight"), 
                    geo = c("11 States", "11 States", "National", "National"), 
                    inner = c(pre_c11, 0, pre_ct, 0))

cost_d$geo = factor(cost_d$geo, levels = c("11 States", "National"))
cost_d$bout = factor(cost_d$bout, levels = c("Preterm (Lower) + Mortality (Upper)", "Low-Birthweight"))

# Create the bar chart using ggpattern
cost_hist = ggplot(cost_d, aes(x = bout, y = costs, fill = geo)) +
  geom_bar_pattern(
    stat = "identity", 
    position = position_dodge(width = 0.9),
    aes(pattern = geo),
    pattern_fill = "black", 
    pattern_density = 0.1, 
    pattern_spacing = 0.02, 
    pattern_key_scale_factor = 0.3, 
    alpha = 0.7,
    width = 0.8
  ) +
  geom_bar(
    aes(y = inner, group = geo),
    stat = "identity",
    position = position_dodge(width = 0.9),
    fill = "black",
    alpha = 0.6,
    width = 0.8
  ) + 
  labs(y = "Annual Costs ($ Billion)", fill = "") +
  scale_fill_manual(name = "", 
                    values = c(`11 States` = "orchid4", National = "seagreen4"), 
                    labels = c("11 States", "National")) +
  scale_pattern_manual(name = "", 
                       values = c(`11 States` = "none", National = "stripe"), 
                       labels = c("11 States", "National")) + 
  xlab("") + 
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.size = unit(4, "lines"),
    legend.title = element_blank(),
    axis.title = element_text(size = 60),
    legend.text = element_text(size = 60),
    axis.text = element_text(size = 50),
    plot.title = element_text(hjust = 0.5, size = 80), 
    panel.grid.major = element_line(color = "grey60", size = 0.5),
    panel.grid.minor = element_line(color = "grey60", size = 0.25),
    legend.spacing.x = unit(1, 'cm')
  ) + 
  guides(alpha = "none")

cost_hist +
  geom_text(aes(label = costs, y = costs + 0.6), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25,
            size = 16) +
  geom_text(aes(label = se, y = costs + 0.2), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25,
            size = 14)

ggsave(modify_path3("Figures/Figure3/full_cost.png"), width = 10358, height = 6133, units = "px", limitsize = FALSE)
