#preterm costs in 11 states
pre_c11 = vpre_cost + mpre_cost + lpre_cost
#read in covariance terms
load(modify_path("Data_Verify/RData/cov_preterm.RData"))
#read in se terms
load(modify_path("Data_Verify/RData/preterm_sd.RData"))
#variance of preterm costs in 11 states
var_pre11 = sum(bs$pred_pfas * bs$births)^2 * 
  ((36728/10^9)^2 * lpreterm_sd^2 + (205041/10^9)^2 * mpreterm_sd^2 + (204083/10^9)^2 * vpreterm_sd^2 + #variance terms
     2 * 36728/10^9 * 205041/10^9 * cov_pre_lm + 2 * 36728/10^9 * 204083/10^9 * cov_pre_lv + 2 * 205041/10^9 * 204083/10^9 * cov_pre_mv) #covariance terms
sd_pre11 = sqrt(var_pre11)

#low birthweight costs in 11 states
lbw_c11 = vlbw_cost + mlbw_cost
#read in covariance terms
load(modify_path("Data_Verify/RData/cov_lbw.RData"))
#read in se terms
load(modify_path("Data_Verify/RData/lbw_sd.RData"))
#variance of low birthweight costs in 11 states
var_lbw11 = sum(bs$pred_pfas * bs$births)^2 *
  ((5133739.83/10^9)^2 * vlbw_sd^2 + (1634411.22/10^9)^2 * mlbw_sd^2 + #variance terms
     2 * 5133739.83/10^9 * 1634411.22/10^9 * cov_lbw_mv) #covariance term
sd_lbw11 = sqrt(var_lbw11) 

#total costs (multiply by 2.91)
pre_ct = pre_c11 * 2.91
pre_ct_sd = sd_pre11 * 2.91

lbw_ct = lbw_c11 * 2.91
lbw_ct_sd = sd_lbw11 * 2.91

cost_d = data.frame(costs = c(round(pre_c11, digits = 2), round(lbw_c11, digits = 2), round(pre_ct, digits = 2), round(lbw_ct, digits = 2)),
                    se = c(paste0("(", round(sd_pre11, digits = 2), ")"), paste0("(", round(sd_lbw11, digits = 2), ")"), 
                           paste0("(", round(pre_ct_sd, digits = 2), ")"), paste0("(", round(lbw_ct_sd, digits = 2), ")")) , 
                    bout = c("Preterm", "Low-Birthweight", "Preterm", "Low-Birthweight"), 
                    geo = c("11 States", "11 States", "National", "National"))

cost_d$geo = factor(cost_d$geo, levels = c("11 States", "National"))
cost_d$bout = factor(cost_d$bout, levels = c("Preterm", "Low-Birthweight"))

# Create the bar chart using ggpattern
cost_hist = ggplot(cost_d, aes(x = bout, y = costs, fill = geo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha=0.8) +
  labs(y = "Annual Cost ($ Billion)",
       fill = "") +
  xlab("") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(4, "lines"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 60),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 60),
        axis.text.x = element_text(size = 50),
        plot.title = element_text(hjust = 0.5, size = 80), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25),
        axis.text.y.right = element_text(size = 60)
        ) + 
  scale_fill_manual(values=c("11 States" = "dodgerblue3", "National" = "firebrick4"))

cost_hist +
  geom_text(aes(label = costs, y = costs + 0.6), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25,
            size = 16) +
  geom_text(aes(label = se, y = costs + 0.2), 
            position = position_dodge(width = 0.9), 
            vjust = -0.25,
            size = 14)

ggsave(modify_path3("Figures/Figure3/full_cost.png"), scale = 3)
