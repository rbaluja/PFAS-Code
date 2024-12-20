source("PFAS-Code/Pub/config.R")

source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/figure2_rob_fns.R")
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/figure2_rob_mort_fns.R")
#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}
ppt = 1000
load(modify_path(paste0("Data_Verify/RData/preterm_iv_coef", ppt, ".RData")))
pre1000 = preterm_iv
lpre1000 = lpreterm_iv
mpre1000 = mpreterm_iv
vpre1000 = vpreterm_iv

load(modify_path(paste0("Data_Verify/RData/lbw_iv_coef", ppt, ".RData")))
lbw1000 = lbw_iv
llbw1000 = llbw_iv
mlbw1000 = mlbw_iv
vlbw1000 = vlbw_iv

load(modify_path(paste0("Data_Verify/RData/mort_iv_coef", ppt, ".RData")))
mort1000 = mort_iv


load(modify_path(paste0("Data_Verify/RData/preterm_sd", ppt, ".RData")))
pre1000_sd = preterm_sd
lpre1000_sd = lpreterm_sd
mpre1000_sd = mpreterm_sd
vpre1000_sd = vpreterm_sd

load(modify_path(paste0("Data_Verify/RData/lbw_sd", ppt, ".RData")))
lbw1000_sd = lbw_sd
llbw1000_sd = llbw_sd
mlbw1000_sd = mlbw_sd
vlbw1000_sd = vlbw_sd

load(modify_path(paste0("Data_Verify/RData/mort_sd", ppt, ".RData")))
mort1000_sd = mort_sd




ppt = 500
load(modify_path(paste0("Data_Verify/RData/preterm_iv_coef", ppt, ".RData")))
pre500 = preterm_iv
lpre500 = lpreterm_iv
mpre500 = mpreterm_iv
vpre500 = vpreterm_iv

load(modify_path(paste0("Data_Verify/RData/lbw_iv_coef", ppt, ".RData")))
lbw500 = lbw_iv
llbw500 = llbw_iv
mlbw500 = mlbw_iv
vlbw500 = vlbw_iv

load(modify_path(paste0("Data_Verify/RData/mort_iv_coef", ppt, ".RData")))
mort500 = mort_iv


load(modify_path(paste0("Data_Verify/RData/preterm_sd", ppt, ".RData")))
pre500_sd = preterm_sd
lpre500_sd = lpreterm_sd
mpre500_sd = mpreterm_sd
vpre500_sd = vpreterm_sd

load(modify_path(paste0("Data_Verify/RData/lbw_sd", ppt, ".RData")))
lbw500_sd = lbw_sd
llbw500_sd = llbw_sd
mlbw500_sd = mlbw_sd
vlbw500_sd = vlbw_sd

load(modify_path(paste0("Data_Verify/RData/mort_sd", ppt, ".RData")))
mort500_sd = mort_sd

data_pre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(pre500, pre1000),
  StdError = c(pre500_sd, pre1000_sd),
  pval = c(1 - pnorm(pre500/pre500_sd), 1 - pnorm(pre1000/pre1000_sd))
)


data_pre$Check = factor(data_pre$Check, c("500 ppt", "1000 ppt"))


data_pre$down = data_pre$Estimate
data_pre$up = data_pre$Upgradient
data_pre$d_lower = data_pre$down - 1.96 * data_pre$StdError
data_pre$d_upper = data_pre$down + 1.96 * data_pre$StdError
data_pre$pval_label = sprintf("%.5f", data_pre$pval)
data_pre$health_outcome = "preterm"

pre_any = figure2_fun_ivthresh(data_pre, "Any", FALSE, TRUE, "Any", TRUE)




#Moderately preterm
data_mpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(lpre500, lpre1000),
  StdError = c(lpre500_sd, lpre1000_sd),
  pval = c(1 - pnorm(lpre500/lpre500_sd), 1 - pnorm(lpre1000/lpre1000_sd))
)


data_mpre$Check = factor(data_mpre$Check, c("500 ppt", "1000 ppt"))


data_mpre$down = data_mpre$Estimate
data_mpre$up = data_mpre$Upgradient
data_mpre$d_lower = data_mpre$down - 1.96 * data_mpre$StdError
data_mpre$d_upper = data_mpre$down + 1.96 * data_mpre$StdError
data_mpre$pval_label = sprintf("%.5f", data_mpre$pval)
data_mpre$health_outcome = "preterm"

mpre = figure2_fun_ivthresh(data_mpre, "Moderately", FALSE, FALSE, "Moderately", TRUE)



#Very preterm
data_vpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(mpre500, mpre1000),
  StdError = c(mpre500_sd, mpre1000_sd),
  pval = c(1 - pnorm(mpre500/mpre500_sd), 1 - pnorm(mpre1000/mpre1000_sd))
)


data_vpre$Check = factor(data_vpre$Check, c("500 ppt", "1000 ppt"))


data_vpre$down = data_vpre$Estimate
data_vpre$up = data_vpre$Upgradient
data_vpre$d_lower = data_vpre$down - 1.96 * data_vpre$StdError
data_vpre$d_upper = data_vpre$down + 1.96 * data_vpre$StdError
data_vpre$pval_label = sprintf("%.5f", data_vpre$pval)
data_vpre$health_outcome = "preterm"

vpre = figure2_fun_ivthresh(data_vpre, "Very", FALSE, FALSE, "Very", TRUE)


#Extremeley preterm
data_epre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(vpre500, vpre1000),
  StdError = c(vpre500_sd, vpre1000_sd),
  pval = c(1 - pnorm(vpre500/vpre500_sd), 1 - pnorm(vpre1000/vpre1000_sd))
)


data_epre$Check = factor(data_epre$Check, c("500 ppt", "1000 ppt"))


data_epre$down = data_epre$Estimate
data_epre$up = data_epre$Upgradient
data_epre$d_lower = data_epre$down - 1.96 * data_epre$StdError
data_epre$d_upper = data_epre$down + 1.96 * data_epre$StdError
data_epre$pval_label = sprintf("%.5f", data_epre$pval)
data_epre$health_outcome = "preterm"

epre = figure2_fun_ivthresh(data_epre, "Extremely", TRUE, FALSE, "Extremely", TRUE)

pre_fig = pre_any/mpre/vpre/epre




#birthweight
#low birthweight
data_lbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(lbw500, lbw1000),
  StdError = c(lbw500_sd, lbw1000_sd),
  pval = c(1 - pnorm(lbw500/lbw500_sd), 1 - pnorm(lbw1000/lbw1000_sd))
)


data_lbw$Check = factor(data_lbw$Check, c("500 ppt", "1000 ppt"))


data_lbw$down = data_lbw$Estimate
data_lbw$up = data_lbw$Upgradient
data_lbw$d_lower = data_lbw$down - 1.96 * data_lbw$StdError
data_lbw$d_upper = data_lbw$down + 1.96 * data_lbw$StdError
data_lbw$pval_label = sprintf("%.5f", data_lbw$pval)
data_lbw$health_outcome = "lbw"

lbw_any = figure2_fun_ivthresh(data_lbw, "Any", FALSE, TRUE, "Any", FALSE)




#Moderately lbw
data_mlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(llbw500, llbw1000),
  StdError = c(llbw500_sd, llbw1000_sd),
  pval = c(1 - pnorm(llbw500/llbw500_sd), 1 - pnorm(llbw1000/llbw1000_sd))
)


data_mlbw$Check = factor(data_mlbw$Check, c("500 ppt", "1000 ppt"))


data_mlbw$down = data_mlbw$Estimate
data_mlbw$up = data_mlbw$Upgradient
data_mlbw$d_lower = data_mlbw$down - 1.96 * data_mlbw$StdError
data_mlbw$d_upper = data_mlbw$down + 1.96 * data_mlbw$StdError
data_mlbw$pval_label = sprintf("%.5f", data_mlbw$pval)
data_mlbw$health_outcome = "lbw"

mlbw = figure2_fun_ivthresh(data_mlbw, "Moderately", FALSE, FALSE, "Moderately", FALSE)



#Very lbw
data_vlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(mlbw500, mlbw1000),
  StdError = c(mlbw500_sd, mlbw1000_sd),
  pval = c(1 - pnorm(mlbw500/mlbw500_sd), 1 - pnorm(mlbw1000/mlbw1000_sd))
)


data_vlbw$Check = factor(data_vlbw$Check, c("500 ppt", "1000 ppt"))


data_vlbw$down = data_vlbw$Estimate
data_vlbw$up = data_vlbw$Upgradient
data_vlbw$d_lower = data_vlbw$down - 1.96 * data_vlbw$StdError
data_vlbw$d_upper = data_vlbw$down + 1.96 * data_vlbw$StdError
data_vlbw$pval_label = sprintf("%.5f", data_vlbw$pval)
data_vlbw$health_outcome = "lbw"

vlbw = figure2_fun_ivthresh(data_vlbw, "Very", FALSE, FALSE, "Very", FALSE)



#Extremeley lbw
data_elbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(vlbw500, vlbw1000),
  StdError = c(vlbw500_sd, vlbw1000_sd),
  pval = c(1 - pnorm(vlbw500/vlbw500_sd), 1 - pnorm(vlbw1000/vlbw1000_sd))
)


data_elbw$Check = factor(data_elbw$Check, c("500 ppt", "1000 ppt"))


data_elbw$down = data_elbw$Estimate
data_elbw$up = data_elbw$Upgradient
data_elbw$d_lower = data_elbw$down - 1.96 * data_elbw$StdError
data_elbw$d_upper = data_elbw$down + 1.96 * data_elbw$StdError
data_elbw$pval_label = sprintf("%.5f", data_elbw$pval)
data_elbw$health_outcome = "lbw"

elbw = figure2_fun_ivthresh(data_elbw, "Extremely", TRUE, FALSE, "Extremely", FALSE)


lbw = lbw_any/mlbw/vlbw/elbw

legend_data <- data.frame(
  category = factor(c("Any", "Moderately", "Very", "Extremely"), levels = c("Any", "Moderately", "Very", "Extremely")),
  color = c("dodgerblue", "coral", "darkseagreen", "orchid4")
)

# Create a dummy plot for the legend
lplot = ggplot(legend_data) +
  geom_point(aes(x = category, y = 1, color = category), shape = 16) +
  scale_color_manual(values = legend_data$color) +
  theme_void() + 
  theme(legend.position = "bottom", legend.title = element_text(size = 40), legend.text = element_text(size = 40)) +   
  guides(color = guide_legend(title = "Severity", override.aes = list(size = 8))) + ylim(0, 0.1)


ptplot = ggplot() +
  labs(title = "Preterm") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.9, size = 70, face = "bold"))
btplot = ggplot() +
  labs(title = "Low Birthweight") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.73, size = 70, face = "bold"))

title = (ptplot | btplot)
main_fig = (pre_fig | lbw) + plot_layout(widths = c(1.5, 1))

fig2 = (title/main_fig)  + plot_layout(heights = c(0.5, 50))

ggsave("Figures Revision/figure2_ivthresh.png", fig2, width = 17000, height = 5500, units = "px", limitsize = F)



#infant mortality
data_mort = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("500 ppt", "1000 ppt"),
  Estimate = c(mort500, mort1000),
  StdError = c(mort500_sd, mort1000_sd),
  pval = c(1 - pnorm(mort500/mort500_sd), 1 - pnorm(mort1000/mort1000_sd))
)


data_mort$Check = factor(data_mort$Check, c("500 ppt", "1000 ppt"))


data_mort$down = data_mort$Estimate
data_mort$up = data_mort$Upgradient
data_mort$d_lower = data_mort$down - 1.96 * data_mort$StdError
data_mort$d_upper = data_mort$down + 1.96 * data_mort$StdError
data_mort$pval_label = sprintf("%.5f", data_mort$pval)
data_mort$health_outcome = "lbw"

mort_f2 = figure2_still_fun_ivthresh(data_mort, "Infant Mortality", TRUE, TRUE, "Infant Mortality", FALSE)
mort_f2 = mort_f2 + ggtitle("Infant Mortality") + theme_void() + theme(plot.title = element_text(hjust = -1.5, size = 70, face = "bold"))
ggsave("Figures Revision/figure2_mort_ivthresh.png", mort_f2, width = 11000, height = 2000, units = "px", limitsize = F)
