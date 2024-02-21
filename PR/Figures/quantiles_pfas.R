#set up quantiles and pfas in ppb (pred_pfas_level)
df$pred_pfas_level = sinh(df$pred_pfas)/1000
df_nn = df[which(!is.na(df$pred_pfas)), ]
quantiles = quantile(df_nn$pred_pfas, c(0, 0.2, 0.4, 0.6, 0.8, 1))
df_nn$quant_pfas = as.integer(cut(df_nn$pred_pfas, breaks = quantiles, include.lowest = TRUE, labels = 1:5))

preterm = fixest::feols(preterm ~ as.factor(quant_pfas) + asinh(pfas) + 
                          n_sites + wind_exposure + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)


lpreterm = fixest::feols(I(gestation < 37 & gestation >= 32) ~ as.factor(quant_pfas) + asinh(pfas) + 
                           n_sites + wind_exposure + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)


mpreterm = fixest::feols(I(gestation < 32 & gestation >= 28) ~ as.factor(quant_pfas) + asinh(pfas) + 
                           n_sites + wind_exposure + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)


vpreterm = fixest::feols(I(gestation < 28) ~ as.factor(quant_pfas) + asinh(pfas) + 
                           n_sites + wind_exposure + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)


lbw = fixest::feols(I(bweight < 2500) ~  as.factor(quant_pfas) + asinh(pfas) + 
                      n_sites + wind_exposure + 
                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)



llbw = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  as.factor(quant_pfas) + asinh(pfas) + 
                       n_sites + wind_exposure + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)



mlbw = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  as.factor(quant_pfas) + asinh(pfas) + 
                       n_sites + wind_exposure + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)


vlbw = fixest::feols(I(bweight < 1000) ~  as.factor(quant_pfas) + asinh(pfas) + 
                       n_sites + wind_exposure + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn)


#saving data
reg_data = data.frame(matrix(ncol = 25, nrow = 0))
colnames(reg_data) = c("pre_coef", "pre_se", "pre_p",
                       "lpre_coef", "lpre_se", "lpre_p",
                       "mpre_coef", "mpre_se","mpre_p",
                       "vpre_coef", "vpre_se", "vpre_p",
                       "lbw_coef","lbw_se", "lbw_p",
                       "llbw_coef","llbw_se","llbw_p",
                       "mlbw_coef","mlbw_se", "mlbw_p",
                       "vlbw_coef", "vlbw_se", "vlbw_p", "quantile")


index = 2
for (i in 2:5){
  label = paste0("as.factor(quant_pfas)", i)
  reg_data[index, "pre_coef"] =  preterm$coefficients[label]
  reg_data[index, "mpre_coef"] =  mpreterm$coefficients[label]
  reg_data[index, "lpre_coef"] =  lpreterm$coefficients[label]
  reg_data[index, "vpre_coef"] =  vpreterm$coefficients[label]
  
  reg_data[index, "lbw_coef"] = lbw$coefficients[label]
  reg_data[index, "llbw_coef"] = llbw$coefficients[label]
  reg_data[index, "mlbw_coef"] = mlbw$coefficients[label]
  reg_data[index, "vlbw_coef"] = vlbw$coefficients[label]
  reg_data[index, "quantile"] = index
  
  
  index = index + 1
}

#need to run boostrap quantiles to get these values
reg_data$pre_se[2] = p2_sd
reg_data$pre_se[3] = p3_sd
reg_data$pre_se[4] = p4_sd
reg_data$pre_se[5] = p5_sd

reg_data$lpre_se[2] = lp2_sd
reg_data$lpre_se[3] = lp3_sd
reg_data$lpre_se[4] = lp4_sd
reg_data$lpre_se[5] = lp5_sd

reg_data$mpre_se[2] = mp2_sd
reg_data$mpre_se[3] = mp3_sd
reg_data$mpre_se[4] = mp4_sd
reg_data$mpre_se[5] = mp5_sd

reg_data$vpre_se[2] = vp2_sd
reg_data$vpre_se[3] = vp3_sd
reg_data$vpre_se[4] = vp4_sd
reg_data$vpre_se[5] = vp5_sd

reg_data$lbw_se[2] = lbw2_sd
reg_data$lbw_se[3] = lbw3_sd
reg_data$lbw_se[4] = lbw4_sd
reg_data$lbw_se[5] = lbw5_sd

reg_data$llbw_se[2] = llbw2_sd
reg_data$llbw_se[3] = llbw3_sd
reg_data$llbw_se[4] = llbw4_sd
reg_data$llbw_se[5] = llbw5_sd

reg_data$mlbw_se[2] = mlbw2_sd
reg_data$mlbw_se[3] = mlbw3_sd
reg_data$mlbw_se[4] = mlbw4_sd
reg_data$mlbw_se[5] = mlbw5_sd

reg_data$vlbw_se[2] = vlbw2_sd
reg_data$vlbw_se[3] = vlbw3_sd
reg_data$vlbw_se[4] = vlbw4_sd
reg_data$vlbw_se[5] = vlbw5_sd


#set first quantile values
reg_data[1, "pre_coef"] = 0
reg_data[1, "pre_se"] = 0
reg_data[1, "lpre_coef"] = 0
reg_data[1, "lpre_se"] = 0
reg_data[1, "mpre_coef"] = 0
reg_data[1, "mpre_se"] = 0
reg_data[1, "vpre_coef"] = 0
reg_data[1, "vpre_se"] = 0

reg_data[1, "lbw_coef"] = 0
reg_data[1, "lbw_se"] = 0
reg_data[1, "llbw_coef"] = 0
reg_data[1, "llbw_se"] = 0
reg_data[1, "mlbw_coef"] = 0
reg_data[1, "mlbw_se"] = 0
reg_data[1, "vlbw_coef"] = 0
reg_data[1, "vlbw_se"] = 0
reg_data[1, "quantile"] = 1

#get p values
reg_data[2:5, "pre_p"] = 1 - pnorm(reg_data[2:5, "pre_coef"]/reg_data[2:5, "pre_se"])
reg_data[2:5, "lpre_p"] = 1 - pnorm(reg_data[2:5, "lpre_coef"]/reg_data[2:5, "lpre_se"])
reg_data[2:5, "mpre_p"] = 1 - pnorm(reg_data[2:5, "mpre_coef"]/reg_data[2:5, "mpre_se"])
reg_data[2:5, "vpre_p"] = 1 - pnorm(reg_data[2:5, "vpre_coef"]/reg_data[2:5, "vpre_se"])

reg_data[2:5, "lbw_p"] = 1 - pnorm(reg_data[2:5, "lbw_coef"]/reg_data[2:5, "lbw_se"])
reg_data[2:5, "llbw_p"] = 1 - pnorm(reg_data[2:5, "llbw_coef"]/reg_data[2:5, "llbw_se"])
reg_data[2:5, "mlbw_p"] = 1 - pnorm(reg_data[2:5, "mlbw_coef"]/reg_data[2:5, "mlbw_se"])
reg_data[2:5, "vlbw_p"] = 1 - pnorm(reg_data[2:5, "vlbw_coef"]/reg_data[2:5, "vlbw_se"])


#confidence bands
reg_data$pre_low = reg_data$pre_coef - 1.96 * reg_data$pre_se
reg_data$pre_high = reg_data$pre_coef + 1.96 * reg_data$pre_se
reg_data$lpre_low = reg_data$lpre_coef - 1.96 * reg_data$lpre_se
reg_data$lpre_high = reg_data$lpre_coef + 1.96 * reg_data$lpre_se
reg_data$mpre_low = reg_data$mpre_coef - 1.96 * reg_data$mpre_se
reg_data$mpre_high = reg_data$mpre_coef + 1.96 * reg_data$mpre_se
reg_data$vpre_low = reg_data$vpre_coef - 1.96 * reg_data$vpre_se
reg_data$vpre_high = reg_data$vpre_coef + 1.96 * reg_data$vpre_se

reg_data$lbw_low = reg_data$lbw_coef - 1.96 * reg_data$lbw_se
reg_data$lbw_high = reg_data$lbw_coef + 1.96 * reg_data$lbw_se
reg_data$llbw_low = reg_data$llbw_coef - 1.96 * reg_data$llbw_se
reg_data$llbw_high = reg_data$llbw_coef + 1.96 * reg_data$llbw_se
reg_data$mlbw_low = reg_data$mlbw_coef - 1.96 * reg_data$mlbw_se
reg_data$mlbw_high = reg_data$mlbw_coef + 1.96 * reg_data$mlbw_se
reg_data$vlbw_low = reg_data$vlbw_coef - 1.96 * reg_data$vlbw_se
reg_data$vlbw_high = reg_data$vlbw_coef + 1.96 * reg_data$vlbw_se

breaks = seq(1, 5, by = 1)
labels = as.character(breaks)

reg_data$pval_label = sprintf("%.4f", reg_data$pre_p)
pr_pfas_fig = ggplot(reg_data, aes(x=quantile, y=pre_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=pre_low , ymax=pre_high), width=0.1, alpha = 0.5) + 
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = pre_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = rel(1)), # Decrease facet label size
    panel.spacing.x = unit(0.1, "lines"), 
    legend.position = "bottom", 
    legend.margin = margin(t = -2, unit = "pt"),
    axis.text.y = element_text(face = "bold", size = 18),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 26)
    
  )+
  xlab("") + ylab("All Preterm") + 
  scale_x_continuous(breaks = breaks, labels = labels)  + ylim(c(-0.06, 0.09)) + 
  ggtitle("Preterm")

reg_data$pval_label = sprintf("%.4f", reg_data$lpre_p)
lpr_pfas_fig = ggplot(reg_data, aes(x=quantile, y=lpre_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=lpre_low , max=lpre_high), width=0.1, alpha = 0.5) + 
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = lpre_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = rel(1)), # Decrease facet label size
    panel.spacing.x = unit(0.1, "lines"),  
    legend.position = "bottom",  
    legend.title = element_blank(), 
    legend.margin = margin(t = -2, unit = "pt"),  
    plot.title = element_text(hjust = 0.5), 
    axis.text.y = element_text(face = "bold", size = 18)
    
  )+
  xlab("") + ylab("Late Preterm") + 
  scale_x_continuous(breaks = breaks, labels = labels) + ylim(c(-0.06, 0.09))

reg_data$pval_label = sprintf("%.4f", reg_data$mpre_p)
mpr_pfas_fig = ggplot(reg_data, aes(x=quantile, y=mpre_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=mpre_low , max=mpre_high), width=0.1, alpha = 0.5) + 
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = mpre_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = rel(1)), # Decrease facet label size
    panel.spacing.x = unit(0.1, "lines"),  
    legend.position = "bottom",  
    legend.title = element_blank(),  
    legend.margin = margin(t = -2, unit = "pt"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.y = element_text(face = "bold", size = 18)
    
  )+
  xlab("") + ylab("Mod. Preterm") + 
  scale_x_continuous(breaks = breaks, labels = labels) + ylim(c(-0.06, 0.09))

reg_data$pval_label = sprintf("%.4f", reg_data$vpre_p)
vpr_pfas_fig = ggplot(reg_data, aes(x=quantile, y=vpre_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=vpre_low , ymax=vpre_high), width=0.1, alpha = 0.5) + 
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = vpre_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(axis.text = element_text(size = 18, face = "bold"), 
        axis.title = element_text(size = 18, face = "bold")) + 
  xlab("Predicted PFAS Quintile") + ylab("Very Preterm") + 
  scale_x_continuous(breaks = breaks, labels = labels) + ylim(c(-0.06, 0.09))


pfas_hist = ggplot(df_nn, aes(x=pred_pfas_level)) +
  geom_density() + 
  theme_minimal() + 
  theme(axis.text = element_text(face = "bold", size = 18), 
        axis.title = element_text(face = "bold", size = 18), 
        plot.margin = margin(t = 50, unit = "pt")) + 
  xlab("Predicted PFAS (ppb)") + ylab("Density") + 
 xlim(c(0, 1)) + geom_vline(xintercept = quantile(df_nn$pred_pfas_level, 0.2)) + 
  geom_vline(xintercept = quantile(df_nn$pred_pfas_level, 0.4)) + geom_vline(xintercept = quantile(df_nn$pred_pfas_level, 0.6)) + 
  geom_vline(xintercept = quantile(df_nn$pred_pfas_level, 0.8))



reg_data$pval_label = sprintf("%.4f", reg_data$lbw_p)
lbw_pfas_fig = ggplot(reg_data, aes(x=quantile, y=lbw_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=lbw_low, ymax=lbw_high), width=0.1, alpha = 0.5) + 
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = lbw_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = rel(1)), # Decrease facet label size
    panel.spacing.x = unit(0.1, "lines"),  
    legend.position = "bottom",  
    legend.margin = margin(t = -2, unit = "pt"),
    axis.text.y = element_text(face = "bold", size = 18),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 26)
    
  ) + 
  xlab("") + ylab("All Low Birthweight")+ ylim(c(-0.06, 0.09)) + 
  ggtitle("Low Birthweight")

reg_data$pval_label = sprintf("%.4f", reg_data$llbw_p)
llbw_pfas_fig = ggplot(reg_data, aes(x=quantile, y=llbw_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=llbw_low, ymax=llbw_high), width=0.1, alpha = 0.5) +
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = llbw_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = rel(1)), # Decrease facet label size
    panel.spacing.x = unit(0.1, "lines"),  
    legend.position = "bottom",  
    legend.title = element_blank(),  
    legend.margin = margin(t = -2, unit = "pt"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.y = element_text(face = "bold", size = 18)
    
  ) + 
  xlab("") + ylab("Low Birthweight") + ylim(c(-0.06, 0.09))

reg_data$pval_label = sprintf("%.4f", reg_data$mlbw_p)
mlbw_pfas_fig = ggplot(reg_data, aes(x=quantile, y=mlbw_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=mlbw_low, ymax=mlbw_high), width=0.1, alpha = 0.5) + 
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = mlbw_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(
    axis.title.y = element_text(face = "bold", size = 18),
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = rel(1)), # Decrease facet label size
    panel.spacing.x = unit(0.1, "lines"),  
    legend.position = "bottom",  
    legend.title = element_blank(),  
    legend.margin = margin(t = -2, unit = "pt"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.y = element_text(face = "bold", size = 18)
    
  ) + 
  xlab("") + ylab("Mod. Low Birthweight")+ ylim(c(-0.06, 0.09))

reg_data$pval_label = sprintf("%.4f", reg_data$vlbw_p)
vlbw_pfas_fig = ggplot(reg_data, aes(x=quantile, y=vlbw_coef)) +
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=vlbw_low, ymax=vlbw_high), width=0.1, alpha = 0.5) + 
  geom_text(data = filter(reg_data, !is.na(pre_p)), aes(x = quantile, y = vlbw_high, label = pval_label), nudge_y = 0.01, size = 5) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.6) +  
  theme_minimal() + 
  theme(axis.text = element_text(size = 18, face = "bold"), 
        axis.title = element_text(size = 18, face = "bold")) + 
  xlab("Predicted PFAS Quintile") + ylab("Very Low Birthweight") + 
  scale_x_continuous(breaks = breaks, labels = labels) + ylim(c(-0.06, 0.09))




((pr_pfas_fig | lbw_pfas_fig)/
  (lpr_pfas_fig | llbw_pfas_fig)/
  (mpr_pfas_fig | mlbw_pfas_fig)/
  (vpr_pfas_fig | vlbw_pfas_fig))/
  pfas_hist
