################
###Table 2 Note, standard errors are read in from bootstrap_iv.R run
load(modify_path("Data_Verify/RData/preterm_sd.RData"))
load(modify_path("Data_Verify/RData/lbw_sd.RData"))

#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

r_coefs = data.frame(matrix(ncol = 9, nrow = 0))
colnames(r_coefs) = c("sev", "coef", "se", "effect_size", "es_se", "lower_es", "upper_es", "b_outcome", "p_value")
index = 1

#preterm
r1 = fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[1, "sev"] = "Any"
r_coefs[1, "coef"] = r1$coeftable["pred_pfas", 1]
r_coefs[1, "se"] = preterm_sd

r_coefs[1, "effect_size"] = (r1$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation < 37) * 100
r_coefs[1, "es_se"] = (preterm_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation < 37) * 100
r_coefs[1, "lower_es"] = (r1$coefficients["pred_pfas"] - 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 37) * 100
r_coefs[1, "upper_es"] = (r1$coefficients["pred_pfas"] + 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 37) * 100
r_coefs[1, "b_outcome"] = "Preterm"
r_coefs[1, "sig"] = "Yes"
r_coefs[1, "p_value"] = 1 - pnorm(r1$coefficients["pred_pfas"]/preterm_sd)
r_coefs[1, "p_value_s"] = ifelse(r_coefs[1, "p_value"] < 0.001, "<0.001", round(r_coefs[1, "p_value"], 3))


r2 = fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[2, "sev"] = "Slightly"
r_coefs[2, "coef"] = r2$coeftable["pred_pfas", 1]
r_coefs[2, "se"] = lpreterm_sd

r_coefs[2, "effect_size"] = (r2$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation >= 32 & df$gestation < 37) * 100
r_coefs[2, "es_se"] = (lpreterm_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation >= 32 & df$gestation < 37) * 100
r_coefs[2, "lower_es"] = (r2$coefficients["pred_pfas"] - 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 32 & df$gestation < 37) * 100
r_coefs[2, "upper_es"] = (r2$coefficients["pred_pfas"] + 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 32 & df$gestation < 37) * 100
r_coefs[2, "b_outcome"] = "Preterm"
r_coefs[2, "sig"] = "Yes"
r_coefs[2, "p_value"] = 1 - pnorm(r2$coefficients["pred_pfas"]/lpreterm_sd)
r_coefs[2, "p_value_s"] = ifelse(r_coefs[2, "p_value"] < 0.001, "<0.001", round(r_coefs[2, "p_value"], 3))

r3 = fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
r_coefs[3, "sev"] = "Moderately"
r_coefs[3, "coef"] = r3$coeftable["pred_pfas", 1]
r_coefs[3, "se"] = mpreterm_sd

r_coefs[3, "effect_size"] = (r3$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation >= 28 & df$gestation < 32) * 100
r_coefs[3, "es_se"] = (mpreterm_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation >= 28 & df$gestation < 32) * 100
r_coefs[3, "lower_es"] = (r3$coefficients["pred_pfas"] - 1.96 * mpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 28 & df$gestation < 32) * 100
r_coefs[3, "upper_es"] = (r3$coefficients["pred_pfas"] + 1.96 * mpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 28 & df$gestation < 32) * 100
r_coefs[3, "b_outcome"] = "Preterm"
r_coefs[3, "sig"] = "Yes"
r_coefs[3, "p_value"] = 1 - pnorm(r3$coefficients["pred_pfas"]/mpreterm_sd)
r_coefs[3, "p_value_s"] = ifelse(r_coefs[3, "p_value"] < 0.001, "<0.001", round(r_coefs[3, "p_value"], 3))

r4 = fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
r_coefs[4, "sev"] = "Very"
r_coefs[4, "coef"] = r4$coeftable["pred_pfas", 1]
r_coefs[4, "se"] = vpreterm_sd

r_coefs[4, "effect_size"] = r4$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 28) * 100
r_coefs[4, "es_se"] = (vpreterm_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation < 28) * 100
r_coefs[4, "lower_es"] = (r4$coefficients["pred_pfas"] - 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 28) * 100
r_coefs[4, "upper_es"] = (r4$coefficients["pred_pfas"] + 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 28) * 100
r_coefs[4, "b_outcome"] = "Preterm"
r_coefs[4, "sig"] = "Yes"
r_coefs[4, "p_value"] = 1 - pnorm(r4$coefficients["pred_pfas"]/vpreterm_sd)
r_coefs[4, "p_value_s"] = ifelse(r_coefs[4, "p_value"] < 0.001, "<0.001", round(r_coefs[4, "p_value"], 3))


r5 = fixest::feols(I(bweight < 2500 ) ~ pred_pfas + asinh(pfas) +
                                                 n_sites + wind_exposure + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
r_coefs[5, "sev"] = "Any"
r_coefs[5, "coef"] = r5$coeftable["pred_pfas", 1]
r_coefs[5, "se"] = lbw_sd

r_coefs[5, "effect_size"] = (r5$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 2500) * 100
r_coefs[5, "es_se"] = (lbw_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 2500) * 100
r_coefs[5, "lower_es"] = (r5$coefficients["pred_pfas"] - 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500) * 100
r_coefs[5, "upper_es"] = (r5$coefficients["pred_pfas"] + 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500) * 100
r_coefs[5, "b_outcome"] = "Low-Birthweight"
r_coefs[5, "sig"] = "Yes"
r_coefs[5, "p_value"] = 1 - pnorm(r5$coefficients["pred_pfas"]/lbw_sd)
r_coefs[5, "p_value_s"] = ifelse(r_coefs[5, "p_value"] < 0.001, "<0.001", round(r_coefs[5, "p_value"], 3))

r6 = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[6, "sev"] = "Slightly"
r_coefs[6, "coef"] = r6$coeftable["pred_pfas", 1]
r_coefs[6, "se"] = llbw_sd

r_coefs[6, "effect_size"] = (r6$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
r_coefs[6, "es_se"] = (llbw_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
r_coefs[6, "lower_es"] = (r6$coefficients["pred_pfas"] - 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
r_coefs[6, "upper_es"] = (r6$coefficients["pred_pfas"] + 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
r_coefs[6, "b_outcome"] = "Low-Birthweight"
r_coefs[6, "sig"] = "Yes"
r_coefs[6, "p_value"] = 1 - pnorm(r6$coefficients["pred_pfas"]/llbw_sd)
r_coefs[6, "p_value_s"] = ifelse(r_coefs[6, "p_value"] < 0.001, "<0.001", round(r_coefs[6, "p_value"], 3))

r7 = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[7, "sev"] = "Moderately"
r_coefs[7, "coef"] = r7$coeftable["pred_pfas", 1]
r_coefs[7, "se"] = mlbw_sd

r_coefs[7, "effect_size"] = (r7$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
r_coefs[7, "es_se"] = (mlbw_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
r_coefs[7, "lower_es"] = (r7$coefficients["pred_pfas"] - 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
r_coefs[7, "upper_es"] = (r7$coefficients["pred_pfas"] + 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
r_coefs[7, "b_outcome"] = "Low-Birthweight"
r_coefs[7, "sig"] = "Yes"
r_coefs[7, "p_value"] = 1 - pnorm(r7$coefficients["pred_pfas"]/mlbw_sd)
r_coefs[7, "p_value_s"] = ifelse(r_coefs[7, "p_value"] < 0.001, "<0.001", round(r_coefs[7, "p_value"], 3))

r8 = fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) +
                                                      n_sites + wind_exposure + 
                                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[8, "sev"] = "Very"
r_coefs[8, "coef"] = r8$coeftable["pred_pfas", 1]
r_coefs[8, "se"] = vlbw_sd

r_coefs[8, "effect_size"] = (r8$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 1000) * 100
r_coefs[8, "es_se"] = (vlbw_sd/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 1000) * 100
r_coefs[8, "lower_es"] = (r8$coefficients["pred_pfas"] - 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1000) * 100
r_coefs[8, "upper_es"] = (r8$coefficients["pred_pfas"] + 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1000) * 100
r_coefs[8, "b_outcome"] = "Low-Birthweight"
r_coefs[8, "sig"] = "Yes"
r_coefs[8, "p_value"] = 1 - pnorm(r8$coefficients["pred_pfas"]/vlbw_sd)
r_coefs[8, "p_value_s"] = ifelse(r_coefs[8, "p_value"] < 0.001, "<0.001", round(r_coefs[8, "p_value"], 3))
r_coefs$sev = factor(r_coefs$sev, levels = c("Any", "Slightly", "Moderately", "Very"))





#with jitter
r_coefs$sev_num = as.numeric(as.factor(r_coefs$sev))
r_coefs_jittered1 = r_coefs[r_coefs$b_outcome == "Preterm", ]
r_coefs_jittered1$sev_num = r_coefs_jittered1$sev_num - 0.2  # Shift left

r_coefs_jittered2 = r_coefs[r_coefs$b_outcome == "Low-Birthweight", ]
r_coefs_jittered2$sev_num = r_coefs_jittered2$sev_num + 0.2  # Shift right

# Plot using ggplot2
iv_fig = ggplot() +
  geom_point(data = r_coefs_jittered1, aes(x = sev_num, y = effect_size, color = b_outcome), size = 10) +
  geom_errorbar(data = r_coefs_jittered1, aes(x = sev_num, ymin = lower_es, ymax = upper_es, color = b_outcome), width = 0.075, size = 2) +
  geom_point(data = r_coefs_jittered2, aes(x = sev_num, y = effect_size, color = b_outcome), size = 10) +
  geom_errorbar(data = r_coefs_jittered2, aes(x = sev_num, ymin = lower_es, ymax = upper_es, color = b_outcome), width = 0.075, size = 2) +
  scale_x_continuous(breaks = 1:4, labels = levels(as.factor(r_coefs$sev))) +
  scale_color_manual(values = c("Preterm" = "dodgerblue3", "Low-Birthweight" = "firebrick4")) +
  labs(x = "", y = "Effect on Reproductive Outcomes (%↑ from +1000 ppt PFAS)", color = "Birth Outcome") +
  theme_minimal() +
  theme(axis.text = element_text(size = 50), 
        axis.title = element_text(size = 52), 
        legend.position = "bottom",   
        legend.box = "horizontal",   
        legend.title.align = 0.5, 
        legend.text = element_text(size = 50), 
        legend.title = element_text(size = 52), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25), 
        legend.key.size = unit(5, "lines")) + 
  ylim(0, 170) + 
  geom_hline(yintercept = 0, linetype = "dashed")

#add p value label
iv_fig = iv_fig + 
  geom_text(data = r_coefs_jittered1, aes(x = sev_num, y = upper_es, 
                                                 label = ifelse(p_value_s == "<0.001", 
                                                                paste0("p ", p_value_s),
                                                                paste0("p = ", p_value_s)), 
                                          vjust = -1), size = 10) +
  geom_text(data = r_coefs_jittered2, aes(x = sev_num, y = upper_es, 
                                          label = ifelse(p_value_s == "<0.001", 
                                                         paste0("p ", p_value_s),
                                                         paste0("p = ", p_value_s)), 
                                          vjust = -1), size = 10)

#add effect size label
iv_fig = iv_fig + 
  geom_text(data = r_coefs_jittered1, aes(x = sev_num, y = upper_es, 
                                          label = format(round(effect_size, 2), nsmall = 2), 
                                          vjust = -4), size = 12) +
  geom_text(data = r_coefs_jittered2, aes(x = sev_num, y = upper_es, 
                                          label = format(round(effect_size, 2), nsmall = 2), 
                                          vjust = -4), size = 12)

#add se label
iv_fig + 
  geom_text(data = r_coefs_jittered1, aes(x = sev_num, y = upper_es, 
                                          label = paste0("(", format(round(es_se, 2), nsmall = 2), ")"), 
                                          vjust = -3), size = 10) +
  geom_text(data = r_coefs_jittered2, aes(x = sev_num, y = upper_es, 
                                          label = paste0("(", format(round(es_se, 2), nsmall = 2), ")"), 
                                          vjust = -3), size = 10)


ggsave(modify_path("Figures/IV/iv_figure.png"), scale = 3.7)

