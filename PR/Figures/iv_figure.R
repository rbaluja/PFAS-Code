################
###Table 2 Note, standard errors are read in from bootstrap_iv.R run
load(modify_path("Data_Verify/RData/linear_iv_se.RData"))

r_coefs = data.frame(matrix(ncol = 6, nrow = 0))
colnames(r_coefs) = c("weeks", "coef", "se", "effect_size", "lower_es", "upper_es")
index = 1

#preterm
r4 = fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[4, "weeks"] = "Any"
r_coefs[4, "coef"] = r4$coeftable["pred_pfas", 1]
r_coefs[4, "se"] = preterm_sd

r_coefs[4, "effect_size"] = (r4$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation < 37) * 100
r_coefs[4, "lower_es"] = (r4$coefficients["pred_pfas"] - 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 37) * 100
r_coefs[4, "upper_es"] = (r4$coefficients["pred_pfas"] + 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 37) * 100


r3 = fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[3, "weeks"] = "Slightly (32-36 Weeks)"
r_coefs[3, "coef"] = r3$coeftable["pred_pfas", 1]
r_coefs[3, "se"] = lpreterm_sd

r_coefs[3, "effect_size"] = (r3$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation >= 32 & df$gestation < 37) * 100
r_coefs[3, "lower_es"] = (r3$coefficients["pred_pfas"] - 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 32 & df$gestation < 37) * 100
r_coefs[3, "upper_es"] = (r3$coefficients["pred_pfas"] + 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 32 & df$gestation < 37) * 100


r2 = fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
r_coefs[2, "weeks"] = "Moderately (28-31 Weeks)"
r_coefs[2, "coef"] = r2$coeftable["pred_pfas", 1]
r_coefs[2, "se"] = mpreterm_sd

r_coefs[2, "effect_size"] = (r2$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$gestation >= 28 & df$gestation < 32) * 100
r_coefs[2, "lower_es"] = (r2$coefficients["pred_pfas"] - 1.96 * mpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 28 & df$gestation < 32) * 100
r_coefs[2, "upper_es"] = (r2$coefficients["pred_pfas"] + 1.96 * mpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation >= 28 & df$gestation < 32) * 100

r1 = fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
r_coefs[1, "weeks"] = "Very (<28 Weeks)"
r_coefs[1, "coef"] = r1$coeftable["pred_pfas", 1]
r_coefs[1, "se"] = vpreterm_sd

r_coefs[1, "effect_size"] = r1$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 28) * 100
r_coefs[1, "lower_es"] = (r1$coefficients["pred_pfas"] - 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 28) * 100
r_coefs[1, "upper_es"] = (r1$coefficients["pred_pfas"] + 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$gestation < 28) * 100


r_coefs$weeks = factor(r_coefs$weeks, levels = c("Any", "Slightly (32-36 Weeks)", 
                                                 "Moderately (28-31 Weeks)", "Very (<28 Weeks)"))

r_coefs$sig = c(1, 1, 1, 1)

# Plot
preterm_plot = ggplot(r_coefs, aes(x = weeks, y = effect_size, group = weeks)) +
  geom_point(aes(color = as.factor(sig)), size = 3) + # Adjust point size here
  geom_errorbar(data = subset(r_coefs, sig == 0), aes(ymin = lower_es, ymax = upper_es), color = "darkgrey", width = 0.1, size = 0.75) +
  # Significant error bars: blue and thicker
  geom_errorbar(data = subset(r_coefs, sig == 1), aes(ymin = lower_es, ymax = upper_es), color = "blue", width = 0.1, size = 1.5) +
  scale_color_manual(values = c("1" = "blue", "0" = "darkgrey")) + # Assign colors based on sig
  geom_vline(xintercept =  1.5, linetype= "dashed") + 
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = c("Any", "Slightly (32-36 Weeks)", 
                              "Moderately (28-31 Weeks)", "Very (<28 Weeks)")) +
  labs(x = "", y = "% Increase in Preterm Births") +
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 18, face = "bold"), 
    axis.text = element_text(face = "bold", size = 14), 
    title = element_text(size = 18, face = "bold")) + 
  guides(color = "none") +   ylim(0, 200) 




r_coefs = data.frame(matrix(ncol = 6, nrow = 0))
colnames(r_coefs) = c("grams", "coef", "se", "effect_size", "lower_es", "upper_es")
index = 1
#lbw

r4 = fixest::feols(I(bweight < 2500 ) ~ pred_pfas + asinh(pfas) +
                                                 n_sites + wind_exposure + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
r_coefs[4, "grams"] = "Any"
r_coefs[4, "coef"] = r4$coeftable["pred_pfas", 1]
r_coefs[4, "se"] = lbw_sd

r_coefs[4, "effect_size"] = (r4$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 2500) * 100
r_coefs[4, "lower_es"] = (r4$coefficients["pred_pfas"] - 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500) * 100
r_coefs[4, "upper_es"] = (r4$coefficients["pred_pfas"] + 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500) * 100


r3 = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[3, "grams"] = "Slightly (1500-2500g)"
r_coefs[3, "coef"] = r3$coeftable["pred_pfas", 1]
r_coefs[3, "se"] = llbw_sd

r_coefs[3, "effect_size"] = (r3$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
r_coefs[3, "lower_es"] = (r3$coefficients["pred_pfas"] - 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
r_coefs[3, "upper_es"] = (r3$coefficients["pred_pfas"] + 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100


r2 = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[2, "grams"] = "Moderately (1000-1500g)"
r_coefs[2, "coef"] = r2$coeftable["pred_pfas", 1]
r_coefs[2, "se"] = mlbw_sd

r_coefs[2, "effect_size"] = (r2$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
r_coefs[2, "lower_es"] = (r2$coefficients["pred_pfas"] - 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
r_coefs[2, "upper_es"] = (r2$coefficients["pred_pfas"] + 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100


r1 = fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) +
                                                      n_sites + wind_exposure + 
                                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

r_coefs[1, "grams"] = "Very (<1000g)"
r_coefs[1, "coef"] = r1$coeftable["pred_pfas", 1]
r_coefs[1, "se"] = vlbw_sd

r_coefs[1, "effect_size"] = (r1$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2)))/mean(df$bweight < 1000) * 100
r_coefs[1, "lower_es"] = (r1$coefficients["pred_pfas"] - 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1000) * 100
r_coefs[1, "upper_es"] = (r1$coefficients["pred_pfas"] + 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))/mean(df$bweight < 1000) * 100


r_coefs$weeks = factor(r_coefs$grams, levels = c("Any",
                                                 "Slightly (1500-2500g)",
                                                 "Moderately (1000-1500g)", 
                                                 "Very (<1000g)"))

r_coefs$sig = c("Yes", "Yes", "Yes", "Yes")
r_coefs$sig = factor(r_coefs$sig, levels = c("Yes", "No"))
r_coefs <- rbind(r_coefs, data.frame(grams = NA, coef = NA, se = NA, effect_size = NA, lower_es = NA, upper_es = NA, weeks= NA, sig = "No"))

# Plot
bweight_plot = ggplot(r_coefs, aes(x = grams, y = effect_size, group = grams)) +
  geom_point(aes(color = sig), size = 3) + # Adjust point size here
  geom_errorbar(data = subset(r_coefs, sig == "No"), aes(ymin = lower_es, ymax = upper_es), color = "darkgrey", width = 0.1, size = 0.75) +
  # Significant error bars: blue and thicker
  geom_errorbar(data = subset(r_coefs, sig == "Yes"), aes(ymin = lower_es, ymax = upper_es), color = "blue", width = 0.1, size = 1.5) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "darkgrey")) + # Assign colors based on sig
  geom_vline(xintercept =  1.5, linetype= "dashed") + 
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = c("Any",
                              "Slightly (1500-2500g)",
                              "Moderately (1000-1500g)", 
                              "Very (<1000g)")) +
  labs(x = "", y = "% Increase in Low-Birthweight", color = expression("Test against H" [0]: "``Downgradient'' effect being weakly negative is significant at 5% level")) +
  theme_minimal() + 
  theme(
    axis.title = element_text(size = 18, face = "bold"), 
    axis.text = element_text(face = "bold", size = 14), 
    title = element_text(size = 18, face = "bold"), 
    legend.position = "bottom", 
    legend.text = element_text(size = 18, face = "bold"), 
    legend.title =  element_text(size = 18, face = "bold")) + 
  ylim(0, 200) 


preterm_plot/bweight_plot

