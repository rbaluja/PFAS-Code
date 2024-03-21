r_coefs = data.frame(matrix(ncol = 5, nrow = 0))
colnames(r_coefs) = c("weeks", "coef", "se", "effect_size", "es_sd")
index = 1

r1 = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
r_coefs[1, "weeks"] = "Very (<28 Weeks)"
r_coefs[1, "coef"] = r1$coeftable["down", 1]
r_coefs[1, "se"] = r1$coeftable["down", 2]
r_coefs[1, "effect_size"] = r1$coeftable["down", 1]/mean(df$gestation < 28) * 100
r_coefs[1, "es_sd"] = r1$coeftable["down", 2]/mean(df$gestation < 28) * 100



r2 = fixest::feols(I(gestation >= 28 & gestation < 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
r_coefs[2, "weeks"] = "Moderately (28-31 Weeks)"
r_coefs[2, "coef"] = r2$coeftable["down", 1]
r_coefs[2, "se"] = r2$coeftable["down", 2]
r_coefs[2, "effect_size"] = r2$coeftable["down", 1]/mean(df$gestation > 28 & df$gestation <= 32) * 100
r_coefs[2, "es_sd"] = r2$coeftable["down", 2]/mean(df$gestation > 28 & df$gestation <= 32) * 100


r3 = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
r_coefs[3, "weeks"] = "Slightly (32-36 Weeks)"
r_coefs[3, "coef"] = r3$coeftable["down", 1]
r_coefs[3, "se"] = r3$coeftable["down", 2]
r_coefs[3, "effect_size"] = r3$coeftable["down", 1]/mean(df$gestation < 37 & df$gestation >= 32) * 100
r_coefs[3, "es_sd"] = r3$coeftable["down", 2]/mean(df$gestation < 37 & df$gestation >= 32) * 100


r4 = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
r_coefs[4, "weeks"] = "Any"
r_coefs[4, "coef"] = r4$coeftable["down", 1]
r_coefs[4, "se"] = r4$coeftable["down", 2]
r_coefs[4, "effect_size"] = r4$coeftable["down", 1]/mean(df$gestation < 37) * 100
r_coefs[4, "es_sd"] = r4$coeftable["down", 2]/mean(df$gestation < 37) * 100


r_coefs = r_coefs %>%
  mutate(lower = effect_size - 1.96 * es_sd,
         upper = effect_size + 1.96 * es_sd)

# Create a factor with levels in the desired order for "weeks"
r_coefs$weeks = factor(r_coefs$weeks, levels = c("Any", "Slightly (32-36 Weeks)", 
                                                 "Moderately (28-31 Weeks)", "Very (<28 Weeks)"))

r_coefs$sig = c(1, 0, 0, 0)

# Plot
preterm_plot = ggplot(r_coefs, aes(x = weeks, y = effect_size, group = weeks)) +
  geom_point(aes(color = as.factor(sig)), size = 3) + # Adjust point size here
  geom_errorbar(data = subset(r_coefs, sig == 0), aes(ymin = lower, ymax = upper), color = "darkgrey", width = 0.2, size = 0.75) +
  # Significant error bars: blue and thicker
  geom_errorbar(data = subset(r_coefs, sig == 1), aes(ymin = lower, ymax = upper), color = "blue", width = 0.2, size = 1.5) +
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
  guides(color = "none") + ylim(-75, 275)




#birthweight

r_coefs = data.frame(matrix(ncol = 5, nrow = 0))
colnames(r_coefs) = c("grams", "coef", "se", "effect_size", "es_sd")
index = 1

r1 = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 + fa_resid+ wind_exposure
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

r_coefs[1, "grams"] = "Very (<1000g)"
r_coefs[1, "coef"] = r1$coeftable["down", 1]
r_coefs[1, "se"] = r1$coeftable["down", 2]
r_coefs[1, "effect_size"] = r1$coeftable["down", 1]/mean(df$bweight < 1000) * 100
r_coefs[1, "es_sd"] = r1$coeftable["down", 2]/mean(df$bweight < 1000) * 100



r2 = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 + fa_resid + wind_exposure
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

r_coefs[2, "grams"] = "Moderately (1000-1500g)"
r_coefs[2, "coef"] = r2$coeftable["down", 1]
r_coefs[2, "se"] = r2$coeftable["down", 2]
r_coefs[2, "effect_size"] = r2$coeftable["down", 1]/mean(df$bweight < 1500 & df$bweight > 1000) * 100
r_coefs[2, "es_sd"] = r2$coeftable["down", 2]/mean(df$bweight < 1500 & df$bweight > 1000) * 100


r3 = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 + fa_resid + wind_exposure
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

r_coefs[3, "grams"] = "Slightly (1500-2500g)"
r_coefs[3, "coef"] = r3$coeftable["down", 1]
r_coefs[3, "se"] = r3$coeftable["down", 2]
r_coefs[3, "effect_size"] = r3$coeftable["down", 1]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
r_coefs[3, "es_sd"] = r3$coeftable["down", 2]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100


r4 = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +  fa_resid +  wind_exposure
                   |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

r_coefs[4, "grams"] = "Any"
r_coefs[4, "coef"] = r4$coeftable["down", 1]
r_coefs[4, "se"] = r4$coeftable["down", 2]
r_coefs[4, "effect_size"] = r4$coeftable["down", 1]/mean(df$bweight < 2500) * 100
r_coefs[4, "es_sd"] = r4$coeftable["down", 2]/mean(df$bweight < 2500) * 100

r5 = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 + fa_resid + wind_exposure
                   |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

r_coefs[5, "grams"] = "Any - Full Term"
r_coefs[5, "coef"] = r5$coeftable["down", 1]
r_coefs[5, "se"] = r5$coeftable["down", 2]
r_coefs[5, "effect_size"] = r5$coeftable["down", 1]/mean(df[which(df$gestation >= 37), ]$bweight < 2500) * 100
r_coefs[5, "es_sd"] = r5$coeftable["down", 2]/mean(df[which(df$gestation >= 37), ]$bweight < 2500) * 100


r_coefs = r_coefs %>%
  mutate(lower = effect_size - 1.96 * es_sd,
         upper = effect_size + 1.96 * es_sd)

# Create a factor with levels in the desired order for "weeks"
r_coefs$weeks = factor(r_coefs$grams, levels = c("Any", "Any - Full Term", 
                                                 "Slightly (1500-2500g)",
                                                 "Moderately (1000-1500g)", 
                                                 "Very (<1000g)"))

r_coefs$sig = c("Yes", "No", "Yes", "Yes", "Yes")
r_coefs$sig = factor(r_coefs$sig, levels = c("Yes", "No"))

# Plot
bweight_plot = ggplot(r_coefs, aes(x = grams, y = effect_size, group = grams)) +
  geom_point(aes(color = as.factor(sig)), size = 3) + # Adjust point size here
  geom_errorbar(data = subset(r_coefs, sig == "No"), aes(ymin = lower, ymax = upper), color = "darkgrey", width = 0.2, size = 0.75) +
  # Significant error bars: blue and thicker
  geom_errorbar(data = subset(r_coefs, sig == "Yes"), aes(ymin = lower, ymax = upper), color = "blue", width = 0.2, size = 1.5) +
  scale_color_manual(values = c("Yes" = "blue", "No" = "darkgrey")) + # Assign colors based on sig
  geom_vline(xintercept =  2.5, linetype= "dashed") + 
  geom_hline(yintercept = 0) +
  scale_x_discrete(limits = c("Any", "Any - Full Term", 
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
  ylim(-75, 275) 
bweight_plot


preterm_plot / bweight_plot




