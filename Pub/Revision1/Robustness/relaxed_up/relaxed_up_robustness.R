df$ym = paste0(df$year, "-", df$month)
pr_rup = glm(I(gestation < 37) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         + factor(birth_race_dsc_1), data = df, family = "binomial")
lpr_rup = glm(I(gestation < 37 & gestation >= 32) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     + factor(birth_race_dsc_1), data = df, family = "binomial")

mpr_rup = glm(I(gestation < 32 & gestation >= 28) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               + factor(birth_race_dsc_1), data = df, family = "binomial")

vpr_rup = glm(I(gestation < 28) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid+ wind_exposure
                                         + factor(birth_race_dsc_1), data = df, family = "binomial")



#binary low birthweight
lbw_rup = glm(I(bweight < 2500) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 + factor(birth_race_dsc_1), data = df, family = "binomial")

llbw_rup = glm(I(bweight < 2500 & bweight >= 1500) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid + wind_exposure
               + factor(birth_race_dsc_1), data = df, family = "binomial")

lbw_ft_rup = glm(I(bweight < 2500) ~  down + I(pfas/10^3) + dist  + n_sites + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid + wind_exposure 
                 + factor(birth_race_dsc_1), data = df[which(df$gestation >= 37), ], family = "binomial")

mlbw_rup = glm(I(bweight < 1500 & bweight >= 1000) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                         m_height + tri5 + fa_resid + wind_exposure
                                                       + factor(birth_race_dsc_1), data = df, family = "binomial")

vlbw_rup = glm(I(bweight < 1000) ~  down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid+ wind_exposure
                                                 + factor(birth_race_dsc_1), data = df, family = "binomial")

mort_rup = glm(death ~  down +  I(pfas/10^3) + dist  + n_sites + 
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 +fa_resid + wind_exposure 
                            + factor(birth_race_dsc_1), data = df, family = "binomial")


save(lbw_rup, lbw_ft_rup, llbw_rup, mlbw_rup, vlbw_rup, pr_rup, lpr_rup, mpr_rup, vpr_rup, mort_rup,
     file = modify_path("Data_Verify/Robustness/relaxed_up_robust_logit.RData"))

sd_p_all_rup = sqrt(vcovCL(pr_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) 
sd_lpr_rup = sqrt(vcovCL(lpr_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) 
sd_mpr_rup = sqrt(vcovCL(mpr_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])
sd_vpr_rup = sqrt(vcovCL(vpr_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) 
sd_lbw_rup = sqrt(vcovCL(lbw_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) 
sd_llbw_rup = sqrt(vcovCL(llbw_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) 
sd_mlbw_rup = sqrt(vcovCL(mlbw_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) 
sd_vlbw_rup = sqrt(vcovCL(vlbw_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) 
sd_mort_rup = sqrt(vcovCL(mort_rup, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])

save(sd_p_all_rup, sd_lpr_rup, sd_mpr_rup, sd_vpr_rup, sd_lbw_rup, sd_llbw_rup, sd_mlbw_rup, sd_vlbw_rup, sd_mort_rup, 
     file = modify_path("Data_Verify/Robustness/relaxed_up_robust_logit_sd.RData"))
