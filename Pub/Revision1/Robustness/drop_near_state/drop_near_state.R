df$ym = paste0(df$year, "-", df$month)
p_all_ds = glm(I(gestation < 37) ~  down + updown  + I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure 
               + factor(birth_race_dsc_1), data = df, family = "binomial")

lp_ds = glm(I(gestation < 37 & gestation >= 32) ~  down + updown + I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     + factor(birth_race_dsc_1), data = df, family = "binomial")

mp_ds = glm(I(gestation < 32 & gestation >= 28) ~  down + updown + I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               + factor(birth_race_dsc_1), data = df, family = "binomial")

vp_ds = glm(I(gestation < 28) ~  down + updown + I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid+ wind_exposure
                                         + factor(birth_race_dsc_1), data = df, family = "binomial")


lbw_all_ds = glm(I(bweight < 2500) ~  down + updown + I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure 
                                                 + factor(birth_race_dsc_1), data = df, family = "binomial")

lbw_ft_ds = glm(I(bweight < 2500) ~  down + updown + I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure 
                + factor(birth_race_dsc_1), data = df[which(df$gestation >= 37), ], family = "binomial")

llbw_ds = glm(I(bweight < 2500 & bweight >= 1500) ~  down + updown + I(pfas/10^3) + dist  + n_sites + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid + wind_exposure
                                             + factor(birth_race_dsc_1), data = df, family = "binomial")

mlbw_ds = glm(I(bweight < 1500 & bweight >= 1000) ~  down + updown + I(pfas/10^3) + dist  + n_sites + 
                                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                         m_height + tri5 + fa_resid + wind_exposure
                                                       + factor(birth_race_dsc_1), data = df, family = "binomial")

vlbw_ds = glm(I(bweight < 1000) ~  down + updown + I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid+ wind_exposure
                                                 + factor(birth_race_dsc_1), data = df, family = "binomial")

mort_ds = glm(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 +fa_resid + wind_exposure 
                        + factor(birth_race_dsc_1), data = df, family = "binomial")

save(p_all_ds, lp_ds, mp_ds, vp_ds, lbw_all_ds, lbw_ft_ds, llbw_ds, mlbw_ds, vlbw_ds, mort_ds, 
     file = modify_path("Data_Verify/Robustness/drop_nearby_state_robustness_logit.RData"))


sd_p_all_ds = sqrt(vcovCL(p_all_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"])
sd_lp_ds = sqrt(vcovCL(lp_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"])
sd_mp_ds = sqrt(vcovCL(mp_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"]) 
sd_vp_ds = sqrt(vcovCL(vp_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"])
sd_lbw_all_ds = sqrt(vcovCL(lbw_all_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"])
sd_llbw_ds = sqrt(vcovCL(llbw_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"]) 
sd_mlbw_ds = sqrt(vcovCL(mlbw_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"]) 
sd_vlbw_ds = sqrt(vcovCL(vlbw_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"])
sd_mort_ds = sqrt(vcovCL(mort_ds, cluster = ~ df_dns_logit$site + df_dns_logit$ym, type = "HC0")["down", "down"]) 

save(sd_p_all_ds, sd_lp_ds, sd_mp_ds, sd_vp_ds, sd_lbw_all_ds, sd_llbw_ds, sd_mlbw_ds, sd_vlbw_ds, sd_mort_ds, 
     file = modify_path("Data_Verify/Robustness/drop_nearby_state_robustness_logit_sd.RData"))
