
p_all_ds = fixest::feols(I(gestation < 37) ~  updown + down  + I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure 
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

lp_ds = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down + I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mp_ds = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down + I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

vp_ds = fixest::feols(I(gestation < 28) ~  updown + down + I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid+ wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


lbw_all_ds = fixest::feols(I(bweight < 2500) ~  updown + down + I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure 
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

lbw_ft_ds = fixest::feols(I(bweight < 2500) ~  updown + down + I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure 
                           |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))

llbw_ds = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down + I(pfas/10^3) + dist  + n_sites + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid + wind_exposure
                                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mlbw_ds = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down + I(pfas/10^3) + dist  + n_sites + 
                                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                         m_height + tri5 + fa_resid + wind_exposure
                                                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

vlbw_ds = fixest::feols(I(bweight < 1000) ~  updown + down + I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid+ wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

stillbrn_ds = fixest::feols(stillbrn ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 +fa_resid + wind_exposure 
                        |county + year^month + birth_race_dsc_1, data = df[which(df$chld_dead_live != 9), ], warn = F, notes = F, cluster = c("site", "year^month"))

save(p_all_ds, lp_ds, mp_ds, vp_ds, lbw_all_ds, lbw_ft_ds, llbw_ds, mlbw_ds, vlbw_ds, stillbrn_ds, file = modify_path("Data_Verify/Robustness/drop_nearby_state_robustness.RData"))
