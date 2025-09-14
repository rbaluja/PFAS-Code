df %>% 
  left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, industry), by = "site") -> df
#Industry
#Preterm
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +  fa_resid +wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid +wind_exposure
                                              |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table1_preterm_industry100.tex"))

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +  fa_resid +  wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 + fa_resid + wind_exposure
                                                                 |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ] %>% dplyr::filter(industry == "Industry") , warn = F, notes = F, cluster = c("site", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 + fa_resid + wind_exposure
                                                           |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid+ wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table1_lbw_industry100.tex")) 


#mortality
mort_table = list()

mort_table[["Binary"]] = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 +fa_resid + wind_exposure
                                       |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Industry"), warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(mort_table, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table1_mort_industry100.tex")) 


#Landfill
#Preterm
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +  fa_resid +wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid +wind_exposure
                                              |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table1_preterm_landfill100.tex"))

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +  fa_resid +  wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df %>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 + fa_resid + wind_exposure
                                                                 |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ] %>% dplyr::filter(industry == "Landfill") , warn = F, notes = F, cluster = c("site", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 + fa_resid + wind_exposure
                                                           |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid+ wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table1_lbw_landfill100.tex")) 


#mortality
mort_table = list()

mort_table[["Binary"]] = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 +fa_resid + wind_exposure
                                       |county + year^month + birth_race_dsc_1, data = df%>% dplyr::filter(industry == "Landfill"), warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(mort_table, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table1_mort_landfill100.tex")) 