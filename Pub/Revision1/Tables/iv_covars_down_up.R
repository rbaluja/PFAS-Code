#sample used in estimation:
df$college = ifelse(df$max_educ >= 6, 1, 0)
df[which(df$n_prenatal == 99), ]$n_prenatal = NA
df$ind_prenatal = ifelse(df$n_prenatal >= 15 & !is.na(df$n_prenatal), 1, 0)
df$old = ifelse(df$m_age > 40, 1, 0)
df$young = ifelse(df$m_age < 20, 1, 0)
df$no_hs = ifelse(df$max_educ < 3, 1, 0)
df$group = as.factor(ifelse(df$down == 1, 1, ifelse(df$up == 1, 2, 3)))
df$med_hprice = df$med_hprice/10^4
df$med_inc = df$med_inc/10^3
df2 = df[which(!is.na(df$dist) & df$dist <= meters), ]
df2 = df2 %>% 
  dplyr::filter(!is.na(gestation) & 
                  !is.na(m_age) & 
                  !is.na(m_married) & 
                  !is.na(private_insurance) & 
                  !is.na(nbr_cgrtt) & 
                  !is.na(m_educ) & 
                  !is.na(f_educ) & 
                  !is.na(pm25) & 
                  !is.na(temp) & 
                  !is.na(p_manuf) & 
                  !is.na(n_hunits) & 
                  !is.na(med_hprice) & 
                  !is.na(well_elev) & 
                  !is.na(resid_elev) & 
                  !is.na(mr_04) & 
                  !is.na(mr_18) & 
                  !is.na(mr_21) & 
                  !is.na(mr_26) & 
                  !is.na(mr_27) & 
                  !is.na(mthr_wgt_dlv) & 
                  !is.na(mthr_pre_preg_wgt) & 
                  !is.na(m_height) & 
                  !is.na(tri5) & 
                  !is.na(county) & 
                  !is.na(year) & 
                  !is.na(month) & 
                  !is.na(birth_race_dsc_1) & 
                  !is.na(wic))



table_preterm = list()

table_preterm[["Any"]] = fixest::feols(I(gestation < 37) ~  1
                        |county + year^month + birth_race_dsc_1
                        |updown + down ~
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                          m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                          , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

table_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  1
                                       |county + year^month + birth_race_dsc_1
                                       |updown + down ~
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                       , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

table_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  1
                                              |county + year^month + birth_race_dsc_1
                                              |updown + down ~
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                                m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                              , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

table_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  1
                                              |county + year^month + birth_race_dsc_1
                                              |updown + down ~
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                                m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                              , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

modelsummary::modelsummary(table_preterm, 
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("fit_down" = "Downgradient", 
                                        "fit_updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_iv_covs.tex")) 


table_lbw = list()

table_lbw[["Any"]] = fixest::feols(I(bweight < 2500) ~  1
                                       |county + year^month + birth_race_dsc_1
                                       |updown + down ~
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                       , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

table_lbw[["Any Full-Term"]] = fixest::feols(I(bweight < 2500) ~  1
                                   |county + year^month + birth_race_dsc_1
                                   |updown + down ~
                                     m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                     m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                     m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                   , data = df2[which(df2$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

table_lbw[["Moderately"]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  1
                                              |county + year^month + birth_race_dsc_1
                                              |updown + down ~
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                                m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                              , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

table_lbw[["Very"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  1
                                        |county + year^month + birth_race_dsc_1
                                        |updown + down ~
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                          m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                        , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

table_lbw[["Extremely"]] = fixest::feols(I(bweight < 1000) ~  1
                                             |county + year^month + birth_race_dsc_1
                                             |updown + down ~
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                               m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                                             , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

modelsummary::modelsummary(table_lbw, 
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("fit_down" = "Downgradient", 
                                        "fit_updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_iv_covs.tex")) 


mort_tab = fixest::feols(death ~  1
                         |county + year^month + birth_race_dsc_1
                         |updown + down ~
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                           m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                         , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(list(mort_tab), 
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("fit_down" = "Downgradient", 
                                        "fit_updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_iv_covs.tex")) 