######################
###########Table 1
#Preterm
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid + wind_exposure
                                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
table1_preterm[["Late"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height + tri5 + fa_resid+ wind_exposure
                                    |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



modelsummary::modelsummary(table1, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid+ wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 

################
###Table 2
#preterm
table2_preterm = list()

table2_preterm[["All"]]= fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                                     n_sites + wind_exposure + 
                                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                     m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Late"]]= fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                                      n_sites + wind_exposure + 
                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_preterm[["Moderately"]]= fixest::feols(I(gestation < 32 & gestation >= 29) ~ pred_pfas + asinh(pfas) + 
                                      n_sites + wind_exposure + 
                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Very"]]= fixest::feols(I(gestation < 29) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

modelsummary::modelsummary(table2_preterm, 
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center")
#p values
1 - pnorm(0.011/0.008)
1 - pnorm(0.0062/0.006)
1 - pnorm(0.00053/0.0005)
1 - pnorm(0.0039/0.0016)

#marginal effect
table2_preterm[["Preterm"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))
(table2_preterm[["Preterm"]]$coefficients["pred_pfas"] - 1.96 * 0.008) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))
(table2_preterm[["Preterm"]]$coefficients["pred_pfas"] + 1.96 * 0.008) * 1/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))

table2_preterm[["lPreterm"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas)/1000, na.rm = T)^2))
(table2_preterm[["lPreterm"]]$coefficients["pred_pfas"] - 1.96 * 0.006) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_preterm[["lPreterm"]]$coefficients["pred_pfas"] + 1.96 * 0.006) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))

table2_preterm[["mPreterm"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_preterm[["mPreterm"]]$coefficients["pred_pfas"] - 1.96 * 0.0005) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_preterm[["mPreterm"]]$coefficients["pred_pfas"] + 1.96 * 0.0005) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))

table2_preterm[["Very"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_preterm[["Very"]]$coefficients["pred_pfas"] - 1.96 * 0.0016) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_preterm[["Very"]]$coefficients["pred_pfas"] + 1.96 * 0.0016) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))



#lbw
table2_lbw = list()
table2_lbw[["Low Birthweight"]]= fixest::feols(I(bweight < 2500 ) ~ pred_pfas + asinh(pfas) +
                                             n_sites + wind_exposure + 
                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["lLow Birthweight"]]= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                                              n_sites + wind_exposure + 
                                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                              m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["mLow Birthweight"]]= fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) +
                                              n_sites + wind_exposure + 
                                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                              m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_lbw[["Very Low Birthweight"]]= fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


modelsummary::modelsummary(table2_lbw, 
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("bin_pred_pfas", "pred_pfas"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 
#p vales
1 - pnorm(0.0109/0.0064)
1 - pnorm(0.0042/0.003)
1 - pnorm(0.00191/0.00094)
1 - pnorm(0.0048/0.0023)

#marginal effects
table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * 0.0064) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * 0.0064) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))

table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * 0.003) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * 0.003) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))

table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * 0.00094) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * 0.00094) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))

table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * 0.0023) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))
(table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * 0.0023) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2))



# Note: Table S-1 is based on authors' calculations while running (Code/PR/Data/data_head.R)
#Tables S-4 (Code/PR/Robustness/Oster.R), S-5 (Code/PR/Robustness/Placebo/placebo_head.R),
#S-9 (Code/PR/bootstrap.R), S-12 (Code/PR/Robustness/NY)

#######################
####Table S-2 (balance table)
df$college = ifelse(df$max_educ >= 6, 1, 0)
df[which(df$n_prenatal == 99), ]$n_prenatal = NA
df$ind_prenatal = ifelse(df$n_prenatal >= 15 & !is.na(df$n_prenatal), 1, 0)
df$old = ifelse(df$m_age > 40, 1, 0)
df$young = ifelse(df$m_age < 20, 1, 0)
df$no_hs = ifelse(df$max_educ < 3, 1, 0)
df$group = as.factor(ifelse(df$down == 1, 1, ifelse(df$up == 1, 2, 3)))
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
df2 = df2 %>% 
  dplyr::select(`Maternal Age` = m_age, 
                `College` = college,
                `Less than High School` = no_hs,
                `Maternal Marital Status` = m_married, 
                `Maternal Tobacco Use` = cig, 
                White = white, 
                group,
                `WIC` = wic, 
                `Private Insurance` = private_insurance, 
                `Months in Residence` = m_months_res, 
                `Younger than 20` = young, 
                `Older than 40` = old,
                `Prenatal Care Visits` = n_prenatal,
                `Pre-Pregancy Diabetes` = mr_04, 
                `Gestational Diabetes` = mr_18, 
                Hypertension = mr_08, 
                `Gestational Hypertension` = mr_23, 
                Eclampsia = mr_10
  )
df2 = as.data.frame(df2)


datasummary_balance(~group, 
                    data = df2, 
                    na.rm = T, 
                    fmt = modelsummary::fmt_significant(2, scientific = F, 
                                                        zero.print = T, drop0trailing = F, 
                                                        nsmall = 2), 
                    output = "latex") %>% 
  kable_styling(font_size = 6, fixed_thead = T, position = "center")


##########################
#####Table S-3
df$college = ifelse(df$max_educ >= 6, 1, 0)
df[which(df$n_prenatal == 99), ]$n_prenatal = NA
df$ind_prenatal = ifelse(df$n_prenatal >= 15 & !is.na(df$n_prenatal), 1, 0)
df$old = ifelse(df$m_age > 40, 1, 0)
df$young = ifelse(df$m_age < 20, 1, 0)
df$no_hs = ifelse(df$max_educ < 3, 1, 0)
df$group = as.factor(ifelse(df$down == 1, 1, ifelse(df$up == 1, 2, 3)))
df$llbw = as.numeric(I(df$bweight < 2500 & df$bweight >= 1500))
df$mlbw = as.numeric(I(df$bweight < 1500 & df$bweight >= 1000))
df$vlbw = as.numeric(I(df$bweight < 1000))
df$lpre = as.numeric(I(df$gestation < 37 & df$gestation >= 32))
df$mpre = as.numeric(I(df$gestation < 32 & df$gestation >= 28))
df$vpre = as.numeric(I(df$gestation < 28))



df2 = df[which(!is.na(df$dist) & df$dist <= 5000), ]
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
df2 = df2 %>% 
  dplyr::select(`Number of Sites w/in 5km` = n_sites, 
                `Well Distance` = dist, 
                `Residence Distance` = csite_dist, 
                `PFOA + PFOS at Site` = pfas, 
                `Very Preterm` = vpre, 
                `Moderately Preterm` = mpre, 
                `Late Preterm` = lpre, 
                `Very Low Birthweight` = vlbw, 
                `Moderately Low Birthweight` = mlbw, 
                `Low Birthweight` = llbw,
                group
  )
df2 = as.data.frame(df2)


datasummary_balance(~group, 
                    data = df2, 
                    na.rm = T, 
                    fmt = modelsummary::fmt_significant(2, scientific = F, 
                                                        zero.print = T, drop0trailing = F, 
                                                        nsmall = 2), 
                    output = "latex") %>% 
  kable_styling(font_size = 6, fixed_thead = T, position = "center")


##########################
#####Table S-6 (interaction with pfas)
#preterm
tables6_preterm = list() 
tables6_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  (updown + down) *I(dist/100) + I(pfas/10^3)  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
tables6_preterm[["Late"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  (updown + down) *I(dist/100) + I(pfas/10^3)  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_preterm[["Moderately"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  (updown + down) *I(dist/100) + I(pfas/10^3)  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_preterm[["Very"]] = fixest::feols(I(gestation < 28) ~  (updown + down) *I(dist/100) + I(pfas/10^3)  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid+ wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(tables6_preterm, 
                           stars = c("*" = 0.2, "**" = 0.10, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 




#low birthweight
tables6_lbw = list() 
tables6_lbw[["All"]] = fixest::feols(I(bweight < 2500) ~   (updown + down) *I(dist/100)  +I(pfas/10^3)  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Full Term"]] = fixest::feols(I(bweight < 2500) ~   (updown + down) *I(dist/100)  + I(pfas/10^3)  + n_sites + 
                                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                               m_height + tri5 + fa_resid + wind_exposure
                                                             |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Low"]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~   (updown + down) *I(dist/100)  + I(pfas/10^3)  + n_sites + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid + wind_exposure
                                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Moderately"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~   (updown + down) *I(dist/100)  + I(pfas/10^3)  + n_sites + 
                                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                         m_height + tri5 + fa_resid + wind_exposure
                                                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Very"]] = fixest::feols(I(bweight < 1000) ~   (updown + down) *I(dist/100)  + I(pfas/10^3)  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid+ wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



modelsummary::modelsummary(tables6_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 

##########################
#####Table S-6 (interaction with distance)
#preterm
tables7_preterm = list() 
tables7_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
tables7_preterm[["Late"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                            m_height + tri5 + fa_resid + wind_exposure
                                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_preterm[["Moderately"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid + wind_exposure
                                                |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_preterm[["Very"]] = fixest::feols(I(gestation < 28) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                            m_height + tri5 + fa_resid+ wind_exposure
                                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(tables7_preterm, 
                           stars = c("*" = 0.2, "**" = 0.10, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 




#low birthweight
tables7_lbw = list() 
tables7_lbw[["All"]] = fixest::feols(I(bweight < 2500) ~   (updown + down) *I(pfas/10^3)  + dist  + n_sites + 
                                                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                        m_height + tri5 + fa_resid + wind_exposure
                                                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_lbw[["Full Term"]] = fixest::feols(I(bweight < 2500) ~   (updown + down) *I(pfas/10^3)  + dist  + n_sites + 
                                                                    m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                    pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                    mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                    mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                    m_height + tri5 + fa_resid + wind_exposure
                                                                  |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

tables7_lbw[["Low"]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~   (updown + down) *I(pfas/10^3)  + dist  + n_sites + 
                                                    m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                    pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                    mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                    mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                    m_height + tri5 + fa_resid + wind_exposure
                                                  |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_lbw[["Moderately"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~   (updown + down) *I(pfas/10^3)  + dist  + n_sites + 
                                                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                              m_height + tri5 + fa_resid + wind_exposure
                                                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_lbw[["Very"]] = fixest::feols(I(bweight < 1000) ~   (updown + down) *I(pfas/10^3)  + dist  + n_sites + 
                                                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                        m_height + tri5 + fa_resid+ wind_exposure
                                                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



modelsummary::modelsummary(tables7_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 


########################
#### Table S-8 (other outcomes)
######################
###########Table 1
#Preterm
tables8_preterm = list() 
tables8_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure + bweight
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
tables8_preterm[["Late"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid + wind_exposure+ bweight
                                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables8_preterm[["Moderately"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                         m_height + tri5 + fa_resid + wind_exposure+ bweight
                                                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables8_preterm[["Very"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid+ wind_exposure+ bweight
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



modelsummary::modelsummary(tables8, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 

#low birthweight
tables8_lbw = list() 
tables8_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid + wind_exposure + gestation
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables8_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 + fa_resid + wind_exposure+ gestation
                                                                 |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))


tables8_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure+ gestation
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables8_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 + fa_resid + wind_exposure+ gestation
                                                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables8_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid+ wind_exposure+ gestation
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(tables8_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down", "down:I(pfas/10^3)", "updown", "updown:I(pfas/10^3)" , "gestation","bweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 


#######################
#### Table S-11 (first stage)
w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                        updown + wind_exposure + domestic + temp + pm25 + med_inc +
                        p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont) 

modelsummary::modelsummary(list(w_reg),
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
gof_map = c("nobs", "r.squared"), 
                           output = "latex") %>% 
  kable_styling(fixed_thead = T, position = "center") 