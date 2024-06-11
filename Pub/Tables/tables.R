#read in bootstrap functions
source("PFAS-Code/Pub/Tables/bs_functions.R")


######################
###########Table 1
#Preterm
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +  fa_resid +wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid +wind_exposure
                                              |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table1_preterm.tex")) 

table1_preterm[["Extremely"]]$coefficients["down"]/mean(df$gestation < 28)
(table1_preterm[["Extremely"]]$coefficients["down"] - 1.96 * table1_preterm[["Extremely"]]$se["down"])/mean(df$gestation < 28)
(table1_preterm[["Extremely"]]$coefficients["down"] + 1.96 * table1_preterm[["Extremely"]]$se["down"])/mean(df$gestation < 28)

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +  fa_resid +  wind_exposure
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
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table1_lbw.tex")) 
table1_lbw[["Very Low Birthweight"]]$coefficients["down"]/mean(df$bweight < 1000)
(table1_lbw[["Very Low Birthweight"]]$coefficients["down"] - 1.96 * table1_lbw[["Very Low Birthweight"]]$se["down"])/mean(df$bweight < 1000)
(table1_lbw[["Very Low Birthweight"]]$coefficients["down"] + 1.96 * table1_lbw[["Very Low Birthweight"]]$se["down"])/mean(df$bweight < 1000)

################
###Table 2 Note, standard errors are read in from bootstrap_iv.R run
if (!file.exists(modify_path("Data_Verify/RData/bootstrap.RData"))) {
  stop("Bootstrap standard errors")
}
load(modify_path("Data_Verify/RData/bootstrap.RData"))

#preterm
table2_preterm = list()

table2_preterm[["All"]]= fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Moderately"]]= fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_preterm[["Very"]]= fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Extremely"]]= fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                                               n_sites + wind_exposure + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

modelsummary::modelsummary(table2_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table2_preterm.tex"))

preterm_iv = table2_preterm[["All"]]$coefficients["pred_pfas"]
lpreterm_iv = table2_preterm[["Moderately"]]$coefficients["pred_pfas"]
mpreterm_iv = table2_preterm[["Very"]]$coefficients["pred_pfas"]
vpreterm_iv = table2_preterm[["Extremely"]]$coefficients["pred_pfas"]

save(preterm_iv, lpreterm_iv, mpreterm_iv, vpreterm_iv, file = modify_path("Data_Verify/RData/preterm_iv_coef.RData"))

#calculate bootstrapped standard errors
preterm_sd = linear_bootstrap(boot_coefs, "preterm", table2_preterm[["All"]])
lpreterm_sd = linear_bootstrap(boot_coefs, "lpreterm", table2_preterm[["Moderately"]])
mpreterm_sd = linear_bootstrap(boot_coefs, "mpreterm", table2_preterm[["Very"]])
vpreterm_sd = linear_bootstrap(boot_coefs, "vpreterm", table2_preterm[["Extremely"]])
save(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd, file = modify_path("Data_Verify/RData/preterm_sd.RData"))
1 - pnorm(table2_preterm[["All"]]$coefficients["pred_pfas"]/preterm_sd)
1 - pnorm(table2_preterm[["Moderately"]]$coefficients["pred_pfas"]/lpreterm_sd)
1 - pnorm(table2_preterm[["Very"]]$coefficients["pred_pfas"]/mpreterm_sd)
1 - pnorm(table2_preterm[["Extremely"]]$coefficients["pred_pfas"]/vpreterm_sd)


#marginal effect
table2_preterm[["All"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["All"]]$coefficients["pred_pfas"] - 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["All"]]$coefficients["pred_pfas"] + 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table2_preterm[["Moderately"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["Moderately"]]$coefficients["pred_pfas"] - 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["Moderately"]]$coefficients["pred_pfas"] + 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table2_preterm[["Very"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["Very"]]$coefficients["pred_pfas"] - 1.96 * mpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["Very"]]$coefficients["pred_pfas"] + 1.96 *mpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table2_preterm[["Extremely"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["Extremely"]]$coefficients["pred_pfas"] - 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_preterm[["Extremely"]]$coefficients["pred_pfas"] + 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

if(bs_cov){ #calculate and save covariance terms for national cost analysis
  cov_pre_lm = cov_boot(boot_coefs, "lpreterm", table2_preterm[["Moderately"]], "mpreterm", table2_preterm[["Very"]])
  cov_pre_lv = cov_boot(boot_coefs, "lpreterm", table2_preterm[["Moderately"]], "vpreterm", table2_preterm[["Extremely"]])
  cov_pre_mv = cov_boot(boot_coefs, "mpreterm", table2_preterm[["Very"]], "vpreterm", table2_preterm[["Extremely"]])
  save(cov_pre_lm, cov_pre_lv, cov_pre_mv, file = modify_path("Data_Verify/RData/cov_preterm.RData"))  
}



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
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table2_lbw.tex")) 

lbw_iv = table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"]
llbw_iv = table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"]
mlbw_iv = table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"]
vlbw_iv = table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"]

save(lbw_iv, llbw_iv, mlbw_iv, vlbw_iv, file = modify_path("Data_Verify/RData/lbw_iv_coef.RData"))

#calculate bootstrapped standard errors
lbw_sd = linear_bootstrap(boot_coefs, "lbw", table2_lbw[["Low Birthweight"]])
llbw_sd = linear_bootstrap(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]])
mlbw_sd = linear_bootstrap(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]])
vlbw_sd = linear_bootstrap(boot_coefs, "vlbw", table2_lbw[["Very Low Birthweight"]])
save(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd, file = modify_path("Data_Verify/RData/lbw_sd.RData"))

#marginal effects
table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

if(bs_cov){ #calculate and save covariance terms for national cost analysis
  cov_lbw_lm = cov_boot(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]], "mlbw", table2_lbw[["mLow Birthweight"]])
  cov_lbw_lv = cov_boot(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]], "vlbw", table2_lbw[["Very Low Birthweight"]])
  cov_lbw_mv = cov_boot(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]], "vlbw", table2_lbw[["Very Low Birthweight"]])
  save(cov_lbw_lm, cov_lbw_lv, cov_lbw_mv, file = modify_path("Data_Verify/RData/cov_lbw.RData"))  
}
1 - pnorm(table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"]/lbw_sd)
1 - pnorm(table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"]/llbw_sd)
1 - pnorm(table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"]/mlbw_sd)
1 - pnorm(table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"]/vlbw_sd)


#####################
### Mortality effects
mort_table = list()

mort_table[["Binary"]] = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 +fa_resid + wind_exposure
                                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["PFAS Interaction"]] = fixest::feols(death ~ (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["Distance Interaction"]] = fixest::feols(death ~ (updown + down) *I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid + wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["Contr. Health"]] = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 +fa_resid + wind_exposure + gestation + bweight
                                              |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["IV"]] = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                                     n_sites + wind_exposure + 
                                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                     m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)
mort_table[["Binary"]]$coefficients["down"]/mean(df$death)
(mort_table[["Binary"]]$coefficients["down"] - 1.96 * mort_table[["Binary"]]$se["down"])/mean(df$death)
(mort_table[["Binary"]]$coefficients["down"] + 1.96 * mort_table[["Binary"]]$se["down"])/mean(df$death)

modelsummary::modelsummary(mort_table, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient", 
                                        "down:I(dist/1000)" = "Downgradient \times Dist.", 
                                        "updown:I(dist/1000)" = "Upgradient \times Dist.",
                                        "down:I(pfas/10^3)" ="Downgradient \times PFAS",
                                        "updown:I(pfas/10^3)"="Upgradient \times PFAS",
                                        "pred_pfas" = "Predicted PFAS", 
                                        "gestation" = "Gestational Age", 
                                        "bweight" = "Birthweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_s4.tex")) 

#mortality standard error
mort_sd = linear_bootstrap(boot_coefs, "mort", mort_table[["IV"]])
save(mort_sd, file = modify_path("Data_Verify/RData/mort_sd.RData"))
#p value for IV
1 - pnorm(mort_table[["IV"]]$coefficients["pred_pfas"]/mort_sd)

#mortality IV estimate
mort_iv = mort_table[["IV"]]$coefficients["pred_pfas"]
save(mort_iv, file = modify_path("Data_Verify/RData/mort_iv_coef.RData"))

#mortality marginal effect
mort_table[["IV"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(mort_table[["IV"]]$coefficients["pred_pfas"] - 1.96 * mort_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(mort_table[["IV"]]$coefficients["pred_pfas"] + 1.96 * mort_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

if (bs_cov){
  cov_mort_pl = cov_boot(boot_coefs, "mort", mort_table[["IV"]], "lpreterm", table2_preterm[["Moderately"]])
  cov_mort_pm = cov_boot(boot_coefs, "mort", mort_table[["IV"]], "mpreterm", table2_preterm[["Very"]])
  cov_mort_pv = cov_boot(boot_coefs, "mort", mort_table[["IV"]], "vpreterm", table2_preterm[["Extremely"]])
  
  cov_mort_bl = cov_boot(boot_coefs, "mort", mort_table[["IV"]], "llbw", table2_lbw[["lLow Birthweight"]])
  cov_mort_bm = cov_boot(boot_coefs, "mort", mort_table[["IV"]], "mlbw", table2_lbw[["mLow Birthweight"]])
  cov_mort_bv = cov_boot(boot_coefs, "mort", mort_table[["IV"]], "vlbw", table2_lbw[["Very Low Birthweight"]])
  
  save(cov_mort_pl, cov_mort_pm, cov_mort_pv, cov_mort_bl, cov_mort_bm, cov_mort_bv, file = modify_path("Data_Verify/RData/cov_mort.RData"))
}



# Note: Table S-1 is based on authors' calculations while running (Code/Pub/Data/data_head.R)
#Tables S-4 (Code/Pub/Robustness/Oster.R), S-5 (Code/Pub/Robustness/Placebo/placebo_head.R),
#S-9 (Code/Pub/bootstrap.R), S-12 (Code/Pub/Robustness/NY)

#######################
####Table S-2 (balance table)
if (code_check){
  df$max_educ = max(df$m_educ, df$f_educ)
  df$n_prenatal = sample(1:20, nrow(df), replace = T)
  df$cig = sample(0:1, nrow(df), replace = T)
  df$white = sample(0:1, nrow(df), replace = T)
  df$m_months_res = sample(1:20, nrow(df), replace = T)
  df$mr_23 = sample(0:1, nrow(df), replace = T)
  df$mr_04 = sample(0:1, nrow(df), replace = T)
  df$mr_18 = sample(0:1, nrow(df), replace = T)
  df$mr_08 = sample(0:1, nrow(df), replace = T)
  df$mr_10 = sample(0:1, nrow(df), replace = T)
}
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
                    output = modify_path2("Tables/table_s2.tex")) 

length(which(df2$group == 1))


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
  dplyr::select(`Infant Mortality` = death,
                `Extremely Preterm` = vpre, 
                `Very Preterm` = mpre, 
                `Moderately Preterm` = lpre, 
                `Extremely Low Birthweight` = vlbw, 
                `Very Low Birthweight` = mlbw, 
                `Moderately Low Birthweight` = llbw,
                `Number of Sites w/in 5km` = n_sites, 
                `Well Distance` = dist, 
                `Residence Distance` = csite_dist, 
                `PFOA + PFOS at Site` = pfas, 
                group
  )
df2 = as.data.frame(df2)
df2$`Well Distance` = df2$`Well Distance`/1000
df2$`Residence Distance`= df2$`Residence Distance`/1000
df2$`PFOA + PFOS at Site` = df2$`PFOA + PFOS at Site`/1000


datasummary_balance(~group, 
                    data = df2, 
                    na.rm = T, 
                    fmt = modelsummary::fmt_significant(2, scientific = F, 
                                                        zero.print = T, drop0trailing = F, 
                                                        nsmall = 2), 
                    output =modify_path2("Tables/table_s3.tex"))





##########################
#####Table S-7 (interaction with distance)
#preterm
tables6_preterm = list() 
tables6_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  (updown + down) *I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
tables6_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  (updown + down) *I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid + wind_exposure
                                                |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  (updown + down) *I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                            m_height + tri5 + fa_resid + wind_exposure
                                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  (updown + down) *I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid+ wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(tables6_preterm, 
                           stars = c("*" = 0.2, "**" = 0.10, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "down:I(dist/1000)" = "Downgradient $\times$ Dist", 
                                        "updown:I(dist/1000)" = "Upgradient $\times$ Dist"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_s7_preterm.tex"))




#low birthweight
tables6_lbw = list() 
tables6_lbw[["All"]] = fixest::feols(I(bweight < 2500) ~   (updown + down) *I(dist/1000)  +I(pfas/10^3)  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Full Term"]] = fixest::feols(I(bweight < 2500) ~   (updown + down) *I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height + tri5 + fa_resid + wind_exposure
                                           |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Low"]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~   (updown + down) *I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Very"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~   (updown + down) *I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                        m_height + tri5 + fa_resid + wind_exposure
                                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables6_lbw[["Extremely"]] = fixest::feols(I(bweight < 1000) ~   (updown + down) *I(dist/1000)  + I(pfas/10^3)  + n_sites + 
                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height + tri5 + fa_resid+ wind_exposure
                                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



modelsummary::modelsummary(tables6_lbw, 
                           stars = c("*" = 0.2, "**" = 0.10, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "down:I(dist/1000)" = "Downgradient $\times$ Dist", 
                                        "updown:I(dist/1000)" = "Upgradient $\times$ Dist"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_s7_lbw.tex"))

##########################
#####Table S-6 (interaction with PFAS)
#preterm
tables7_preterm = list() 
tables7_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
tables7_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid + wind_exposure
                                                |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                            m_height + tri5 + fa_resid + wind_exposure
                                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid+ wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(tables7_preterm, 
                           stars = c("*" = 0.2, "**" = 0.10, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "down:I(pfas/10^3)" = "Downgradient PFAS", 
                                        "updown:I(pfas/10^3)" = "Upgradient PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_s6_preterm.tex"))




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

tables7_lbw[["Very"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~   (updown + down) *I(pfas/10^3)  + dist  + n_sites + 
                                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                        m_height + tri5 + fa_resid + wind_exposure
                                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables7_lbw[["Extremely"]] = fixest::feols(I(bweight < 1000) ~   (updown + down) *I(pfas/10^3)  + dist  + n_sites + 
                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height + tri5 + fa_resid+ wind_exposure
                                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



modelsummary::modelsummary(tables7_lbw, 
                           stars = c("*" = 0.2, "**" = 0.10, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "down:I(pfas/10^3)" = "Downgradient PFAS", 
                                        "updown:I(pfas/10^3)" = "Upgradient PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_s6_lbw.tex"))


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
tables8_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid + wind_exposure+ bweight
                                                |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables8_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                            m_height + tri5 + fa_resid + wind_exposure+ bweight
                                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tables8_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid+ wind_exposure+ bweight
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))




modelsummary::modelsummary(tables8_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "bweight" = "Birthweight", 
                                        "gestation" = "Gestational Age"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_s8_preterm.tex"))

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
                           stars = c("*" = 0.2, "**" = 0.10, "***" = 0.02), 
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "bweight" = "Birthweight", 
                                        "gestation" = "Gestational Age"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/table_s8_lbw.tex"))



#######################
#### Table S-12 (first stage)
w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                        updown + wind_exposure + domestic + temp + pm25 + med_inc +
                        p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont) 

table_s11 = modelsummary::modelsummary(list(w_reg),
                                       stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                                       fmt = modelsummary::fmt_significant(2, scientific = F), 
                                       gof_map = c("nobs", "r.squared"), 
                                       output = modify_path2("Tables/table_s12.tex"))



#Oster Coefficient - \delta in paper (Table S-5)
source("PFAS-Code/Pub/Robustness/oster_selection.R")
sink(modify_path2("Tables/table_s5.tex"))
print(d_mort)
print(d_pre)
print(d_lpre)
print(d_mpre)
print(d_vpre)
print(d_lbw)
print(d_llbw)
print(d_mlbw)
print(d_vlbw)
sink() 