#read in bootstrap functions
source("PFAS-Code/Pub/Tables/bs_functions.R")
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}


#Preterm
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +  fa_resid +wind_exposure
                                         |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid +wind_exposure
                                              |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_pws.tex")) 

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
                                                     |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 + fa_resid + wind_exposure
                                                                 |county + sys_id + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 + fa_resid + wind_exposure
                                                           |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid+ wind_exposure
                                                     |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_pws.tex")) 
table1_lbw[["Very Low Birthweight"]]$coefficients["down"]/mean(df$bweight < 1000)
(table1_lbw[["Very Low Birthweight"]]$coefficients["down"] - 1.96 * table1_lbw[["Very Low Birthweight"]]$se["down"])/mean(df$bweight < 1000)
(table1_lbw[["Very Low Birthweight"]]$coefficients["down"] + 1.96 * table1_lbw[["Very Low Birthweight"]]$se["down"])/mean(df$bweight < 1000)


mort_table = list()

mort_table[["Binary"]] = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 +fa_resid + wind_exposure
                                       |county + sys_id + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

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
                           output = modify_path2("Tables/Revisions/mort_pws.tex")) 



#IV tables
################
load(modify_path(paste0("Data_Verify/RData/linear_w_reg", ppt, ".RData")))
load(modify_path(paste0("Data_Verify/RData/nott_w_reg", ppt, ".RData")))



df$pred_pfas_linear = predict(w_reg_linear, df)
df$pred_pfas_nott = predict(w_reg_nott, df)

if (!file.exists(modify_path("Data_Verify/RData/Revision2/bootstrap_linear.RData"))) {
  stop("Bootstrap standard errors")
}
load(modify_path("Data_Verify/RData/Revision2/bootstrap_linear.RData"))

if (!file.exists(modify_path("Data_Verify/RData/Revision2/bootstrap_nott.RData"))) {
  stop("Bootstrap standard errors")
}
load(modify_path("Data_Verify/RData/Revision2/bootstrap_nott.RData"))

#preterm
table2_preterm = list()

table2_preterm[["All"]]= fixest::feols(I(gestation < 37) ~ I(pred_pfas_linear/1000) + pfas + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Moderately"]]= fixest::feols(I(gestation < 37 & gestation >= 32) ~ I(pred_pfas_linear/1000) + pfas + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_preterm[["Very"]]= fixest::feols(I(gestation < 32 & gestation >= 28) ~ I(pred_pfas_linear/1000) + pfas + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Extremely"]]= fixest::feols(I(gestation < 28) ~ I(pred_pfas_linear/1000) + pfas + 
                                               n_sites + wind_exposure + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

modelsummary::modelsummary(table2_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("I(pred_pfas_linear/1000)" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_preterm_linear.tex"))

preterm_iv = table2_preterm[["All"]]$coefficients["I(pred_pfas_linear/1000)"]
lpreterm_iv = table2_preterm[["Moderately"]]$coefficients["I(pred_pfas_linear/1000)"]
mpreterm_iv = table2_preterm[["Very"]]$coefficients["I(pred_pfas_linear/1000)"]
vpreterm_iv = table2_preterm[["Extremely"]]$coefficients["I(pred_pfas_linear/1000)"]

#calculate bootstrapped standard errors
preterm_sd = linear_bootstrap(boot_coefs, "preterm", table2_preterm[["All"]])
lpreterm_sd = linear_bootstrap(boot_coefs, "lpreterm", table2_preterm[["Moderately"]])
mpreterm_sd = linear_bootstrap(boot_coefs, "mpreterm", table2_preterm[["Very"]])
vpreterm_sd = linear_bootstrap(boot_coefs, "vpreterm", table2_preterm[["Extremely"]])
save(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd, file = modify_path(paste0("Data_Verify/RData/preterm_sd_linear", ppt, ".RData")))

save(preterm_iv, lpreterm_iv, mpreterm_iv, vpreterm_iv, file = modify_path(paste0("Data_Verify/RData/preterm_iv_coef_linear", ppt, ".RData")))


#lbw
table2_lbw = list()
table2_lbw[["Low Birthweight"]]= fixest::feols(I(bweight < 2500 ) ~ I(pred_pfas_linear/1000) + pfas +
                                                 n_sites + wind_exposure + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["lLow Birthweight"]]= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ I(pred_pfas_linear/1000) + pfas +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["mLow Birthweight"]]= fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ I(pred_pfas_linear/1000) + pfas +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_lbw[["Very Low Birthweight"]]= fixest::feols(I(bweight < 1000) ~ I(pred_pfas_linear/1000) + pfas +
                                                      n_sites + wind_exposure + 
                                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


modelsummary::modelsummary(table2_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("I(pred_pfas_linear/1000)" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_lbw_linear.tex")) 

lbw_iv = table2_lbw[["Low Birthweight"]]$coefficients["I(pred_pfas_linear/1000)"]
llbw_iv = table2_lbw[["lLow Birthweight"]]$coefficients["I(pred_pfas_linear/1000)"]
mlbw_iv = table2_lbw[["mLow Birthweight"]]$coefficients["I(pred_pfas_linear/1000)"]
vlbw_iv = table2_lbw[["Very Low Birthweight"]]$coefficients["I(pred_pfas_linear/1000)"]

save(lbw_iv, llbw_iv, mlbw_iv, vlbw_iv, file = modify_path(paste0("Data_Verify/RData/lbw_iv_coef_linear", ppt, ".RData")))

lbw_sd = linear_bootstrap(boot_coefs, "lbw", table2_lbw[["Low Birthweight"]])
llbw_sd = linear_bootstrap(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]])
mlbw_sd = linear_bootstrap(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]])
vlbw_sd = linear_bootstrap(boot_coefs, "vlbw", table2_lbw[["Very Low Birthweight"]])
save(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd, file = modify_path(paste0("Data_Verify/RData/lbw_sd_linear", ppt, ".RData")))

mort_table = list()
mort_table[["IV"]] = fixest::feols(death ~ I(pred_pfas_linear/1000) + pfas + 
                                     n_sites + wind_exposure + 
                                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                     m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)

modelsummary::modelsummary(mort_table, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient", 
                                        "down:I(dist/1000)" = "Downgradient \times Dist.", 
                                        "updown:I(dist/1000)" = "Upgradient \times Dist.",
                                        "down:I(pfas/10^3)" ="Downgradient \times PFAS",
                                        "updown:I(pfas/10^3)"="Upgradient \times PFAS",
                                        "I(pred_pfas_linear/1000)" = "Predicted PFAS", 
                                        "gestation" = "Gestational Age", 
                                        "bweight" = "Birthweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_mort_linear.tex")) 


mort_iv = mort_table[["IV"]]$coefficients["I(pred_pfas_linear/1000)"]
save(mort_iv, file = modify_path(paste0("Data_Verify/RData/mort_iv_coef_linear", ppt, ".RData")))

mort_sd = linear_bootstrap(boot_coefs, "mort", mort_table[["IV"]])
save(mort_sd, file = modify_path(paste0("Data_Verify/RData/mort_sd_linear", ppt, ".RData")))

###No Time trend
#preterm
table2_preterm = list()

table2_preterm[["All"]]= fixest::feols(I(gestation < 37) ~ pred_pfas_nott + asinh(pfas) + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Moderately"]]= fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas_nott + asinh(pfas) + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_preterm[["Very"]]= fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas_nott + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Extremely"]]= fixest::feols(I(gestation < 28) ~ pred_pfas_nott + asinh(pfas) + 
                                               n_sites + wind_exposure + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

modelsummary::modelsummary(table2_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas_nott" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_preterm_nott.tex"))

preterm_iv = table2_preterm[["All"]]$coefficients["pred_pfas_nott"]
lpreterm_iv = table2_preterm[["Moderately"]]$coefficients["pred_pfas_nott"]
mpreterm_iv = table2_preterm[["Very"]]$coefficients["pred_pfas_nott"]
vpreterm_iv = table2_preterm[["Extremely"]]$coefficients["pred_pfas_nott"]

save(preterm_iv, lpreterm_iv, mpreterm_iv, vpreterm_iv, file = modify_path(paste0("Data_Verify/RData/preterm_iv_coef_nott", ppt, ".RData")))

#calculate bootstrapped standard errors
preterm_sd = linear_bootstrap(boot_coefs, "preterm", table2_preterm[["All"]])
lpreterm_sd = linear_bootstrap(boot_coefs, "lpreterm", table2_preterm[["Moderately"]])
mpreterm_sd = linear_bootstrap(boot_coefs, "mpreterm", table2_preterm[["Very"]])
vpreterm_sd = linear_bootstrap(boot_coefs, "vpreterm", table2_preterm[["Extremely"]])
save(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd, file = modify_path(paste0("Data_Verify/RData/preterm_sd_nott", ppt, ".RData")))


#lbw
table2_lbw = list()
table2_lbw[["Low Birthweight"]]= fixest::feols(I(bweight < 2500 ) ~ pred_pfas_nott + asinh(pfas) +
                                                 n_sites + wind_exposure + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["lLow Birthweight"]]= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas_nott + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["mLow Birthweight"]]= fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas_nott + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_lbw[["Very Low Birthweight"]]= fixest::feols(I(bweight < 1000) ~ pred_pfas_nott + asinh(pfas) +
                                                      n_sites + wind_exposure + 
                                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


modelsummary::modelsummary(table2_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas_nott" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_lbw_nott.tex")) 

lbw_iv = table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas_nott"]
llbw_iv = table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas_nott"]
mlbw_iv = table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas_nott"]
vlbw_iv = table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas_nott"]

save(lbw_iv, llbw_iv, mlbw_iv, vlbw_iv, file = modify_path(paste0("Data_Verify/RData/lbw_iv_coef_nott", ppt, ".RData")))

lbw_sd = linear_bootstrap(boot_coefs, "lbw", table2_lbw[["Low Birthweight"]])
llbw_sd = linear_bootstrap(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]])
mlbw_sd = linear_bootstrap(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]])
vlbw_sd = linear_bootstrap(boot_coefs, "vlbw", table2_lbw[["Very Low Birthweight"]])
save(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd, file = modify_path(paste0("Data_Verify/RData/lbw_sd_linear_nott", ppt, ".RData")))

mort_table = list()
mort_table[["IV"]] = fixest::feols(death ~ pred_pfas_nott + asinh(pfas) + 
                                     n_sites + wind_exposure + 
                                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                     m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)

modelsummary::modelsummary(mort_table, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient", 
                                        "down:I(dist/1000)" = "Downgradient \times Dist.", 
                                        "updown:I(dist/1000)" = "Upgradient \times Dist.",
                                        "down:I(pfas/10^3)" ="Downgradient \times PFAS",
                                        "updown:I(pfas/10^3)"="Upgradient \times PFAS",
                                        "pred_pfas_nott" = "Predicted PFAS", 
                                        "gestation" = "Gestational Age", 
                                        "bweight" = "Birthweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_mort_nott.tex")) 


mort_iv = mort_table[["IV"]]$coefficients["pred_pfas_nott"]
save(mort_iv, file = modify_path(paste0("Data_Verify/RData/mort_iv_coef_nott", ppt, ".RData")))

mort_sd = linear_bootstrap(boot_coefs, "mort", mort_table[["IV"]])
save(mort_sd, file = modify_path(paste0("Data_Verify/RData/mort_sd_nott", ppt, ".RData")))


#######################
#### Alternate first stage
w_reg = fixest::feols(I(wellpfas/1000) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + pfas + log(dist)*down + 
                        updown + wind_exposure + domestic + temp + pm25 + med_inc +
                        p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont) 

table_s11 = modelsummary::modelsummary(list(w_reg),
                                       stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                                       fmt = modelsummary::fmt_significant(2, scientific = F), 
                                       gof_map = c("nobs", "r.squared"), 
                                       output = modify_path2("Tables/Revisions/first_stage_linear.tex"))

w_reg_nott = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                        updown + wind_exposure + domestic + temp + pm25 + med_inc +
                        p_manuf + n_hunits + med_hprice + elevation + tri5, data = fs_cont) 

table_s11 = modelsummary::modelsummary(list(w_reg_nott),
                                       stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                                       fmt = modelsummary::fmt_significant(2, scientific = F), 
                                       gof_map = c("nobs", "r.squared"), 
                                       output = modify_path2("Tables/Revisions/first_stage_nott.tex"))