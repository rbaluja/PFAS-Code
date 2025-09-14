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
load(modify_path(paste0("Data_Verify/RData/nd_w_reg", ppt, ".RData")))
load(modify_path(paste0("Data_Verify/RData/nott_w_reg", ppt, ".RData")))



df$pred_pfas_nd = predict(w_reg_nd, df)
df$pred_pfas_nott = predict(w_reg_nott, df)

if (!file.exists(modify_path("Data_Verify/RData/Revision2/bootstrap_ndomestic.RData"))) {
  stop("Bootstrap standard errors")
}
boot_coefs_nd = get(load(modify_path("Data_Verify/RData/Revision2/bootstrap_ndomestic.RData")))

if (!file.exists(modify_path("Data_Verify/RData/Revision2/bootstrap_nott.RData"))) {
  stop("Bootstrap standard errors")
}
boot_coefs_nott = get(load(modify_path("Data_Verify/RData/Revision2/bootstrap_nott.RData")))

#preterm
table2_preterm = list()

table2_preterm[["All"]]= fixest::feols(I(gestation < 37) ~ pred_pfas_nd + asinh(pfas) +
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Moderately"]]= fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas_nd + asinh(pfas) + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_preterm[["Very"]]= fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas_nd + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Extremely"]]= fixest::feols(I(gestation < 28) ~ pred_pfas_nd + asinh(pfas) + 
                                               n_sites + wind_exposure + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

modelsummary::modelsummary(table2_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas_nd" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_preterm_nd.tex"))

preterm_iv = table2_preterm[["All"]]$coefficients["pred_pfas_nd"]
lpreterm_iv = table2_preterm[["Moderately"]]$coefficients["pred_pfas_nd"]
mpreterm_iv = table2_preterm[["Very"]]$coefficients["pred_pfas_nd"]
vpreterm_iv = table2_preterm[["Extremely"]]$coefficients["pred_pfas_nd"]

#calculate bootstrapped standard errors
preterm_sd = linear_nd_bootstrap(boot_coefs_nd, "preterm", table2_preterm[["All"]])
lpreterm_sd = linear_nd_bootstrap(boot_coefs_nd, "lpreterm", table2_preterm[["Moderately"]])
mpreterm_sd = linear_nd_bootstrap(boot_coefs_nd, "mpreterm", table2_preterm[["Very"]])
vpreterm_sd = linear_nd_bootstrap(boot_coefs_nd, "vpreterm", table2_preterm[["Extremely"]])
save(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd, file = modify_path(paste0("Data_Verify/RData/preterm_sd_nd", ppt, ".RData")))

save(preterm_iv, lpreterm_iv, mpreterm_iv, vpreterm_iv, file = modify_path(paste0("Data_Verify/RData/preterm_iv_coef_nd", ppt, ".RData")))

write.csv(data.frame(
  "outcome" = c("All", "Moderately", "Very", "Extremely"),
  "iv" = c(preterm_iv, lpreterm_iv, mpreterm_iv, vpreterm_iv),
  "sd" = c(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd), 
  "t_stat" = c(preterm_iv/preterm_sd, lpreterm_iv/lpreterm_sd, mpreterm_iv/mpreterm_sd, vpreterm_iv/vpreterm_sd)
  ), 
  file = modify_path2(paste0("Tables/Revisions/preterm_iv_nd", ppt, ".csv"))
)


#lbw
table2_lbw = list()
table2_lbw[["Low Birthweight"]]= fixest::feols(I(bweight < 2500 ) ~ pred_pfas_nd + asinh(pfas) +
                                                 n_sites + wind_exposure + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["lLow Birthweight"]]= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas_nd + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["mLow Birthweight"]]= fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas_nd + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_lbw[["Very Low Birthweight"]]= fixest::feols(I(bweight < 1000) ~ pred_pfas_nd + asinh(pfas) +
                                                      n_sites + wind_exposure + 
                                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


modelsummary::modelsummary(table2_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas_nd" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_lbw_nd.tex")) 

lbw_iv = table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas_nd"]
llbw_iv = table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas_nd"]
mlbw_iv = table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas_nd"]
vlbw_iv = table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas_nd"]

save(lbw_iv, llbw_iv, mlbw_iv, vlbw_iv, file = modify_path(paste0("Data_Verify/RData/lbw_iv_coef_nd", ppt, ".RData")))

lbw_sd = linear_nd_bootstrap(boot_coefs_nd, "lbw", table2_lbw[["Low Birthweight"]])
llbw_sd = linear_nd_bootstrap(boot_coefs_nd, "llbw", table2_lbw[["lLow Birthweight"]])
mlbw_sd = linear_nd_bootstrap(boot_coefs_nd, "mlbw", table2_lbw[["mLow Birthweight"]])
vlbw_sd = linear_nd_bootstrap(boot_coefs_nd, "vlbw", table2_lbw[["Very Low Birthweight"]])
save(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd, file = modify_path(paste0("Data_Verify/RData/lbw_sd_nd", ppt, ".RData")))

write.csv(data.frame(
  "outcome" = c("All", "Moderately", "Very", "Extremely"),
  "iv" = c(lbw_iv, llbw_iv, mlbw_iv, vlbw_iv), 
  "sd" = c(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd), 
  "t_stat" = c(lbw_iv/lbw_sd, llbw_iv/llbw_sd, mlbw_iv/mlbw_sd, vlbw_iv/vlbw_sd)
), 
file = modify_path2(paste0("Tables/Revisions/lbw_iv_nd", ppt, ".csv"))
)

mort_table = list()
mort_table[["IV"]] = fixest::feols(death ~ pred_pfas_nd + asinh(pfas) + 
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
                                        "pred_pfas_nd" = "Predicted PFAS", 
                                        "gestation" = "Gestational Age", 
                                        "bweight" = "Birthweight"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/table2_mort_nd.tex")) 


mort_iv = mort_table[["IV"]]$coefficients["pred_pfas_nd"]
save(mort_iv, file = modify_path(paste0("Data_Verify/RData/mort_iv_coef_nd", ppt, ".RData")))

mort_sd = linear_nd_bootstrap(boot_coefs_nd, "mort", mort_table[["IV"]])
save(mort_sd, file = modify_path(paste0("Data_Verify/RData/mort_sd_nd", ppt, ".RData")))

write.csv(data.frame(
  "outcome" = c("Mortality"),
  "iv" = c(mort_iv), 
  "sd" = c(mort_sd), 
  "t_stat" = c(mort_iv/mort_sd)
),
file = modify_path2(paste0("Tables/Revisions/mort_iv_nd", ppt, ".csv"))
)

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
preterm_sd = linear_nott_bootstrap(boot_coefs, "preterm", table2_preterm[["All"]])
lpreterm_sd = linear_nott_bootstrap(boot_coefs, "lpreterm", table2_preterm[["Moderately"]])
mpreterm_sd = linear_nott_bootstrap(boot_coefs, "mpreterm", table2_preterm[["Very"]])
vpreterm_sd = linear_nott_bootstrap(boot_coefs, "vpreterm", table2_preterm[["Extremely"]])
save(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd, file = modify_path(paste0("Data_Verify/RData/preterm_sd_nott", ppt, ".RData")))

write.csv(data.frame(
  "outcome" = c("All", "Moderately", "Very", "Extremely"),
  "iv" = c(preterm_iv, lpreterm_iv, mpreterm_iv, vpreterm_iv),
  "sd" = c(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd), 
  "t_stat" = c(preterm_iv/preterm_sd, lpreterm_iv/lpreterm_sd, mpreterm_iv/mpreterm_sd, vpreterm_iv/vpreterm_sd)
),
file = modify_path2(paste0("Tables/Revisions/preterm_iv_nott", ppt, ".csv"))
)


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

lbw_sd = linear_nott_bootstrap(boot_coefs, "lbw", table2_lbw[["Low Birthweight"]])
llbw_sd = linear_nott_bootstrap(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]])
mlbw_sd = linear_nott_bootstrap(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]])
vlbw_sd = linear_nott_bootstrap(boot_coefs, "vlbw", table2_lbw[["Very Low Birthweight"]])
save(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd, file = modify_path(paste0("Data_Verify/RData/lbw_sd_linear_nott", ppt, ".RData")))

write.csv(data.frame(
  "outcome" = c("All", "Moderately", "Very", "Extremely"),
  "iv" = c(lbw_iv, llbw_iv, mlbw_iv, vlbw_iv), 
  "sd" = c(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd), 
  "t_stat" = c(lbw_iv/lbw_sd, llbw_iv/llbw_sd, mlbw_iv/mlbw_sd, vlbw_iv/vlbw_sd)
),
file = modify_path2(paste0("Tables/Revisions/lbw_iv_nott", ppt, ".csv"))
)

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

mort_sd = linear_nott_bootstrap(boot_coefs, "mort", mort_table[["IV"]])
save(mort_sd, file = modify_path(paste0("Data_Verify/RData/mort_sd_nott", ppt, ".RData")))

write.csv(data.frame(
  "outcome" = c("Mortality"),
  "iv" = c(mort_iv), 
  "sd" = c(mort_sd), 
  "t_stat" = c(mort_iv/mort_sd)
),
file = modify_path2(paste0("Tables/Revisions/mort_iv_nott", ppt, ".csv"))
)



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



w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, sand, clay, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                        updown + wind_exposure + domestic + temp + pm25 + med_inc +
                        p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont) 

modelsummary::modelsummary(list(w_reg),
                                       stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                                       fmt = modelsummary::fmt_significant(2, scientific = F), 
                                       gof_map = c("nobs", "r.squared"), 
                                       output = modify_path2("Tables/Revisions/first_stage_rev2.tex"))

w_reg_nd = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, sand, clay, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                           updown + wind_exposure + temp + pm25 + med_inc +
                           p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont %>% filter(domestic == 0)) 

modelsummary::modelsummary(list(w_reg_nd),
                                       stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                                       fmt = modelsummary::fmt_significant(2, scientific = F), 
                                       gof_map = c("nobs", "r.squared"), 
                                       output = modify_path2("Tables/Revisions/first_stage_nod.tex"))


#missing obs indicators
#Preterm
df %>%
  mutate(mis_m_age = ifelse(is.na(m_age), 1, 0), 
         mis_m_married = ifelse(is.na(m_married), 1, 0),
         mis_private_insurance = ifelse(is.na(private_insurance), 1, 0),
         mis_nbr_cgrtt = ifelse(is.na(nbr_cgrtt), 1, 0),
         mis_m_educ = ifelse(is.na(m_educ), 1, 0),
         mis_f_educ = ifelse(is.na(f_educ), 1, 0),
         mis_pm25 = ifelse(is.na(pm25), 1, 0),
         mis_temp = ifelse(is.na(temp), 1, 0),
         mis_med_inc = ifelse(is.na(med_inc), 1, 0),
         mis_p_manuf = ifelse(is.na(p_manuf), 1, 0),
         mis_n_hunits = ifelse(is.na(n_hunits), 1, 0),
         mis_med_hprice = ifelse(is.na(med_hprice), 1, 0),
         mis_well_elev = ifelse(is.na(well_elev), 1, 0),
         mis_resid_elev = ifelse(is.na(resid_elev), 1, 0),
         mis_csite_dist = ifelse(is.na(csite_dist), 1, 0),
         mis_mthr_wgt_dlv = ifelse(is.na(mthr_wgt_dlv), 1, 0),
         mis_mthr_pre_preg_wgt = ifelse(is.na(mthr_pre_preg_wgt), 1, 0),
         mis_m_height = ifelse(is.na(m_height), 1, 0),
         mis_fa_resid = ifelse(is.na(fa_resid), 1, 0), 
         mis_wic = ifelse(is.na(wic), 1, 0), 
         mis_mr_04 = ifelse(is.na(mr_04), 1, 0),
         mis_mr_08 = ifelse(is.na(mr_08), 1, 0),
         mis_mr_18 = ifelse(is.na(mr_18), 1, 0),
         mis_mr_21 = ifelse(is.na(mr_21), 1, 0),
         mis_mr_26 = ifelse(is.na(mr_26), 1, 0),
         mis_mr_27 = ifelse(is.na(mr_27), 1, 0), 
         mis_tri5 = ifelse(is.na(tri5), 1, 0), 
         mis_birth_race = ifelse(is.na(birth_race_dsc_1), 1, 0)) -> df

  
          
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~ mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                          mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                          mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                          mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                          mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                          mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                          
                                        |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                                 mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                                 mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                                 mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                                 mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                                 mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                               
                                               |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                           mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                           mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                           mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                           mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                           mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                         
                                         |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                                mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                                mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                                mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                                mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                                mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                              
                                              |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("mis_m_age" = "Missing Maternal Age", 
                                        "mis_m_married" = "Missing Maternal Marital Status",
                                        "mis_private_insurance" = "Missing Private Insurance",
                                        "mis_nbr_cgrtt" = "Missing Number of Cigarettes",
                                        "mis_m_educ" = "Missing Maternal Education",
                                        "mis_f_educ" = "Missing Paternal Education",
                                        "mis_pm25" = "Missing PM2.5",
                                        "mis_temp" = "Missing Temperature",
                                        "mis_med_inc" = "Missing Median Income",
                                        "mis_p_manuf" = "Missing Percent Manufacturing",
                                        "mis_n_hunits" = "Missing Number of Housing Units",
                                        "mis_med_hprice" = "Missing Median Home Price",
                                        "mis_well_elev" = "Missing Well Elevation",
                                        "mis_resid_elev" = "Missing Residential Elevation",
                                        "mis_csite_dist" = "Missing Contaminated Site Distance",
                                        "mis_wic" = "Missing WIC",
                                        "mis_mr_04" = "Missing Pre-Pregnancy Diabetes",
                                        "mis_mr_18" = "Missing Gestational Diabetes",
                                        "mis_mr_08" = "Missing Hypertension",
                                        "mis_mr_21" = "Missing Previous C-Section",
                                        "mis_mr_26" = "Missing Fertility Enhancing Drugs",
                                        "mis_mr_27" = "Missing Invitro Fertilization",
                                        "mis_mthr_wgt_dlv" = "Missing Maternal Delivery Weight",
                                        "mis_mthr_pre_preg_wgt" = "Missing Maternal Pre-Pregnancy Weight",
                                        "mis_m_height" = "Missing Maternal Height",
                                        "mis_fa_resid" = "Missing Flow Accumulation at Residence",
                                        "mis_tri5" = "Missing TRI sites",
                                        "mis_birth_race" = "Missing Birth Race"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_miss_covars.tex")) 


#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                                       mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                                       mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                                       mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                                       mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                                       mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                                     
                                                     |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                                                   mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                                                   mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                                                   mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                                                   mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                                                   mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                                                 
                                                                 |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                                   mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                                   mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                                   mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                                   mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                                   mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                                 
                                                 |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                                             mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                                             mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                                             mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                                             mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                                             mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                                           
                                                           |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                                       mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                                       mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                                       mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                                       mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                                       mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                                     
                                                     |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("mis_m_age" = "Missing Maternal Age", 
                                        "mis_m_married" = "Missing Maternal Marital Status",
                                        "mis_private_insurance" = "Missing Private Insurance",
                                        "mis_nbr_cgrtt" = "Missing Number of Cigarettes",
                                        "mis_m_educ" = "Missing Maternal Education",
                                        "mis_f_educ" = "Missing Paternal Education",
                                        "mis_pm25" = "Missing PM2.5",
                                        "mis_temp" = "Missing Temperature",
                                        "mis_med_inc" = "Missing Median Income",
                                        "mis_p_manuf" = "Missing Percent Manufacturing",
                                        "mis_n_hunits" = "Missing Number of Housing Units",
                                        "mis_med_hprice" = "Missing Median Home Price",
                                        "mis_well_elev" = "Missing Well Elevation",
                                        "mis_resid_elev" = "Missing Residential Elevation",
                                        "mis_csite_dist" = "Missing Contaminated Site Distance",
                                        "mis_wic" = "Missing WIC",
                                        "mis_mr_04" = "Missing Pre-Pregnancy Diabetes",
                                        "mis_mr_18" = "Missing Gestational Diabetes",
                                        "mis_mr_08" = "Missing Hypertension",
                                        "mis_mr_21" = "Missing Previous C-Section",
                                        "mis_mr_26" = "Missing Fertility Enhancing Drugs",
                                        "mis_mr_27" = "Missing Invitro Fertilization",
                                        "mis_mthr_wgt_dlv" = "Missing Maternal Delivery Weight",
                                        "mis_mthr_pre_preg_wgt" = "Missing Maternal Pre-Pregnancy Weight",
                                        "mis_m_height" = "Missing Maternal Height",
                                        "mis_fa_resid" = "Missing Flow Accumulation at Residence",
                                        "mis_tri5" = "Missing TRI sites",
                                        "mis_birth_race" = "Missing Birth Race"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_mis_covars.tex")) 

mort_table = list()

mort_table[["Binary"]] = fixest::feols(death ~  mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                                         mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                                         mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                                         mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                                         mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                                         mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                                       
                                       |county +  year^month, data = df %>% filter(!is.na(down) & !is.na(updown) & !is.na(dist) & dist <= 5000), warn = F, notes = F)

modelsummary::modelsummary(mort_table, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("mis_m_age" = "Missing Maternal Age", 
                                                   "mis_m_married" = "Missing Maternal Marital Status",
                                                   "mis_private_insurance" = "Missing Private Insurance",
                                                   "mis_nbr_cgrtt" = "Missing Number of Cigarettes",
                                                   "mis_m_educ" = "Missing Maternal Education",
                                                   "mis_f_educ" = "Missing Paternal Education",
                                                   "mis_pm25" = "Missing PM2.5",
                                                   "mis_temp" = "Missing Temperature",
                                                   "mis_med_inc" = "Missing Median Income",
                                                   "mis_p_manuf" = "Missing Percent Manufacturing",
                                                   "mis_n_hunits" = "Missing Number of Housing Units",
                                                   "mis_med_hprice" = "Missing Median Home Price",
                                                   "mis_well_elev" = "Missing Well Elevation",
                                                   "mis_resid_elev" = "Missing Residential Elevation",
                                                   "mis_csite_dist" = "Missing Contaminated Site Distance",
                                                   "mis_wic" = "Missing WIC",
                                                   "mis_mr_04" = "Missing Pre-Pregnancy Diabetes",
                                                   "mis_mr_18" = "Missing Gestational Diabetes",
                                                   "mis_mr_08" = "Missing Hypertension",
                                                   "mis_mr_21" = "Missing Previous C-Section",
                                                   "mis_mr_26" = "Missing Fertility Enhancing Drugs",
                                                   "mis_mr_27" = "Missing Invitro Fertilization",
                                                   "mis_mthr_wgt_dlv" = "Missing Maternal Delivery Weight",
                                                   "mis_mthr_pre_preg_wgt" = "Missing Maternal Pre-Pregnancy Weight",
                                                   "mis_m_height" = "Missing Maternal Height",
                                                   "mis_fa_resid" = "Missing Flow Accumulation at Residence",
                                                   "mis_tri5" = "Missing TRI sites",
                                                   "mis_birth_race" = "Missing Birth Race"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_mis_covars.tex")) 


#by industry of contaminated site
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
                           output = modify_path2("Tables/Revisions/table1_preterm_industry.tex"))

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
                           output = modify_path2("Tables/Revisions/table1_lbw_industry.tex")) 


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
                           output = modify_path2("Tables/Revisions/table1_mort_industry.tex")) 


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
                           output = modify_path2("Tables/Revisions/table1_preterm_landfill.tex"))

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
                           output = modify_path2("Tables/Revisions/table1_lbw_landfill.tex")) 


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
                           output = modify_path2("Tables/Revisions/table1_mort_landfill.tex")) 

