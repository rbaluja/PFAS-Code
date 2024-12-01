source("PFAS-Code/Pub/config.R")
ppt = 500
IV = TRUE
drop_states = FALSE
relaxed_up = FALSE
#build watersheds for contamination sources with ppt above theshold
source("PFAS-Code/Pub/Revision1/cont_watershed.R")
#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")
#run analysis
source("PFAS-Code/Pub/Revision1/main_analy_head.R")
#load in bootstrap dataframe
load(modify_path("Data_Verify/Revision 1/RData/bootstrap_iv_ppt500.RData"))
source("PFAS-Code/Pub/Tables/bs_functions.R")
#subset data to those without missing obs
df = df[which(!is.na(df$dist) & df$dist <= meters), ]
df = df %>% 
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
                           output = modify_path2("Tables/preterm_iv_500cutoff.tex"))
iv_pre500 = table2_preterm[["All"]]$coefficients["pred_pfas"]
iv_mpre500 = table2_preterm[["Moderately"]]$coefficients["pred_pfas"]
iv_vpre500 = table2_preterm[["Very"]]$coefficients["pred_pfas"]
iv_epre500 = table2_preterm[["Extremely"]]$coefficients["pred_pfas"]
save(iv_pre500, iv_mpre500, iv_vpre500, iv_epre500, file = modify_path("Data_Verify/RData/iv_pre500_coef.RData"))
preterm_sd = linear_bootstrap(boot_coefs, "preterm", table2_preterm[["All"]])
lpreterm_sd = linear_bootstrap(boot_coefs, "lpreterm", table2_preterm[["Moderately"]])
mpreterm_sd = linear_bootstrap(boot_coefs, "mpreterm", table2_preterm[["Very"]])
vpreterm_sd = linear_bootstrap(boot_coefs, "vpreterm", table2_preterm[["Extremely"]])
save(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd, file = modify_path("Data_Verify/RData/preterm_sd500.RData"))
cov_pre_lm = cov_boot(boot_coefs, "lpreterm", table2_preterm[["Moderately"]], "mpreterm", table2_preterm[["Very"]])
cov_pre_lv = cov_boot(boot_coefs, "lpreterm", table2_preterm[["Moderately"]], "vpreterm", table2_preterm[["Extremely"]])
cov_pre_mv = cov_boot(boot_coefs, "mpreterm", table2_preterm[["Very"]], "vpreterm", table2_preterm[["Extremely"]])
save(cov_pre_lm, cov_pre_lv, cov_pre_mv, file = modify_path("Data_Verify/RData/cov_preterm500.RData"))  



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
                           output = modify_path2("Tables/lbw_iv_500cutoff.tex")) 
iv_lbw500 = table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"]
iv_mlbw500 = table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"]
iv_vlbw500 = table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"]
iv_elbw500 = table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"]
save(iv_lbw500, iv_mlbw500, iv_vlbw500, iv_elbw500, file = modify_path("Data_Verify/RData/iv_lbw500_coef.RData"))
lbw_sd = linear_bootstrap(boot_coefs, "lbw", table2_lbw[["Low Birthweight"]])
llbw_sd = linear_bootstrap(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]])
mlbw_sd = linear_bootstrap(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]])
vlbw_sd = linear_bootstrap(boot_coefs, "vlbw", table2_lbw[["Very Low Birthweight"]])
save(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd, file = modify_path("Data_Verify/RData/lbw_sd500.RData"))

cov_lbw_lm = cov_boot(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]], "mlbw", table2_lbw[["mLow Birthweight"]])
cov_lbw_lv = cov_boot(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]], "vlbw", table2_lbw[["Very Low Birthweight"]])
cov_lbw_mv = cov_boot(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]], "vlbw", table2_lbw[["Very Low Birthweight"]])
save(cov_lbw_lm, cov_lbw_lv, cov_lbw_mv, file = modify_path("Data_Verify/RData/cov_lbw500.RData"))  


mort_tab = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                           n_sites + wind_exposure + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)
modelsummary::modelsummary(list(mort_tab), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("pred_pfas" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/mort_iv_500cutoff.tex")) 
iv_mort500 = mort_tab$coefficients["pred_pfas"]
save(iv_mort500, file = modify_path("Data_Verify/RData/iv_mort500_coef.RData"))
mort_sd = linear_bootstrap(boot_coefs, "mort", mort_tab)
save(mort_sd, file = modify_path("Data_Verify/RData/mort_sd500.RData"))

cov_mort_pl = cov_boot(boot_coefs, "mort", mort_tab, "lpreterm", table2_preterm[["Moderately"]])
cov_mort_pm = cov_boot(boot_coefs, "mort", mort_tab, "mpreterm", table2_preterm[["Very"]])
cov_mort_pv = cov_boot(boot_coefs, "mort", mort_tab, "vpreterm", table2_preterm[["Extremely"]])

cov_mort_bl = cov_boot(boot_coefs, "mort", mort_tab, "llbw", table2_lbw[["lLow Birthweight"]])
cov_mort_bm = cov_boot(boot_coefs, "mort", mort_tab, "mlbw", table2_lbw[["mLow Birthweight"]])
cov_mort_bv = cov_boot(boot_coefs, "mort", mort_tab, "vlbw", table2_lbw[["Very Low Birthweight"]])

save(cov_mort_pl, cov_mort_pm, cov_mort_pv, cov_mort_bl, cov_mort_bm, cov_mort_bv, file = modify_path("Data_Verify/RData/cov_mort500.RData"))


#first stage with 500
w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                        updown + wind_exposure + domestic + temp + pm25 + med_inc +
                        p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont) 

modelsummary::modelsummary(list(w_reg),
                                       stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), 
                                       fmt = modelsummary::fmt_significant(2, scientific = F), 
                                       gof_map = c("nobs", "r.squared"), 
                                       output = modify_path2("Tables/table_s12_500.tex"))

w_reg_nat = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                            updown, data = fs_cont) 

w_reg_nos = fixest::feols(asinh(wellpfas) ~ down + asinh(pfas) + log(dist)*down + 
                            updown, data = fs_cont) 

save(w_reg, w_reg_nat, w_reg_nos, file = modify_path("Data_Verify/RData/w_reg500.RData"))


#save iv figure with 500ppt as the threshold
source("PFAS-Code/Pub/Revision1/Figures/iv_figure.R")