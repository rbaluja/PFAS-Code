#set working directory
setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, tidycensus)
options(modelsummary_format_numeric_latex = "mathmode")

natality_path = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/" #set path to natality data in Box Health
meters = 5000 #buffer for base spec
wind_dist= dist_allow = 10000 #wind distance cutoff
ppt = 1000 #cutoff for primary contamination site
run_cleaning = FALSE #clean natality data?
match_wells = FALSE #Re match natality data to wells?
domestic = FALSE #include individuals outside of PWS boundaries?
drop_far_down = TRUE
drop_far_up = FALSE
IV = TRUE #Run IV spec?
rerun_fs_clean = FALSE #clean first stage data?
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
GIS_create = FALSE #recreate watershed shapes?
create_figures = FALSE #output figures?
nat_run_cont_ws = FALSE#recreate national watershed shapes?
nat_reassn = FALSE #reassign national CBGs to release sites?
nat_redo_soil = FALSE #recalculate soil stats for national data?
oster_robust = FALSE #run Oster (2019) selection on unobservables?
false_test = FALSE #run falsification test?
census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82"
code_check = TRUE
n_cores = 1

source("PFAS-Code/PR/Data/data_head.R")

source("PFAS-Code/PR/Main Analysis/binary.R")

source("PFAS-Code/PR/Main Analysis/flow_accumulation.R")

#bootstrap IV
bts = 10000
n_boot_cont = 9310
source("PFAS-Code/PR/Bootstrap/bootstrap_setup.R")

boot_err = function(i, df, fs_cont){
  boot_coefs = data.frame(matrix(ncol = 10, nrow = 1))
  colnames(boot_coefs) = c("preterm", "lpreterm", "mpreterm", "vpreterm", 
                           "lbw", "llbw", "mlbw", "vlbw",
                           "gestation", "bweight")
  
  
  fs_cont_bs = fs_cont[sample(nrow(fs_cont), size = n_boot_cont, replace = TRUE), ]
  
  w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                          updown + wind_exposure + domestic + temp + pm25 + med_inc +
                          p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont_bs) 
  
  
  df$domestic = 0
  df$elevation = df$well_elev
  df$t = as.numeric(df$year) - 2010
  df$pred_pfas = predict(w_reg, df)
  
  
  
  
  #regressions
  preterm = fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  lpreterm = fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                                       n_sites + wind_exposure + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  mpreterm = fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                             n_sites + wind_exposure + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  vpreterm = fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                             n_sites + wind_exposure + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  lbw = fixest::feols(I(bweight < 2500) ~ pred_pfas + asinh(pfas) + 
                        n_sites + wind_exposure + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  llbw = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) + 
                        n_sites + wind_exposure + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  mlbw = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) + 
                        n_sites + wind_exposure + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  vlbw = fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) + 
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  
  
  #continuous outcomes
  gestation = fixest::feols(gestation ~ pred_pfas + asinh(pfas) + 
                              n_sites + wind_exposure + 
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  bw = fixest::feols(bweight ~ pred_pfas + asinh(pfas) + 
                       n_sites + wind_exposure + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  
  
  boot_coefs[1, 'preterm'] = preterm$coefficients["pred_pfas"]
  boot_coefs[1, 'lpreterm'] = lpreterm$coefficients["pred_pfas"]
  boot_coefs[1, 'mpreterm'] = mpreterm$coefficients["pred_pfas"]
  boot_coefs[1, 'vpreterm'] = vpreterm$coefficients["pred_pfas"]
  boot_coefs[1, 'lbw'] = lbw$coefficients["pred_pfas"]
  boot_coefs[1, 'llbw'] = llbw$coefficients["pred_pfas"]
  boot_coefs[1, 'mlbw'] = mlbw$coefficients["pred_pfas"]
  boot_coefs[1, 'vlbw'] = vlbw$coefficients["pred_pfas"]
  boot_coefs[1, 'gestation'] = gestation$coefficients["pred_pfas"]
  boot_coefs[1, 'bw'] = bw$coefficients["pred_pfas"]
  
  return(boot_coefs)
}
boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_err, df, fs_cont, cl = 1))
save(boot_coefs, file = modify_path("Data_Verify/RData/bootstrap.RData")) 


#quantiles bootstrap
boot_err_quant = function(i, df, fs_cont){
  
  boot_coefs = data.frame(matrix(ncol = 32, nrow = 1))
  colnames(boot_coefs) = c("preterm5", "preterm2", "preterm3", "preterm4",
                           "lpreterm5", "lpreterm2", "lpreterm3", "lpreterm4",
                           "mpreterm5", "mpreterm2", "mpreterm3", "mpreterm4",
                           "vpreterm5", "vpreterm2", "vpreterm3", "vpreterm4", 
                           "lbw5", "lbw2", "lbw3", "lbw4", 
                           "llbw5", "llbw2", "llbw3", "llbw4", 
                           "mlbw5", "mlbw2", "mlbw3", "mlbw4", 
                           "vlbw5", "vlbw2", "vlbw3", "vlbw4")
  
  
  fs_cont_bs = fs_cont[sample(nrow(fs_cont), size = n_boot_cont, replace = TRUE), ]
  
  w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                          updown + wind_exposure + domestic + temp + pm25 + med_inc +
                          p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont_bs) 
  
  
  df$domestic = 0
  df$elevation = df$well_elev
  df$t = as.numeric(df$year) - 2010
  df$pred_pfas = predict(w_reg, df)
  
  
  
  
  #regressions
  df_nn = df[which(!is.na(df$pred_pfas)), ]
  quantiles = quantile(df_nn$pred_pfas, c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  # Create a new variable categorizing each observation based on these quantiles
  # Include the lowest and highest values to ensure all data is categorized
  df_nn$quant_pfas = as.integer(cut(df_nn$pred_pfas, breaks = quantiles, include.lowest = TRUE, labels = 1:5))
  
  preterm = fixest::feols(preterm ~ as.factor(quant_pfas) + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county" )
  
  lpreterm = fixest::feols(I(gestation < 37 & gestation >= 32) ~ as.factor(quant_pfas) + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county" )
  
  mpreterm = fixest::feols(I(gestation < 32 & gestation >= 28) ~ as.factor(quant_pfas) + asinh(pfas) + 
                             n_sites + wind_exposure + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county" )
  
  vpreterm = fixest::feols(I(gestation < 28) ~ as.factor(quant_pfas) + asinh(pfas) + 
                             n_sites + wind_exposure + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county")
  
  lbw = fixest::feols(I(bweight < 2500) ~  as.factor(quant_pfas) + asinh(pfas) + 
                        n_sites + wind_exposure + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county")
  
  llbw = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  as.factor(quant_pfas) + asinh(pfas) + 
                        n_sites + wind_exposure + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county")
  
  
  mlbw = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  as.factor(quant_pfas) + asinh(pfas) + 
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county")
  
  vlbw = fixest::feols(I(bweight < 1000) ~  as.factor(quant_pfas) + asinh(pfas) + 
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df_nn, cluster = "county")
  
  
  
  
  boot_coefs[1, 'preterm2'] = preterm$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'preterm3'] = preterm$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'preterm4'] = preterm$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'preterm5'] = preterm$coefficients["as.factor(quant_pfas)5"]
  
  boot_coefs[1, 'lpreterm2'] = lpreterm$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'lpreterm3'] = lpreterm$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'lpreterm4'] = lpreterm$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'lpreterm5'] = lpreterm$coefficients["as.factor(quant_pfas)5"]
  
  boot_coefs[1, 'mpreterm2'] = mpreterm$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'mpreterm3'] = mpreterm$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'mpreterm4'] = mpreterm$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'mpreterm5'] = mpreterm$coefficients["as.factor(quant_pfas)5"]
  
  boot_coefs[1, 'vpreterm2'] = vpreterm$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'vpreterm3'] = vpreterm$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'vpreterm4'] = vpreterm$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'vpreterm5'] = vpreterm$coefficients["as.factor(quant_pfas)5"]
  
  boot_coefs[1, 'lbw2'] = lbw$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'lbw3'] = lbw$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'lbw4'] = lbw$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'lbw5'] = lbw$coefficients["as.factor(quant_pfas)5"]
  
  boot_coefs[1, 'llbw2'] = llbw$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'llbw3'] = llbw$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'llbw4'] = llbw$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'llbw5'] = llbw$coefficients["as.factor(quant_pfas)5"]
  
  boot_coefs[1, 'mlbw2'] = mlbw$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'mlbw3'] = mlbw$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'mlbw4'] = mlbw$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'mlbw5'] = mlbw$coefficients["as.factor(quant_pfas)5"]
  
  boot_coefs[1, 'vlbw2'] = vlbw$coefficients["as.factor(quant_pfas)2"]
  boot_coefs[1, 'vlbw3'] = vlbw$coefficients["as.factor(quant_pfas)3"]
  boot_coefs[1, 'vlbw4'] = vlbw$coefficients["as.factor(quant_pfas)4"]
  boot_coefs[1, 'vlbw5'] = vlbw$coefficients["as.factor(quant_pfas)5"]

  return(boot_coefs)
}
boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_err_quant, df, fs_cont, cl = 1))
save(boot_coefs, file = modify_path("Data_Verify/RData/bootstrap_quant.RData")) 




boot_err_sb = function(i, df, fs_cont){
  boot_coefs = data.frame(matrix(ncol = 1, nrow = 1))
  colnames(boot_coefs) = c("stillborn")
  
  
  fs_cont_bs = fs_cont[sample(nrow(fs_cont), size = n_boot_cont, replace = TRUE), ]
  
  w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                          updown + wind_exposure + domestic + temp + pm25 + med_inc +
                          p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont_bs) 
  
  
  df$domestic = 0
  df$elevation = df$well_elev
  df$t = as.numeric(df$year) - 2010
  df$pred_pfas = predict(w_reg, df)
  
  #regressions
  stillbrn = fixest::feols(stillbrn ~ pred_pfas + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df[which(df$chld_dead_live != 9), ])
  
  
  
  boot_coefs[1, "stillborn"] = stillbrn$coefficients["pred_pfas"]
  
  return(boot_coefs)
}
boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_err_sb, df, fs_cont, cl = 1))
save(boot_coefs, file = modify_path("Data_Verify/RData/bootstrap_sb.RData")) 


#clear memory
rm(list = ls())