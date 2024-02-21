#clear memory
rm(list = ls())
#restart R
.rs.restartR()

#set working directory
if (file.exists('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH')){
  setwd('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH') 
}else{
  setwd('/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH')
}

#load in helper functions
source("Code/Primary/env_functions.R")
source("Code/Primary/Watersheds/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets)
options(modelsummary_format_numeric_latex = "mathmode")

#set up environment
meters = 5000
wind_dist= dist_allow = 10000
ppt = 1000
run_cleaning = FALSE
match_wells = FALSE
old_wells = FALSE
domestic = FALSE
system = FALSE
drop_dups = TRUE #needs to be false if calculating se on difference in theta
drop_far_down = TRUE
drop_far_up = FALSE
well_fd = test_fd = FALSE #flow line distance?
IV = FALSE
fa_resid = TRUE
soil_well = TRUE
drop_states = FALSE
relaxed_up = FALSE

#obtain theta info for Northeastern contamination data
source("Code/PR/Data/pfas_lab_sites.R")

#well location and service area data (NHDES)
source("Code/PR/Data/NHDES_PWS.R")

#set up wind
source("Code/PR/Main Analysis/wind.R")

#load natality data
load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_matched122023.RData") 


#main analysis
source("Code/PR/Main Analysis/main_analy_head.R")


#bootstrap IV
bts = 10000
n_boot_cont = 10000
source("Code/PR/Main Analysis//bootstrap_setup.R")

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

boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_err, df, fs_cont, cl = 2))
#save(boot_coefs, file = "New Hampshire/Data/bootstrap29.RData")
load("New Hampshire/Data/bootstrap29.RData")


preterm_sd = sqrt(sum((boot_coefs$preterm - 0.011)^2)/9999)
0.011/preterm_sd # significant at 5%
preterm_sd

lpreterm_sd = sqrt(sum((boot_coefs$lpreterm - 0.0062)^2)/9999)
0.0062/lpreterm_sd # significant at 5%
lpreterm_sd

mpreterm_sd = sqrt(sum((boot_coefs$mpreterm + 0.00142)^2)/9999)
0.00142/mpreterm_sd # significant at 5%
mpreterm_sd

vpreterm_sd = sqrt(sum((boot_coefs$vpreterm - 0.0058)^2)/9999)
0.0058/vpreterm_sd # significant at 5%
vpreterm_sd

lbw_sd = sqrt(sum((boot_coefs$lbw - 0.0109)^2)/9999)
0.0109/lbw_sd # significant at 5%
lbw_sd

llbw_sd = sqrt(sum((boot_coefs$llbw - 0.0042)^2)/9999)
0.0042/llbw_sd # significant at 5%
llbw_sd

mlbw_sd = sqrt(sum((boot_coefs$mlbw - 0.00191)^2)/9999)
0.00191/mlbw_sd # significant at 5%
mlbw_sd

vlbw_sd = sqrt(sum((boot_coefs$vlbw - 0.0048)^2)/9999)
0.0048/vlbw_sd # significant at 5%
vlbw_sd



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
boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_err_quant, df, fs_cont, cl = 2))
#save(boot_coefs, file = "New Hampshire/Data/bootstrap_quant28.RData")
load("New Hampshire/Data/bootstrap_quant28.RData")


p2_sd = sqrt(sum((boot_coefs$preterm2 - reg_data[2, "pre_coef"])^2)/9999)
p3_sd = sqrt(sum((boot_coefs$preterm3 - reg_data[3, "pre_coef"])^2)/9999)
p4_sd = sqrt(sum((boot_coefs$preterm4 - reg_data[4, "pre_coef"])^2)/9999)
p5_sd = sqrt(sum((boot_coefs$preterm5 - reg_data[5, "pre_coef"])^2)/9999)

lp2_sd = sqrt(sum((boot_coefs$lpreterm2 - reg_data[2, "lpre_coef"])^2)/9999)
lp3_sd = sqrt(sum((boot_coefs$lpreterm3 - reg_data[3, "lpre_coef"])^2)/9999)
lp4_sd = sqrt(sum((boot_coefs$lpreterm4 - reg_data[4, "lpre_coef"])^2)/9999)
lp5_sd = sqrt(sum((boot_coefs$lpreterm5 - reg_data[5, "lpre_coef"])^2)/9999)

mp2_sd = sqrt(sum((boot_coefs$mpreterm2 - reg_data[2, "mpre_coef"])^2)/9999)
mp3_sd = sqrt(sum((boot_coefs$mpreterm3 - reg_data[3, "mpre_coef"])^2)/9999)
mp4_sd = sqrt(sum((boot_coefs$mpreterm4 - reg_data[4, "mpre_coef"])^2)/9999)
mp5_sd = sqrt(sum((boot_coefs$mpreterm5 - reg_data[5, "mpre_coef"])^2)/9999)

vp2_sd = sqrt(sum((boot_coefs$vpreterm2 - reg_data[2, "vpre_coef"])^2)/9999)
vp3_sd = sqrt(sum((boot_coefs$vpreterm3 - reg_data[3, "vpre_coef"])^2)/9999)
vp4_sd = sqrt(sum((boot_coefs$vpreterm4 - reg_data[4, "vpre_coef"])^2)/9999)
vp5_sd = sqrt(sum((boot_coefs$vpreterm5 - reg_data[5, "vpre_coef"])^2)/9999)

lbw2_sd = sqrt(sum((boot_coefs$lbw2 - reg_data[2, "lbw_coef"])^2)/9999)
lbw3_sd = sqrt(sum((boot_coefs$lbw3 - reg_data[3, "lbw_coef"])^2)/9999)
lbw4_sd = sqrt(sum((boot_coefs$lbw4 - reg_data[4, "lbw_coef"])^2)/9999)
lbw5_sd = sqrt(sum((boot_coefs$lbw5 - reg_data[5, "lbw_coef"])^2)/9999)

llbw2_sd = sqrt(sum((boot_coefs$llbw2 - reg_data[2, "llbw_coef"])^2)/9999)
llbw3_sd = sqrt(sum((boot_coefs$llbw3 - reg_data[3, "llbw_coef"])^2)/9999)
llbw4_sd = sqrt(sum((boot_coefs$llbw4 - reg_data[4, "llbw_coef"])^2)/9999)
llbw5_sd = sqrt(sum((boot_coefs$llbw5 - reg_data[5, "llbw_coef"])^2)/9999)

mlbw2_sd = sqrt(sum((boot_coefs$mlbw2 - reg_data[2, "mlbw_coef"])^2)/9999)
mlbw3_sd = sqrt(sum((boot_coefs$mlbw3 - reg_data[3, "mlbw_coef"])^2)/9999)
mlbw4_sd = sqrt(sum((boot_coefs$mlbw4 - reg_data[4, "mlbw_coef"])^2)/9999)
mlbw5_sd = sqrt(sum((boot_coefs$mlbw5 - reg_data[5, "mlbw_coef"])^2)/9999)

vlbw2_sd = sqrt(sum((boot_coefs$vlbw2 - reg_data[2, "vlbw_coef"])^2)/9999)
vlbw3_sd = sqrt(sum((boot_coefs$vlbw3 - reg_data[3, "vlbw_coef"])^2)/9999)
vlbw4_sd = sqrt(sum((boot_coefs$vlbw4 - reg_data[4, "vlbw_coef"])^2)/9999)
vlbw5_sd = sqrt(sum((boot_coefs$vlbw5 - reg_data[5, "vlbw_coef"])^2)/9999)