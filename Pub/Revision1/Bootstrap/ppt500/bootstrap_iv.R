source("PFAS-Code/Pub/config.R")
ppt = 500
drop_states = FALSE
relaxed_up = FALSE

source("PFAS-Code/Pub/Data/data_head.R")

source("PFAS-Code/Pub/Main Analysis/binary.R")

source("PFAS-Code/Pub/Main Analysis/flow_accumulation.R")

#bootstrap IV
bts = 10000
n_boot_cont = 9310
source("PFAS-Code/Pub/Revision1/Bootstrap/ppt500/bootstrap_setup.R")

boot_err = function(i, df, fs_cont){
  boot_coefs = data.frame(matrix(ncol = 11, nrow = 1))
  colnames(boot_coefs) = c("preterm", "lpreterm", "mpreterm", "vpreterm", 
                           "lbw", "llbw", "mlbw", "vlbw",
                           "gestation", "bweight", "mort")
  
  
  fs_cont_bs = fs_cont[sample(nrow(fs_cont), size = n_boot_cont, replace = TRUE), ]
  
  w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
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
  
  mort = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)
  
  
  
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
  boot_coefs[1, 'mort'] = mort$coefficients["pred_pfas"]
  boot_coefs[1, 'gestation'] = gestation$coefficients["pred_pfas"]
  boot_coefs[1, 'bw'] = bw$coefficients["pred_pfas"]
  
  return(boot_coefs)
}
boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_err, df, fs_cont, cl = n_cores))
save(boot_coefs, file = modify_path("Data_Verify/Revision 1/RData/bootstrap_iv_ppt500.RData")) 


boot_ivbin_comp = function(i, df, fs_cont){
  boot_coefs = data.frame(matrix(ncol = 9, nrow = 1))
  colnames(boot_coefs) = c("pre", "mpre", "vpre", "epre", "lbw", "mlbw", "vlbw", "elbw", "mort")
  
  
  fs_cont_bs = fs_cont[sample(nrow(fs_cont), size = n_boot_cont, replace = TRUE), ]
  
  w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                          updown + wind_exposure + domestic + temp + pm25 + med_inc +
                          p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont_bs) 
  
  
  df$domestic = 0
  df$elevation = df$well_elev
  df$t = as.numeric(df$year) - 2010
  df$pred_pfas = predict(w_reg, df)
  
  
  
  
  pre_iv = fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                           n_sites + wind_exposure + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  
  lpre_iv = fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  vpre_iv = fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  
  epre_iv = fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  
  
  
  lbw_iv = fixest::feols(I(bweight < 2500 ) ~ pred_pfas + asinh(pfas) +
                           n_sites + wind_exposure + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  mlbw_iv= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                           n_sites + wind_exposure + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  vlbw_iv = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) +
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  
  elbw_iv = fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) +
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  
  mort = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)
  
  
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
  
  df2$pred_pre = predict(pre_iv, newdata = df2)
  df2$pred_lpre = predict(lpre_iv, newdata = df2)
  df2$pred_vpre = predict(vpre_iv, newdata = df2)
  df2$pred_epre = predict(epre_iv, newdata = df2)
  df2$pred_lbw = predict(lbw_iv, newdata = df2)
  df2$pred_mlbw = predict(mlbw_iv, newdata = df2)
  df2$pred_vlbw = predict(vlbw_iv, newdata = df2)
  df2$pred_elbw = predict(elbw_iv, newdata = df2)
  df2$pred_mort = predict(mort, newdata = df2)
  
  #get change in predicted probability from median up to median down
  boot_coefs[1, "pre"] = (mean(df2[which(df2$down == 1), ]$pred_pre) - mean(df2[which(df2$up == 1), ]$pred_pre))/mean(df$gestation < 37) * 100
  boot_coefs[1, "mpre"] = (mean(df2[which(df2$down == 1), ]$pred_lpre) - mean(df2[which(df2$up == 1), ]$pred_lpre))/mean(df$gestation < 37 & df$gestation >= 32) * 100
  boot_coefs[1, "vpre"] = (mean(df2[which(df2$down == 1), ]$pred_vpre) - mean(df2[which(df2$up == 1), ]$pred_vpre))/mean(df$gestation < 32 & df$gestation >= 28) * 100
  boot_coefs[1, "epre"] = (mean(df2[which(df2$down == 1), ]$pred_epre) - mean(df2[which(df2$up == 1), ]$pred_epre))/mean(df$gestation < 28) * 100
  
  boot_coefs[1, "lbw"] = (mean(df2[which(df2$down == 1), ]$pred_lbw) - mean(df2[which(df2$up == 1), ]$pred_lbw))/mean(df$bweight < 2500) * 100
  boot_coefs[1, "mlbw"] = (mean(df2[which(df2$down == 1), ]$pred_mlbw) - mean(df2[which(df2$up == 1), ]$pred_mlbw))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
  boot_coefs[1, "vlbw"] = (mean(df2[which(df2$down == 1), ]$pred_vlbw) - mean(df2[which(df2$up == 1), ]$pred_vlbw))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
  boot_coefs[1, "elbw"] = (mean(df2[which(df2$down == 1), ]$pred_elbw) - mean(df2[which(df2$up == 1), ]$pred_elbw))/mean(df$bweight < 1000) * 100
  
  boot_coefs[1, "mort"] = (mean(df2[which(df2$down == 1), ]$pred_mort) - mean(df2[which(df2$up == 1), ]$pred_mort))/mean(df$death) * 100
  
  
  return(boot_coefs)
}
boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_ivbin_comp, df, fs_cont, cl = n_cores))
save(boot_coefs, file = modify_path(paste0("Data_Verify/Revision 1/RData/bootstrap_iv_bin_comp", ppt, ".RData"))) 
