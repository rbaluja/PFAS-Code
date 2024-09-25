source("PFAS-Code/Pub/config.R")
drop_states = FALSE
relaxed_up = FALSE

source("PFAS-Code/Pub/Data/data_head.R")

source("PFAS-Code/Pub/Main Analysis/binary.R")

source("PFAS-Code/Pub/Main Analysis/flow_accumulation.R")

#bootstrap IV
bts = 10000
n_boot_cont = 9310
source("PFAS-Code/Pub/Revision1/Bootstrap/bootstrap_setup.R")

boot_err = function(i, df, fs_cont){
  boot_coefs = data.frame(matrix(ncol = 4, nrow = 1))
  colnames(boot_coefs) = c("gestation_all", "gestation_pre", "bweight_all", "bweight_lbw")
  
  
  fs_cont_bs = fs_cont[sample(nrow(fs_cont), size = n_boot_cont, replace = TRUE), ]
  
  w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, clay, sand, silt, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                          updown + wind_exposure + domestic + temp + pm25 + med_inc +
                          p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont_bs) 
  
  
  df$domestic = 0
  df$elevation = df$well_elev
  df$t = as.numeric(df$year) - 2010
  df$pred_pfas = predict(w_reg, df)
  
  
  
  
  #regressions
  ges = fixest::feols(gestation ~ pred_pfas + asinh(pfas) + 
                            n_sites + wind_exposure + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  ges_pre = fixest::feols(gestation ~ pred_pfas + asinh(pfas) + 
                             n_sites + wind_exposure + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df[which(df$gestation < 37), ] )
  
  bw = fixest::feols(bweight ~ pred_pfas + asinh(pfas) + 
                             n_sites + wind_exposure + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )
  
  bw_lbw = fixest::feols(bweight ~ pred_pfas + asinh(pfas) + 
                             n_sites + wind_exposure + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df[which(df$bweight < 2500), ] )
  
  
  
  boot_coefs[1, 'gestation_all'] = ges$coefficients["pred_pfas"]
  boot_coefs[1, 'gestation_pre'] = ges_pre$coefficients["pred_pfas"]
  boot_coefs[1, 'bweight_all'] = bw$coefficients["pred_pfas"]
  boot_coefs[1, 'bweight_lbw'] = bw_lbw$coefficients["pred_pfas"]
  
  return(boot_coefs)
}
boot_coefs = dplyr::bind_rows(pblapply(1:bts, boot_err, df, fs_cont, cl = n_cores))
save(boot_coefs, file = modify_path("Data_Verify/Revision 1/RData/bootstrap_cont.RData")) 