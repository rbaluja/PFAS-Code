source("PFAS-Code/Pub/config.R")
#set up environment
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
tables = TRUE
figures = TRUE

n_births = data.frame(
  n_down1 = rep(NA, length(seq(from = 0, to = 2000, by = 100))), 
  n_up1 = rep(NA, length(seq(from = 0, to = 2000, by = 100))),
  n_sample = rep(NA, length(seq(from = 0, to = 2000, by = 100))),
  n_down_g1 = rep(NA, length(seq(from = 0, to = 2000, by = 100))),
  n_up_g1 = rep(NA, length(seq(from = 0, to = 2000, by = 100)))
)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

reg_data = data.frame(matrix(ncol = 46, nrow = 0))
colnames(reg_data) = c('threshold', 
                       'pre_down', 'pre_dse', 'pre_updown', 'pre_udse', 'pre_down_p',
                       'lpre_down', 'lpre_dse', 'lpre_updown', 'lpre_udse', 'lpre_down_p',
                       'mpre_down', 'mpre_dse', 'mpre_updown', 'mpre_udse', 'mpre_down_p',
                       'vpre_down', 'vpre_dse', 'vpre_updown', 'vpre_udse', 'vpre_down_p',
                       'lbw_down','lbw_dse', "lbw_updown", "lbw_udse", 'lbw_down_p',
                       'llbw_down','llbw_dse', "llbw_updown", "llbw_udse",'llbw_down_p',
                       'vlbw_down','vlbw_dse', "vlbw_updown", "vlbw_udse",'vlbw_down_p',
                       'elbw_down','elbw_dse', "elbw_updown", "elbw_udse", 'elbw_down_p', 
                       'mort_down','mort_dse', "mort_updown", "mort_udse", 'mort_down_p')

index = 1
for (thresh in seq(from = 100, to = 1000, by = 100)){
  ppt = thresh
  #build watersheds for contamination sources with ppt above theshold
  source("PFAS-Code/Pub/Revision1/cont_watershed.R")
  #data cleaning
  source("PFAS-Code/Pub/Data/data_head.R")
  
  #run analysis
  source("PFAS-Code/Pub/Revision1/main_analy_head.R")
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
  
  n_births[index, "threshold"] = thresh
  n_births[index, "n_down1"] = length(which(df$down == 1 & df$nsites_down == 1))
  n_births[index, "n_up1"] = length(which(df$up == 1 & df$nsites_up == 1))
  n_births[index, "n_sample"] = nrow(df)
  n_births[index, "n_down_g1"] = length(which(df$down == 1 & df$nsites_down > 1))
  n_births[index, "n_up_g1"] = length(which(df$up == 1 & df$nsites_up > 1))
  
  #save regression coefs and standard errors
  preterm_any = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                m_height + tri5 + fa_resid + wind_exposure
                              |county + year^month + birth_race_dsc_1, data = df, 
                              warn = F, notes = F, cluster = c("site", "year^month"))
  
  lpreterm = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure
                           |county + year^month + birth_race_dsc_1, data = df, 
                           warn = F, notes = F, cluster = c("site", "year^month"))
  
  mpreterm = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure
                           |county + year^month + birth_race_dsc_1, data = df, 
                           warn = F, notes = F, cluster = c("site", "year^month"))
  
  vpreterm = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure
                           |county + year^month + birth_race_dsc_1, data = df, 
                           warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  
  lbw_any = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid + wind_exposure
                          |county + year^month + birth_race_dsc_1, data = df, 
                          warn = F, notes = F, cluster = c("site", "year^month"))
  
  lbw = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, 
                      warn = F, notes = F, cluster = c("site", "year^month"))
  
  vlbw = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid + wind_exposure
                       |county + year^month + birth_race_dsc_1, data = df, 
                       warn = F, notes = F, cluster = c("site", "year^month"))
  
  elbw = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid + wind_exposure
                       |county + year^month + birth_race_dsc_1, data = df, 
                       warn = F, notes = F, cluster = c("site", "year^month"))
  
  mort = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid + wind_exposure
                       |county + year^month + birth_race_dsc_1, data = df, 
                       warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  
  pre_v = vcov(preterm_any, cluster = c("site", "year^month"))
  lpre_v = vcov(lpreterm, cluster = c("site", "year^month"))
  mpre_v = vcov(mpreterm, cluster = c("site", "year^month"))
  vpre_v = vcov(vpreterm, cluster = c("site", "year^month"))
  
  lbw_v = vcov(lbw_any, cluster = c("site", "year^month"))
  llbw_v = vcov(lbw, cluster = c("site", "year^month"))
  vlbw_v = vcov(vlbw, cluster = c("site", "year^month"))
  elbw_v = vcov(elbw, cluster = c("site", "year^month"))
  
  mort_v = vcov(mort, cluster = c("site", "year^month"))
  
  
  reg_data[index, "threshold"] = thresh
  reg_data[index, "pre_down"] = preterm_any$coeftable["down", 1]
  reg_data[index, "pre_dse"] = sqrt(pre_v["down", "down"])
  reg_data[index, "pre_updown"] = preterm_any$coeftable["updown", 1]
  reg_data[index, "pre_udse"] =  sqrt(pre_v["updown", "updown"])
  reg_data[index, "pre_down_p"] = one_sp(preterm_any$coeftable["down", "t value"], 
                                         preterm_any$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "lpre_down"] = lpreterm$coeftable["down", 1]
  reg_data[index, "lpre_dse"] = sqrt(lpre_v["down", "down"])
  reg_data[index, "lpre_updown"] = lpreterm$coeftable["updown", 1]
  reg_data[index, "lpre_udse"] =  sqrt(lpre_v["updown", "updown"])
  reg_data[index, "lpre_down_p"] = one_sp(lpreterm$coeftable["down", "t value"], 
                                          lpreterm$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "mpre_down"] = mpreterm$coeftable["down", 1]
  reg_data[index, "mpre_dse"] = sqrt(mpre_v["down", "down"])
  reg_data[index, "mpre_updown"] = mpreterm$coeftable["updown", 1]
  reg_data[index, "mpre_udse"] = sqrt(mpre_v["updown", "updown"])
  reg_data[index, "mpre_down_p"] = one_sp(mpreterm$coeftable["down", "t value"], 
                                          mpreterm$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "vpre_down"] = vpreterm$coeftable["down", 1]
  reg_data[index, "vpre_dse"] = sqrt(vpre_v["down", "down"])
  reg_data[index, "vpre_updown"] = vpreterm$coeftable["updown", 1]
  reg_data[index, "vpre_udse"] = sqrt(vpre_v["updown", "updown"])
  reg_data[index, "vpre_down_p"] = one_sp(vpreterm$coeftable["down", "t value"], 
                                          vpreterm$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "lbw_down"] = lbw_any$coeftable["down", 1]
  reg_data[index, "lbw_dse"] = sqrt(lbw_v["down", "down"])
  reg_data[index, "lbw_updown"] = lbw$coeftable["updown", 1]
  reg_data[index, "lbw_udse"] = sqrt(lbw_v["updown", "updown"])
  reg_data[index, "lbw_down_p"] = one_sp(lbw_any$coeftable["down", "t value"], 
                                         lbw_any$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "llbw_down"] = lbw$coeftable["down", 1]
  reg_data[index, "llbw_dse"] = sqrt(llbw_v["down", "down"])
  reg_data[index, "llbw_updown"] = lbw$coeftable["updown", 1]
  reg_data[index, "llbw_udse"] = sqrt(llbw_v["updown", "updown"])
  reg_data[index, "llbw_down_p"] = one_sp(lbw$coeftable["down", "t value"], 
                                          lbw$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "vlbw_down"] = vlbw$coeftable["down", 1]
  reg_data[index, "vlbw_dse"] = sqrt(vlbw_v["down", "down"])
  reg_data[index, "vlbw_updown"] = vlbw$coeftable["updown", 1]
  reg_data[index, "vlbw_udse"] = sqrt(vlbw_v["updown", "updown"])
  reg_data[index, "vlbw_down_p"] = one_sp(vlbw$coeftable["down", "t value"], 
                                          vlbw$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "elbw_down"] = elbw$coeftable["down", 1]
  reg_data[index, "elbw_dse"] = sqrt(elbw_v["down", "down"])
  reg_data[index, "elbw_updown"] = elbw$coeftable["updown", 1]
  reg_data[index, "elbw_udse"] = sqrt(elbw_v["updown", "updown"])
  reg_data[index, "elbw_down_p"] = one_sp(elbw$coeftable["down", "t value"], 
                                          elbw$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "mort_down"] = mort$coeftable["down", 1]
  reg_data[index, "mort_dse"] = sqrt(mort_v["down", "down"])
  reg_data[index, "mort_updown"] = mort$coeftable["updown", 1]
  reg_data[index, "mort_udse"] = sqrt(mort_v["updown", "updown"])
  reg_data[index, "mort_down_p"] = one_sp(mort$coeftable["down", "t value"], 
                                          mort$coeftable["down", "Pr(>|t|)"])
  
  index = index + 1
}
fwrite(n_births, "Data Revisions/n_births_thresh.csv")
save(reg_data, file = "Data Revisions/RData/reg_data_thresh.RData")


#save these to a latex table
#transpose n_births, so that the last column is now the first row
n_births1 = n_births %>% 
  dplyr::filter(threshold %in% seq(from = 100, to = 1000, by = 100))

n_births1 = t(n_births1)
#shift all rows down 1, put the last row in the first row
n_births1 = rbind(n_births1[nrow(n_births1), ], n_births1[-nrow(n_births1), ])
rownames(n_births1) = c("Threshold", "N Down of 1", "N Up of 1", "Est Sample", "N Down $>$ 1", "N Up $>$ 1")

n_births1_df = as.data.frame(n_births1)

# Convert the data frame to an xtable object
n_births1_xtable = xtable::xtable(n_births1_df)

# Save the LaTeX table to a file
fileConn = file(modify_path2("Tables/Revisions/n_births_thresh_s.tex"))
print(n_births1_xtable, file = fileConn)
close(fileConn)

#create threshold figure
source("PFAS-Code/Pub/Revision1/change_thresh_fig.R")
