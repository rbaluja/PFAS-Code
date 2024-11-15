source("PFAS-Code/Pub/config.R")
#set up environment
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
tables = TRUE
figures = TRUE


one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

ppt = 100
#build watersheds for contamination sources with ppt above theshold
source("PFAS-Code/Pub/Revision1/cont_watershed.R")
#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")

#run analysis
#binary setup
source("PFAS-Code/Pub/Revision1/binary_max.R")
#flow accumulation at well and residence 
source("PFAS-Code/Pub/Main Analysis/flow_accumulation.R")

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

#save regression coefs and standard errors
table1_preterm = list()
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000))  + dist  + n_sites + 
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid + wind_exposure
                            |county + year^month + birth_race_dsc_1, data = df, 
                            warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid + wind_exposure
                         |county + year^month + birth_race_dsc_1, data = df, 
                         warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid + wind_exposure
                         |county + year^month + birth_race_dsc_1, data = df, 
                         warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid + wind_exposure
                         |county + year^month + birth_race_dsc_1, data = df, 
                         warn = F, notes = F, cluster = c("site", "year^month"))

modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "down:I(pfas >= 250 & pfas < 500)" = "Downgradient x PFAS 250-500",
                                        "down:I(pfas >= 500 & pfas < 750)" = "Downgradient x PFAS 500-750",
                                        "down:I(pfas >= 750 & pfas < 1000)" = "Downgradient x PFAS 750-1000",
                                        "down:I(pfas >= 1000)" = "Downgradient x PFAS 1000+",
                                        "updown" = "Upgradient",
                                        "updown:I(pfas >= 250 & pfas < 500)" = "Upgradient x PFAS 250-500",
                                        "updown:I(pfas >= 500 & pfas < 750)" = "Upgradient x PFAS 500-750",
                                        "updown:I(pfas >= 750 & pfas < 1000)" = "Upgradient x PFAS 750-1000",
                                        "updown:I(pfas >= 1000)" = "Upgradient x PFAS 1000+"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_100ppt_np.tex")) 


table1_lbw = list()
table1_lbw[["All"]] = fixest::feols(I(bweight < 2500) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df, 
                        warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["All full term"]] = fixest::feols(I(bweight < 2500) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height + tri5 + fa_resid + wind_exposure
                                    |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], 
                                    warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately"]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                      m_height + tri5 + fa_resid + wind_exposure
                    |county + year^month + birth_race_dsc_1, data = df, 
                    warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid + wind_exposure
                     |county + year^month + birth_race_dsc_1, data = df, 
                     warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Extremely"]] = fixest::feols(I(bweight < 1000) ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid + wind_exposure
                     |county + year^month + birth_race_dsc_1, data = df, 
                     warn = F, notes = F, cluster = c("site", "year^month"))

modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "down:I(pfas >= 250 & pfas < 500)" = "Downgradient x PFAS 250-500",
                                        "down:I(pfas >= 500 & pfas < 750)" = "Downgradient x PFAS 500-750",
                                        "down:I(pfas >= 750 & pfas < 1000)" = "Downgradient x PFAS 750-1000",
                                        "down:I(pfas >= 1000)" = "Downgradient x PFAS 1000+",
                                        "updown" = "Upgradient",
                                        "updown:I(pfas >= 250 & pfas < 500)" = "Upgradient x PFAS 250-500",
                                        "updown:I(pfas >= 500 & pfas < 750)" = "Upgradient x PFAS 500-750",
                                        "updown:I(pfas >= 750 & pfas < 1000)" = "Upgradient x PFAS 750-1000",
                                        "updown:I(pfas >= 1000)" = "Upgradient x PFAS 1000+"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_100ppt_np.tex"))  

mort = fixest::feols(death ~  (updown + down) * (I(pfas >= 250 & pfas < 500) + I(pfas >= 500 & pfas < 750) + I(pfas >= 750 & pfas < 1000) + I(pfas >= 1000)) + dist  + n_sites + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid + wind_exposure
                     |county + year^month + birth_race_dsc_1, data = df, 
                     warn = F, notes = F, cluster = c("site", "year^month"))

modelsummary::modelsummary(list(mort), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "down:I(pfas >= 250 & pfas < 500)" = "Downgradient x PFAS 250-500",
                                        "down:I(pfas >= 500 & pfas < 750)" = "Downgradient x PFAS 500-750",
                                        "down:I(pfas >= 750 & pfas < 1000)" = "Downgradient x PFAS 750-1000",
                                        "down:I(pfas >= 1000)" = "Downgradient x PFAS 1000+",
                                        "updown" = "Upgradient",
                                        "updown:I(pfas >= 250 & pfas < 500)" = "Upgradient x PFAS 250-500",
                                        "updown:I(pfas >= 500 & pfas < 750)" = "Upgradient x PFAS 500-750",
                                        "updown:I(pfas >= 750 & pfas < 1000)" = "Upgradient x PFAS 750-1000",
                                        "updown:I(pfas >= 1000)" = "Upgradient x PFAS 1000+"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_100ppt_np.tex"))  


