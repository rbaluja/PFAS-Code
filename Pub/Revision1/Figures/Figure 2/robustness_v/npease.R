source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/figure2_rob_fns.R")
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/figure2_rob_mort_fns.R")
#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

#get distance from each well to Pease AFB (largest site)
wells$p_dist = as.numeric(st_distance(wells %>% 
                                        as_tibble() %>% 
                                        dplyr::select(sys_id, source, lng, lat) %>%
                                        st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                                        st_transform(32110), cont_sites %>% dplyr::filter(site == "Pease Air Force Base") %>%  st_transform(32110)))
#merge distance with natality data by assigned well
df_est = 
  df_est %>% 
  left_join(wells %>% as_tibble() %>% dplyr::select(sys_id, source, p_dist))


#preterm
full = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_pre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 37) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 37) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 37) * 100, 
               np$se["down"]/mean(df_est$gestation < 37) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_pre$Check = factor(data_pre$Check, c("Baseline", "Drop Pease AFB Births"))


data_pre$down = data_pre$Estimate
data_pre$up = data_pre$Upgradient
data_pre$d_lower = data_pre$down - 1.96 * data_pre$StdError
data_pre$d_upper = data_pre$down + 1.96 * data_pre$StdError
data_pre$pval_label = sprintf("%.5f", data_pre$pval)
data_pre$health_outcome = "preterm"

pre_any = figure2_fun_npease(data_pre, "Any", FALSE, TRUE, "Any", TRUE)




#Moderately preterm
full = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_mpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               np$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_mpre$Check = factor(data_mpre$Check, c("Baseline", "Drop Pease AFB Births"))


data_mpre$down = data_mpre$Estimate
data_mpre$up = data_mpre$Upgradient
data_mpre$d_lower = data_mpre$down - 1.96 * data_mpre$StdError
data_mpre$d_upper = data_mpre$down + 1.96 * data_mpre$StdError
data_mpre$pval_label = sprintf("%.5f", data_mpre$pval)
data_mpre$health_outcome = "preterm"

mpre = figure2_fun_npease(data_mpre, "Moderately", FALSE, FALSE, "Moderately", TRUE)



#Very preterm
full = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_vpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               np$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_vpre$Check = factor(data_vpre$Check, c("Baseline", "Drop Pease AFB Births"))


data_vpre$down = data_vpre$Estimate
data_vpre$up = data_vpre$Upgradient
data_vpre$d_lower = data_vpre$down - 1.96 * data_vpre$StdError
data_vpre$d_upper = data_vpre$down + 1.96 * data_vpre$StdError
data_vpre$pval_label = sprintf("%.5f", data_vpre$pval)
data_vpre$health_outcome = "preterm"

vpre = figure2_fun_npease(data_vpre, "Very", FALSE, FALSE, "Very", TRUE)



#Extremeley preterm
full = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_epre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 28) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 28) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 28) * 100, 
               np$se["down"]/mean(df_est$gestation < 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_epre$Check = factor(data_epre$Check, c("Baseline", "Drop Pease AFB Births"))


data_epre$down = data_epre$Estimate
data_epre$up = data_epre$Upgradient
data_epre$d_lower = data_epre$down - 1.96 * data_epre$StdError
data_epre$d_upper = data_epre$down + 1.96 * data_epre$StdError
data_epre$pval_label = sprintf("%.5f", data_epre$pval)
data_epre$health_outcome = "preterm"

epre = figure2_fun_npease(data_epre, "Extremely", TRUE, FALSE, "Extremely", TRUE)

pre_fig = pre_any/mpre/vpre/epre




#birthweight
#low birthweight
full = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_lbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 2500) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 2500) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 2500) * 100, 
               np$se["down"]/mean(df_est$bweight < 2500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_lbw$Check = factor(data_lbw$Check, c("Baseline", "Drop Pease AFB Births"))


data_lbw$down = data_lbw$Estimate
data_lbw$up = data_lbw$Upgradient
data_lbw$d_lower = data_lbw$down - 1.96 * data_lbw$StdError
data_lbw$d_upper = data_lbw$down + 1.96 * data_lbw$StdError
data_lbw$pval_label = sprintf("%.5f", data_lbw$pval)
data_lbw$health_outcome = "lbw"

lbw_any = figure2_fun_npease(data_lbw, "Any", FALSE, TRUE, "Any", FALSE)




#Moderately lbw
full = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_mlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               np$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_mlbw$Check = factor(data_mlbw$Check, c("Baseline", "Drop Pease AFB Births"))


data_mlbw$down = data_mlbw$Estimate
data_mlbw$up = data_mlbw$Upgradient
data_mlbw$d_lower = data_mlbw$down - 1.96 * data_mlbw$StdError
data_mlbw$d_upper = data_mlbw$down + 1.96 * data_mlbw$StdError
data_mlbw$pval_label = sprintf("%.5f", data_mlbw$pval)
data_mlbw$health_outcome = "lbw"

mlbw = figure2_fun_npease(data_mlbw, "Moderately", FALSE, FALSE, "Moderately", FALSE)



#Very lbw
full = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_vlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               np$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_vlbw$Check = factor(data_vlbw$Check, c("Baseline", "Drop Pease AFB Births"))


data_vlbw$down = data_vlbw$Estimate
data_vlbw$up = data_vlbw$Upgradient
data_vlbw$d_lower = data_vlbw$down - 1.96 * data_vlbw$StdError
data_vlbw$d_upper = data_vlbw$down + 1.96 * data_vlbw$StdError
data_vlbw$pval_label = sprintf("%.5f", data_vlbw$pval)
data_vlbw$health_outcome = "lbw"

vlbw = figure2_fun_npease(data_vlbw, "Very", FALSE, FALSE, "Very", FALSE)



#Extremeley lbw
full = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 +fa_resid + wind_exposure 
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


data_elbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 1000) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 1000) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 1000) * 100, 
               np$se["down"]/mean(df_est$bweight < 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_elbw$Check = factor(data_elbw$Check, c("Baseline", "Drop Pease AFB Births"))


data_elbw$down = data_elbw$Estimate
data_elbw$up = data_elbw$Upgradient
data_elbw$d_lower = data_elbw$down - 1.96 * data_elbw$StdError
data_elbw$d_upper = data_elbw$down + 1.96 * data_elbw$StdError
data_elbw$pval_label = sprintf("%.5f", data_elbw$pval)
data_elbw$health_outcome = "lbw"

elbw = figure2_fun_npease(data_elbw, "Extremely", TRUE, FALSE, "Extremely", FALSE)


lbw = lbw_any/mlbw/vlbw/elbw

legend_data <- data.frame(
  category = factor(c("Any", "Moderately", "Very", "Extremely"), levels = c("Any", "Moderately", "Very", "Extremely")),
  color = c("dodgerblue", "coral", "darkseagreen", "orchid4")
)

# Create a dummy plot for the legend
lplot = ggplot(legend_data) +
  geom_point(aes(x = category, y = 1, color = category), shape = 16) +
  scale_color_manual(values = legend_data$color) +
  theme_void() + 
  theme(legend.position = "bottom", legend.title = element_text(size = 40), legend.text = element_text(size = 40)) +   
  guides(color = guide_legend(title = "Severity", override.aes = list(size = 8))) + ylim(0, 0.1)


ptplot = ggplot() +
  labs(title = "Preterm") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.9, size = 70, face = "bold"))
btplot = ggplot() +
  labs(title = "Low Birthweight") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.73, size = 70, face = "bold"))

title = (ptplot | btplot)
main_fig = (pre_fig | lbw) + plot_layout(widths = c(1.5, 1))

fig2 = (title/main_fig)  + plot_layout(heights = c(0.5, 50))

ggsave("Figures Revision/figure2_npease.png", fig2, width = 16000, height = 5500, units = "px", limitsize = F)



#infant mortality
full = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                     m_height + tri5 + fa_resid
                   |county + year^month + birth_race_dsc_1, data = df_est[which(df_est$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))

data_mort = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Drop Pease AFB Births"),
  Estimate = c(full$coefficients["down"]/mean(df_est$death) * 100, 
               np$coefficients["down"]/mean(df_est$death) * 100),
  StdError = c(full$se["down"]/mean(df_est$death) * 100, 
               np$se["down"]/mean(df_est$death) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_mort$Check = factor(data_mort$Check, c("Baseline", "Drop Pease AFB Births"))


data_mort$down = data_mort$Estimate
data_mort$up = data_mort$Upgradient
data_mort$d_lower = data_mort$down - 1.96 * data_mort$StdError
data_mort$d_upper = data_mort$down + 1.96 * data_mort$StdError
data_mort$pval_label = sprintf("%.5f", data_mort$pval)
data_mort$health_outcome = "lbw"

mort_f2 = figure2_still_fun_npease(data_mort, "Infant Mortality", TRUE, TRUE, "Infant Mortality", FALSE)
mort_f2 = mort_f2 + ggtitle("Infant Mortality") + theme_void() + theme(plot.title = element_text(hjust = -1.5, size = 70, face = "bold"))
ggsave("Figures Revision/figure2_mort_npease.png", mort_f2, width = 11000, height = 2000, units = "px", limitsize = F)
