source("PFAS-Code/Pub/Revision2/Figures/figure2_fns.R")
source("PFAS-Code/Pub/Revision2/Figures/figure2_mort_fns.R")
#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

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

df %>%
  mutate(m_age_adj = ifelse(is.na(m_age), mean(m_age, na.rm = T), m_age), 
         m_married_adj = ifelse(is.na(m_married), mean(m_married, na.rm = T), m_married), 
         private_insurance_adj = ifelse(is.na(private_insurance), mean(private_insurance, na.rm = T), private_insurance), 
         nbr_cgrtt_adj = ifelse(is.na(nbr_cgrtt), mean(nbr_cgrtt, na.rm = T), nbr_cgrtt), 
         m_educ_adj = ifelse(is.na(m_educ), mean(m_educ, na.rm = T), m_educ), 
         f_educ_adj = ifelse(is.na(f_educ), mean(f_educ, na.rm = T), f_educ),
         pm25_adj = ifelse(is.na(pm25), mean(pm25, na.rm = T), pm25),
         temp_adj = ifelse(is.na(temp), mean(temp, na.rm = T), temp),
         med_inc_adj = ifelse(is.na(med_inc), mean(med_inc, na.rm = T), med_inc), 
         p_manuf_adj = ifelse(is.na(p_manuf), mean(p_manuf, na.rm = T), p_manuf),
         n_hunits_adj = ifelse(is.na(n_hunits), mean(n_hunits, na.rm = T), n_hunits), 
         med_hprice_adj = ifelse(is.na(med_hprice), mean(med_hprice, na.rm = T), med_hprice),
         well_elev_adj = ifelse(is.na(well_elev), mean(well_elev, na.rm = T), well_elev),
         resid_elev_adj = ifelse(is.na(resid_elev), mean(resid_elev, na.rm = T), resid_elev),
         csite_dist_adj = ifelse(is.na(csite_dist), mean(csite_dist, na.rm = T), csite_dist),
         mthr_wgt_dlv_adj = ifelse(is.na(mthr_wgt_dlv), mean(mthr_wgt_dlv, na.rm = T), mthr_wgt_dlv),
         mthr_pre_preg_wgt_adj = ifelse(is.na(mthr_pre_preg_wgt), mean(mthr_pre_preg_wgt, na.rm = T), mthr_pre_preg_wgt),
         m_height_adj = ifelse(is.na(m_height), mean(m_height, na.rm = T), m_height),
         fa_resid_adj = ifelse(is.na(fa_resid), mean(fa_resid, na.rm = T), fa_resid),
         wic_adj = ifelse(is.na(wic), mean(wic, na.rm = T), wic),
         mr_04_adj = ifelse(is.na(mr_04), mean(mr_04, na.rm = T), mr_04),
         mr_08_adj = ifelse(is.na(mr_08), mean(mr_08, na.rm = T), mr_08),
         mr_18_adj = ifelse(is.na(mr_18), mean(mr_18, na.rm = T), mr_18),
         mr_21_adj = ifelse(is.na(mr_21), mean(mr_21, na.rm = T), mr_21),
         mr_26_adj = ifelse(is.na(mr_26), mean(mr_26, na.rm = T), mr_26),
         mr_27_adj = ifelse(is.na(mr_27), mean(mr_27, na.rm = T), mr_27),
         tri5_adj = ifelse(is.na(tri5), mean(tri5, na.rm = T), tri5), 
         birth_race_dsc_1_adj = ifelse(is.na(birth_race_dsc_1), "", birth_race_dsc_1)
  ) -> df

#preterm
full = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_pre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 37) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 37) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 37) * 100, 
               np$se["down"]/mean(df_est$gestation < 37) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_pre$Check = factor(data_pre$Check, c("Baseline", "Include Missing Obs"))


data_pre$down = data_pre$Estimate
data_pre$up = data_pre$Upgradient
data_pre$d_lower = data_pre$down - 1.96 * data_pre$StdError
data_pre$d_upper = data_pre$down + 1.96 * data_pre$StdError
data_pre$pval_label = sprintf("%.5f", data_pre$pval)
data_pre$health_outcome = "preterm"

pre_any = figure2_fun_imo(data_pre, "Any", FALSE, TRUE, "Any", TRUE)




#Moderately preterm
full = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_mpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               np$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_mpre$Check = factor(data_mpre$Check, c("Baseline", "Include Missing Obs"))


data_mpre$down = data_mpre$Estimate
data_mpre$up = data_mpre$Upgradient
data_mpre$d_lower = data_mpre$down - 1.96 * data_mpre$StdError
data_mpre$d_upper = data_mpre$down + 1.96 * data_mpre$StdError
data_mpre$pval_label = sprintf("%.5f", data_mpre$pval)
data_mpre$health_outcome = "preterm"

mpre = figure2_fun_imo(data_mpre, "Moderately", FALSE, FALSE, "Moderately", TRUE)



#Very preterm
full = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_vpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               np$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_vpre$Check = factor(data_vpre$Check, c("Baseline", "Include Missing Obs"))


data_vpre$down = data_vpre$Estimate
data_vpre$up = data_vpre$Upgradient
data_vpre$d_lower = data_vpre$down - 1.96 * data_vpre$StdError
data_vpre$d_upper = data_vpre$down + 1.96 * data_vpre$StdError
data_vpre$pval_label = sprintf("%.5f", data_vpre$pval)
data_vpre$health_outcome = "preterm"

vpre = figure2_fun_imo(data_vpre, "Very", FALSE, FALSE, "Very", TRUE)



#Extremeley preterm
full = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_epre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 28) * 100, 
               np$coefficients["down"]/mean(df_est$gestation < 28) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 28) * 100, 
               np$se["down"]/mean(df_est$gestation < 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_epre$Check = factor(data_epre$Check, c("Baseline", "Include Missing Obs"))


data_epre$down = data_epre$Estimate
data_epre$up = data_epre$Upgradient
data_epre$d_lower = data_epre$down - 1.96 * data_epre$StdError
data_epre$d_upper = data_epre$down + 1.96 * data_epre$StdError
data_epre$pval_label = sprintf("%.5f", data_epre$pval)
data_epre$health_outcome = "preterm"

epre = figure2_fun_imo(data_epre, "Extremely", TRUE, FALSE, "Extremely", TRUE)

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
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_lbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 2500) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 2500) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 2500) * 100, 
               np$se["down"]/mean(df_est$bweight < 2500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_lbw$Check = factor(data_lbw$Check, c("Baseline", "Include Missing Obs"))


data_lbw$down = data_lbw$Estimate
data_lbw$up = data_lbw$Upgradient
data_lbw$d_lower = data_lbw$down - 1.96 * data_lbw$StdError
data_lbw$d_upper = data_lbw$down + 1.96 * data_lbw$StdError
data_lbw$pval_label = sprintf("%.5f", data_lbw$pval)
data_lbw$health_outcome = "lbw"

lbw_any = figure2_fun_imo(data_lbw, "Any", FALSE, TRUE, "Any", FALSE)




#Moderately lbw
full = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_mlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               np$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_mlbw$Check = factor(data_mlbw$Check, c("Baseline", "Include Missing Obs"))


data_mlbw$down = data_mlbw$Estimate
data_mlbw$up = data_mlbw$Upgradient
data_mlbw$d_lower = data_mlbw$down - 1.96 * data_mlbw$StdError
data_mlbw$d_upper = data_mlbw$down + 1.96 * data_mlbw$StdError
data_mlbw$pval_label = sprintf("%.5f", data_mlbw$pval)
data_mlbw$health_outcome = "lbw"

mlbw = figure2_fun_imo(data_mlbw, "Moderately", FALSE, FALSE, "Moderately", FALSE)



#Very lbw
full = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_vlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               np$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_vlbw$Check = factor(data_vlbw$Check, c("Baseline", "Include Missing Obs"))


data_vlbw$down = data_vlbw$Estimate
data_vlbw$up = data_vlbw$Upgradient
data_vlbw$d_lower = data_vlbw$down - 1.96 * data_vlbw$StdError
data_vlbw$d_upper = data_vlbw$down + 1.96 * data_vlbw$StdError
data_vlbw$pval_label = sprintf("%.5f", data_vlbw$pval)
data_vlbw$health_outcome = "lbw"

vlbw = figure2_fun_imo(data_vlbw, "Very", FALSE, FALSE, "Very", FALSE)



#Extremeley lbw
full = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_elbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 1000) * 100, 
               np$coefficients["down"]/mean(df_est$bweight < 1000) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 1000) * 100, 
               np$se["down"]/mean(df_est$bweight < 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_elbw$Check = factor(data_elbw$Check, c("Baseline", "Include Missing Obs"))


data_elbw$down = data_elbw$Estimate
data_elbw$up = data_elbw$Upgradient
data_elbw$d_lower = data_elbw$down - 1.96 * data_elbw$StdError
data_elbw$d_upper = data_elbw$down + 1.96 * data_elbw$StdError
data_elbw$pval_label = sprintf("%.5f", data_elbw$pval)
data_elbw$health_outcome = "lbw"

elbw = figure2_fun_imo(data_elbw, "Extremely", TRUE, FALSE, "Extremely", FALSE)


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

ggsave("Figures Revision/figure2_imo.png", fig2, width = 16000, height = 5500, units = "px", limitsize = F)



#infant mortality
full = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df_est, warn = F, notes = F, cluster = c("site", "year^month"))

np = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                     m_age_adj + m_married_adj  + private_insurance_adj  + nbr_cgrtt_adj  + m_educ_adj + f_educ_adj +
                     pm25_adj + temp_adj +med_inc_adj+ p_manuf_adj + n_hunits_adj + med_hprice_adj  + well_elev_adj + resid_elev_adj + csite_dist_adj + wic_adj+
                     mr_04_adj + mr_18_adj + mr_08_adj + mr_21_adj + mr_26_adj + mr_27_adj + 
                     mthr_wgt_dlv_adj +mthr_pre_preg_wgt_adj + 
                     m_height_adj + tri5_adj +fa_resid_adj + wind_exposure + 
                     mis_m_age + mis_m_married + mis_private_insurance + mis_nbr_cgrtt + 
                     mis_m_educ + mis_f_educ + mis_pm25 + mis_temp + mis_med_inc + mis_p_manuf + mis_n_hunits +
                     mis_med_hprice + mis_well_elev + mis_resid_elev + mis_csite_dist + mis_wic +
                     mis_mr_04 + mis_mr_18 + mis_mr_08 + mis_mr_21 + mis_mr_26 + mis_mr_27 +
                     mis_mthr_wgt_dlv + mis_mthr_pre_preg_wgt +
                     mis_m_height + mis_fa_resid + mis_tri5 + mis_birth_race
                   |county + year^month + birth_race_dsc_1_adj, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_mort = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Baseline", "Include Missing Obs"),
  Estimate = c(full$coefficients["down"]/mean(df_est$death) * 100, 
               np$coefficients["down"]/mean(df_est$death) * 100),
  StdError = c(full$se["down"]/mean(df_est$death) * 100, 
               np$se["down"]/mean(df_est$death) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(np$coeftable["down", "t value"], np$coeftable["down", "Pr(>|t|)"]))
)


data_mort$Check = factor(data_mort$Check, c("Baseline", "Include Missing Obs"))


data_mort$down = data_mort$Estimate
data_mort$up = data_mort$Upgradient
data_mort$d_lower = data_mort$down - 1.96 * data_mort$StdError
data_mort$d_upper = data_mort$down + 1.96 * data_mort$StdError
data_mort$pval_label = sprintf("%.5f", data_mort$pval)
data_mort$health_outcome = "lbw"

mort_f2 = figure2_still_fun_imo(data_mort, "Infant Mortality", TRUE, TRUE, "Infant Mortality", FALSE)
mort_f2 = mort_f2 + ggtitle("Infant Mortality") + theme_void() + theme(plot.title = element_text(hjust = -1.5, size = 70, face = "bold"))
ggsave(modify_path3("Figures/figure2_mort_imo.png"), mort_f2, width = 11000, height = 2000, units = "px", limitsize = F)
