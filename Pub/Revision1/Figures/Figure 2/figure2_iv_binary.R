source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/figure2_rob_fns.R")
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/figure2_rob_mort_fns.R")
source("PFAS-Code/Pub/Tables/bs_functions.R")
#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}


#run iv comparisons
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


pre_iv = fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )


lpre_iv = fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                          n_sites + wind_exposure + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )

vpre_iv = fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                          n_sites + wind_exposure + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )


epre_iv = fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                          n_sites + wind_exposure + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )




lbw_iv = fixest::feols(I(bweight < 2500 ) ~ pred_pfas + asinh(pfas) +
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )

mlbw_iv= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                         n_sites + wind_exposure + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )

vlbw_iv = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) +
                          n_sites + wind_exposure + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )


elbw_iv = fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) +
                          n_sites + wind_exposure + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2 )


mort = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                       n_sites + wind_exposure + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df2)

df2$pred_pre = predict(pre_iv, newdata = df2)
df2$pred_lpre = predict(lpre_iv, newdata = df2)
df2$pred_vpre = predict(vpre_iv, newdata = df2)
df2$pred_epre = predict(epre_iv, newdata = df2)
df2$pred_lbw = predict(lbw_iv, newdata = df2)
df2$pred_mlbw = predict(mlbw_iv, newdata = df2)
df2$pred_vlbw = predict(vlbw_iv, newdata = df2)
df2$pred_elbw = predict(elbw_iv, newdata = df2)
df2$pred_mort = predict(mort, newdata = df2)

#get change in predicted probability from mean down to mean up
pre_ivc = (mean(df2[which(df2$down == 1), ]$pred_pre) - mean(df2[which(df2$up == 1), ]$pred_pre))/mean(df$gestation < 37) * 100
mpre_ivc = (mean(df2[which(df2$down == 1), ]$pred_lpre) - mean(df2[which(df2$up == 1), ]$pred_lpre))/mean(df$gestation < 37 & df$gestation >= 32) * 100
vpre_ivc = (mean(df2[which(df2$down == 1), ]$pred_vpre) - mean(df2[which(df2$up == 1), ]$pred_vpre))/mean(df$gestation < 32 & df$gestation >= 28) * 100
epre_ivc = (mean(df2[which(df2$down == 1), ]$pred_epre) - mean(df2[which(df2$up == 1), ]$pred_epre))/mean(df$gestation < 28) * 100

lbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_lbw) - mean(df2[which(df2$up == 1), ]$pred_lbw))/mean(df$bweight < 2500) * 100
mlbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_mlbw) - mean(df2[which(df2$up == 1), ]$pred_mlbw))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
vlbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_vlbw) - mean(df2[which(df2$up == 1), ]$pred_vlbw))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
elbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_elbw) - mean(df2[which(df2$up == 1), ]$pred_elbw))/mean(df$bweight < 1000) * 100

mort_ivc = (mean(df2[which(df2$down == 1), ]$pred_mort) - mean(df2[which(df2$up == 1), ]$pred_mort))/mean(df$death) * 100


#load bootstrap data from these statistics
load(modify_path(paste0("Data_Verify/Revision 1/RData/bootstrap_iv_bin_comp", ppt, ".RData")))
pre_ivc_se = med_boot(boot_coefs, "pre", pre_ivc)
mpre_ivc_se = med_boot(boot_coefs, "mpre", mpre_ivc)
vpre_ivc_se = med_boot(boot_coefs, "vpre", vpre_ivc)
epre_ivc_se = med_boot(boot_coefs, "epre", epre_ivc)

lbw_ivc_se = med_boot(boot_coefs, "lbw", lbw_ivc)
mlbw_ivc_se = med_boot(boot_coefs, "mlbw", mlbw_ivc)
vlbw_ivc_se = med_boot(boot_coefs, "vlbw", vlbw_ivc)
elbw_ivc_se = med_boot(boot_coefs, "elbw", elbw_ivc)

mort_ivc_se = med_boot(boot_coefs, "mort", mort_ivc)




#preterm
full = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_pre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 37) * 100, 
               pre_ivc),
  StdError = c(full$se["down"]/mean(df$gestation < 37) * 100, 
               pre_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(pre_ivc, mean = 0, sd = pre_ivc_se, lower.tail = F))
)


data_pre$Check = factor(data_pre$Check, c("Model 1", "Model 2"))


data_pre$down = data_pre$Estimate
data_pre$d_lower = data_pre$down - 1.96 * data_pre$StdError
data_pre$d_upper = data_pre$down + 1.96 * data_pre$StdError
data_pre$pval_label = sprintf("%.5f", data_pre$pval)
data_pre$health_outcome = "preterm"
data_pre <- data_pre %>% 
  arrange(Check)
pre_any = figure2_fun_ivbin(data_pre, "Any", FALSE, TRUE, "Any", TRUE)




#Moderately preterm
full = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


data_mpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               mpre_ivc),
  StdError = c(full$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               mpre_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(mpre_ivc, mean = 0, sd = mpre_ivc_se, lower.tail = F))
)


data_mpre$Check = factor(data_mpre$Check, c("Model 1", "Model 2"))


data_mpre$down = data_mpre$Estimate
data_mpre$d_lower = data_mpre$down - 1.96 * data_mpre$StdError
data_mpre$d_upper = data_mpre$down + 1.96 * data_mpre$StdError
data_mpre$pval_label = sprintf("%.5f", data_mpre$pval)
data_mpre$health_outcome = "preterm"

mpre = figure2_fun_ivbin(data_mpre, "Moderately", FALSE, FALSE, "Moderately", TRUE)



#Very preterm
full = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_vpre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               vpre_ivc),
  StdError = c(full$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               vpre_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(vpre_ivc, mean = 0, sd = vpre_ivc_se, lower.tail = F))
)


data_vpre$Check = factor(data_vpre$Check, c("Model 1", "Model 2"))


data_vpre$down = data_vpre$Estimate
data_vpre$up = data_vpre$Upgradient
data_vpre$d_lower = data_vpre$down - 1.96 * data_vpre$StdError
data_vpre$d_upper = data_vpre$down + 1.96 * data_vpre$StdError
data_vpre$pval_label = sprintf("%.5f", data_vpre$pval)
data_vpre$health_outcome = "preterm"

vpre = figure2_fun_ivbin(data_vpre, "Very", FALSE, FALSE, "Very", TRUE)



#Extremeley preterm
full = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_epre = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 28) * 100, 
               epre_ivc),
  StdError = c(full$se["down"]/mean(df$gestation < 28) * 100, 
               epre_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(epre_ivc, mean = 0, sd = epre_ivc_se, lower.tail = F))
)


data_epre$Check = factor(data_epre$Check, c("Model 1", "Model 2"))


data_epre$down = data_epre$Estimate
data_epre$up = data_epre$Upgradient
data_epre$d_lower = data_epre$down - 1.96 * data_epre$StdError
data_epre$d_upper = data_epre$down + 1.96 * data_epre$StdError
data_epre$pval_label = sprintf("%.5f", data_epre$pval)
data_epre$health_outcome = "preterm"

epre = figure2_fun_ivbin(data_epre, "Extremely", TRUE, FALSE, "Extremely", TRUE)

pre_fig = pre_any/mpre/vpre/epre




#birthweight
#low birthweight
full = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_lbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 2500) * 100, 
               lbw_ivc),
  StdError = c(full$se["down"]/mean(df$bweight < 2500) * 100, 
               lbw_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(lbw_ivc, mean = 0, sd = lbw_ivc_se, lower.tail = F))
)


data_lbw$Check = factor(data_lbw$Check, c("Model 1", "Model 2"))


data_lbw$down = data_lbw$Estimate
data_lbw$up = data_lbw$Upgradient
data_lbw$d_lower = data_lbw$down - 1.96 * data_lbw$StdError
data_lbw$d_upper = data_lbw$down + 1.96 * data_lbw$StdError
data_lbw$pval_label = sprintf("%.5f", data_lbw$pval)
data_lbw$health_outcome = "lbw"

lbw_any = figure2_fun_ivbin(data_lbw, "Any", FALSE, TRUE, "Any", FALSE)




#Moderately lbw
full = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_mlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               mlbw_ivc),
  StdError = c(full$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               mlbw_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(mlbw_ivc, mean = 0, sd = mlbw_ivc_se, lower.tail = F))
)


data_mlbw$Check = factor(data_mlbw$Check, c("Model 1", "Model 2"))


data_mlbw$down = data_mlbw$Estimate
data_mlbw$up = data_mlbw$Upgradient
data_mlbw$d_lower = data_mlbw$down - 1.96 * data_mlbw$StdError
data_mlbw$d_upper = data_mlbw$down + 1.96 * data_mlbw$StdError
data_mlbw$pval_label = sprintf("%.5f", data_mlbw$pval)
data_mlbw$health_outcome = "lbw"

mlbw = figure2_fun_ivbin(data_mlbw, "Moderately", FALSE, FALSE, "Moderately", FALSE)



#Very lbw
full = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_vlbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               vlbw_ivc),
  StdError = c(full$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               vlbw_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(vlbw_ivc, mean = 0, sd = vlbw_ivc_se, lower.tail = F))
)


data_vlbw$Check = factor(data_vlbw$Check, c("Model 1", "Model 2"))


data_vlbw$down = data_vlbw$Estimate
data_vlbw$up = data_vlbw$Upgradient
data_vlbw$d_lower = data_vlbw$down - 1.96 * data_vlbw$StdError
data_vlbw$d_upper = data_vlbw$down + 1.96 * data_vlbw$StdError
data_vlbw$pval_label = sprintf("%.5f", data_vlbw$pval)
data_vlbw$health_outcome = "lbw"

vlbw = figure2_fun_ivbin(data_vlbw, "Very", FALSE, FALSE, "Very", FALSE)



#Extremeley lbw
full = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_elbw = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 1000) * 100, 
               elbw_ivc),
  StdError = c(full$se["down"]/mean(df$bweight < 1000) * 100, 
               elbw_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(elbw_ivc, mean = 0, sd = elbw_ivc_se, lower.tail = F))
)


data_elbw$Check = factor(data_elbw$Check, c("Model 1", "Model 2"))


data_elbw$down = data_elbw$Estimate
data_elbw$up = data_elbw$Upgradient
data_elbw$d_lower = data_elbw$down - 1.96 * data_elbw$StdError
data_elbw$d_upper = data_elbw$down + 1.96 * data_elbw$StdError
data_elbw$pval_label = sprintf("%.5f", data_elbw$pval)
data_elbw$health_outcome = "lbw"

elbw = figure2_fun_ivbin(data_elbw, "Extremely", TRUE, FALSE, "Extremely", FALSE)


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

ggsave(paste0("Figures Revision/figure2_ivbin", ppt, ".png"), fig2, width = 17000, height = 5500, units = "px", limitsize = F)



#infant mortality
full = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_mort = data.frame(
  Category = c("Baseline", "Controls"),
  Check = c("Model 1", "Model 2"),
  Estimate = c(full$coefficients["down"]/mean(df$death) * 100, 
               mort_ivc),
  StdError = c(full$se["down"]/mean(df$death) * 100, 
               mort_ivc_se),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           pnorm(mort_ivc, mean = 0, sd = mort_ivc_se, lower.tail = F))
)


data_mort$Check = factor(data_mort$Check, c("Model 1", "Model 2"))


data_mort$down = data_mort$Estimate
data_mort$up = data_mort$Upgradient
data_mort$d_lower = data_mort$down - 1.96 * data_mort$StdError
data_mort$d_upper = data_mort$down + 1.96 * data_mort$StdError
data_mort$pval_label = sprintf("%.5f", data_mort$pval)
data_mort$health_outcome = "lbw"

mort_f2 = figure2_still_fun_ivbin(data_mort, "Infant Mortality", TRUE, TRUE, "Infant Mortality", FALSE)
mort_f2 = mort_f2 + ggtitle("Infant Mortality") + theme_void() + theme(plot.title = element_text(hjust = -1.5, size = 70, face = "bold"))
ggsave(paste0("Figures Revision/figure2_mort_ivbin", ppt, ".png"), mort_f2, width = 11000, height = 2000, units = "px", limitsize = F)
