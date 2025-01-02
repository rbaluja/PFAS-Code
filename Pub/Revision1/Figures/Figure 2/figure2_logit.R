#robustness figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2_function_logit.R")
#function for one sided pvalue (upper)
one_sp_logit = function(expb, sd){
  return(1 - pnorm(log(expb), mean = 0, sd = sd))
}
#preterm
load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness_logit.RData"))
load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness_logit_sd.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust_logit.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust_logit_sd.RData"))
df$ym = paste0(df$year, "-", df$month)

full = glm(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
           + factor(birth_race_dsc_1), data = df, family = "binomial")


drop_close = glm(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(preterm ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(preterm ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")



data_pre = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               exp(pre_2016$coefficients["down"]), 
               exp(p_all_ds$coefficients["down"]), 
               exp(pr_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]) ,
               sd_p_all_ds , 
               sd_p_all_rup,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"])) , 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(p_all_ds$coefficients["down"]), sd_p_all_ds), 
           one_sp_logit(exp(pr_rup$coefficients["down"]), sd_p_all_rup), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]))
  )
)


data_pre$Check = factor(data_pre$Check, c("No Medical Controls", 
                                          "No Demographics", "Relax Upgradient Def'n", 
                                          "Drop Border Sites", 
                                          "Drop After 2015",
                                          "Drop within 1km", "Baseline"))


data_pre$down = data_pre$Estimate
data_pre$up = data_pre$Upgradient
data_pre$d_lower = exp(log(data_pre$down) - 1.96 * data_pre$StdError)
data_pre$d_upper = exp(log(data_pre$down) + 1.96 * data_pre$StdError)
data_pre$pval_label = sprintf("%.5f", data_pre$pval)
data_pre$health_outcome = "preterm"

pre_any = figure2_fun_logit(data_pre, "Any", FALSE, TRUE, "Any", TRUE)


#late preterm
full = glm(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
           + factor(birth_race_dsc_1), data = df, family = "binomial")


drop_close = glm(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(I(gestation < 37 & gestation >= 32) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                        + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(I(gestation < 37 & gestation >= 32) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")


data_mpre = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               exp(pre_2016$coefficients["down"]), 
               exp(lp_ds$coefficients["down"]), 
               exp(lpr_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]),
               sd_lp_ds, 
               sd_lpr_rup,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]),
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(lp_ds$coefficients["down"]), sd_lp_ds), 
           one_sp_logit(exp(lpr_rup$coefficients["down"]), sd_lpr_rup), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]))
  )
)


data_mpre$Check = factor(data_mpre$Check, c("No Medical Controls", 
                                          "No Demographics", "Relax Upgradient Def'n", 
                                          "Drop Border Sites", 
                                          "Drop After 2015",
                                          "Drop within 1km", "Baseline"))

data_mpre$down = data_mpre$Estimate
data_mpre$up = data_mpre$Upgradient
data_mpre$d_lower = exp(log(data_mpre$down) - 1.96 * data_mpre$StdError)
data_mpre$d_upper = exp(log(data_mpre$down) + 1.96 * data_mpre$StdError)
data_mpre$pval_label = sprintf("%.5f", data_mpre$pval)
data_mpre$health_outcome = "mod. preterm"

pre_late = figure2_fun_logit(data_mpre, "Moderately", FALSE, FALSE, "Moderately", TRUE)


#very preterm
full = glm(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
           + factor(birth_race_dsc_1), data = df, family = "binomial")

drop_close = glm(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(I(gestation < 32 & gestation >= 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(I(gestation < 32 & gestation >= 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")



data_vpre = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               exp(pre_2016$coefficients["down"]), 
               exp(mp_ds$coefficients["down"]), 
               exp(mpr_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]),
               sd_mp_ds, 
               sd_mpr_rup,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(mp_ds$coefficients["down"]), sd_mp_ds), 
           one_sp_logit(exp(mpr_rup$coefficients["down"]), sd_mpr_rup), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]))
  )
)


data_vpre$Check = factor(data_vpre$Check, c("No Medical Controls", 
                                          "No Demographics", "Relax Upgradient Def'n", 
                                          "Drop Border Sites", 
                                          "Drop After 2015",
                                          "Drop within 1km", "Baseline"))



data_vpre$down = data_vpre$Estimate
data_vpre$up = data_vpre$Upgradient
data_vpre$d_lower = exp(log(data_vpre$down) - 1.96 * data_vpre$StdError)
data_vpre$d_upper = exp(log(data_vpre$down) + 1.96 * data_vpre$StdError)
data_vpre$pval_label = sprintf("%.5f", data_vpre$pval)
data_vpre$health_outcome = "very preterm"

pre_mod = figure2_fun_logit(data_vpre, "Very", FALSE, FALSE, "Very", TRUE)



#extremely preterm
full = glm(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
           + factor(birth_race_dsc_1), data = df, family = "binomial")


drop_close = glm(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(I(gestation < 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(I(gestation < 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")



data_epre = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               exp(pre_2016$coefficients["down"]), 
               NA, 
               exp(vpr_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]),
               NA, 
               sd_vpr_rup,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"])), 
           NA, 
           one_sp_logit(exp(vpr_rup$coefficients["down"]), sd_vpr_rup), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])))
  )


data_epre$Check = factor(data_epre$Check, c("No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_epre$down = data_epre$Estimate
data_epre$up = data_epre$Upgradient
data_epre$d_lower = exp(log(data_epre$down) - 1.96 * data_epre$StdError)
data_epre$d_upper = exp(log(data_epre$down) + 1.96 * data_epre$StdError)
data_epre$pval_label = sprintf("%.5f", data_epre$pval)
data_epre$health_outcome = "extremely preterm"


pre_very = figure2_fun_logit(data_epre, "Extremely", TRUE, FALSE, "Extremely", TRUE)


pre_fig = pre_any/pre_late/pre_mod/pre_very



####Birthweight
#low birthweight
full = glm(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
           + factor(birth_race_dsc_1), data = df, family = "binomial")


drop_close = glm(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(I(bweight < 2500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(I(bweight < 2500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")



data_lbw = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               exp(pre_2016$coefficients["down"]), 
               exp(lbw_all_ds$coefficients["down"]), 
               exp(lbw_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]),
               sd_lbw_all_ds * exp(lbw_all_ds$coefficients["down"]), 
               sd_lbw_rup * exp(lbw_rup$coefficients["down"]),
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(lbw_all_ds$coefficients["down"]), sd_lbw_all_ds), 
           one_sp_logit(exp(lbw_rup$coefficients["down"]), sd_lbw_rup), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]))))


data_lbw$Check = factor(data_lbw$Check, c("No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_lbw$down = data_lbw$Estimate
data_lbw$up = data_lbw$Upgradient
data_lbw$d_lower = exp(log(data_lbw$down) - 1.96 * data_lbw$StdError)
data_lbw$d_upper = exp(log(data_lbw$down) + 1.96 * data_lbw$StdError)
data_lbw$pval_label = sprintf("%.5f", data_lbw$pval)
data_lbw$health_outcome = "low birthweight"

lbw_all = figure2_fun_logit(data_lbw, "Any", FALSE, TRUE, "Any", FALSE)


#late low birthweight
full = glm(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
           + factor(birth_race_dsc_1), data = df, family = "binomial")


drop_close = glm(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(I(bweight < 2500 & bweight >= 1500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(I(bweight < 2500 & bweight >= 1500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")




data_mlbw = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               exp(pre_2016$coefficients["down"]), 
               exp(llbw_ds$coefficients["down"]), 
               exp(llbw_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]),
               sd_llbw_ds, 
               sd_llbw_rup ,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])) ,
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(llbw_ds$coefficients["down"]), sd_llbw_ds), 
           one_sp_logit(exp(llbw_rup$coefficients["down"]), sd_llbw_rup), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) )))


data_mlbw$Check = factor(data_mlbw$Check, c("No Medical Controls", 
                                          "No Demographics", "Relax Upgradient Def'n", 
                                          "Drop Border Sites", 
                                          "Drop After 2015",
                                          "Drop within 1km", "Baseline"))


data_mlbw$down = data_mlbw$Estimate
data_mlbw$up = data_mlbw$Upgradient
data_mlbw$d_lower = exp(log(data_mlbw$down) - 1.96 * data_mlbw$StdError)
data_mlbw$d_upper = exp(log(data_mlbw$down) + 1.96 * data_mlbw$StdError)
data_mlbw$pval_label = sprintf("%.5f", data_mlbw$pval)
data_mlbw$health_outcome = "mod. low birthweight"

lbw_slight = figure2_fun_logit(data_mlbw, "Moderately", FALSE, FALSE, "Moderately", FALSE)


#moderatelty low birthweight
full = glm(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     + factor(birth_race_dsc_1), data = df, family = "binomial")


drop_close = glm(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(I(bweight < 1500 & bweight >= 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(I(bweight < 1500 & bweight >= 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")



data_vlbw = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               exp(pre_2016$coefficients["down"]), 
               exp(mlbw_ds$coefficients["down"]), 
               exp(mlbw_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]) ,
               sd_mlbw_ds , 
               sd_mlbw_rup ,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) ),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]) ), 
           one_sp_logit(exp(mlbw_ds$coefficients["down"]), sd_mlbw_ds ), 
           one_sp_logit(exp(mlbw_rup$coefficients["down"]), sd_mlbw_rup ), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) ), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) )))


data_vlbw$Check = factor(data_vlbw$Check, c("No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_vlbw$down = data_vlbw$Estimate
data_vlbw$up = data_vlbw$Upgradient
data_vlbw$d_lower = exp(log(data_vlbw$down) - 1.96 * data_vlbw$StdError)
data_vlbw$d_upper = exp(log(data_vlbw$down) + 1.96 * data_vlbw$StdError)
data_vlbw$pval_label = sprintf("%.5f", data_vlbw$pval)
data_vlbw$health_outcome = "very low birthweight"

lbw_mod = figure2_fun_logit(data_vlbw, "Very", FALSE, FALSE, "Very", FALSE)


#very low birthweight
full = glm(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
           + factor(birth_race_dsc_1), data = df, family = "binomial")


drop_close = glm(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(I(bweight < 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(I(bweight < 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")



data_elbw = data.frame(
  Category = c("Baseline", 
               "Sample", 
               "Sample", 
               "Sample", 
               "Sample",
               "Controls", 
               "Controls"),
  Check = c("Baseline", 
            "Drop within 1km", 
            "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", 
            "No Demographics", 
            "No Medical Controls"),
  Estimate = c(exp(full$coefficients["down"]), 
               exp(drop_close$coefficients["down"]), 
               NA, 
               NA, 
               exp(vlbw_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]) , 
               NA,
               NA , 
               sd_vlbw_rup ,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) ),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) ), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]) ), 
           NA, 
           NA, 
           one_sp_logit(exp(vlbw_rup$coefficients["down"]), sd_vlbw_rup ), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) ), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) )))


data_elbw$Check = factor(data_elbw$Check, c("No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_elbw$down = data_elbw$Estimate
data_elbw$up = data_elbw$Upgradient
data_elbw$d_lower = exp(log(data_elbw$down) - 1.96 * data_elbw$StdError)
data_elbw$d_upper = exp(log(data_elbw$down) + 1.96 * data_elbw$StdError)
data_elbw$pval_label = sprintf("%.5f", data_elbw$pval)
data_elbw$health_outcome = "extremely low birthweight"

lbw_very = 
  figure2_fun_logit(data_elbw, "Extremely", TRUE, FALSE, "Extremely", FALSE)


lbw = lbw_all/lbw_slight/lbw_mod/lbw_very

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

ggsave("Figures Revision/figure2_logit.png", fig2, width = 17000, height = 10000, units = "px", limitsize = F)


fig2_data = rbind(data_pre, data_mpre, data_vpre, data_epre, 
                  data_lbw, data_mlbw, data_vlbw, data_elbw)
fwrite(fig2_data, "Figures Revision/Data/figure2_logit_data.csv")