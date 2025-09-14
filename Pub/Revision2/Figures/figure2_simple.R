#robustness figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/Baseline/figure2_function.R")
#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}
#preterm
load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust.RData"))

full = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

site  = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(preterm ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(preterm ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(preterm ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



data_pre = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 37) * 100, 
               drop_close$coefficients["down"]/mean(df_est$gestation < 37) * 100, pre_2016$coefficients["down"]/mean(df_est$gestation < 37) * 100, 
               p_all_ds$coefficients["down"]/mean(df_est$gestation < 37) * 100,  pr_rup$coefficients["down"]/mean(df_est$gestation < 37) * 100,
               no_pers$coefficients["down"]/mean(df_est$gestation < 37) * 100, 
               no_med$coefficients["down"]/mean(df_est$gestation < 37) * 100, site$coefficients["down"]/mean(df_est$gestation < 37) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 37) * 100, 
               drop_close$se["down"]/mean(df_est$gestation < 37) * 100, pre_2016$se["down"]/mean(df_est$gestation < 37) * 100,
               p_all_ds$se["down"]/mean(df_est$gestation < 37) * 100, 
               pr_rup$se["down"]/mean(df_est$gestation < 37) * 100,
               no_pers$se["down"]/mean(df_est$gestation < 37) * 100, 
               no_med$se["down"]/mean(df_est$gestation < 37) * 100, site$se["down"]/mean(df_est$gestation < 37) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           
           one_sp(p_all_ds$coeftable["down", "t value"], p_all_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pr_rup$coeftable["down", "t value"], pr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)


data_pre$Check = factor(data_pre$Check, c("Site Fixed Effects", "No Medical Controls", 
                                          "No Demographics", "Relax Upgradient Def'n", 
                                          "Drop Border Sites", 
                                          "Drop After 2015",
                                          "Drop within 1km", "Baseline"))


data_pre$down = data_pre$Estimate
data_pre$up = data_pre$Upgradient
data_pre$d_lower = data_pre$down - 1.96 * data_pre$StdError
data_pre$d_upper = data_pre$down + 1.96 * data_pre$StdError
data_pre$pval_label = sprintf("%.5f", data_pre$pval)
data_pre$health_outcome = "preterm"

pre_any = figure2_fun(data_pre, "Any", FALSE, TRUE, "Any", TRUE)


#late preterm
full = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


site  = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(gestation < 37 & gestation >= 32) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(gestation < 37 & gestation >= 32) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(gestation < 37 & gestation >= 32) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data_mpre = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               drop_close$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, pre_2016$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               lp_ds$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100,  lpr_rup$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100,
               no_pers$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               no_med$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, site$coefficients["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               drop_close$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, pre_2016$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               lp_ds$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               lpr_rup$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100,
               no_pers$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, 
               no_med$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100, site$se["down"]/mean(df_est$gestation < 37 & df_est$gestation >= 32) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lp_ds$coeftable["down", "t value"], lp_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lpr_rup$coeftable["down", "t value"], lpr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)


data_mpre$Check = factor(data_mpre$Check, c("Site Fixed Effects", "No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))

data_mpre$down = data_mpre$Estimate
data_mpre$up = data_mpre$Upgradient
data_mpre$d_lower = data_mpre$down - 1.96 * data_mpre$StdError
data_mpre$d_upper = data_mpre$down + 1.96 * data_mpre$StdError
data_mpre$pval_label = sprintf("%.5f", data_mpre$pval)
data_mpre$health_outcome = "mod. preterm"

pre_late = figure2_fun(data_mpre, "Moderately", FALSE, FALSE, "Moderately", TRUE)


#very preterm
full = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(gestation < 32 & gestation >= 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(gestation < 32 & gestation >= 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(gestation < 32 & gestation >= 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(gestation < 32 & gestation >= 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(gestation < 32 & gestation >= 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

data_vpre = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               drop_close$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, pre_2016$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               mp_ds$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100,  mpr_rup$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100,
               no_pers$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               no_med$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, site$coefficients["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               drop_close$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, pre_2016$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               mp_ds$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               mpr_rup$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100,
               no_pers$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, 
               no_med$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100, site$se["down"]/mean(df_est$gestation < 32 & df_est$gestation >= 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mp_ds$coeftable["down", "t value"], mp_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mpr_rup$coeftable["down", "t value"], mpr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)



data_vpre$Check = factor(data_vpre$Check, c("Site Fixed Effects", "No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))



data_vpre$down = data_vpre$Estimate
data_vpre$up = data_vpre$Upgradient
data_vpre$d_lower = data_vpre$down - 1.96 * data_vpre$StdError
data_vpre$d_upper = data_vpre$down + 1.96 * data_vpre$StdError
data_vpre$pval_label = sprintf("%.5f", data_vpre$pval)
data_vpre$health_outcome = "very preterm"

pre_mod = figure2_fun(data_vpre, "Very", FALSE, FALSE, "Very", TRUE)



#extremely preterm
full = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(gestation < 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(gestation < 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(gestation < 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(gestation < 28) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(gestation < 28) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data_epre = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$gestation < 28) * 100, 
               drop_close$coefficients["down"]/mean(df_est$gestation < 28) * 100, pre_2016$coefficients["down"]/mean(df_est$gestation < 28) * 100, 
               vp_ds$coefficients["down"]/mean(df_est$gestation < 28) * 100,  vpr_rup$coefficients["down"]/mean(df_est$gestation < 28) * 100,
               no_pers$coefficients["down"]/mean(df_est$gestation < 28) * 100, 
               no_med$coefficients["down"]/mean(df_est$gestation < 28) * 100, site$coefficients["down"]/mean(df_est$gestation < 28) * 100),
  StdError = c(full$se["down"]/mean(df_est$gestation < 28) * 100, 
               drop_close$se["down"]/mean(df_est$gestation < 28) * 100, pre_2016$se["down"]/mean(df_est$gestation < 28) * 100, 
               vp_ds$se["down"]/mean(df_est$gestation < 28) * 100, 
               vpr_rup$se["down"]/mean(df_est$gestation < 28) * 100,
               no_pers$se["down"]/mean(df_est$gestation < 28) * 100, 
               no_med$se["down"]/mean(df_est$gestation < 28) * 100, site$se["down"]/mean(df_est$gestation < 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vp_ds$coeftable["down", "t value"], vp_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vpr_rup$coeftable["down", "t value"], vpr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)



data_epre$Check = factor(data_epre$Check, c("Site Fixed Effects", "No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_epre$down = data_epre$Estimate
data_epre$up = data_epre$Upgradient
data_epre$d_lower = data_epre$down - 1.96 * data_epre$StdError
data_epre$d_upper = data_epre$down + 1.96 * data_epre$StdError
data_epre$pval_label = sprintf("%.5f", data_epre$pval)
data_epre$health_outcome = "extremely preterm"


pre_very = figure2_fun(data_epre, "Extremely", TRUE, FALSE, "Extremely", TRUE)

pre_fig = pre_any/pre_very



####Birthweight
#low birthweight
full = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 2500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 2500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 2500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 2500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 2500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data_lbw = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 2500) * 100, 
               drop_close$coefficients["down"]/mean(df_est$bweight < 2500) * 100, pre_2016$coefficients["down"]/mean(df_est$bweight < 2500) * 100, 
               lbw_all_ds$coefficients["down"]/mean(df_est$bweight < 2500) * 100,  lbw_rup$coefficients["down"]/mean(df_est$bweight < 2500) * 100,
               no_pers$coefficients["down"]/mean(df_est$bweight < 2500) * 100, 
               no_med$coefficients["down"]/mean(df_est$bweight < 2500) * 100, site$coefficients["down"]/mean(df_est$bweight < 2500) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 2500) * 100, 
               drop_close$se["down"]/mean(df_est$bweight < 2500) * 100, pre_2016$se["down"]/mean(df_est$bweight < 2500) * 100, 
               lbw_all_ds$se["down"]/mean(df_est$bweight < 2500) * 100, 
               lbw_rup$se["down"]/mean(df_est$bweight < 2500) * 100,
               no_pers$se["down"]/mean(df_est$bweight < 2500) * 100, 
               no_med$se["down"]/mean(df_est$bweight < 2500) * 100, site$se["down"]/mean(df_est$bweight < 2500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lbw_all_ds$coeftable["down", "t value"], lbw_all_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lbw_rup$coeftable["down", "t value"], lbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)


data_lbw$Check = factor(data_lbw$Check, c("Site Fixed Effects", "No Medical Controls", 
                                          "No Demographics", "Relax Upgradient Def'n", 
                                          "Drop Border Sites", 
                                          "Drop After 2015",
                                          "Drop within 1km", "Baseline"))


data_lbw$down = data_lbw$Estimate
data_lbw$up = data_lbw$Upgradient
data_lbw$d_lower = data_lbw$down - 1.96 * data_lbw$StdError
data_lbw$d_upper = data_lbw$down + 1.96 * data_lbw$StdError
data_lbw$pval_label = sprintf("%.5f", data_lbw$pval)
data_lbw$health_outcome = "low birthweight"

lbw_all = figure2_fun(data_lbw, "Any", FALSE, TRUE, "Any", FALSE)


#late low birthweight
full = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data_mlbw = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               drop_close$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, pre_2016$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               llbw_ds$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100,  llbw_rup$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100,
               no_pers$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               no_med$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, site$coefficients["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               drop_close$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, pre_2016$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               llbw_ds$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               llbw_rup$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100,
               no_pers$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, 
               no_med$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100, site$se["down"]/mean(df_est$bweight < 2500 & df_est$bweight >= 1500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(llbw_ds$coeftable["down", "t value"], llbw_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(llbw_rup$coeftable["down", "t value"], llbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)



data_mlbw$Check = factor(data_mlbw$Check, c("Site Fixed Effects", "No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_mlbw$down = data_mlbw$Estimate
data_mlbw$up = data_mlbw$Upgradient
data_mlbw$d_lower = data_mlbw$down - 1.96 * data_mlbw$StdError
data_mlbw$d_upper = data_mlbw$down + 1.96 * data_mlbw$StdError
data_mlbw$pval_label = sprintf("%.5f", data_mlbw$pval)
data_mlbw$health_outcome = "mod. low birthweight"

lbw_slight = figure2_fun(data_mlbw, "Moderately", FALSE, FALSE, "Moderately", FALSE)



#moderatelty low birthweight
full = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


data_vlbw = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               drop_close$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, pre_2016$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               mlbw_ds$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100,  mlbw_rup$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100,
               no_pers$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               no_med$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, site$coefficients["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               drop_close$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, pre_2016$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               mlbw_ds$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               mlbw_rup$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100,
               no_pers$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, 
               no_med$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100, site$se["down"]/mean(df_est$bweight < 1500 & df_est$bweight >= 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mlbw_ds$coeftable["down", "t value"], mlbw_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mlbw_rup$coeftable["down", "t value"], mlbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)




data_vlbw$Check = factor(data_vlbw$Check, c("Site Fixed Effects", "No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_vlbw$down = data_vlbw$Estimate
data_vlbw$up = data_vlbw$Upgradient
data_vlbw$d_lower = data_vlbw$down - 1.96 * data_vlbw$StdError
data_vlbw$d_upper = data_vlbw$down + 1.96 * data_vlbw$StdError
data_vlbw$pval_label = sprintf("%.5f", data_vlbw$pval)
data_vlbw$health_outcome = "very low birthweight"

lbw_mod = figure2_fun(data_vlbw, "Very", FALSE, FALSE, "Very", FALSE)


#very low birthweight
full = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 1000) ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 1000) ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data_elbw = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df_est$bweight < 1000) * 100, 
               drop_close$coefficients["down"]/mean(df_est$bweight < 1000) * 100, pre_2016$coefficients["down"]/mean(df_est$bweight < 1000) * 100, 
               vlbw_ds$coefficients["down"]/mean(df_est$bweight < 1000) * 100,  vlbw_rup$coefficients["down"]/mean(df_est$bweight < 1000) * 100,
               no_pers$coefficients["down"]/mean(df_est$bweight < 1000) * 100, 
               no_med$coefficients["down"]/mean(df_est$bweight < 1000) * 100, site$coefficients["down"]/mean(df_est$bweight < 1000) * 100),
  StdError = c(full$se["down"]/mean(df_est$bweight < 1000) * 100, 
               drop_close$se["down"]/mean(df_est$bweight < 1000) * 100, pre_2016$se["down"]/mean(df_est$bweight < 1000) * 100, 
               vlbw_ds$se["down"]/mean(df_est$bweight < 1000) * 100, 
               vlbw_rup$se["down"]/mean(df_est$bweight < 1000) * 100,
               no_pers$se["down"]/mean(df_est$bweight < 1000) * 100, 
               no_med$se["down"]/mean(df_est$bweight < 1000) * 100, site$se["down"]/mean(df_est$bweight < 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vlbw_ds$coeftable["down", "t value"], vlbw_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vlbw_rup$coeftable["down", "t value"], vlbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)




data_elbw$Check = factor(data_elbw$Check, c("Site Fixed Effects", "No Medical Controls", 
                                            "No Demographics", "Relax Upgradient Def'n", 
                                            "Drop Border Sites", 
                                            "Drop After 2015",
                                            "Drop within 1km", "Baseline"))


data_elbw$down = data_elbw$Estimate
data_elbw$up = data_elbw$Upgradient
data_elbw$d_lower = data_elbw$down - 1.96 * data_elbw$StdError
data_elbw$d_upper = data_elbw$down + 1.96 * data_elbw$StdError
data_elbw$pval_label = sprintf("%.5f", data_elbw$pval)
data_elbw$health_outcome = "extremely low birthweight"

lbw_very = figure2_fun(data_elbw, "Extremely", TRUE, FALSE, "Extremely", FALSE)


lbw = lbw_all/lbw_very

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

ggsave("Figures Revision/figure2_simple.png", fig2, width = 17000, height = 5000, units = "px", limitsize = F)


fig2_data = rbind(data_pre, data_mpre, data_vpre, data_epre, 
                  data_lbw, data_mlbw, data_vlbw, data_elbw)
fwrite(fig2_data, "Figures Revision/Data/figure2_data_simple.csv")