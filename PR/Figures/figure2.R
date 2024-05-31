#robustness figure
source("PFAS-Code/PR/Figures/figure2_function.R")
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
load(modify_path("Data_Verify/Robustness/side_robustness.RData"))
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



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 37) * 100, 
               drop_close$coefficients["down"]/mean(df$gestation < 37) * 100, pre_2016$coefficients["down"]/mean(df$gestation < 37) * 100, pr_rside$coefficients["down"]/mean(df$gestation < 37) * 100,
               p_all_ds$coefficients["down"]/mean(df$gestation < 37) * 100,  pr_rup$coefficients["down"]/mean(df$gestation < 37) * 100,
               no_pers$coefficients["down"]/mean(df$gestation < 37) * 100, 
               no_med$coefficients["down"]/mean(df$gestation < 37) * 100, site$coefficients["down"]/mean(df$gestation < 37) * 100),
  StdError = c(full$se["down"]/mean(df$gestation < 37) * 100, 
               drop_close$se["down"]/mean(df$gestation < 37) * 100, pre_2016$se["down"]/mean(df$gestation < 37) * 100, pr_rside$se["down"]/mean(df$gestation < 37) * 100,
               p_all_ds$se["down"]/mean(df$gestation < 37) * 100, 
               pr_rup$se["down"]/mean(df$gestation < 37) * 100,
               no_pers$se["down"]/mean(df$gestation < 37) * 100, 
               no_med$se["down"]/mean(df$gestation < 37) * 100, site$se["down"]/mean(df$gestation < 37) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pr_rside$coeftable["down", "t value"], pr_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(p_all_ds$coeftable["down", "t value"], p_all_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pr_rup$coeftable["down", "t value"], pr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)
data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

pre_any = figure2_fun(data, "Any", FALSE, TRUE, "Any", TRUE)


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

data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               drop_close$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, pre_2016$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, lpr_rside$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,
               lp_ds$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,  lpr_rup$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,
               no_pers$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               no_med$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, site$coefficients["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100),
  StdError = c(full$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               drop_close$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, pre_2016$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, lpr_rside$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,
               lp_ds$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               lpr_rup$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,
               no_pers$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               no_med$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, site$se["down"]/mean(df$gestation < 37 & df$gestation >= 32) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lpr_rside$coeftable["down", "t value"], lpr_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lp_ds$coeftable["down", "t value"], lp_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lpr_rup$coeftable["down", "t value"], lpr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)
data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))

data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

pre_late = figure2_fun(data, "Moderately", FALSE, FALSE, "Moderately", TRUE)


#moderately preterm
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

data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               drop_close$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, pre_2016$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, mpr_rside$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100,
               mp_ds$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100,  mpr_rup$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100,
               no_pers$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               no_med$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, site$coefficients["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100),
  StdError = c(full$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               drop_close$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, pre_2016$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, mpr_rside$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100,
               mp_ds$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               mpr_rup$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100,
               no_pers$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               no_med$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, site$se["down"]/mean(df$gestation < 32 & df$gestation >= 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mpr_rside$coeftable["down", "t value"], mpr_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mp_ds$coeftable["down", "t value"], mp_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mpr_rup$coeftable["down", "t value"], mpr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)

data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))



data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

pre_mod = figure2_fun(data, "Very", FALSE, FALSE, "Very", TRUE)



#very preterm
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



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$gestation < 28) * 100, 
               drop_close$coefficients["down"]/mean(df$gestation < 28) * 100, pre_2016$coefficients["down"]/mean(df$gestation < 28) * 100, vpr_rside$coefficients["down"]/mean(df$gestation < 28) * 100,
               vp_ds$coefficients["down"]/mean(df$gestation < 28) * 100,  vpr_rup$coefficients["down"]/mean(df$gestation < 28) * 100,
               no_pers$coefficients["down"]/mean(df$gestation < 28) * 100, 
               no_med$coefficients["down"]/mean(df$gestation < 28) * 100, site$coefficients["down"]/mean(df$gestation < 28) * 100),
  StdError = c(full$se["down"]/mean(df$gestation < 28) * 100, 
               drop_close$se["down"]/mean(df$gestation < 28) * 100, pre_2016$se["down"]/mean(df$gestation < 28) * 100, vpr_rside$se["down"]/mean(df$gestation < 28) * 100,
               vp_ds$se["down"]/mean(df$gestation < 28) * 100, 
               vpr_rup$se["down"]/mean(df$gestation < 28) * 100,
               no_pers$se["down"]/mean(df$gestation < 28) * 100, 
               no_med$se["down"]/mean(df$gestation < 28) * 100, site$se["down"]/mean(df$gestation < 28) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vpr_rside$coeftable["down", "t value"], vpr_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vp_ds$coeftable["down", "t value"], vp_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vpr_rup$coeftable["down", "t value"], vpr_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)

data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)


pre_very = figure2_fun(data, "Extremely", TRUE, FALSE, "Extremely", TRUE)

pre_fig = pre_any/pre_late/pre_mod/pre_very



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



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 2500) * 100, 
               drop_close$coefficients["down"]/mean(df$bweight < 2500) * 100, pre_2016$coefficients["down"]/mean(df$bweight < 2500) * 100, lbw_rside$coefficients["down"]/mean(df$bweight < 2500) * 100,
               lbw_all_ds$coefficients["down"]/mean(df$bweight < 2500) * 100,  lbw_rup$coefficients["down"]/mean(df$bweight < 2500) * 100,
               no_pers$coefficients["down"]/mean(df$bweight < 2500) * 100, 
               no_med$coefficients["down"]/mean(df$bweight < 2500) * 100, site$coefficients["down"]/mean(df$bweight < 2500) * 100),
  StdError = c(full$se["down"]/mean(df$bweight < 2500) * 100, 
               drop_close$se["down"]/mean(df$bweight < 2500) * 100, pre_2016$se["down"]/mean(df$bweight < 2500) * 100, lbw_rside$se["down"]/mean(df$bweight < 2500) * 100,
               lbw_all_ds$se["down"]/mean(df$bweight < 2500) * 100, 
               lbw_rup$se["down"]/mean(df$bweight < 2500) * 100,
               no_pers$se["down"]/mean(df$bweight < 2500) * 100, 
               no_med$se["down"]/mean(df$bweight < 2500) * 100, site$se["down"]/mean(df$bweight < 2500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lbw_rside$coeftable["down", "t value"], lbw_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lbw_all_ds$coeftable["down", "t value"], lbw_all_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lbw_rup$coeftable["down", "t value"], lbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)
data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

lbw_all = figure2_fun(data, "Any", FALSE, TRUE, "Any", FALSE)


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



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               drop_close$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, pre_2016$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, llbw_rside$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100,
               llbw_ds$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100,  llbw_rup$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100,
               no_pers$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               no_med$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, site$coefficients["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100),
  StdError = c(full$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               drop_close$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, pre_2016$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, llbw_rside$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100,
               llbw_ds$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               llbw_rup$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100,
               no_pers$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               no_med$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, site$se["down"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(llbw_rside$coeftable["down", "t value"], llbw_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(llbw_ds$coeftable["down", "t value"], llbw_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(llbw_rup$coeftable["down", "t value"], llbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)

data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

lbw_slight = figure2_fun(data, "Moderately", FALSE, FALSE, "Moderately", FALSE)



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


data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               drop_close$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, pre_2016$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, mlbw_rside$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,
               mlbw_ds$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,  mlbw_rup$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,
               no_pers$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               no_med$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, site$coefficients["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100),
  StdError = c(full$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               drop_close$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, pre_2016$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, mlbw_rside$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,
               mlbw_ds$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               mlbw_rup$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,
               no_pers$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               no_med$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, site$se["down"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mlbw_rside$coeftable["down", "t value"], mlbw_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mlbw_ds$coeftable["down", "t value"], mlbw_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mlbw_rup$coeftable["down", "t value"], mlbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)


data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

lbw_mod = figure2_fun(data, "Very", FALSE, FALSE, "Very", FALSE)


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



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", "Sample",
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df$bweight < 1000) * 100, 
               drop_close$coefficients["down"]/mean(df$bweight < 1000) * 100, pre_2016$coefficients["down"]/mean(df$bweight < 1000) * 100, vlbw_rside$coefficients["down"]/mean(df$bweight < 1000) * 100,
               vlbw_ds$coefficients["down"]/mean(df$bweight < 1000) * 100,  vlbw_rup$coefficients["down"]/mean(df$bweight < 1000) * 100,
               no_pers$coefficients["down"]/mean(df$bweight < 1000) * 100, 
               no_med$coefficients["down"]/mean(df$bweight < 1000) * 100, site$coefficients["down"]/mean(df$bweight < 1000) * 100),
  StdError = c(full$se["down"]/mean(df$bweight < 1000) * 100, 
               drop_close$se["down"]/mean(df$bweight < 1000) * 100, pre_2016$se["down"]/mean(df$bweight < 1000) * 100, vlbw_rside$se["down"]/mean(df$bweight < 1000) * 100,
               vlbw_ds$se["down"]/mean(df$bweight < 1000) * 100, 
               vlbw_rup$se["down"]/mean(df$bweight < 1000) * 100,
               no_pers$se["down"]/mean(df$bweight < 1000) * 100, 
               no_med$se["down"]/mean(df$bweight < 1000) * 100, site$se["down"]/mean(df$bweight < 1000) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vlbw_rside$coeftable["down", "t value"], vlbw_rside$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vlbw_ds$coeftable["down", "t value"], vlbw_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(vlbw_rup$coeftable["down", "t value"], vlbw_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)


data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

lbw_very = figure2_fun(data, "Extremely", TRUE, FALSE, "Extremely", FALSE)


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


# Combine your plots using patchwork syntax and apply the layout
ptplot = ggplot() +
  labs(title = "Preterm") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.9, size = 70, face = "bold"))
btplot = ggplot() +
  labs(title = "Low-Birthweight") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.73, size = 70, face = "bold"))

title = (ptplot | btplot)
main_fig = (pre_fig | lbw) + plot_layout(widths = c(1.5, 1))

fig2 = (title/main_fig)  + plot_layout(heights = c(0.5, 50))
# /
#   lplot

ggsave(modify_path3("Figures/figure2.png"), fig2, width = 17000, height = 10000, units = "px", limitsize = F)
