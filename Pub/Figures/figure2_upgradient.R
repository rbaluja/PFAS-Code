#robustness figure
source("PFAS-Code/PR/Figures/figure2_upgradient_fn.R")
#function for one sided pvalue (upper)
one_sp_up = function(tval, pval){
  if (tval > 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}
#preterm
load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness.RData"))
load(modify_path("Data_Verify/Robustness/side_robustness.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust.RData"))

full = fixest::feols(preterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

site  = fixest::feols(preterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(preterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(preterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(preterm ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(preterm ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(preterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
             "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$gestation < 37) * 100, 
               drop_close$coefficients["updown"]/mean(df$gestation < 37) * 100, pre_2016$coefficients["updown"]/mean(df$gestation < 37) * 100, pr_rside$coefficients["updown"]/mean(df$gestation < 37) * 100,
               p_all_ds$coefficients["updown"]/mean(df$gestation < 37) * 100, 
               no_pers$coefficients["updown"]/mean(df$gestation < 37) * 100, 
               no_med$coefficients["updown"]/mean(df$gestation < 37) * 100, site$coefficients["updown"]/mean(df$gestation < 37) * 100),
  StdError = c(full$se["updown"]/mean(df$gestation < 37) * 100, 
               drop_close$se["updown"]/mean(df$gestation < 37) * 100, pre_2016$se["updown"]/mean(df$gestation < 37) * 100, pr_rside$se["updown"]/mean(df$gestation < 37) * 100,
               p_all_ds$se["updown"]/mean(df$gestation < 37) * 100, 
               no_pers$se["updown"]/mean(df$gestation < 37) * 100, 
               no_med$se["updown"]/mean(df$gestation < 37) * 100, site$se["updown"]/mean(df$gestation < 37) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pr_rside$coeftable["updown", "t value"], pr_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(p_all_ds$coeftable["updown", "t value"], p_all_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)
data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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
full = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


site  = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(gestation < 37 & gestation >= 32) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(gestation < 37 & gestation >= 32) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               drop_close$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, pre_2016$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, lpr_rside$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,
               lp_ds$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,  
               no_pers$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               no_med$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, site$coefficients["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100),
  StdError = c(full$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               drop_close$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, pre_2016$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, lpr_rside$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100,
               lp_ds$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               no_pers$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, 
               no_med$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100, site$se["updown"]/mean(df$gestation < 37 & df$gestation >= 32) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(lpr_rside$coeftable["updown", "t value"], lpr_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(lp_ds$coeftable["updown", "t value"], lp_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)
data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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
full = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(gestation < 32 & gestation >= 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(gestation < 32 & gestation >= 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(gestation < 32 & gestation >= 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(gestation < 32 & gestation >= 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               drop_close$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, pre_2016$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, mpr_rside$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100,
               mp_ds$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               no_pers$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               no_med$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, site$coefficients["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100),
  StdError = c(full$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               drop_close$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, pre_2016$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, mpr_rside$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100,
               mp_ds$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               no_pers$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, 
               no_med$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100, site$se["updown"]/mean(df$gestation < 32 & df$gestation >= 28) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(mpr_rside$coeftable["updown", "t value"], mpr_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(mp_ds$coeftable["updown", "t value"], mp_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)

data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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
full = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(gestation < 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(gestation < 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(gestation < 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(gestation < 28) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$gestation < 28) * 100, 
               drop_close$coefficients["updown"]/mean(df$gestation < 28) * 100, pre_2016$coefficients["updown"]/mean(df$gestation < 28) * 100, vpr_rside$coefficients["updown"]/mean(df$gestation < 28) * 100,
               vp_ds$coefficients["updown"]/mean(df$gestation < 28) * 100,
               no_pers$coefficients["updown"]/mean(df$gestation < 28) * 100, 
               no_med$coefficients["updown"]/mean(df$gestation < 28) * 100, site$coefficients["updown"]/mean(df$gestation < 28) * 100),
  StdError = c(full$se["updown"]/mean(df$gestation < 28) * 100, 
               drop_close$se["updown"]/mean(df$gestation < 28) * 100, pre_2016$se["updown"]/mean(df$gestation < 28) * 100, vpr_rside$se["updown"]/mean(df$gestation < 28) * 100,
               vp_ds$se["updown"]/mean(df$gestation < 28) * 100, 
               no_pers$se["updown"]/mean(df$gestation < 28) * 100, 
               no_med$se["updown"]/mean(df$gestation < 28) * 100, site$se["updown"]/mean(df$gestation < 28) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(vpr_rside$coeftable["updown", "t value"], vpr_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(vp_ds$coeftable["updown", "t value"], vp_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)

data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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
full = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$bweight < 2500) * 100, 
               drop_close$coefficients["updown"]/mean(df$bweight < 2500) * 100, pre_2016$coefficients["updown"]/mean(df$bweight < 2500) * 100, lbw_rside$coefficients["updown"]/mean(df$bweight < 2500) * 100,
               lbw_all_ds$coefficients["updown"]/mean(df$bweight < 2500) * 100,  
               no_pers$coefficients["updown"]/mean(df$bweight < 2500) * 100, 
               no_med$coefficients["updown"]/mean(df$bweight < 2500) * 100, site$coefficients["updown"]/mean(df$bweight < 2500) * 100),
  StdError = c(full$se["updown"]/mean(df$bweight < 2500) * 100, 
               drop_close$se["updown"]/mean(df$bweight < 2500) * 100, pre_2016$se["updown"]/mean(df$bweight < 2500) * 100, lbw_rside$se["updown"]/mean(df$bweight < 2500) * 100,
               lbw_all_ds$se["updown"]/mean(df$bweight < 2500) * 100, 
               no_pers$se["updown"]/mean(df$bweight < 2500) * 100, 
               no_med$se["updown"]/mean(df$bweight < 2500) * 100, site$se["updown"]/mean(df$bweight < 2500) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(lbw_rside$coeftable["updown", "t value"], lbw_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(lbw_all_ds$coeftable["updown", "t value"], lbw_all_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)
data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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
full = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               drop_close$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, pre_2016$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, llbw_rside$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100,
               llbw_ds$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               no_pers$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               no_med$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, site$coefficients["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100),
  StdError = c(full$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               drop_close$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, pre_2016$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, llbw_rside$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100,
               llbw_ds$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               no_pers$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, 
               no_med$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100, site$se["updown"]/mean(df$bweight < 2500 & df$bweight >= 1500) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(llbw_rside$coeftable["updown", "t value"], llbw_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(llbw_ds$coeftable["updown", "t value"], llbw_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)

data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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
full = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               drop_close$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, pre_2016$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, mlbw_rside$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,
               mlbw_ds$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,  
               no_pers$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               no_med$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, site$coefficients["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100),
  StdError = c(full$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               drop_close$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, pre_2016$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, mlbw_rside$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100,
               mlbw_ds$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               no_pers$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, 
               no_med$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100, site$se["updown"]/mean(df$bweight < 1500 & df$bweight >= 1000) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(mlbw_rside$coeftable["updown", "t value"], mlbw_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(mlbw_ds$coeftable["updown", "t value"], mlbw_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)


data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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
full = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 1000) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("county", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", "No Downgradient Homes", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df$bweight < 1000) * 100, 
               drop_close$coefficients["updown"]/mean(df$bweight < 1000) * 100, pre_2016$coefficients["updown"]/mean(df$bweight < 1000) * 100, vlbw_rside$coefficients["updown"]/mean(df$bweight < 1000) * 100,
               vlbw_ds$coefficients["updown"]/mean(df$bweight < 1000) * 100,  
               no_pers$coefficients["updown"]/mean(df$bweight < 1000) * 100, 
               no_med$coefficients["updown"]/mean(df$bweight < 1000) * 100, site$coefficients["updown"]/mean(df$bweight < 1000) * 100),
  StdError = c(full$se["updown"]/mean(df$bweight < 1000) * 100, 
               drop_close$se["updown"]/mean(df$bweight < 1000) * 100, pre_2016$se["updown"]/mean(df$bweight < 1000) * 100, vlbw_rside$se["updown"]/mean(df$bweight < 1000) * 100,
               vlbw_ds$se["updown"]/mean(df$bweight < 1000) * 100, 
               no_pers$se["updown"]/mean(df$bweight < 1000) * 100, 
               no_med$se["updown"]/mean(df$bweight < 1000) * 100, site$se["updown"]/mean(df$bweight < 1000) * 100),
  pval = c(one_sp_up(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(vlbw_rside$coeftable["updown", "t value"], vlbw_rside$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(vlbw_ds$coeftable["updown", "t value"], vlbw_ds$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           one_sp_up(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
  )
)


data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", 
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

fig2_up = (title/main_fig)  + plot_layout(heights = c(0.5, 50))
# /
#   lplot

ggsave(modify_path3("Figures/figure2_upgradient.png"), fig2_up, width = 17000, height = 10000, units = "px", limitsize = F)