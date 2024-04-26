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

full = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))

year_county = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5 + fa_resid
                            |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))

year_countym = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                               m_height + tri5 + fa_resid
                             |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("county", "year^month"))

site  = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000 & df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016 & df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_yc = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             private_insurance  + nbr_cgrtt +
                             pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5
                           |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))


no_med_yc = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                            m_age + m_married  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            + tri5
                          |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("site", "year^month"))

no_pers_ymc = fixest::feols(I(bweight < 2500) ~ updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                              private_insurance  + nbr_cgrtt +
                              pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                              mthr_wgt_dlv +mthr_pre_preg_wgt + 
                              m_height + tri5
                            |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("county", "year^month"))


no_med_ymc = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             + tri5
                           |county + year^month + birth_race_dsc_1, data = df[df$gestation >= 37, ], warn = F, notes = F, cluster = c("county", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Cont. Sites w/in 5km of State Border",
            "Relaxed Upgradient Definition", "No Demographics", "No Medical Controls", "Contaminated Site Fixed Effect"),
  Estimate = c(full$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, 
               drop_close$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, pre_2016$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, 
               lbw_ft_ds$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100,  lbw_ft_rup$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100,
               no_pers$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, 
               no_med$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, site$coefficients["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100),
  StdError = c(full$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, 
               drop_close$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, pre_2016$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, 
               lbw_ft_ds$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, 
               lbw_ft_rup$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100,
               no_pers$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, 
               no_med$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100, site$se["down"]/mean(df[df$gestation >= 37, ]$bweight < 2500) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lbw_ft_ds$coeftable["down", "t value"], lbw_ft_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(lbw_ft_rup$coeftable["down", "t value"], lbw_ft_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)
data = data %>% 
  dplyr::filter(Check != "No Downgradient Homes")

data$Check = factor(data$Check, c("Contaminated Site Fixed Effect", "No Medical Controls", 
                                  "No Demographics", "Relaxed Upgradient Definition", 
                                  "Drop Cont. Sites w/in 5km of State Border", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = data$down - 1.96 * data$StdError
data$d_upper = data$down + 1.96 * data$StdError
data$pval_label = sprintf("%.5f", data$pval)

lbw_ft_f2 = figure2_fun(data, "Any Low-Birthweight (Full-Term)", TRUE, TRUE, "Any Low-Birthweight (Full-Term)", TRUE)

ggsave(modify_path3("Figures/figure2_lbw_ft.png"), lbw_ft_f2, scale = 5, limitsize = F)
