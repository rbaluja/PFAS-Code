#robustness figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2_mortfun.R")
#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust.RData"))

full = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

site  = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(death ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(death ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"], 
               drop_close$coefficients["down"], pre_2016$coefficients["down"], 
               mort_ds$coefficients["down"],  mort_rup$coefficients["down"],
               no_pers$coefficients["down"], 
               no_med$coefficients["down"], site$coefficients["down"]),
  StdError = c(full$se["down"], 
               drop_close$se["down"], pre_2016$se["down"], 
               mort_ds$se["down"], 
               mort_rup$se["down"],
               no_pers$se["down"], 
               no_med$se["down"], site$se["down"]),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mort_ds$coeftable["down", "t value"], mort_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mort_rup$coeftable["down", "t value"], mort_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)

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
data$health_outcome = "infant mortality"

mort_f2 = figure2_still_fun(data, "Infant Mortality", TRUE, TRUE, "Infant Mortality", FALSE)
mort_f2 = mort_f2 + ggtitle("Infant Mortality") + theme_void() + theme(plot.title = element_text(hjust = -1.5, size = 70, face = "bold"))


ggsave("Figures Revision/figure2_mort_levels.png", mort_f2, width = 10000, height = 3500, units = "px", limitsize = F)
fwrite(data, "Figures Revision/Data/figure2_mortality_levels.csv")


#counts
data_c = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"] * 100000, 
               drop_close$coefficients["down"] * 100000, pre_2016$coefficients["down"] * 100000, 
               mort_ds$coefficients["down"] * 100000,  mort_rup$coefficients["down"] * 100000,
               no_pers$coefficients["down"] * 100000, 
               no_med$coefficients["down"] * 100000, site$coefficients["down"] * 100000),
  StdError = c(full$se["down"] * 100000, 
               drop_close$se["down"] * 100000, pre_2016$se["down"] * 100000, 
               mort_ds$se["down"] * 100000, 
               mort_rup$se["down"] * 100000,
               no_pers$se["down"] * 100000, 
               no_med$se["down"] * 100000, site$se["down"] * 100000),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mort_ds$coeftable["down", "t value"], mort_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(mort_rup$coeftable["down", "t value"], mort_rup$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_pers$coeftable["down", "t value"], no_pers$coeftable["down", "Pr(>|t|)"]), 
           one_sp(no_med$coeftable["down", "t value"], no_med$coeftable["down", "Pr(>|t|)"]), 
           one_sp(site$coeftable["down", "t value"], site$coeftable["down", "Pr(>|t|)"])
  )
)

data_c$Check = factor(data_c$Check, c("Site Fixed Effects", "No Medical Controls", 
                                  "No Demographics", "Relax Upgradient Def'n", 
                                  "Drop Border Sites", 
                                  "Drop After 2015",
                                  "Drop within 1km", "Baseline"))


data_c$down = data_c$Estimate
data_c$up = data_c$Upgradient
data_c$d_lower = data_c$down - 1.96 * data_c$StdError
data_c$d_upper = data_c$down + 1.96 * data_c$StdError
data_c$pval_label = sprintf("%.5f", data_c$pval)
data_c$health_outcome = "infant mortality"

mort_f2_c = figure2_still_fun_count(data_c, "Infant Mortality", TRUE, TRUE, "Infant Mortality", FALSE)
mort_f2_c = mort_f2_c + ggtitle("Infant Mortality") + theme_void() + theme(plot.title = element_text(hjust = -1.5, size = 70, face = "bold"))


ggsave("Figures Revision/figure2_mort_counts.png", mort_f2_c, width = 10000, height = 3500, units = "px", limitsize = F)
fwrite(data_c, "Figures Revision/Data/figure2_mortality_counts.csv")


