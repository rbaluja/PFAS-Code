#robustness figure
source("PFAS-Code/PR/Figures/figure2_stillfun.R")
#function for one sided pvalue (upper)
one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness.RData"))
load(modify_path("Data_Verify/Robustness/side_robustness.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust.RData"))

full = fixest::feols(stillbrn ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid
                     |county + year^month + birth_race_dsc_1, data = df[df$chld_dead_live != 9, ], warn = F, notes = F, cluster = c("site", "year^month"))

site  = fixest::feols(stillbrn ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |site + county + year^month + birth_race_dsc_1, data = df[df$chld_dead_live != 9, ], warn = F, notes = F, cluster = c("site", "year^month"))

no_fe = fixest::feols(stillbrn ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid
                      |birth_race_dsc_1, data = df[df$chld_dead_live != 9, ], warn = F, notes = F, cluster = c("site", "year^month"))

drop_close = fixest::feols(stillbrn ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist > 1000 & df$chld_dead_live != 9), ], warn = F, notes = F, cluster = c("site", "year^month"))

pre_2016 = fixest::feols(stillbrn ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
                         |county + year^month + birth_race_dsc_1, data = df[which(df$year < 2016 & df$chld_dead_live != 9), ], warn = F, notes = F, cluster = c("site", "year^month"))


no_pers = fixest::feols(stillbrn ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
                        |county + year^month + birth_race_dsc_1, data = df[df$chld_dead_live != 9, ], warn = F, notes = F, cluster = c("site", "year^month"))


no_med = fixest::feols(stillbrn ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
                       |county + year^month + birth_race_dsc_1, data = df[df$chld_dead_live != 9, ], warn = F, notes = F, cluster = c("site", "year^month"))



data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "Relax Upgradient Def'n", "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               drop_close$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, pre_2016$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               stillbrn_ds$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100,  stillbrn_rup$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100,
               no_pers$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               no_med$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, site$coefficients["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100),
  StdError = c(full$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               drop_close$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, pre_2016$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               stillbrn_ds$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               stillbrn_rup$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100,
               no_pers$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               no_med$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, site$se["down"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100),
  pval = c(one_sp(full$coeftable["down", "t value"], full$coeftable["down", "Pr(>|t|)"]), 
           one_sp(drop_close$coeftable["down", "t value"], drop_close$coeftable["down", "Pr(>|t|)"]), 
           one_sp(pre_2016$coeftable["down", "t value"], pre_2016$coeftable["down", "Pr(>|t|)"]), 
           one_sp(stillbrn_ds$coeftable["down", "t value"], stillbrn_ds$coeftable["down", "Pr(>|t|)"]), 
           one_sp(stillbrn_rup$coeftable["down", "t value"], stillbrn_rup$coeftable["down", "Pr(>|t|)"]), 
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

stillbrn_f2 = figure2_still_fun(data, "Stillbirth", TRUE, TRUE, "Stillbirth", FALSE)
stillbrn_f2 = stillbrn_f2 + ggtitle("Stillbirth") + theme_void() + theme(plot.title = element_text(hjust = -0.75, size = 70, face = "bold"))


ggsave(modify_path3("Figures/figure2_stillbrn.png"), stillbrn_f2, width = 10000, height = 3500, units = "px", limitsize = F)


#upgradient
data = data.frame(
  Category = c("Baseline", 
               "Sample", "Sample", "Sample", 
               "Controls", "Controls", "Controls"),
  Check = c("Baseline", "Drop within 1km", "Drop After 2015", 
            "Drop Border Sites",
            "No Demographics", "No Medical Controls", "Site Fixed Effects"),
  Estimate = c(full$coefficients["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               drop_close$coefficients["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, pre_2016$coefficients["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               stillbrn_ds$coefficients["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100,  
               no_pers$coefficients["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               no_med$coefficients["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, site$coefficients["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100),
  StdError = c(full$se["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               drop_close$se["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, pre_2016$se["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               stillbrn_ds$se["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               no_pers$se["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, 
               no_med$se["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100, site$se["updown"]/mean(df[df$chld_dead_live != 9, ]$stillbrn) * 100),
  pval = c(1 - one_sp(full$coeftable["updown", "t value"], full$coeftable["updown", "Pr(>|t|)"]), 
           1 - one_sp(drop_close$coeftable["updown", "t value"], drop_close$coeftable["updown", "Pr(>|t|)"]), 
           1 - one_sp(pre_2016$coeftable["updown", "t value"], pre_2016$coeftable["updown", "Pr(>|t|)"]), 
           1 - one_sp(stillbrn_ds$coeftable["updown", "t value"], stillbrn_ds$coeftable["updown", "Pr(>|t|)"]), 
           1 - one_sp(no_pers$coeftable["updown", "t value"], no_pers$coeftable["updown", "Pr(>|t|)"]), 
           1 - one_sp(no_med$coeftable["updown", "t value"], no_med$coeftable["updown", "Pr(>|t|)"]), 
           1 - one_sp(site$coeftable["updown", "t value"], site$coeftable["updown", "Pr(>|t|)"])
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

stillbrn_f2up = figure2_still_fun(data, "Stillbirth", TRUE, TRUE, "Stillbirth", FALSE)
stillbrn_f2up = stillbrn_f2up + ggtitle("Stillbirth") + theme_void() + theme(plot.title = element_text(hjust = -0.75, size = 70, face = "bold"))


ggsave(modify_path3("Figures/figure2_stillbrn_up.png"), stillbrn_f2up, width = 10000, height = 3500, units = "px", limitsize = F)
