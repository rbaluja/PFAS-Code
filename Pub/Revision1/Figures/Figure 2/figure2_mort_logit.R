#robustness figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2_mortfun.R")
#function for one sided pvalue (upper)
one_sp_logit = function(expb, sd){
  return(1 - pnorm(expb, mean = 1, sd = sd))
}
#preterm
load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness_logit.RData"))
load(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness_logit_sd.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust_logit.RData"))
load(modify_path("Data_Verify/Robustness/relaxed_up_robust_logit_sd.RData"))
df$ym = paste0(df$year, "-", df$month)

full = glm(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
             mthr_wgt_dlv +mthr_pre_preg_wgt + 
             m_height + tri5 +fa_resid + wind_exposure + 
             factor(birth_race_dsc_1), data = df, family = "binomial")

drop_close = glm(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid
                 + factor(birth_race_dsc_1), data = df[which(df$dist > 1000), ], family = "binomial")

pre_2016 = glm(death ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height + tri5 + fa_resid
               + factor(birth_race_dsc_1), data = df[which(df$year < 2016), ], family = "binomial")


no_pers = glm(death ~ down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                          private_insurance  + nbr_cgrtt +
                          pm25 + temp +med_inc  + well_elev + resid_elev + csite_dist +
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5
              + factor(birth_race_dsc_1), data = df, family = "binomial")


no_med = glm(death ~  down + updown +  I(pfas/10^3) + dist  + n_sites + wind_exposure +
                         m_age + m_married  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         + tri5
             + factor(birth_race_dsc_1), data = df, family = "binomial")



data = data.frame(
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
               exp(mort_rup$coefficients["down"]),
               exp(no_pers$coefficients["down"]), 
               exp(no_med$coefficients["down"])),
  StdError = c(sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]), 
               sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]) ,
               NA, 
               sd_mort_rup ,
               sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) , 
               sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])),
  pval = c(one_sp_logit(exp(full$coefficients["down"]), sqrt(vcovCL(full, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"])), 
           one_sp_logit(exp(drop_close$coefficients["down"]), sqrt(vcovCL(drop_close, cluster = ~ df[which(df$dist > 1000), ]$site + df[which(df$dist > 1000), ]$ym, type = "HC0")["down", "down"]) ), 
           one_sp_logit(exp(pre_2016$coefficients["down"]), sqrt(vcovCL(pre_2016, cluster = ~ df[which(df$year < 2016), ]$site + df[which(df$year < 2016), ]$ym, type = "HC0")["down", "down"]) ), 
           NA, 
           one_sp_logit(exp(mort_rup$coefficients["down"]), sd_mort_rup ), 
           one_sp_logit(exp(no_pers$coefficients["down"]), sqrt(vcovCL(no_pers, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]) ), 
           one_sp_logit(exp(no_med$coefficients["down"]), sqrt(vcovCL(no_med, cluster = ~ df$site + df$ym, type = "HC0")["down", "down"]))
  )
)


data$Check = factor(data$Check, c("No Medical Controls", 
                                          "No Demographics", "Relax Upgradient Def'n", 
                                          "Drop Border Sites", 
                                          "Drop After 2015",
                                          "Drop within 1km", "Baseline"))


data$down = data$Estimate
data$up = data$Upgradient
data$d_lower = exp(log(data$down) - 1.96 * data$StdError)
data$d_upper = exp(log(data$down) + 1.96 * data$StdError)
data$pval_label = sprintf("%.5f", data$pval)
data$health_outcome = "infant mortality"

mort_f2 = figure2_still_fun_logit(data, "Infant Mortality", TRUE, TRUE, "Infant Mortality", FALSE)
mort_f2 = mort_f2 + ggtitle("Infant Mortality") + theme_void() + theme(plot.title = element_text(hjust = -1.5, size = 70, face = "bold"))


ggsave("Figures Revision/figure2_mort_logit.png", mort_f2, width = 10000, height = 3500, units = "px", limitsize = F)
fwrite(data, "Figures Revision/Data/figure2_mortality_logit.csv")
