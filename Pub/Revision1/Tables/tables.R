#read in bootstrap functions
source("PFAS-Code/Pub/Tables/bs_functions.R")


#First set of tables, health outcomes on covariates without anything to do with exposure to PFAS
#sample used in estimation:
df$college = ifelse(df$max_educ >= 6, 1, 0)
df[which(df$n_prenatal == 99), ]$n_prenatal = NA
df$ind_prenatal = ifelse(df$n_prenatal >= 15 & !is.na(df$n_prenatal), 1, 0)
df$old = ifelse(df$m_age > 40, 1, 0)
df$young = ifelse(df$m_age < 20, 1, 0)
df$no_hs = ifelse(df$max_educ < 3, 1, 0)
df$group = as.factor(ifelse(df$down == 1, 1, ifelse(df$up == 1, 2, 3)))
df$med_hprice = df$med_hprice/10^4
df$med_inc = df$med_inc/10^3
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

#get number of unique water systems serving individuals in the estimation sample, for ref 1 comment 6
length(unique(df2$sys_id))
#number of fixed effects in the estimation sample, for ref 2 comment 2
length(unique(df2$county)) + length(unique(paste0(df2$year, "-", df2$month)))
#save estimation sample to UA Box Health
save(df2, file = paste0(natality_path, "[UA Box Health] birth_records_estimation_sample.RData"))

#This table gives the correlation between downgradient and the covariates (Table R2)
r1_down = list() 
r1_down[["Downgradient"]] = fixest::feols(down ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                               m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height+ med_hprice + med_inc + well_elev + resid_elev+ temp + pm25|county + year^month + birth_race_dsc_1, 
                                          data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(r1_down, 
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("m_age" = "Maternal Age", 
                                        "m_married" = "Married", 
                                        "private_insurance" = "Private Insurance", 
                                        "nbr_cgrtt" = "Number Cigarettes", 
                                        "m_educ" = "Maternal Education",
                                        "p_educ" = "Paternal Education",
                                        "mr_04" = "Pre-Pregancy Diabetes", 
                                        "mr_18" = "Gestational Diabetes", 
                                        "mr_08" = "Hypertension", 
                                        "mr_21" = "Previous C-Section", 
                                        "mr_26" = "Fertility Enhancing Drugs", 
                                        "mr_27" = "Invitro Fertilization", 
                                        "mthr_wgt_dlv" = "Mother's Weight at Delivery", 
                                        "mthr_pre_preg_wgt" = "Mother's Pre-Pregnancy Weight",
                                        "m_height" = "Mother's Height", 
                                        "med_hprice" = "Median Housing Price",
                                        "med_inc" = "Median Income", 
                                        "well_elev" = "Well Elevation", 
                                        "resid_elev" = "Residential Elevation", 
                                        "temp" = "Temperature", 
                                        "pm25" = "PM2.5"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/r1_down.tex")) 



#Next table: summary stats table with median income (Table R1)
df2s = df2 %>% 
  dplyr::select(`Maternal Age` = m_age, 
                `College` = college,
                `Less than High School` = no_hs,
                `Maternal Marital Status` = m_married, 
                `Maternal Tobacco Use` = cig, 
                White = white,
                `Median Housing Price` = med_hprice,
                `Median Income` = med_inc,
                group,
                `WIC` = wic, 
                `Private Insurance` = private_insurance, 
                `Months in Residence` = m_months_res, 
                `Younger than 20` = young, 
                `Older than 40` = old,
                `Prenatal Care Visits` = n_prenatal,
                `Pre-Pregancy Diabetes` = mr_04, 
                `Gestational Diabetes` = mr_18, 
                Hypertension = mr_08, 
                `Gestational Hypertension` = mr_23, 
                Eclampsia = mr_10
  )
df2s = as.data.frame(df2s)


datasummary_balance(~group, 
                    data = df2s, 
                    na.rm = T, 
                    fmt = modelsummary::fmt_significant(2, scientific = F, 
                                                        zero.print = T, drop0trailing = F, 
                                                        nsmall = 2), 
                    output = modify_path2("Tables/Revisions/table_s2.tex")) 



#Table: number of individuals with each missing covariate (Table R10)
#number of missing observations for each covariate
df2 = df[which(!is.na(df$dist) & df$dist <= meters), ]

mobs = data.frame(
  m_age = length(which(is.na(df2$m_age))),
  m_married = length(which(is.na(df2$m_married))),
  private_insurance = length(which(is.na(df2$private_insurance))),
  nbr_cgrtt = length(which(is.na(df2$nbr_cgrtt))),
  m_educ = length(which(is.na(df2$m_educ))),
  f_educ = length(which(is.na(df2$f_educ))),
  pm25 = length(which(is.na(df2$pm25))),
  temp = length(which(is.na(df2$temp))),
  p_manuf = length(which(is.na(df2$p_manuf))),
  n_hunits = length(which(is.na(df2$n_hunits))),
  med_hprice = length(which(is.na(df2$med_hprice))),
  well_elev = length(which(is.na(df2$well_elev))),
  resid_elev = length(which(is.na(df2$resid_elev))),
  wic = length(which(is.na(df2$wic))),
  mr_04 = length(which(is.na(df2$mr_04))),
  mr_18 = length(which(is.na(df2$mr_18))),
  mr_21 = length(which(is.na(df2$mr_21))),
  mr_26 = length(which(is.na(df2$mr_26))),
  mr_27 = length(which(is.na(df2$mr_27))),
  mr_08 = length(which(is.na(df2$mr_08))),
  mthr_wgt_dlv = length(which(is.na(df2$mthr_wgt_dlv))),
  mthr_pre_preg_wgt = length(which(is.na(df2$mthr_pre_preg_wgt))),
  m_height = length(which(is.na(df2$m_height))),
  tri5 = length(which(is.na(df2$tri5))),
  fa_resid = length(which(is.na(df2$fa_resid))),
  wind_exposure = length(which(is.na(df2$wind_exposure)))
)

transposed_mobs = t(mobs)
result = data.frame(
  Variable = rownames(transposed_mobs),
  `Number Missing` = transposed_mobs[, 1]
)
result$Variable = c(
  "Mother's Age",
  "Mother Married",
  "Private Insurance",
  "Number of Cigarettes",
  "Mother's Education",
  "Father's Education",
  "PM2.5",
  "Temperature",
  "Percent in Manufacturing",
  "Number of Housing Units",
  "Median Housing Price",
  "Well Elevation",
  "Residential Elevation",
  "WIC",
  "Pre-Pregnancy Diabetes",
  "Gestational Diabetes",
  "Previous C-Section",
  "Fertility Enhancing Drugs",
  "Invitro Fertilization",
  "Hypertension",
  "Mother's Weight at Delivery",
  "Mother's Pre-Pregnancy Weight",
  "Mother's Height",
  "Number of Nearby Toxic Release Sites",
  "Accumulated Surface Flow from PFAS Sites",
  "Wind Exposure"
)

# Create the LaTeX table using xtable
latex_table = xtable::xtable(result)

# Export to LaTeX file
print(latex_table, type = "latex", file = modify_path2("Tables/Revisions/nmiss_cov.tex"), include.rownames = FALSE)



#Table: Oster calcs with factor of 2 and 5 (Table R6)
#Oster with a maximal R2 of 2*empirical R2
oster_factor = 5
source("PFAS-Code/Pub/Revision1/Tables/oster_bound_coef.R")
sink(modify_path2("Tables/Revisions/oster5.tex"))
print("Mortality")
print(as.numeric(d_mort))
print("Preterm")
print(as.numeric(d_pre))
print("Moderately Preterm")
print(as.numeric(d_lpre))
print("Very Preterm")
print(as.numeric(d_mpre))
print("Extremely Preterm")
print(as.numeric(d_vpre))
print("Low Birthweight")
print(as.numeric(d_lbw))
print("Moderately Low Birthweight")
print(as.numeric(d_llbw))
print("Very Low Birthweight")
print(as.numeric(d_mlbw))
print("Extremely Low Birthweight")
print(as.numeric(d_vlbw))
sink() 

oster_factor = 2
source("PFAS-Code/Pub/Revision1/Tables/oster_bound_coef.R")
sink(modify_path2("Tables/Revisions/oster2.tex"))
print("Mortality")
print(as.numeric(d_mort))
print("Preterm")
print(as.numeric(d_pre))
print("Moderately Preterm")
print(as.numeric(d_lpre))
print("Very Preterm")
print(as.numeric(d_mpre))
print("Extremely Preterm")
print(as.numeric(d_vpre))
print("Low Birthweight")
print(as.numeric(d_lbw))
print("Moderately Low Birthweight")
print(as.numeric(d_llbw))
print("Very Low Birthweight")
print(as.numeric(d_mlbw))
print("Extremely Low Birthweight")
print(as.numeric(d_vlbw))
sink() 

oster_factor = 1.3
source("PFAS-Code/Pub/Revision1/Tables/oster_bound_coef.R")
sink(modify_path2("Tables/Revisions/oster13.tex"))
print("Mortality")
print(as.numeric(d_mort))
print("Preterm")
print(as.numeric(d_pre))
print("Moderately Preterm")
print(as.numeric(d_lpre))
print("Very Preterm")
print(as.numeric(d_mpre))
print("Extremely Preterm")
print(as.numeric(d_vpre))
print("Low Birthweight")
print(as.numeric(d_lbw))
print("Moderately Low Birthweight")
print(as.numeric(d_llbw))
print("Very Low Birthweight")
print(as.numeric(d_mlbw))
print("Extremely Low Birthweight")
print(as.numeric(d_vlbw))
sink() 


#Table: continuous health outcomes (Table R11)
load(modify_path("Data_Verify/Revision 1/RData/bootstrap_cont.RData"))
table1_cont_ges = list() 
table1_cont_ges[["Gestation"]] = fixest::feols(gestation ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height + tri5 +fa_resid + wind_exposure 
                                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_cont_ges[["Gestation: Preterm"]] = fixest::feols(gestation ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                          m_height + tri5 +fa_resid + wind_exposure 
                                                        |county + year^month + birth_race_dsc_1, data = df[which(df$gestation < 37), ], warn = F, notes = F, cluster = c("site", "year^month"))


table1_cont_ges[["Gestation IV"]]= fixest::feols(gestation ~ pred_pfas + asinh(pfas) + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table1_cont_ges[["Gestation IV: Preterm"]]= fixest::feols(gestation ~ pred_pfas + asinh(pfas) + 
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df[which(df$gestation < 37), ] )


modelsummary::modelsummary(table1_cont_ges, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F, nsmall = 2), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "pred_pfas" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/ges_cont.tex")) 

ges_sd = linear_bootstrap(boot_coefs, "gestation_all", table1_cont_ges[["Gestation IV"]])
ges_pre_sd = linear_bootstrap(boot_coefs, "gestation_pre", table1_cont_ges[["Gestation IV: Preterm"]])
save(ges_sd, ges_pre_sd, file = modify_path("Data_Verify/Revision 1/RData/ges_sd.RData"))
1 - pnorm(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"]/ges_sd)
1 - pnorm(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"]/ges_pre_sd)

table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] - 1.96 * ges_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] + 1.96 * ges_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] - 1.96 * ges_pre_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] + 1.96 * ges_pre_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100



table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] - 1.96 * ges_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] + 1.96 * ges_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] - 1.96 * ges_pre_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] + 1.96 * ges_pre_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)



table1_cont_bw = list() 
table1_cont_bw[["Birthweight"]] = fixest::feols(bweight ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 +fa_resid + wind_exposure 
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_cont_bw[["Birthweight: LBW"]] = fixest::feols(bweight ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                          m_height + tri5 +fa_resid + wind_exposure 
                                                        |county + year^month + birth_race_dsc_1, data = df[which(df$bweight < 2500), ], warn = F, notes = F, cluster = c("site", "year^month"))


table1_cont_bw[["Birthweight IV"]]= fixest::feols(bweight ~ pred_pfas + asinh(pfas) + 
                                                   n_sites + wind_exposure + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table1_cont_bw[["Birthweight IV: LBW"]]= fixest::feols(bweight ~ pred_pfas + asinh(pfas) + 
                                                            n_sites + wind_exposure + 
                                                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                            m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df[which(df$bweight < 2500), ] )


modelsummary::modelsummary(table1_cont_bw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F, nsmall = 2), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient",
                                        "pred_pfas" = "Predicted PFAS"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/bw_cont.tex")) 

bw_sd = linear_bootstrap(boot_coefs, "bweight_all", table1_cont_bw[["Birthweight IV"]])
bw_lbw_sd = linear_bootstrap(boot_coefs, "bweight_lbw", table1_cont_bw[["Birthweight IV: LBW"]])
save(bw_sd, bw_lbw_sd, file = modify_path("Data_Verify/Revision 1/RData/bweight_sd.RData"))
1 - pnorm(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"]/bw_sd)
1 - pnorm(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"]/bw_lbw_sd)


table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] - 1.96 * bw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] + 1.96 * bw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] - 1.96 * bw_lbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] + 1.96 * bw_lbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] - 1.96 * bw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] + 1.96 * bw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] - 1.96 * bw_lbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] + 1.96 * bw_lbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)



#Table: run analysis separately for individuals who have and have no lived in their residence for entire gestation
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 +fa_resid + wind_exposure 
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +fa_resid + wind_exposure 
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 +fa_resid + wind_exposure 
                                              |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient", 
                                        "down:I(m_months_res <= gestation)" = "Downgradient x Moved during Gestation",
                                        "updown:I(m_months_res <= gestation)" = "Upgradient x Moved during Gestation"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_ltr_ges.tex")) 

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +fa_resid + wind_exposure 
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 +fa_resid + wind_exposure 
                                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 +fa_resid + wind_exposure 
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 +fa_resid + wind_exposure 
                                                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +fa_resid + wind_exposure 
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient", 
                                        "down:I(m_months_res <= gestation)" = "Downgradient x Moved during Gestation",
                                        "updown:I(m_months_res <= gestation)" = "Upgradient x Moved during Gestation"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_ltr_ges.tex"))

mort4 = fixest::feols(death ~  (updown + down) * I(m_months_res <= gestation) +  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 +fa_resid + wind_exposure 
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(list(mort4), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient", 
                                        "down:I(m_months_res <= gestation)" = "Downgradient x Moved during Gestation",
                                        "updown:I(m_months_res <= gestation)" = "Upgradient x Moved during Gestation"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_ltr_ges.tex")) 



#Table: run everything as a logit (Table R12)
df$ym = paste0(df$year, "-", df$month)
table1_preterm = list() 

df$preterm = as.numeric(df$gestation < 37)
df$mpreterm = as.numeric(df$gestation < 37 & df$gestation >= 32)
df$vpreterm = as.numeric(df$gestation < 32 & df$gestation >= 28)
df$epreterm = as.numeric(df$gestation < 28)
table1_preterm[["All"]] = glm(preterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                m_height + tri5 +fa_resid + wind_exposure + factor(birth_race_dsc_1), data = df, family = "binomial")

table1_preterm[["Moderately"]] = glm(mpreterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 +fa_resid + wind_exposure + factor(birth_race_dsc_1), data = df, family = "binomial")

table1_preterm[["Very"]] = glm(vpreterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                 m_height + tri5 +fa_resid + wind_exposure + factor(birth_race_dsc_1), data = df, family = "binomial")

table1_preterm[["Extremely"]] = glm(epreterm ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height + tri5 +fa_resid + wind_exposure + factor(birth_race_dsc_1), data = df, family = "binomial")


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r2.tjur"), 
                           output = modify_path2("Tables/Revisions/preterm_logit.tex"), 
                           vcov = list(vcovCL(table1_preterm[["All"]], cluster = ~ df$site + df$ym, type = "HC0"), 
                                       vcovCL(table1_preterm[["Moderately"]], cluster = ~ df$site + df$ym, type = "HC0"),
                                       vcovCL(table1_preterm[["Very"]], cluster = ~ df$site + df$ym, type = "HC0"),
                                       vcovCL(table1_preterm[["Extremely"]], cluster = ~ df$site + df$ym, type = "HC0")), 
                           exponentiate = TRUE, 
                           statistic = "conf.int", 
                           conf_level = 0.95) 

#low birthweight
table1_lbw = list() 

df$lbw = as.numeric(df$bweight < 2500)
df$mlbw = as.numeric(df$bweight < 2500 & df$bweight >= 1500)
df$vlbw = as.numeric(df$bweight < 1500 & df$bweight >= 1000)
df$elbw = as.numeric(df$bweight < 1000)

table1_lbw[["Low Birthweight all "]] = glm(lbw ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height + tri5 +fa_resid + wind_exposure + 
                                             factor(birth_race_dsc_1), data = df, family = "binomial")

table1_lbw[["Low Birthweight among full term "]] = glm(lbw ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                         m_height + tri5 +fa_resid + wind_exposure + 
                                                         factor(birth_race_dsc_1), data = df[which(df$gestation >= 37), ], family = "binomial")


table1_lbw[["Low Birthweight "]] = glm(mlbw ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 +fa_resid + wind_exposure + 
                                         factor(birth_race_dsc_1), data = df, family = "binomial")

table1_lbw[["Moderately Low Birthweight"]] = glm(vlbw ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 +fa_resid + wind_exposure + 
                                                   factor(birth_race_dsc_1), data = df, family = "binomial")

table1_lbw[["Very Low Birthweight"]] = glm(elbw ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height + tri5 +fa_resid + wind_exposure + 
                                             factor(birth_race_dsc_1), data = df, family = "binomial")


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_logit.tex"), 
                           vcov = list(vcovCL(table1_lbw[["Low Birthweight all "]], cluster = ~ df$site + df$ym, type = "HC0"), 
                                       vcovCL(table1_lbw[["Low Birthweight among full term "]], cluster = ~ df[which(df$gestation >= 37), ]$site + df[which(df$gestation >= 37), ]$ym, type = "HC0"),
                                       vcovCL(table1_lbw[["Low Birthweight "]], cluster = ~ df$site + df$ym, type = "HC0"),
                                       vcovCL(table1_lbw[["Moderately Low Birthweight"]], cluster = ~ df$site + df$ym, type = "HC0"), 
                                       vcovCL(table1_lbw[["Very Low Birthweight"]], cluster = ~ df$site + df$ym, type = "HC0")), 
                           exponentiate = TRUE, 
                           statistic = "conf.int", 
                           conf_level = 0.95) 

mort4 = glm(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 +fa_resid + wind_exposure + 
             factor(birth_race_dsc_1), data = df, family = "binomial")


modelsummary::modelsummary(list(mort4), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_logit.tex"),
                           vcov = list(vcovCL(mort4, cluster = ~ df$site + df$ym, type = "HC0")), 
                           exponentiate = TRUE, 
                           statistic = "conf.int", 
                           conf_level = 0.95) 



# Table: Predicted PFAS contamination across treatment groups (Table R14)
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
df2$group = ifelse(df2$down == 1, "down",
                   ifelse(df2$up == 1, "up", "side"))

models = list()
models[["(1)"]] = fixest::feols(sinh(pred_pfas) ~ down + updown, data = df2)
models[["(2)"]] = fixest::feols(sinh(pred_pfas) ~ down + updown|county, data = df2)
models[["(3)"]] = fixest::feols(sinh(pred_pfas) ~ down + updown|site, data = df2)

modelsummary::modelsummary(models,
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/pred_pfas_group_rout.tex")) 


#mortality impact from median in US to median contaminated in NH
mort_iv = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                n_sites + wind_exposure + 
                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)
load(modify_path("Data_Verify/RData/mort_sd.RData"))

mort_iv$coefficients["pred_pfas"]/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(mort_iv$coefficients["pred_pfas"] - 1.96 * mort_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
(mort_iv$coefficients["pred_pfas"] + 1.96 * mort_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)



#check correlation between https://doi.org/10.1126/science.ado6638 and our measure (Table R15)
xgb_out = terra::rast("Data Revisions/National/xgboost_rob/Outputs/prob_public.asc")
#based on the extent, it looks like the projection is EPSG:5070
terra::crs(xgb_out) = "EPSG:5070"
#change wells to lat long
w_ll = wells %>% 
  as_tibble() %>% 
  dplyr::select(!geometry) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(st_crs(xgb_out))

#get the predicted exposure prob from the xgboost model for each point in w_ll
w_ll$xgb_prob = terra::extract(xgb_out, w_ll)$prob_public

nh_verd = list()
nh_verd[["down1"]] = lm(down ~ xgb_prob, data = w_ll)
nh_verd[["down2"]] = lm(down ~ I(xgb_prob > 0.315), data = w_ll)
nh_verd[["down3"]] = lm(down ~ I(xgb_prob > 0.5), data = w_ll)
nh_verd[["down4"]] = lm(down ~ I(xgb_prob > 0.75), data = w_ll)

modelsummary::modelsummary(nh_verd,
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("xgb_prob" = "Prob. of PFAS at Well", 
                                        "I(xgb_prob > 0.315)TRUE" = "I(Prob of PFAS > 0.315)", 
                                        "I(xgb_prob > 0.5)TRUE" = "I(Prob of PFAS > 0.5)", 
                                        "I(xgb_prob > 0.75)TRUE" = "I(Prob of PFAS > 0.75)"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/nh_verd_rout.tex")) 

nh_veru = list()
nh_veru[["up1"]] = lm(up ~ xgb_prob, data = w_ll)
nh_veru[["up2"]] = lm(up ~ I(xgb_prob > 0.315), data = w_ll)
nh_veru[["up3"]] = lm(up ~ I(xgb_prob > 0.5), data = w_ll)
nh_veru[["up4"]] = lm(up ~ I(xgb_prob > 0.75), data = w_ll)

modelsummary::modelsummary(nh_veru,
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("xgb_prob" = "Prob. of PFAS at Well", 
                                        "I(xgb_prob > 0.315)TRUE" = "I(Prob of PFAS > 0.315)", 
                                        "I(xgb_prob > 0.5)TRUE" = "I(Prob of PFAS > 0.5)", 
                                        "I(xgb_prob > 0.75)TRUE" = "I(Prob of PFAS > 0.75)"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/nh_veru_rout.tex")) 


#correlation between months in residence and downgradient
cor(df$down, df$m_months_res, use = "pairwise.complete.obs")
cor(df$up, df$m_months_res, use = "pairwise.complete.obs")



#Base IV table with updated marginal effects (IV Table from main text)
################
###Table 2 Note, standard errors are read in from bootstrap_iv.R run
if (!file.exists(modify_path("Data_Verify/RData/bootstrap.RData"))) {
  stop("Bootstrap standard errors")
}
load(modify_path("Data_Verify/RData/bootstrap.RData"))

#preterm
table2_preterm = list()

table2_preterm[["All"]]= fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Moderately"]]= fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_preterm[["Very"]]= fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_preterm[["Extremely"]]= fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                                               n_sites + wind_exposure + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

#calculate bootstrapped standard errors
preterm_sd = linear_bootstrap(boot_coefs, "preterm", table2_preterm[["All"]])
lpreterm_sd = linear_bootstrap(boot_coefs, "lpreterm", table2_preterm[["Moderately"]])
mpreterm_sd = linear_bootstrap(boot_coefs, "mpreterm", table2_preterm[["Very"]])
vpreterm_sd = linear_bootstrap(boot_coefs, "vpreterm", table2_preterm[["Extremely"]])
save(preterm_sd, lpreterm_sd, mpreterm_sd, vpreterm_sd, file = modify_path(paste0("Data_Verify/RData/preterm_sd", ppt, ".RData")))

cov_pre_lm = cov_boot(boot_coefs, "lpreterm", table2_preterm[["Moderately"]], "mpreterm", table2_preterm[["Very"]])
cov_pre_lv = cov_boot(boot_coefs, "lpreterm", table2_preterm[["Moderately"]], "vpreterm", table2_preterm[["Extremely"]])
cov_pre_mv = cov_boot(boot_coefs, "mpreterm", table2_preterm[["Very"]], "vpreterm", table2_preterm[["Extremely"]])
save(cov_pre_lm, cov_pre_lv, cov_pre_mv, file = modify_path(paste0("Data_Verify/RData/cov_preterm", ppt, ".RData")))  

#marginal effect of 100ppt at median
pre_med100 = table2_preterm[["All"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
pre_med100l = (table2_preterm[["All"]]$coefficients["pred_pfas"] - 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
pre_med100u = (table2_preterm[["All"]]$coefficients["pred_pfas"] + 1.96 * preterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

mpre_med100 = table2_preterm[["Moderately"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
mpre_med100l = (table2_preterm[["Moderately"]]$coefficients["pred_pfas"] - 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
mpre_med100u = (table2_preterm[["Moderately"]]$coefficients["pred_pfas"] + 1.96 * lpreterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

vpre_med100 = table2_preterm[["Very"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
vpre_med100l = (table2_preterm[["Very"]]$coefficients["pred_pfas"] - 1.96 * mpreterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
vpre_med100u = (table2_preterm[["Very"]]$coefficients["pred_pfas"] + 1.96 *mpreterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

epre_med100 = table2_preterm[["Extremely"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
epre_med100l = (table2_preterm[["Extremely"]]$coefficients["pred_pfas"] - 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
epre_med100u = (table2_preterm[["Extremely"]]$coefficients["pred_pfas"] + 1.96 * vpreterm_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

pre_med = table2_preterm[["All"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
pre_medl = (table2_preterm[["All"]]$coefficients["pred_pfas"] - 1.96 * preterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
pre_medu = (table2_preterm[["All"]]$coefficients["pred_pfas"] + 1.96 * preterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

mpre_med = table2_preterm[["Moderately"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
mpre_medl = (table2_preterm[["Moderately"]]$coefficients["pred_pfas"] - 1.96 * lpreterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
mpre_medu = (table2_preterm[["Moderately"]]$coefficients["pred_pfas"] + 1.96 * lpreterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

vpre_med = table2_preterm[["Very"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
vpre_medl = (table2_preterm[["Very"]]$coefficients["pred_pfas"] - 1.96 * mpreterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
vpre_medu = (table2_preterm[["Very"]]$coefficients["pred_pfas"] + 1.96 * mpreterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

epre_med = table2_preterm[["Extremely"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
epre_medl = (table2_preterm[["Extremely"]]$coefficients["pred_pfas"] - 1.96 * vpreterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
epre_medu = (table2_preterm[["Extremely"]]$coefficients["pred_pfas"] + 1.96 * vpreterm_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)



#lbw
table2_lbw = list()
table2_lbw[["Low Birthweight"]]= fixest::feols(I(bweight < 2500 ) ~ pred_pfas + asinh(pfas) +
                                                 n_sites + wind_exposure + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["lLow Birthweight"]]= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

table2_lbw[["mLow Birthweight"]]= fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


table2_lbw[["Very Low Birthweight"]]= fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) +
                                                      n_sites + wind_exposure + 
                                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

#calculate bootstrapped standard errors
lbw_sd = linear_bootstrap(boot_coefs, "lbw", table2_lbw[["Low Birthweight"]])
llbw_sd = linear_bootstrap(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]])
mlbw_sd = linear_bootstrap(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]])
vlbw_sd = linear_bootstrap(boot_coefs, "vlbw", table2_lbw[["Very Low Birthweight"]])
save(lbw_sd, llbw_sd, mlbw_sd, vlbw_sd, file = modify_path(paste0("Data_Verify/RData/lbw_sd", ppt, ".RData")))

cov_lbw_lm = cov_boot(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]], "mlbw", table2_lbw[["mLow Birthweight"]])
cov_lbw_lv = cov_boot(boot_coefs, "llbw", table2_lbw[["lLow Birthweight"]], "vlbw", table2_lbw[["Very Low Birthweight"]])
cov_lbw_mv = cov_boot(boot_coefs, "mlbw", table2_lbw[["mLow Birthweight"]], "vlbw", table2_lbw[["Very Low Birthweight"]])
save(cov_lbw_lm, cov_lbw_lv, cov_lbw_mv, file = modify_path(paste0("Data_Verify/RData/cov_lbw", ppt, ".RData")))

#marginal effects
lbw_med100 = table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
lbw_med100l = (table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
lbw_med100u= (table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * lbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

llbw_med100 = table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
llbw_med100l = (table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
llbw_med100u = (table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * llbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

mlbw_med100 = table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
mlbw_med100l = (table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
mlbw_med100u = (table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * mlbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

vlbw_med100 = table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
vlbw_med100l = (table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
vlbw_med100u = (table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * vlbw_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

lbw_med = table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
lbw_medl = (table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * lbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
lbw_medu = (table2_lbw[["Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * lbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

llbw_med = table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
llbw_medl = (table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * llbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
llbw_medu = (table2_lbw[["lLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * llbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

mlbw_med = table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
mlbw_medl = (table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] - 1.96 * mlbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
mlbw_medu = (table2_lbw[["mLow Birthweight"]]$coefficients["pred_pfas"] + 1.96 * mlbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

vlbw_med = table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
vlbw_medl = (table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] - 1.96 * vlbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
vlbw_medu = (table2_lbw[["Very Low Birthweight"]]$coefficients["pred_pfas"] + 1.96 * vlbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)


#####################
### Mortality effects
mort_table = list()

mort_table[["Binary"]] = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 +fa_resid + wind_exposure
                                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["PFAS Interaction"]] = fixest::feols(death ~ (updown + down) *I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["Distance Interaction"]] = fixest::feols(death ~ (updown + down) *I(dist/1000) + I(pfas/10^3)  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid + wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["Contr. Health"]] = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 +fa_resid + wind_exposure + gestation + bweight
                                              |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

mort_table[["IV"]] = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                                     n_sites + wind_exposure + 
                                     m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                     pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                     mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                     mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                     m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)

#mortality standard error
mort_sd = linear_bootstrap(boot_coefs, "mort", mort_table[["IV"]])
save(mort_sd, file = modify_path(paste0("Data_Verify/RData/mort_sd", ppt, ".RData")))

#mortality marginal effect
mort_med100 = mort_table[["IV"]]$coefficients["pred_pfas"]/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
mort_med100l = (mort_table[["IV"]]$coefficients["pred_pfas"] - 1.96 * mort_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100
mort_med100u = (mort_table[["IV"]]$coefficients["pred_pfas"] + 1.96 * mort_sd) * 1/(sqrt(1 + median(sinh(df_est$pred_pfas), na.rm = T)^2)) * 100

mort_med = mort_table[["IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
mort_medl = (mort_table[["IV"]]$coefficients["pred_pfas"] - 1.96 * mort_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)
mort_medu = (mort_table[["IV"]]$coefficients["pred_pfas"] + 1.96 * mort_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df_est$pred_pfas), na.rm = T) - 28.27)

meff = matrix(c(pre_med100, pre_med100l, pre_med100u, pre_med, pre_medl, pre_medu,
         mpre_med100, mpre_med100l, mpre_med100u, mpre_med, mpre_medl, mpre_medu,
         vpre_med100, vpre_med100l, vpre_med100u, vpre_med, vpre_medl, vpre_medu,
         epre_med100, epre_med100l, epre_med100u, epre_med, epre_medl, epre_medu,
         lbw_med100, lbw_med100l, lbw_med100u, lbw_med, lbw_medl, lbw_medu,
         llbw_med100, llbw_med100l, llbw_med100u, llbw_med, llbw_medl, llbw_medu,
         mlbw_med100, mlbw_med100l, mlbw_med100u, mlbw_med, mlbw_medl, mlbw_medu,
         vlbw_med100, vlbw_med100l, vlbw_med100u, vlbw_med, vlbw_medl, vlbw_medu,
         mort_med100, mort_med100l, mort_med100u, mort_med, mort_medl, mort_medu), nrow = 9, byrow = T)

meff = data.frame(
  row = c(
    "Preterm", "Moderately Preterm", "Very Preterm", "Extremely Preterm",
    "Low Birthweight", "Moderately Low Birthweight", "Very Low Birthweight", "Extremely Low Birthweight",
    "Mortality"
  ),
  meff
)

colnames(meff) = c("", "Median 100", "Median 100 Lower", "Median 100 Upper", "Median", "Median Lower", "Median Upper")

table = xtable::xtable(meff, digits = 4)
print(table, 
      type = "latex", 
      include.rownames = FALSE,
      caption.placement = "top",
      hline.after = c(-1, 0, nrow(meff)), 
      file = modify_path2("Tables/Revisions/marginal_effects_iv.tex"))

#IV of covariates on down and up (Table R4)
source("PFAS-Code/Pub/Revision1/Tables/iv_covars_down_up.R")