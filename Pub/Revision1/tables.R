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
#save estimation sample to UA Box Health
save(df2, file = paste0(natality_path, "[UA Box Health] birth_records_estimation_sample.RData"))
#Preterm
r1_preterm = list() 
r1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                      m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

r1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                             m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                             m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

r1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                       m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

r1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                            m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                            m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(r1_preterm, 
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
                                        "med_inc" = "Median Income"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/r1_preterm.tex")) 


#low birthweight
r1_lbw = list() 
r1_lbw[["All"]] = fixest::feols(I(bweight < 2500) ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                  m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                  m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

r1_lbw[["Full Term"]] = fixest::feols(I(bweight < 2500) ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                        m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                        m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                      , data = df2[which(df2$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

r1_lbw[["Moderately"]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                       , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

r1_lbw[["Very"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                   m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                   m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                 , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

r1_lbw[["Extremely"]] = fixest::feols(I(bweight < 1000) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                        m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                        m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                      , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(r1_lbw, 
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
                                        "med_inc" = "Median Income"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/r1_lbw.tex")) 


mort2 = fixest::feols(death ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                        m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                      , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(list(mort2), 
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
                                        "med_inc" = "Median Income"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/r1_mort.tex")) 


#Next table gives the correlation between downgradient and the covariates
r1_down = list() 
r1_down[["Downgradient"]] = fixest::feols(down ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                               m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, 
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
                                        "med_inc" = "Median Income"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/r1_down.tex")) 



#Next table: summary stats table with median income
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



#Next table: drop parental education as a control
#drop parental education
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +  fa_resid +wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid +wind_exposure
                                              |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_nopeduc.tex")) 

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +  fa_resid +  wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 + fa_resid + wind_exposure
                                                                 |county + year^month + birth_race_dsc_1, data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 + fa_resid + wind_exposure
                                                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 + fa_resid+ wind_exposure
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_nopeduc.tex"))

mort4 = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + 
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid+ wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(list(mort4), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_nopeduc.tex")) 

#Table: number of individuals with each missing covariate
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



#Table: Oster calcs with factor of 2 and 5
#Oster with a maximal R2 of 2*empirical R2
oster_factor = 5
source("PFAS-Code/Pub/Robustness/oster_selection.R")
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
source("PFAS-Code/Pub/Robustness/oster_selection.R")
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




#Table: drop largest site
#get distance from each well to Pease AFB (largest site)
wells$p_dist = as.numeric(st_distance(wells %>% 
                                        as_tibble() %>% 
                                        dplyr::select(sys_id, source, lng, lat) %>%
                                        st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                                        st_transform(32110), cont_sites %>% dplyr::filter(site == "Pease Air Force Base") %>%  st_transform(32110)))
#merge distance with natality data by assigned well
df = 
  df %>% 
  left_join(wells %>% as_tibble() %>% dplyr::select(sys_id, source, p_dist))
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 +fa_resid + wind_exposure 
                                               |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +fa_resid + wind_exposure 
                                         |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 +fa_resid + wind_exposure 
                                              |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_drop_pease.tex")) 

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +fa_resid + wind_exposure 
                                                     |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 +fa_resid + wind_exposure 
                                                                 |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 +fa_resid + wind_exposure 
                                                 |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 +fa_resid + wind_exposure 
                                                           |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +fa_resid + wind_exposure 
                                                     |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_drop_pease.tex"))

mort4 = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 +fa_resid + wind_exposure 
                      |county + year^month + birth_race_dsc_1, data = df[which(df$p_dist > 5000), ], warn = F, notes = F, cluster = c("site", "year^month"))


modelsummary::modelsummary(list(mort4), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_drop_pease.tex")) 



#Table: continuous health outcomes
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

table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] - 1.96 * ges_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] + 1.96 * ges_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] - 1.96 * ges_pre_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] + 1.96 * ges_pre_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100


# table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + quantile(sinh(df$pred_pfas), na.rm = T, 0.25)^2)) * 100
# (table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] - 1.96 * ges_sd) * 1/(sqrt(1 + quantile(sinh(df$pred_pfas), na.rm = T, 0.25)^2)) * 100
# (table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] + 1.96 * ges_sd) * 1/(sqrt(1 + quantile(sinh(df$pred_pfas), na.rm = T, 0.25)^2)) * 100
# 
# table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + quantile(sinh(df$pred_pfas), na.rm = T, 0.25)^2)) * 100
# (table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] - 1.96 * ges_pre_sd) * 1/(sqrt(1 + quantile(sinh(df$pred_pfas), na.rm = T, 0.25)^2)) * 100
# (table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] + 1.96 * ges_pre_sd) * 1/(sqrt(1 + quantile(sinh(df$pred_pfas), na.rm = T, 0.25)^2)) * 100

table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] - 1.96 * ges_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV"]]$coefficients["pred_pfas"] + 1.96 * ges_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)

table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] - 1.96 * ges_pre_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_ges[["Gestation IV: Preterm"]]$coefficients["pred_pfas"] + 1.96 * ges_pre_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)



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


table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] - 1.96 * bw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] + 1.96 * bw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] - 1.96 * bw_lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] + 1.96 * bw_lbw_sd) * 1/(sqrt(1 + median(sinh(df$pred_pfas), na.rm = T)^2)) * 100

table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] - 1.96 * bw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV"]]$coefficients["pred_pfas"] + 1.96 * bw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)

table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] - 1.96 * bw_lbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(table1_cont_bw[["Birthweight IV: LBW"]]$coefficients["pred_pfas"] + 1.96 * bw_lbw_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)




#Table: cluster standard errors at the cws level 
table1_preterm = list() 
table1_preterm[["All"]] = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 +fa_resid + wind_exposure 
                                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))

table1_preterm[["Moderately"]] = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 +fa_resid + wind_exposure 
                                               |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))

table1_preterm[["Very"]] = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 +fa_resid + wind_exposure 
                                         |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))

table1_preterm[["Extremely"]] = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 +fa_resid + wind_exposure 
                                              |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))


modelsummary::modelsummary(table1_preterm, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/preterm_cluster_pws.tex")) 

#low birthweight
table1_lbw = list() 
table1_lbw[["Low Birthweight all "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +fa_resid + wind_exposure 
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))

table1_lbw[["Low Birthweight among full term "]] = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                                   m_height + tri5 +fa_resid + wind_exposure 
                                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))


table1_lbw[["Low Birthweight "]] = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 +fa_resid + wind_exposure 
                                                 |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))

table1_lbw[["Moderately Low Birthweight"]] = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                             m_height + tri5 +fa_resid + wind_exposure 
                                                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))

table1_lbw[["Very Low Birthweight"]] = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                       m_height + tri5 +fa_resid + wind_exposure 
                                                     |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))


modelsummary::modelsummary(table1_lbw, 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/lbw_cluster_pws.tex"))

mort4 = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 +fa_resid + wind_exposure 
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("sys_id", "year^month"))


modelsummary::modelsummary(list(mort4), 
                           stars = c("*" = 0.2, "**" = 0.1, "***" = 0.02), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("down" = "Downgradient", 
                                        "updown" = "Upgradient"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/mort_cluster_pws.tex")) 


#Table: run everything as a logit
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



# Table: Predicted PFAS contamination across treatment groups
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



#mortality impact from median in US to median contaminated in NH
mort_iv = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                n_sites + wind_exposure + 
                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)
load(modify_path("Data_Verify/RData/mort_sd.RData"))

mort_iv$coefficients["pred_pfas"]/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(mort_iv$coefficients["pred_pfas"] - 1.96 * mort_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
(mort_iv$coefficients["pred_pfas"] + 1.96 * mort_sd) * 1/(sqrt(1 + 28.27^2)) * (median(sinh(df$pred_pfas), na.rm = T) - 28.27)
