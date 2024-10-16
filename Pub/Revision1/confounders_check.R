#maternal age, number of cigarettes, maternal education, previous c-section, fertility enhancing druges, ivf, mothers height are significant predictors of downgradient indicator
dreg = fixest::feols(down ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                       m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, 
                     data = df2, warn = F, notes = F, cluster = c("site", "year^month"))
d_m_age = dreg$coeftable["m_age", "Estimate"]
d_nbr_cgrtt = dreg$coeftable["nbr_cgrtt", "Estimate"]
d_m_educ = dreg$coeftable["m_educ", "Estimate"]
d_csec = dreg$coeftable["mr_21", "Estimate"]
d_fed = dreg$coeftable["mr_26", "Estimate"]
d_ivf = dreg$coeftable["mr_27", "Estimate"]
d_m_height = dreg$coeftable["m_height", "Estimate"]


#bring in second stage for preterm
pre = fixest::feols(I(gestation < 37) ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                      m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                      m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

#preterm values
spre_m_age = sign(pre$coeftable["m_age", "Estimate"] * d_m_age)
spre_m_age_sig = ifelse(abs(pre$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                     ifelse(abs(pre$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                            ifelse(abs(pre$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

spre_nbr_cgrtt = sign(pre$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
spre_nbr_cgrtt_sig = ifelse(abs(pre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(pre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(pre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))


spre_m_educ = sign(pre$coeftable["m_educ", "Estimate"] * d_m_educ)
spre_m_educ_sig = ifelse(abs(pre$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                      ifelse(abs(pre$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                             ifelse(abs(pre$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

spre_csec = sign(pre$coeftable["mr_21", "Estimate"] * d_csec)
spre_csec_sig = ifelse(abs(pre$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                    ifelse(abs(pre$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                           ifelse(abs(pre$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

spre_fed = sign(pre$coeftable["mr_26", "Estimate"] * d_fed)
spre_fed_sig = ifelse(abs(pre$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(pre$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(pre$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

spre_ivf = sign(pre$coeftable["mr_27", "Estimate"] * d_ivf)
spre_ivf_sig = ifelse(abs(pre$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(pre$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(pre$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

spre_m_height = sign(pre$coeftable["m_height", "Estimate"] * d_m_height)
spre_m_height_sig = ifelse(abs(pre$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(pre$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(pre$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))


#moderately preterm 
mpre = fixest::feols(I(gestation < 37 & gestation >= 32) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                       m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

smpre_m_age = sign(mpre$coeftable["m_age", "Estimate"] * d_m_age)
smpre_m_age_sig = ifelse(abs(mpre$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                     ifelse(abs(mpre$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                            ifelse(abs(mpre$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

smpre_nbr_cgrtt = sign(mpre$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
smpre_nbr_cgrtt_sig = ifelse(abs(mpre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(mpre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(mpre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

smpre_m_educ = sign(mpre$coeftable["m_educ", "Estimate"] * d_m_educ)
smpre_m_educ_sig = ifelse(abs(mpre$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                      ifelse(abs(mpre$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                             ifelse(abs(mpre$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

smpre_csec = sign(mpre$coeftable["mr_21", "Estimate"] * d_csec)
smpre_csec_sig = ifelse(abs(mpre$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                    ifelse(abs(mpre$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                           ifelse(abs(mpre$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

smpre_fed = sign(mpre$coeftable["mr_26", "Estimate"] * d_fed)
smpre_fed_sig = ifelse(abs(mpre$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(mpre$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(mpre$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

smpre_ivf = sign(mpre$coeftable["mr_27", "Estimate"] * d_ivf)
smpre_ivf_sig = ifelse(abs(mpre$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(mpre$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(mpre$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

smpre_m_height = sign(mpre$coeftable["m_height", "Estimate"] * d_m_height)
smpre_m_height_sig = ifelse(abs(mpre$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(mpre$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(mpre$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))






#Very preterm
vpre = fixest::feols(I(gestation < 32 & gestation >= 28) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                       m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

svpre_m_age = sign(vpre$coeftable["m_age", "Estimate"] * d_m_age)
svpre_m_age_sig = ifelse(abs(vpre$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                     ifelse(abs(vpre$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                            ifelse(abs(vpre$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

svpre_nbr_cgrtt = sign(vpre$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
svpre_nbr_cgrtt_sig = ifelse(abs(vpre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(vpre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(vpre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

svpre_m_educ = sign(vpre$coeftable["m_educ", "Estimate"] * d_m_educ)
svpre_m_educ_sig = ifelse(abs(vpre$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                      ifelse(abs(vpre$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                             ifelse(abs(vpre$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

svpre_csec = sign(vpre$coeftable["mr_21", "Estimate"] * d_csec)
svpre_csec_sig = ifelse(abs(vpre$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                    ifelse(abs(vpre$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                           ifelse(abs(vpre$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

svpre_fed = sign(vpre$coeftable["mr_26", "Estimate"] * d_fed)
svpre_fed_sig = ifelse(abs(vpre$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(vpre$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(vpre$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

svpre_ivf = sign(vpre$coeftable["mr_27", "Estimate"] * d_ivf)
svpre_ivf_sig = ifelse(abs(vpre$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(vpre$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(vpre$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

svpre_m_height = sign(vpre$coeftable["m_height", "Estimate"] * d_m_height)
svpre_m_height_sig = ifelse(abs(vpre$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(vpre$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(vpre$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))



#extremely preterm
epre = fixest::feols(I(gestation < 28) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                       m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

sepre_m_age = sign(epre$coeftable["m_age", "Estimate"] * d_m_age)
sepre_m_age_sig = ifelse(abs(epre$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                     ifelse(abs(epre$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                            ifelse(abs(epre$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

sepre_nbr_cgrtt = sign(epre$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
sepre_nbr_cgrtt_sig = ifelse(abs(epre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(epre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(epre$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

sepre_m_educ = sign(epre$coeftable["m_educ", "Estimate"] * d_m_educ)
sepre_m_educ_sig = ifelse(abs(epre$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                      ifelse(abs(epre$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                             ifelse(abs(epre$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

sepre_csec = sign(epre$coeftable["mr_21", "Estimate"] * d_csec)
sepre_csec_sig = ifelse(abs(epre$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                    ifelse(abs(epre$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                           ifelse(abs(epre$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

sepre_fed = sign(epre$coeftable["mr_26", "Estimate"] * d_fed)
sepre_fed_sig = ifelse(abs(epre$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(epre$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(epre$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

sepre_ivf = sign(epre$coeftable["mr_27", "Estimate"] * d_ivf)
sepre_ivf_sig = ifelse(abs(epre$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(epre$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(epre$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

sepre_m_height = sign(epre$coeftable["m_height", "Estimate"] * d_m_height)
sepre_m_height_sig = ifelse(abs(epre$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(epre$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(epre$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))


#Low birthweight
lbw = fixest::feols(I(bweight < 2500) ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                  m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                  m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

slbw_m_age = sign(lbw$coeftable["m_age", "Estimate"] * d_m_age)
slbw_m_age_sig = ifelse(abs(lbw$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                     ifelse(abs(lbw$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                            ifelse(abs(lbw$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_nbr_cgrtt = sign(lbw$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
slbw_nbr_cgrtt_sig = ifelse(abs(lbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(lbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(lbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_m_educ = sign(lbw$coeftable["m_educ", "Estimate"] * d_m_educ)
slbw_m_educ_sig = ifelse(abs(lbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                      ifelse(abs(lbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                             ifelse(abs(lbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_csec = sign(lbw$coeftable["mr_21", "Estimate"] * d_csec)
slbw_csec_sig = ifelse(abs(lbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                    ifelse(abs(lbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                           ifelse(abs(lbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_fed = sign(lbw$coeftable["mr_26", "Estimate"] * d_fed)
slbw_fed_sig = ifelse(abs(lbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(lbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(lbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_ivf = sign(lbw$coeftable["mr_27", "Estimate"] * d_ivf)
slbw_ivf_sig = ifelse(abs(lbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                   ifelse(abs(lbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                          ifelse(abs(lbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_m_height = sign(lbw$coeftable["m_height", "Estimate"] * d_m_height)
slbw_m_height_sig = ifelse(abs(lbw$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(lbw$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(lbw$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))



#low birthweight amongst full term births
lbw_ft = fixest::feols(I(bweight < 2500) ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                        m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                        m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                      , data = df2[which(df2$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))

slbw_ft_m_age = sign(lbw_ft$coeftable["m_age", "Estimate"] * d_m_age)
slbw_ft_m_age_sig = ifelse(abs(lbw_ft$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(lbw_ft$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(lbw_ft$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_ft_nbr_cgrtt = sign(lbw_ft$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
slbw_ft_nbr_cgrtt_sig = ifelse(abs(lbw_ft$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                             ifelse(abs(lbw_ft$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                    ifelse(abs(lbw_ft$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_ft_m_educ = sign(lbw_ft$coeftable["m_educ", "Estimate"] * d_m_educ)
slbw_ft_m_educ_sig = ifelse(abs(lbw_ft$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                          ifelse(abs(lbw_ft$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                                 ifelse(abs(lbw_ft$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_ft_csec = sign(lbw_ft$coeftable["mr_21", "Estimate"] * d_csec)
slbw_ft_csec_sig = ifelse(abs(lbw_ft$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(lbw_ft$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(lbw_ft$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_ft_fed = sign(lbw_ft$coeftable["mr_26", "Estimate"] * d_fed)
slbw_ft_fed_sig = ifelse(abs(lbw_ft$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(lbw_ft$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(lbw_ft$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_ft_ivf = sign(lbw_ft$coeftable["mr_27", "Estimate"] * d_ivf)
slbw_ft_ivf_sig = ifelse(abs(lbw_ft$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                        ifelse(abs(lbw_ft$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                               ifelse(abs(lbw_ft$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

slbw_ft_m_height = sign(lbw_ft$coeftable["m_height", "Estimate"] * d_m_height)
slbw_ft_m_height_sig = ifelse(abs(lbw_ft$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                             ifelse(abs(lbw_ft$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                                    ifelse(abs(lbw_ft$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))

#moderately low birthweight
mlbw = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                       , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

smlbw_m_age = sign(mlbw$coeftable["m_age", "Estimate"] * d_m_age)
smlbw_m_age_sig = ifelse(abs(mlbw$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                           ifelse(abs(mlbw$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                                  ifelse(abs(mlbw$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

smlbw_nbr_cgrtt = sign(mlbw$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
smlbw_nbr_cgrtt_sig = ifelse(abs(mlbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                               ifelse(abs(mlbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                      ifelse(abs(mlbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

smlbw_m_educ = sign(mlbw$coeftable["m_educ", "Estimate"] * d_m_educ)
smlbw_m_educ_sig = ifelse(abs(mlbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                            ifelse(abs(mlbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                                   ifelse(abs(mlbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

smlbw_csec = sign(mlbw$coeftable["mr_21", "Estimate"] * d_csec)
smlbw_csec_sig = ifelse(abs(mlbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                          ifelse(abs(mlbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                                 ifelse(abs(mlbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

smlbw_fed = sign(mlbw$coeftable["mr_26", "Estimate"] * d_fed)
smlbw_fed_sig = ifelse(abs(mlbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(mlbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(mlbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

smlbw_ivf = sign(mlbw$coeftable["mr_27", "Estimate"] * d_ivf)
smlbw_ivf_sig = ifelse(abs(mlbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(mlbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(mlbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

smlbw_m_height = sign(mlbw$coeftable["m_height", "Estimate"] * d_m_height)
smlbw_m_height_sig = ifelse(abs(mlbw$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                              ifelse(abs(mlbw$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                                     ifelse(abs(mlbw$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))


#very low birthweight
vlbw = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                   m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                   m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                 , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

svlbw_m_age = sign(vlbw$coeftable["m_age", "Estimate"] * d_m_age)
svlbw_m_age_sig = ifelse(abs(vlbw$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                           ifelse(abs(vlbw$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                                  ifelse(abs(vlbw$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

svlbw_nbr_cgrtt = sign(vlbw$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
svlbw_nbr_cgrtt_sig = ifelse(abs(vlbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                               ifelse(abs(vlbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                      ifelse(abs(vlbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

svlbw_m_educ = sign(vlbw$coeftable["m_educ", "Estimate"] * d_m_educ)
svlbw_m_educ_sig = ifelse(abs(vlbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                            ifelse(abs(vlbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                                   ifelse(abs(vlbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

svlbw_csec = sign(vlbw$coeftable["mr_21", "Estimate"] * d_csec)
svlbw_csec_sig = ifelse(abs(vlbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                          ifelse(abs(vlbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                                 ifelse(abs(vlbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

svlbw_fed = sign(vlbw$coeftable["mr_26", "Estimate"] * d_fed)
svlbw_fed_sig = ifelse(abs(vlbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(vlbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(vlbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

svlbw_ivf = sign(vlbw$coeftable["mr_27", "Estimate"] * d_ivf)
svlbw_ivf_sig = ifelse(abs(vlbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(vlbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(vlbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

svlbw_m_height = sign(vlbw$coeftable["m_height", "Estimate"] * d_m_height)
svlbw_m_height_sig = ifelse(abs(vlbw$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                              ifelse(abs(vlbw$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                                     ifelse(abs(vlbw$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))

#extremeley low birthweight
elbw = fixest::feols(I(bweight < 1000) ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                                        m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                        m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                                      , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


selbw_m_age = sign(elbw$coeftable["m_age", "Estimate"] * d_m_age)
selbw_m_age_sig = ifelse(abs(elbw$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                           ifelse(abs(elbw$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                                  ifelse(abs(elbw$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

selbw_nbr_cgrtt = sign(elbw$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
selbw_nbr_cgrtt_sig = ifelse(abs(elbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                               ifelse(abs(elbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                      ifelse(abs(elbw$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

selbw_m_educ = sign(elbw$coeftable["m_educ", "Estimate"] * d_m_educ)
selbw_m_educ_sig = ifelse(abs(elbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                            ifelse(abs(elbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                                   ifelse(abs(elbw$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

selbw_csec = sign(elbw$coeftable["mr_21", "Estimate"] * d_csec)
selbw_csec_sig = ifelse(abs(elbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                          ifelse(abs(elbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                                 ifelse(abs(elbw$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

selbw_fed = sign(elbw$coeftable["mr_26", "Estimate"] * d_fed)
selbw_fed_sig = ifelse(abs(elbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(elbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(elbw$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

selbw_ivf = sign(elbw$coeftable["mr_27", "Estimate"] * d_ivf)
selbw_ivf_sig = ifelse(abs(elbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(elbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(elbw$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

selbw_m_height = sign(elbw$coeftable["m_height", "Estimate"] * d_m_height)
selbw_m_height_sig = ifelse(abs(elbw$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                              ifelse(abs(elbw$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                                     ifelse(abs(elbw$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))


#infant mortality
mort = fixest::feols(death ~  m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                        m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height+ med_hprice + med_inc|county + year^month + birth_race_dsc_1
                      , data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

smort_m_age = sign(mort$coeftable["m_age", "Estimate"] * d_m_age)
smort_m_age_sig = ifelse(abs(mort$coeftable["m_age", "Pr(>|t|)"]) < 0.01, 3, 
                           ifelse(abs(mort$coeftable["m_age", "Pr(>|t|)"]) < 0.05, 2, 
                                  ifelse(abs(mort$coeftable["m_age", "Pr(>|t|)"]) < 0.1, 1, 0)))

smort_nbr_cgrtt = sign(mort$coeftable["nbr_cgrtt", "Estimate"] * d_nbr_cgrtt)
smort_nbr_cgrtt_sig = ifelse(abs(mort$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.01, 3, 
                               ifelse(abs(mort$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.05, 2, 
                                      ifelse(abs(mort$coeftable["nbr_cgrtt", "Pr(>|t|)"]) < 0.1, 1, 0)))

smort_m_educ = sign(mort$coeftable["m_educ", "Estimate"] * d_m_educ)
smort_m_educ_sig = ifelse(abs(mort$coeftable["m_educ", "Pr(>|t|)"]) < 0.01, 3, 
                            ifelse(abs(mort$coeftable["m_educ", "Pr(>|t|)"]) < 0.05, 2, 
                                   ifelse(abs(mort$coeftable["m_educ", "Pr(>|t|)"]) < 0.1, 1, 0)))

smort_csec = sign(mort$coeftable["mr_21", "Estimate"] * d_csec)
smort_csec_sig = ifelse(abs(mort$coeftable["mr_21", "Pr(>|t|)"]) < 0.01, 3, 
                          ifelse(abs(mort$coeftable["mr_21", "Pr(>|t|)"]) < 0.05, 2, 
                                 ifelse(abs(mort$coeftable["mr_21", "Pr(>|t|)"]) < 0.1, 1, 0)))

smort_fed = sign(mort$coeftable["mr_26", "Estimate"] * d_fed)
smort_fed_sig = ifelse(abs(mort$coeftable["mr_26", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(mort$coeftable["mr_26", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(mort$coeftable["mr_26", "Pr(>|t|)"]) < 0.1, 1, 0)))

smort_ivf = sign(mort$coeftable["mr_27", "Estimate"] * d_ivf)
smort_ivf_sig = ifelse(abs(mort$coeftable["mr_27", "Pr(>|t|)"]) < 0.01, 3, 
                         ifelse(abs(mort$coeftable["mr_27", "Pr(>|t|)"]) < 0.05, 2, 
                                ifelse(abs(mort$coeftable["mr_27", "Pr(>|t|)"]) < 0.1, 1, 0)))

smort_m_height = sign(mort$coeftable["m_height", "Estimate"] * d_m_height)
smort_m_height_sig = ifelse(abs(mort$coeftable["m_height", "Pr(>|t|)"]) < 0.01, 3, 
                              ifelse(abs(mort$coeftable["m_height", "Pr(>|t|)"]) < 0.05, 2, 
                                     ifelse(abs(mort$coeftable["m_height", "Pr(>|t|)"]) < 0.1, 1, 0)))

#turn into table
library(xtable)
# Create a function to generate a single cell content
cell_content = function(sign_var, sig_var) {
  symbol = ifelse(sign_var > 0, "+", "-")
  stars = paste(rep("*", sig_var), collapse = "")
  if (stars == "") symbol else paste0(symbol, "^{", stars, "}")
}

create_significance_table = function(outcome, variable_names) {
  
  if (outcome == "pre"){
    # Create the data frame
    df =data.frame(
      Variable = variable_names,
      Any = sapply(variable_names, function(v) cell_content(get(paste0("spre_", v)), get(paste0("spre_", v, "_sig")))),
      Moderately = sapply(variable_names, function(v) cell_content(get(paste0("smpre_", v)), get(paste0("smpre_", v, "_sig")))),
      Very = sapply(variable_names, function(v) cell_content(get(paste0("svpre_", v)), get(paste0("svpre_", v, "_sig")))),
      Extremely = sapply(variable_names, function(v) cell_content(get(paste0("sepre_", v)), get(paste0("sepre_", v, "_sig"))))
    ) 
    # Create and print the xtable
    print(xtable(df, align = c("l", "l", "c", "c", "c", "c")), 
          include.rownames = FALSE, 
          sanitize.text.function = function(x) x)
  }
  
  if (outcome == "lbw"){
    # Create the data frame
    df =data.frame(
      Variable = variable_names,
      Any = sapply(variable_names, function(v) cell_content(get(paste0("slbw_", v)), get(paste0("slbw_", v, "_sig")))),
      Full_Term = sapply(variable_names, function(v) cell_content(get(paste0("slbw_ft_", v)), get(paste0("slbw_ft_", v, "_sig")))),
      Moderately = sapply(variable_names, function(v) cell_content(get(paste0("smlbw_", v)), get(paste0("smlbw_", v, "_sig")))),
      Very = sapply(variable_names, function(v) cell_content(get(paste0("svlbw_", v)), get(paste0("svlbw_", v, "_sig")))),
      Extremely = sapply(variable_names, function(v) cell_content(get(paste0("selbw_", v)), get(paste0("selbw_", v, "_sig"))))
    )
    # Create and print the xtable
    print(xtable(df, align = c("l", "l", "c", "c", "c", "c", "c")), 
          include.rownames = FALSE, 
          sanitize.text.function = function(x) x)
  }
  
  if (outcome == "mort"){
    # Create the data frame
    df =data.frame(
      Variable = variable_names,
      Any = sapply(variable_names, function(v) cell_content(get(paste0("smort_", v)), get(paste0("smort_", v, "_sig")))))
    print(xtable(df, align = c("l", "l", "c")), 
          include.rownames = FALSE, 
          sanitize.text.function = function(x) x)
  }
  
}

# Usage
variable_names <- c("m_age", "nbr_cgrtt", "m_educ", "csec", "fed", "ivf", "m_height")

sink(modify_path2("Tables/Revisions/confounder_sign_pre.tex"))
create_significance_table("pre", variable_names)
sink()
sink(modify_path2("Tables/Revisions/confounder_sign_lbw.tex"))
create_significance_table("lbw", variable_names)
sink()
sink(modify_path2("Tables/Revisions/confounder_sign_mort.tex"))
create_significance_table("mort", variable_names)
sink()
