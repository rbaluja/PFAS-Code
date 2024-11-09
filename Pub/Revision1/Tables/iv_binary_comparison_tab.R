#preterm

pre_iv = fixest::feols(I(gestation < 37) ~ pred_pfas + asinh(pfas) + 
                                         n_sites + wind_exposure + 
                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                         m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


lpre_iv = fixest::feols(I(gestation < 37 & gestation >= 32) ~ pred_pfas + asinh(pfas) + 
                                                n_sites + wind_exposure + 
                                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

vpre_iv = fixest::feols(I(gestation < 32 & gestation >= 28) ~ pred_pfas + asinh(pfas) + 
                                          n_sites + wind_exposure + 
                                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                          m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


epre_iv = fixest::feols(I(gestation < 28) ~ pred_pfas + asinh(pfas) + 
                                               n_sites + wind_exposure + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )




lbw_iv = fixest::feols(I(bweight < 2500 ) ~ pred_pfas + asinh(pfas) +
                                                 n_sites + wind_exposure + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

mlbw_iv= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )

vlbw_iv = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~ pred_pfas + asinh(pfas) +
                                                  n_sites + wind_exposure + 
                                                  m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                  pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                  mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                  mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                  m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


elbw_iv = fixest::feols(I(bweight < 1000) ~ pred_pfas + asinh(pfas) +
                                                      n_sites + wind_exposure + 
                                                      m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                      pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                      mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                      mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                      m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df )


mort = fixest::feols(death ~ pred_pfas + asinh(pfas) + 
                       n_sites + wind_exposure + 
                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                       m_height + tri5 + fa_resid|county + year^month + birth_race_dsc_1, data = df)


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

df2$pred_pre = predict(pre_iv, newdata = df2)
df2$pred_lpre = predict(lpre_iv, newdata = df2)
df2$pred_vpre = predict(vpre_iv, newdata = df2)
df2$pred_epre = predict(epre_iv, newdata = df2)
df2$pred_lbw = predict(lbw_iv, newdata = df2)
df2$pred_mlbw = predict(mlbw_iv, newdata = df2)
df2$pred_vlbw = predict(vlbw_iv, newdata = df2)
df2$pred_elbw = predict(elbw_iv, newdata = df2)
df2$pred_mort = predict(mort, newdata = df2)

#get change in predicted probability from median up to median down
pre_ivc = (median(df2[which(df2$down == 1), ]$pred_pre) - median(df2[which(df2$up == 1), ]$pred_pre))/mean(df$gestation < 37) * 100
mpre_ivc = (median(df2[which(df2$down == 1), ]$pred_lpre) - median(df2[which(df2$up == 1), ]$pred_lpre))/mean(df$gestation < 37 & df$gestation >= 32) * 100
vpre_ivc = (median(df2[which(df2$down == 1), ]$pred_vpre) - median(df2[which(df2$up == 1), ]$pred_vpre))/mean(df$gestation < 32 & df$gestation >= 28) * 100
epre_ivc = (median(df2[which(df2$down == 1), ]$pred_epre) - median(df2[which(df2$up == 1), ]$pred_epre))/mean(df$gestation < 28) * 100

lbw_ivc = (median(df2[which(df2$down == 1), ]$pred_lbw) - median(df2[which(df2$up == 1), ]$pred_lbw))/mean(df$bweight < 2500) * 100
mlbw_ivc = (median(df2[which(df2$down == 1), ]$pred_mlbw) - median(df2[which(df2$up == 1), ]$pred_mlbw))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
vlbw_ivc = (median(df2[which(df2$down == 1), ]$pred_vlbw) - median(df2[which(df2$up == 1), ]$pred_vlbw))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
elbw_ivc = (median(df2[which(df2$down == 1), ]$pred_elbw) - median(df2[which(df2$up == 1), ]$pred_elbw))/mean(df$bweight < 1000) * 100

mort_ivc = (median(df2[which(df2$down == 1), ]$pred_mort) - median(df2[which(df2$up == 1), ]$pred_mort))/mean(df$death) * 100

sink(modify_path2("Tables/Revisions/iv_up_to_down_median.tex"))
print("Mortality")
print(as.numeric(mort_ivc))
print("Preterm")
print(as.numeric(pre_ivc))
print("Moderately Preterm")
print(as.numeric(mpre_ivc))
print("Very Preterm")
print(as.numeric(vpre_ivc))
print("Extremely Preterm")
print(as.numeric(epre_ivc))
print("Low Birthweight")
print(as.numeric(lbw_ivc))
print("Moderately Low Birthweight")
print(as.numeric(mlbw_ivc))
print("Very Low Birthweight")
print(as.numeric(vlbw_ivc))
print("Extremely Low Birthweight")
print(as.numeric(elbw_ivc))
sink() 


#get change in predicted probability from mean up to mean down
pre_ivc = (mean(df2[which(df2$down == 1), ]$pred_pre) - mean(df2[which(df2$up == 1), ]$pred_pre))/mean(df$gestation < 37) * 100
mpre_ivc = (mean(df2[which(df2$down == 1), ]$pred_lpre) - mean(df2[which(df2$up == 1), ]$pred_lpre))/mean(df$gestation < 37 & df$gestation >= 32) * 100
vpre_ivc = (mean(df2[which(df2$down == 1), ]$pred_vpre) - mean(df2[which(df2$up == 1), ]$pred_vpre))/mean(df$gestation < 32 & df$gestation >= 28) * 100
epre_ivc = (mean(df2[which(df2$down == 1), ]$pred_epre) - mean(df2[which(df2$up == 1), ]$pred_epre))/mean(df$gestation < 28) * 100

lbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_lbw) - mean(df2[which(df2$up == 1), ]$pred_lbw))/mean(df$bweight < 2500) * 100
mlbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_mlbw) - mean(df2[which(df2$up == 1), ]$pred_mlbw))/mean(df$bweight < 2500 & df$bweight >= 1500) * 100
vlbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_vlbw) - mean(df2[which(df2$up == 1), ]$pred_vlbw))/mean(df$bweight < 1500 & df$bweight >= 1000) * 100
elbw_ivc = (mean(df2[which(df2$down == 1), ]$pred_elbw) - mean(df2[which(df2$up == 1), ]$pred_elbw))/mean(df$bweight < 1000) * 100

mort_ivc = (mean(df2[which(df2$down == 1), ]$pred_mort) - mean(df2[which(df2$up == 1), ]$pred_mort))/mean(df$death) * 100

sink(modify_path2("Tables/Revisions/iv_up_to_down_mean.tex"))
print("Mortality")
print(as.numeric(mort_ivc))
print("Preterm")
print(as.numeric(pre_ivc))
print("Moderately Preterm")
print(as.numeric(mpre_ivc))
print("Very Preterm")
print(as.numeric(vpre_ivc))
print("Extremely Preterm")
print(as.numeric(epre_ivc))
print("Low Birthweight")
print(as.numeric(lbw_ivc))
print("Moderately Low Birthweight")
print(as.numeric(mlbw_ivc))
print("Very Low Birthweight")
print(as.numeric(vlbw_ivc))
print("Extremely Low Birthweight")
print(as.numeric(elbw_ivc))
sink() 
