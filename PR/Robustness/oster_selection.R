df_ost = df[which(!is.na(df$dist) & df$dist <= 5000), ]
df_ost = df_ost %>% 
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

###Low Birthweight

#intermediate regression
inter_r = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$lbw = as.numeric(df_ost$bweight < 2500)
r2_tilde = 1 - (inter_r$ssr)/(sum((df_ost$lbw - mean(df_ost$lbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 2500) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df_ost$lbw - mean(df_ost$lbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$lbw)
var_x = var(df_ost$down)

d_lbw = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
                    (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
                    2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
                    (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((rmax - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (rmax - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))



#high lbw
inter_r = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$llbw = as.numeric(df_ost$bweight < 2500 & df_ost$bweight >= 1500)
r2_tilde = 1 - (inter_r$ssr)/(sum((df_ost$llbw - mean(df_ost$llbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df_ost$llbw - mean(df_ost$llbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$llbw)
var_x = var(df_ost$down)

d_llbw = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
           (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
           2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
           (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))


#moderate lbw
inter_r = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$mlbw = as.numeric(df_ost$bweight < 1500 & df_ost$bweight >= 1000)
r2_tilde = 1 - (inter_r$ssr)/(sum((df_ost$mlbw - mean(df_ost$mlbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df_ost$mlbw - mean(df_ost$mlbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$mlbw)
var_x = var(df_ost$down)

d_mlbw = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
            (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
            2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
            (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))

#extreme low birthweight
inter_r = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$vlbw = as.numeric(df_ost$bweight < 1000)
r2_tilde = 1 - (inter_r$ssr)/(sum((df_ost$vlbw - mean(df_ost$vlbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 1000) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df_ost$vlbw - mean(df_ost$vlbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$vlbw)
var_x = var(df_ost$down)

d_vlbw = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
            (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
            2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
            (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))


#preterm
#intermediate regression
inter_r = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$preterm = as.numeric(df_ost$gestation < 37)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df_ost$preterm - mean(df_ost$preterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 37) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df_ost$preterm - mean(df_ost$preterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$preterm)
var_x = var(df_ost$down)

d_pre = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
                    (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
                    2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
                    (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))


#late preterm
#intermediate regression
inter_r = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$lpreterm = as.numeric(df_ost$gestation < 37 & df_ost$gestation >= 32)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df_ost$lpreterm - mean(df_ost$lpreterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df_ost$lpreterm - mean(df_ost$lpreterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$lpreterm)
var_x = var(df_ost$down)

d_lpre = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
           (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
           2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
           (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))


#moderately preterm
#intermediate regression
inter_r = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$mpreterm = as.numeric(df_ost$gestation < 32 & df_ost$gestation >= 28)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df_ost$mpreterm - mean(df_ost$mpreterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df_ost$mpreterm - mean(df_ost$mpreterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$mpreterm)
var_x = var(df_ost$down)

d_mpre = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
            (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
            2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
            (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))


#very preterm
#intermediate regression
inter_r = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 + fa_resid + wind_exposure
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df_ost$vpreterm = as.numeric(df_ost$gestation < 28)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df_ost$vpreterm - mean(df_ost$vpreterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 28) ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df_ost$vpreterm - mean(df_ost$vpreterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$vpreterm)
var_x = var(df_ost$down)

d_vpre = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
            (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
            2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
            (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))



#infant mortality
#intermediate regression
inter_r = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                          m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                          pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                          mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                          mthr_wgt_dlv +mthr_pre_preg_wgt + 
                          m_height + tri5 +fa_resid + wind_exposure 
                        |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df_ost$death - mean(df_ost$death))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(death ~  updown + down|county + year^month, data = df_ost, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df_ost$death - mean(df_ost$death))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df_ost, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df_ost$death)
var_x = var(df_ost$down)

d_mort = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
            (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
            2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
            (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))

