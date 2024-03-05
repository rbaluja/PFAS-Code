df = df[which(!is.na(df$dist) & df$dist <= 5000), ]
df = df %>% 
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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$lbw = as.numeric(df$bweight < 2500)
r2_tilde = 1 - (inter_r$ssr)/(sum((df$lbw - mean(df$lbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 2500) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df$lbw - mean(df$lbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$lbw)
var_x = var(df$down)

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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$llbw = as.numeric(df$bweight < 2500 & df$bweight >= 1500)
r2_tilde = 1 - (inter_r$ssr)/(sum((df$llbw - mean(df$llbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df$llbw - mean(df$llbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$llbw)
var_x = var(df$down)

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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$mlbw = as.numeric(df$bweight < 1500 & df$bweight >= 1000)
r2_tilde = 1 - (inter_r$ssr)/(sum((df$mlbw - mean(df$mlbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df$mlbw - mean(df$mlbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$mlbw)
var_x = var(df$down)

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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$vlbw = as.numeric(df$bweight < 1000)
r2_tilde = 1 - (inter_r$ssr)/(sum((df$vlbw - mean(df$vlbw))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(bweight < 1000) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (short_r$ssr)/(sum((df$vlbw - mean(df$vlbw))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$vlbw)
var_x = var(df$down)

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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$preterm = as.numeric(df$gestation < 37)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df$preterm - mean(df$preterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 37) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df$preterm - mean(df$preterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$preterm)
var_x = var(df$down)

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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$lpreterm = as.numeric(df$gestation < 37 & df$gestation >= 32)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df$lpreterm - mean(df$lpreterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df$lpreterm - mean(df$lpreterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$lpreterm)
var_x = var(df$down)

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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$mpreterm = as.numeric(df$gestation < 32 & df$gestation >= 28)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df$mpreterm - mean(df$mpreterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df$mpreterm - mean(df$mpreterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$mpreterm)
var_x = var(df$down)

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
                        |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

b_tilde = inter_r$coefficients["down"]
df$vpreterm = as.numeric(df$gestation < 28)
r2_tilde = 1 - (sum(inter_r$ssr))/(sum((df$vpreterm - mean(df$vpreterm))^2))

#max r^2
rmax = 1.3 * r2_tilde

#short regression
short_r = fixest::feols(I(gestation < 28) ~  updown + down|county + year^month, data = df, warn = F, notes = F)
b_dot = short_r$coefficients["down"]
short_r2 = 1 - (sum(short_r$ssr))/(sum((df$vpreterm - mean(df$vpreterm))^2))

#auxiliary regression
aux_r = fixest::feols(down ~  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))

tau_x = var(aux_r$residuals)

#general setup
var_y = var(df$vpreterm)
var_x = var(df$down)

d_vpre = ((b_tilde - 0) * (r2_tilde - short_r2) * var_y * tau_x + 
            (b_tilde - 0) * var_x * tau_x * (b_dot - b_tilde)^2 + 
            2 * (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
            (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))/
  ((1.3 * r2_tilde - r2_tilde) * var_y * (b_dot - b_tilde) * var_x + 
     (b_tilde - 0) * (1.3 * r2_tilde - r2_tilde) * var_y * (var_x - tau_x) + 
     (b_tilde - 0)^2 * (tau_x * (b_dot - b_tilde) * var_x) + 
     (b_tilde - 0)^3 * (tau_x * var_x - tau_x^2))


#table S-5
d_pre
d_lpre
d_mpre
d_vpre
d_lbw
d_llbw
d_mlbw
d_vlbw