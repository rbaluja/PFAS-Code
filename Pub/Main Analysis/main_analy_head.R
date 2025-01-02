#binary setup
source("PFAS-Code/Pub/Main Analysis/binary.R")

#flow accumulation at well and residence 
source("PFAS-Code/Pub/Main Analysis/flow_accumulation.R")


#create estimation data
df_est = df[which(!is.na(df$dist) & df$dist <= 5000), ]
df_est = df_est %>% 
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

if (drop_states){
  source("PFAS-Code/Pub/Robustness/drop_near_state/drop_near_state.R")
}

if (relaxed_up){
  source("PFAS-Code/Pub/Robustness/relaxed_up/relaxed_up_robustness.R")
}


if (IV){
  source("PFAS-Code/Pub/Main Analysis/first_stage.R") 
}