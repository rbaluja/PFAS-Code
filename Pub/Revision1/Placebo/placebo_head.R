source("PFAS-Code/Pub/config.R")
set.seed(1)
wbt_verbose(FALSE)
drop_states = FALSE
relaxed_up = FALSE

#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")

#bring in rural dummy
urban_s = tigris::urban_areas() %>% 
  st_transform(32110) %>% 
  dplyr::mutate(urban = 1)

df = 
  df %>% 
  st_transform(st_crs(urban_s)) %>% 
  st_join(urban_s %>% dplyr::select(urban))
df[which(is.na(df$urban)), ]$urban = 0
df$rural = as.numeric(df$urban == 0)

nh_shape = tigris::states() %>% 
  dplyr::filter(STUSPS == "NH") %>% 
  st_transform(32110)

#fill sinks
wbt_breach_depressions("Data_Verify_Revision/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff", "Data_Verify_Revision/GIS/filled_dem.tiff")

#flow accumulation
wbt_d8_flow_accumulation("Data_Verify_Revision/GIS/filled_dem.tiff", "Data_Verify_Revision/GIS/flow_acc.tiff")

#flow direction
wbt_d8_pointer("Data_Verify_Revision/GIS/filled_dem.tiff", "Data_Verify_Revision/GIS/flow_dir.tiff")

#read in placebo functions
source("PFAS-Code/Pub/Revision1/Placebo/placebo_functions.R")

if (rerun_placebos == TRUE){
  placebos_1 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_1, file = "Data_Verify_Revision/RData/placebos_1.RData")
  
  placebos_2 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_2, file = "Data_Verify_Revision/RData/placebos_2.RData")
  
  placebos_3 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_3, file = "Data_Verify_Revision/RData/placebos_3.RData")
  
  placebos_4 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_4, file = "Data_Verify_Revision/RData/placebos_4.RData")
  
  placebos_5 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_5, file = "Data_Verify_Revision/RData/placebos_5.RData")
  
  placebos_6 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_6, file = "Data_Verify_Revision/RData/placebos_6.RData")
  
  placebos_7 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_7, file = "Data_Verify/RData/placebos_7.RData")
  
  placebos_8 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_8, file = "Data_Verify_Revision/RData/placebos_8.RData")
  
  placebos_9 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_9, file = "Data_Verify_Revision/RData/placebos_9.RData")
  
  placebos_10 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_10, file = "Data_Verify_Revision/RData/placebos_10.RData")
  

  
  plac = NULL
  # Loop through the files
  for (i in 1:10) {
    #create a new environment for loading the file
    env = new.env()
    
    #load the file into the new environment
    load(paste0("Data_Verify_Revision/RData/placebos_", i, ".RData"), envir = env)
    
    object_name = ls(env)
    
    current_placebo = get(object_name, env)
    
    # bind plac with current_placebo
    if(is.null(plac)) {
      plac = current_placebo
    } else {
      plac = plyr::rbind.fill(plac, current_placebo)
    }
  }
  
  #check for missing iterations, and redo them.
  n_na = length(which(is.na(plac$preterm)))
  while (n_na > 0){
    plac = plac %>%
      tidyr::drop_na(preterm)
  
    plac_na = dplyr::bind_rows(pblapply(1:n_na, placebo, df, wells))
    plac = plyr::rbind.fill(plac, plac_na)
    n_na = length(which(is.na(plac$preterm))) 
  }
  
  save(plac, file = "Data_Verify_Revision/RData/placebos_mort.RData" )
}else{
  load("Data_Verify_Revision/RData/placebos_mort.RData" )
}

#This is the input to table S-5. It counts the number of runs with sig positive coef estimates
sink(modify_path2("Tables/Revision 1/placebo_down_cor.tex"))
plac$m_age_sig_pos = as.numeric(plac$m_age/plac$m_age_se > 1.959964)
plac$m_age_sig_neg = as.numeric(plac$m_age/plac$m_age_se < -1.959964)

plac$m_married_sig_pos = as.numeric(plac$m_married/plac$m_married_se > 1.959964)
plac$m_married_sig_neg = as.numeric(plac$m_married/plac$m_married_se < -1.959964)

plac$private_insurance_sig_pos = as.numeric(plac$private_insurance/plac$private_insurance_se > 1.959964)
plac$private_insurance_sig_neg = as.numeric(plac$private_insurance/plac$private_insurance_se < -1.959964)

plac$nbr_cgrtt_sig_pos = as.numeric(plac$nbr_cgrtt/plac$nbr_cgrtt_se > 1.959964)
plac$nbr_cgrtt_sig_neg = as.numeric(plac$nbr_cgrtt/plac$nbr_cgrtt_se < -1.959964)

plac$m_educ_sig_pos = as.numeric(plac$m_educ/plac$m_educ_se > 1.959964)
plac$m_educ_sig_neg = as.numeric(plac$m_educ/plac$m_educ_se < -1.959964)

plac$f_educ_sig_pos = as.numeric(plac$f_educ/plac$f_educ_se > 1.959964)
plac$f_educ_sig_neg = as.numeric(plac$f_educ/plac$f_educ_se < -1.959964)

plac$mr_04_sig_pos = as.numeric(plac$mr_04/plac$mr_04_se > 1.959964)
plac$mr_04_sig_neg = as.numeric(plac$mr_04/plac$mr_04_se < -1.959964)

plac$mr_18_sig_pos = as.numeric(plac$mr_18/plac$mr_18_se > 1.959964)
plac$mr_18_sig_neg = as.numeric(plac$mr_18/plac$mr_18_se < -1.959964)

plac$mr_08_sig_pos = as.numeric(plac$mr_08/plac$mr_08_se > 1.959964)
plac$mr_08_sig_neg = as.numeric(plac$mr_08/plac$mr_08_se < -1.959964)

plac$mr_21_sig_pos = as.numeric(plac$mr_21/plac$mr_21_se > 1.959964)
plac$mr_21_sig_neg = as.numeric(plac$mr_21/plac$mr_21_se < -1.959964)

plac$mr_26_sig_pos = as.numeric(plac$mr_26/plac$mr_26_se > 1.959964)
plac$mr_26_sig_neg = as.numeric(plac$mr_26/plac$mr_26_se < -1.959964)

plac$mr_27_sig_pos = as.numeric(plac$mr_27/plac$mr_27_se > 1.959964)
plac$mr_27_sig_neg = as.numeric(plac$mr_27/plac$mr_27_se < -1.959964)

plac$mthr_wgt_dlv_sig_pos = as.numeric(plac$mthr_wgt_dlv/plac$mthr_wgt_dlv_se > 1.959964)
plac$mthr_wgt_dlv_sig_neg = as.numeric(plac$mthr_wgt_dlv/plac$mthr_wgt_dlv_se < -1.959964)

plac$mthr_pre_preg_wgt_sig_pos = as.numeric(plac$mthr_pre_preg_wgt/plac$mthr_pre_preg_wgt_se > 1.959964)
plac$mthr_pre_preg_wgt_sig_neg = as.numeric(plac$mthr_pre_preg_wgt/plac$mthr_pre_preg_wgt_se < -1.959964)

plac$m_height_sig_pos = as.numeric(plac$m_height/plac$m_height_se > 1.959964)
plac$m_height_sig_neg = as.numeric(plac$m_height/plac$m_height_se < -1.959964)

plac$med_hprice_sig_pos = as.numeric(plac$med_hprice/plac$med_hprice_se > 1.959964)
plac$med_hprice_sig_neg = as.numeric(plac$med_hprice/plac$med_hprice_se < -1.959964)

plac$med_inc_sig_pos = as.numeric(plac$med_inc/plac$med_inc_se > 1.959964)
plac$med_inc_sig_neg = as.numeric(plac$med_inc/plac$med_inc_se < -1.959964)

plac$rural_sig_pos = as.numeric(plac$rural/plac$rural_se > 1.959964)
plac$rural_sig_neg = as.numeric(plac$rural/plac$rural_se < -1.959964)

plac$well_elev_sig_pos = as.numeric(plac$well_elev/plac$well_elev_se > 1.959964)
plac$well_elev_sig_neg = as.numeric(plac$well_elev/plac$well_elev_se < -1.959964)

plac$resid_elev_sig_pos = as.numeric(plac$resid_elev/plac$resid_elev_se > 1.959964)
plac$resid_elev_sig_neg = as.numeric(plac$resid_elev/plac$resid_elev_se < -1.959964)

plac$temp_sig_pos = as.numeric(plac$temp/plac$temp_se > 1.959964)
plac$temp_sig_neg = as.numeric(plac$temp/plac$temp_se < -1.959964)

plac$pm25_sig_pos = as.numeric(plac$pm25/plac$pm25_se > 1.959964)
plac$pm25_sig_neg = as.numeric(plac$pm25/plac$pm25_se < -1.959964)

print("Mother's Age Positive")
sum(plac$m_age_sig_pos)/1000
print("Mother's Age Negative")
sum(plac$m_age_sig_neg)/1000
print("Mother Married Positive")
sum(plac$m_married_sig_pos)/1000
print("Mother Married Negative")
sum(plac$m_married_sig_neg)/1000
print("Private Insurance Positive")
sum(plac$private_insurance_sig_pos)/1000
print("Private Insurance Negative")
sum(plac$private_insurance_sig_neg)/1000
print("Number of Cigarettes Positive")
sum(plac$nbr_cgrtt_sig_pos)/1000
print("Number of Cigarettes Negative")
sum(plac$nbr_cgrtt_sig_neg)/1000
print("Mother's Education Positive")
sum(plac$m_educ_sig_pos)/1000
print("Mother's Education Negative")
sum(plac$m_educ_sig_neg)/1000
print("Father's Education Positive")
sum(plac$f_educ_sig_pos)/1000
print("Father's Education Negative")
sum(plac$f_educ_sig_neg)/1000
print("Pre-Pregancy Diabetes Positive")
sum(plac$mr_04_sig_pos)/1000
print("Pre-Pregancy Diabetes Negative")
sum(plac$mr_04_sig_neg)/1000
print("Gestational Diabetes Positive")
sum(plac$mr_18_sig_pos)/1000
print("Gestational Diabetes Negative")
sum(plac$mr_18_sig_neg)/1000
print("Hypertension Positive")
sum(plac$mr_08_sig_pos)/1000
print("Hypertension Negative")
sum(plac$mr_08_sig_neg)/1000
print("Previous C-Section Positive")
sum(plac$mr_21_sig_pos)/1000
print("Previous C-Section Negative")
sum(plac$mr_21_sig_neg)/1000
print("Fertility Enhancing Drugs Positive")
sum(plac$mr_26_sig_pos)/1000
print("Fertility Enhancing Drugs Negative")
sum(plac$mr_26_sig_neg)/1000
print("Invitro Fertilization Positive")
sum(plac$mr_27_sig_pos)/1000
print("Invitro Fertilization Negative")
sum(plac$mr_27_sig_neg)/1000
print("Mother's Weight at Delivery Positive")
sum(plac$mthr_wgt_dlv_sig_pos)/1000
print("Mother's Weight at Delivery Negative")
sum(plac$mthr_wgt_dlv_sig_neg)/1000
print("Mother's Pre-Pregnancy Weight Positive")
sum(plac$mthr_pre_preg_wgt_sig_pos)/1000
print("Mother's Pre-Pregnancy Weight Negative")
sum(plac$mthr_pre_preg_wgt_sig_neg)/1000
print("Mother's Height Positive")
sum(plac$m_height_sig_pos)/1000
print("Mother's Height Negative")
sum(plac$m_height_sig_neg)/1000
print("Median House Price Positive")
sum(plac$med_hprice_sig_pos)/1000
print("Median House Price Negative")
sum(plac$med_hprice_sig_neg)/1000
print("Median Income Positive")
sum(plac$med_inc_sig_pos)/1000
print("Median Income Negative")
sum(plac$med_inc_sig_neg)/1000
print("Rural Positive")
sum(plac$rural_sig_pos)/1000
print("Rural Negative")
sum(plac$rural_sig_neg)/1000
print("Well Elevation Positive")
sum(plac$well_elev_sig_pos)/1000
print("Well Elevation Negative")
sum(plac$well_elev_sig_neg)/1000
print("Residential Elevation Positive")
sum(plac$resid_elev_sig_pos)/1000
print("Residential Elevation Negative")
sum(plac$resid_elev_sig_neg)/1000
print("Temperature Positive")
sum(plac$temp_sig_pos)/1000
print("Temperature Negative")
sum(plac$temp_sig_neg)/1000
print("PM2.5 Positive")
sum(plac$pm25_sig_pos)/1000
print("PM2.5 Negative")
sum(plac$pm25_sig_neg)/1000
sink()