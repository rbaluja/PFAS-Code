#obtain theta info for Northeastern contamination data
source("PFAS-Code/Pub/Data/pfas_lab_sites.R")

#well location and service area data (NHDES)
source("PFAS-Code/Pub/Data/NHDES_PWS.R")

#set up wind
source("PFAS-Code/Pub/Data/wind.R")

if (code_check == TRUE){
 
  source("PFAS-Code/Pub/Data/fake_natality.R")
  
  #get covariates for birth records
  source("PFAS-Code/Pub/Data/birth_covars.R")
  
  #match residences to water wells
  source("PFAS-Code/Pub/Data/natality_wells.R")
  
  #get elevation at relevant well and residence
  source("PFAS-Code/Pub/Data/elev_setup.R")
}else{
  if (run_cleaning == TRUE | !file.exists(load(paste0(natality_path, "[UA Box Health] birth_records_matched_cv_", code_verify, ".RData")))){
    
    #read in (and clean) natality data
    source('PFAS-Code/Pub/Data/natality_data.R') 
    
    #get covariates for birth records
    source("PFAS-Code/Pub/Data/birth_covars.R")
    
    #match residences to water wells
    source("PFAS-Code/Pub/Data/natality_wells.R")
    
    #get elevation at relevant well and residence
    source("PFAS-Code/Pub/Data/elev_setup.R")
    
    save(df, file = paste0(natality_path, "[UA Box Health] birth_records_matched_cv_", code_verify, ".RData"))
    
  }else{
    load(paste0(natality_path, "[UA Box Health] birth_records_matched_cv_", code_verify, ".RData")) 
  } 
}

df = df[df$sys_id != "Domestic Well", ] #50874 individuals on domestic water

#merge in death records for infant mortality
path = paste0(natality_path, "[UA Box Health] VR2210_Deliverable/dr_6264_deliverable.xlsx")
df_d = read_excel(path, sheet = 3)

df = df %>% 
  left_join(df_d %>% 
              dplyr::rename(age_death = DECD_AGE_YR, 
                            manner_death = CERTFR_MANNER_DTH_CD, 
                            id = BRTH_CERT_FILE_NBR))

df$death = as.numeric(!is.na(df$age_death))

#save(df, file = paste0(natality_path, "[UA Box Health] birth_records_matched_mortality.RData"))