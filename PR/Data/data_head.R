#obtain theta info for Northeastern contamination data
source("PFAS-Code/PR/Data/pfas_lab_sites.R")

#well location and service area data (NHDES)
source("PFAS-Code/PR/Data/NHDES_PWS.R")

#set up wind
source("PFAS-Code/PR/Data/wind.R")

if (code_check == TRUE){
 
  source("PFAS-Code/PR/Data/fake_natality.R")
  
  #get covariates for birth records
  source("PFAS-Code/PR/Data/birth_covars.R")
  
  #match residences to water wells
  source("PFAS-Code/PR/Data/natality_wells.R")
  
  #get elevation at relevant well and residence
  source("PFAS-Code/PR/Data/elev_setup.R")
}else{
  if (run_cleaning == TRUE){
    
    #read in (and clean) natality data
    source('PFAS-Code/PR/Data/natality_data.R') 
    
    #get covariates for birth records
    source("PFAS-Code/PR/Data/birth_covars.R")
    
    #match residences to water wells
    source("PFAS-Code/PR/Data/natality_wells.R")
    
    #get elevation at relevant well and residence
    source("PFAS-Code/PR/Data/elev_setup.R")
    
  }else if (match_wells == TRUE ){
    load(paste0(natality_path, "[UA Box Health] birth_records_wdem_prematch.RData"))
    #match residences to water wells
    source("PFAS-Code/PR/Data/natality_wells.R")
    
    #get elevation at relevant well and residence
    source("PFAS-Code/PR/Data/elev_setup.R")
  }else{
    load(paste0(natality_path, "[UA Box Health] birth_records_matched.RData")) 
  } 
}
c
if (domestic == FALSE){
  df = df[df$sys_id != "Domestic Well", ] #50874 individuals on domestic water
}

if (IV == TRUE){
  if (rerun_fs_clean == TRUE){
    source("PFAS-Code/PR/Data/cont_cleaning.R")
  }
}