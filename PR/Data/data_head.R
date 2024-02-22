#obtain theta info for Northeastern contamination data
source("Code/PR/Data/pfas_lab_sites.R")

#well location and service area data (NHDES)
source("Code/PR/Data/NHDES_PWS.R")

#set up wind
source("Code/PR/Main Analysis/wind.R")


if (run_cleaning == TRUE){
  #read in (and clean) natality data
  source('Code/PR/Data/natality_data.R')
  
  #get covariates for birth records
  source("Code/PR/Data/birth_covars.R")
  
  #combine the well and natality data
  source("Code/PR/Data/natality_wells.R")
  
  #get elevation at relevant well and residence
  source("Code/PR/Data/elev_setup.R")
  
}else if (match_wells == TRUE){
  load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_wdem122023.RData")
  #combine the well and natality data
  source("Code/PR/Data/natality_wells.R")
  
  #get elevation at relevant well and residence
  source("Code/PR/Data/elev_setup.R")
}else if (old_wells == FALSE){
  load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_matched122023.RData") 
}else{
  load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_matched_oldwells122023.RData")
}

if (domestic == FALSE){
  df = df[df$sys_id != "Domestic Well", ] #50874 individuals on domestic water
}

if (IV == TRUE){
  if (rerun_fs_clean == TRUE){
    source("PFAS-Code/PR/Data/cont_cleaning.R")
  }
}