#binary setup
source("Code/PR/Main Analysis/binary.R")

#flow accumulation at well and residence 
source("Code/PR/Data/flow_accumulation.R")

if (IV == TRUE){
  if (rerun_fs_clean == TRUE){
    source("Code/PR/Data/cont_cleaning.R")
  }
  #first stage
  source("Code/PR/Main Analysis/first_stage.R") 
}