#functions
source("PFAS-Code/PR/Main Analysis/functions.R")

#binary setup
source("PFAS-Code/PR/Main Analysis/binary.R")

#flow accumulation at well and residence 
source("PFAS-Code/PR/Data/flow_accumulation.R")

if (IV == TRUE){
  #first stage
  source("PFAS-Code/PR/Main Analysis/first_stage.R") 
}