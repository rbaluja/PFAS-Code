#binary setup
source("PFAS-Code/PR/Main Analysis/binary.R")

#flow accumulation at well and residence 
source("PFAS-Code/PR/Main Analysis/flow_accumulation.R")

if (IV == TRUE){
  #first stage
  source("PFAS-Code/PR/Main Analysis/first_stage.R") 
}
