#binary setup
source("PFAS-Code/PR/Main Analysis/binary.R")

#flow accumulation at well and residence 
source("PFAS-Code/PR/Main Analysis/flow_accumulation.R")

if (IV == TRUE){
  #first stage
  source("PFAS-Code/PR/Main Analysis/first_stage.R") 
}

if (drop_states == TRUE){
  source("PFAS-Code/PR/Robustness/drop_near_state.R")
}

if (relaxed_up == TRUE){
  source("PFAS-Code/PR/Robustness/relaxed_up_robustness.R")
}
