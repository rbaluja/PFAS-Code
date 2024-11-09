#binary setup
source("PFAS-Code/Pub/Revision1/binary.R")

#flow accumulation at well and residence 
source("PFAS-Code/Pub/Main Analysis/flow_accumulation.R")


if (drop_states){
  source("PFAS-Code/Pub/Revision1/Robustness/drop_near_state/drop_near_state.R")
}

if (relaxed_up){
  source("PFAS-Code/Pub/Revision1/Robustness/relaxed_up/relaxed_up_robustness.R")
}