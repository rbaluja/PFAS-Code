#binary setup
source("PFAS-Code/Pub/Main Analysis/binary.R")

#flow accumulation at well and residence 
source("PFAS-Code/Pub/Main Analysis/flow_accumulation.R")

if (drop_states){
  source("PFAS-Code/Pub/Robustness/drop_near_state/drop_near_state.R")
}

if (relaxed_up){
  source("PFAS-Code/Pub/Robustness/relaxed_up/relaxed_up_robustness.R")
}


if (IV){
  source("PFAS-Code/Pub/Main Analysis/first_stage.R") 
}