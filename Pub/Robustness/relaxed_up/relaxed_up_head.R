source("PFAS-Code/Pub/config.R")
#set up environment
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = TRUE #relaxed upgradient robustness spec?

#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")

#main analysis
source("PFAS-Code/Pub/Main Analysis/main_analy_head.R")
