source("PFAS-Code/Pub/config.R")
#set up environment
drop_states = TRUE #running spec where we drop sites within meters of state border?
IV = FALSE
relaxed_up = FALSE #relaxed upgradient robustness spec?

#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")

#main analysis
source("PFAS-Code/Pub/Revision1/main_analy_head.R")