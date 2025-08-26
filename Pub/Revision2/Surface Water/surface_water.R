source("PFAS-Code/Pub/config.R")
#set up environment
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
tables = TRUE
figures = TRUE
IV = FALSE

#data cleaning
source("PFAS-Code/Pub/Revision2/Surface Water/Data/data_head.R")

#main analysis
source("PFAS-Code/Pub/Revision2/Surface Water/Main Analysis/main_analy_head.R")

#save table
source("PFAS-Code/Pub/Revision2/Surface Water/table.R")