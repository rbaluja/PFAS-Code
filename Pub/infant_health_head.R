source("PFAS-Code/Pub/config.R")
#set up environment
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
tables = TRUE
figures = TRUE
bs_cov = TRUE #bootstrap covariance matrix for IV

#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")

#main analysis
source("PFAS-Code/Pub/Main Analysis/main_analy_head.R")

if (tables){
  source("PFAS-Code/Pub/Tables/tables.R")
}

if (figures){
  source("PFAS-Code/Pub/Figures/figures_head.R") 
}
