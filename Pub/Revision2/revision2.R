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
source("PFAS-Code/Pub/Revision2/main_analy_head.R")

if (!sys_levelbin){
#PWS FE figure
source("PFAS-Code/Pub/Revision2/Figures/pws_fe.R") 

#make tables
source("PFAS-Code/Pub/Revision2/Tables/tables.R")}else{
  
  source("PFAS-Code/Pub/Revision2/Tables/binary_sys_table.R")
  
}
