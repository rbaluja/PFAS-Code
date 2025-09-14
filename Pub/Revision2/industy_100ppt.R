source("PFAS-Code/Pub/config.R")
#set up environment
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
tables = TRUE
figures = TRUE
bs_cov = TRUE #bootstrap covariance matrix for IV
ppt = 100

#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")

#main analysis
source("PFAS-Code/Pub/Revision2/main_analy_head.R")

source("PFAS-Code/Pub/Revision2/Tables/industry_100ppt_tables.R")