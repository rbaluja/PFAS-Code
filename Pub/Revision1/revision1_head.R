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

#create tables
source("PFAS-Code/Pub/Revision1/Tables/tables.R")

#confounder check tables
source("PFAS-Code/Pub/Revision1/Tables/confounders_check.R")

#get iv comparison tables
source("PFAS-Code/Pub/Revision1/Tables/iv_binary_comparison_tab.R")

#make long-term resident figure
source("PFAS-Code/Pub/Revision1/Figures/ltr_figure.R")

#make spline figure
source("PFAS-Code/Pub/Revision1/Figures/dose_response_spline.R")
