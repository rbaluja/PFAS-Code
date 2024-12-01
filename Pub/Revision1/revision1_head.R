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
source("PFAS-Code/Pub/Revision1/main_analy_head.R")

#create tables
source("PFAS-Code/Pub/Revision1/Tables/tables.R")

#confounder check tables
source("PFAS-Code/Pub/Revision1/Tables/confounders_check.R")

#build cws cluster figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/cws_cluster.R")

#build no paternal education figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/nopeduc.R")

#get iv comparison figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2_iv_binary.R")

#make long-term resident figure
source("PFAS-Code/Pub/Revision1/Figures/ltr_figure.R")

#make spline figure
source("PFAS-Code/Pub/Revision1/Figures/dose_response_spline.R")

#make figure 2 in levels
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2.R")
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2_mort.R")
#make figure 2 logit
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2_logit.R")
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/figure2_mort_logit.R")

#make iv of covariates on down and updown table
source("PFAS-Code/Pub/Revision1/Tables/iv_covars_down_up.R")

#build no pease afb figure
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/robustness_v/npease.R")

#build IV figure
source("PFAS-Code/Pub/Figures/iv_figure.R")
