setwd("~/Dropbox/PFAS Infants")
#run bootstrap for continuous health outcomes
source("PFAS-Code/Pub/Revision1/Bootstrap/bootstrap_iv.R")
rm(list = ls())
#get tables and figures which don't change the environment
source("PFAS-Code/Pub/Revision1/revision1_head.R")
rm(list = ls())
#make balance table across surface water, groundwater, and domestic wells
source("PFAS-Code/Pub/Revision1/Tables/balance_sw_gw_dw_est.R")
rm(list = ls())
#create figure for changing the threshold of a contaminated site
source("PFAS-Code/Pub/Revision1/Figures/change_thresh.R")
rm(list = ls())
#create figure of the first stage throughout NH
#source("PFAS-Code/Pub/Revision1/Figures/pred_pfas_map.R")
rm(list = ls())
#create binary cost figure from https://doi.org/10.1126/science.ado6638
source("PFAS-Code/Pub/Revision1/National/xgboost_rob.R")
rm(list = ls())
#logit version of Figure 2
source("PFAS-Code/Pub/Revision1/Robustness/drop_near_state/drop_near_state_head.R")
rm(list = ls())
source("PFAS-Code/Pub/Revision1/Robustness/relaxed_up/relaxed_up_head.R")
rm(list = ls())
source("PFAS-Code/Pub/Revision1/Figures/figure2_fn.R")



