setwd("~/Dropbox/PFAS Infants")
#build 500 ppt watershed
source("PFAS-Code/Pub/config.R")
ppt = 500
source("PFAS-Code/Pub/GIS/cont_watershed.R")
rm(list = ls())
#run bootstrap for continuous health outcomes
source("PFAS-Code/Pub/Revision1/Bootstrap/bootstrap_iv.R")
rm(list = ls())
#run bootstrap for 500ppt threshold
source("PFAS-Code/Pub/Revision1/Bootstrap/ppt500/bootstrap_iv.R")
rm(list = ls())
#run robustness where we drop births near the state border, logit
source("PFAS-Code/Pub/Revision1/Robustness/drop_near_state/drop_near_state_head.R")
rm(list = ls())
#run robustness where we relax the upgradient definition, logit
source("PFAS-Code/Pub/Revision1/Robustness/relaxed_up/relaxed_up_head.R")
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
source("PFAS-Code/Pub/Revision1/Figures/pred_pfas_map.R")
rm(list = ls())
#run IV setup with 500 ppt threshold, and save IV figure
source("PFAS-Code/Pub/Revision1/ppt500/iv_nh.R")
rm(list = ls())
#run placebo for correlations between downgradient and the covariates
source("PFAS-Code/Pub/Revision1/Placebo/placebo_head.R")
#get iv comparison figure for 500 ppt and 1000 ppt
source("PFAS-Code/Pub/Revision1/Figures/Figure 2/iv_500_1000.R")
#create figure of the first stage throughout NH
#source("PFAS-Code/Pub/Revision1/Figures/pred_pfas_map.R")
rm(list = ls())
#run national costs with 500 ppt as the threshold
source("PFAS-Code/Pub/Revision1/national_costs_head500.R")


