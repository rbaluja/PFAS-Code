setwd("~/Dropbox/PFAS Infants")
#run bootstrap for continuous health outcomes
source("PFAS-Code/Pub/Revision1/Bootstrap/bootstrap_iv.R")
#get tables and figures which don't change the environment
source("PFAS-Code/Pub/Revision1/revision1_head.R")
#make balance table across surface water, groundwater, and domestic wells
source("PFAS-Code/Pub/Revision1/balance_sw_gw_dw_est.R")
#create figure for changing the threshold of a contaminated site
source("PFAS-Code/Pub/Revision1/change_thresh.R")
#create figure of the first stage throughout NH
source("PFAS-Code/Pub/Revision1/pred_pfas_map.R")