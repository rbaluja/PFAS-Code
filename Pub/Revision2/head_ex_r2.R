setwd("~/Dropbox/PFAS Infants")

source("PFAS-Code/Pub/Revision2/Bootstrap/bootstrap_iv.R")
rm(list = ls())
gc()
sys_levelbin = FALSE
source("PFAS-Code/Pub/Revision2/revision2.R")
rm(list = ls())
gc()
sys_levelbin = TRUE
source("PFAS-Code/Pub/Revision2/revision2.R")
rm(list = ls())
gc()
sys_levelbin = FALSE
source("PFAS-Code/Pub/Revision2/Surface Water/surface_water.R")