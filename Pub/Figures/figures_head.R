#figure1 sitemap
source("PFAS-Code/Pub/Figures/figure1_sitemap.R")

#Robustness Forest plot (figure 2)
if (!file.exists(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness.RData"))){
  stop("Need to run Pub/Robustness/drop_near_state/drop_near_state_head.R")
}

if (!file.exists(modify_path("Data_Verify/Robustness/relaxed_up_robust.RData"))){
  stop("Need to run Pub/Robustness/relaxed_up/relaxed_up_head.R")
}


source("PFAS-Code/Pub/Figures/figure2.R")

#IV figure
if (!file.exists(modify_path("Data_Verify/RData/preterm_sd.RData")) | 
    !file.exists(modify_path("Data_Verify/RData/lbw_sd.RData"))) {
  stop("Please run bootstrap_iv.R to generate the standard errors.")
}
source("PFAS-Code/Pub/Figures/iv_figure.R")

#well distance density (Figure S2a)
source("PFAS-Code/Pub/Figures/distance_density.R")

#DEM figure 1
source("PFAS-Code/Pub/Figures/ray_figure1.R")


#Upgradient version of figure 2
if (!code_check){ #code_check natality data does not have "up" data
  source("PFAS-Code/Pub/Figures/figure2_upgradient.R") 
}

#stillborn version of figure 2
source("PFAS-Code/Pub/Figures/figure2_mort.R")

#low birthweight amongst full term version of figure 2
source("PFAS-Code/Pub/Figures/figure2_lbw_ft.R")

