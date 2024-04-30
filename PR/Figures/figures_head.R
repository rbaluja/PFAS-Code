#predicted PFAS map
source("PFAS-Code/PR/Figures/pred_pfas_map.R")

#figure1 sitemap
source("PFAS-Code/PR/Figures/figure1_sitemap.R")

#figure1 binned data
source("PFAS-Code/PR/Figures/figure1_bindata.R")

#Robustness Forest plot (figure 2)
if (!file.exists(modify_path("Data_Verify/Robustness/drop_nearby_state_robustness.RData"))){
  stop("Need to run drop nearby states robustness: run infant_health_head through main_analysis with drop states true")
}

if (!file.exists(modify_path("Data_Verify/Robustness/relaxed_up_robust.RData"))){
  stop("Need to run relaxed upgradient robustness: run infant_health_head through main_analysis with relaxed_up true")
}

if (!file.exists(modify_path("Data_Verify/Robustness/side_robustness.RData"))){
  stop("Need to run PR/GIS/df_watershed.R and then Robustness/resid_side_comparison.R")
}
source("PFAS-Code/PR/Figures/figure2.R")

#IV figure
if (!file.exists(modify_path("Data_Verify/RData/preterm_sd.RData")) | 
    !file.exists(modify_path("Data_Verify/RData/lbw_sd.RData"))) {
  stop("Please run bootstrap_iv.R to generate the standard errors.")
}
source("PFAS-Code/PR/Figures/iv_figure.R")

#well distance density (Figure S2a)
source("PFAS-Code/PR/Figures/distance_density.R")

#DEM figure 1
source("PFAS-Code/PR/Figures/figure1_dem.R")

if (rob_app_fig){
  source("PFAS-Code/PR/Figures/robustness_figure.R")
}

#Upgradient version of figure 2
if (!code_check){ #code_check natality data does not have "up" data
  source("PFAS-Code/PR/Figures/figure2_upgradient.R") 
}

#stillborn version of figure 2
source("PFAS-Code/PR/Figures/figure2_stillborn.R")

#low birthweight amongst full term version of figure 2
source("PFAS-Code/PR/Figures/figure2_lbw_ft.R")

