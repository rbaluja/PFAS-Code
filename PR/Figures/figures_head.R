#figure1 contamination sites
source("PFAS-Code/PR/Figures/figure1_sites.R")

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
  stop("Need to run Robustness/resid_side_comparison.R")
}
source("PFAS-Code/PR/Figures/figure2.R")


#contamination site downgradient watersheds (Figure S1)
source("PFAS-Code/PR/Figures/cs_downstream.R")

#well distance density (Figure S2a)
source("PFAS-Code/PR/Figures/distance_density.R")

#sample size by matching buffer (Figure S2b)
source("PFAS-Code/PR/Figures/pop_matching_cutoff.R")

