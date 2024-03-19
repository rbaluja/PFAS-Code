#Primary figure (figure 2)
source("Code/PR/Figures/figure2.R")

#buffer cutoff figure (Figure 2) - This will restart R, don't run it until you don't need anything in memory!
source("Code/PR/Figures/meters_cutoff.R")

#robustness figure (Figure 3) - This will restart R, don't run it until you don't need anything in memory!
source("Code/PR/Figures/robustness_figure.R")

#contamination site downgradient watersheds (Figure S1)
source("Code/PR/Figures/cs_downstream.R")

#well distance density (Figure S2a)
source("Code/PR/Figures/distance_density.R")

#sample size by matching buffer (Figure S2b)
source("Code/PR/Figures/pop_matching_cutoff.R")


#need this to run again for this file right now
#data cleaning
source("Code/PR/Data/data_head.R")

#main analysis
source("Code/PR/Main Analysis/main_analy_head.R")
#quantiles and histogram of predicted pfas (Figure S3)
source("Code/PR/Figures/quantiles_pfas.R")
