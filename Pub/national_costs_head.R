#clean raw births data
if (nb_cbg){
  source("PFAS-Code/Pub/National Costs/nat_births.R") 
  births = cbg_births
  rm(cbg_births)
}else{
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character", tract = "character"))
}

#get watershed for each cont site
if (nat_run_cont_ws){
  source("PFAS-Code/Pub/National Costs/nat_watersheds.R")  
  cont_ws = get(load(modify_path("Data_Verify/RData/nat_cont_watershed.RData")))
  births_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character"))
}else{
  #load in watersheds
  cont_ws = get(load(modify_path("Data_Verify/RData/nat_cont_watershed.RData")))
  births_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
}

#assign cbg to sites
if (nat_reassn){
  source("PFAS-Code/Pub/National Costs/nat_assn.R")
}else{
  load(modify_path("Data_Verify/National/births_sites_assigned.RData"))
}

#run national costs (primary - Figure 3 part 1)
source("PFAS-Code/Pub/National Costs/nat_costs.R")

#national costs map (figure 3 part 2)
source("PFAS-Code/Pub/Figures/figure3_map.R")

#national costs map preterm (figure 3)
source("PFAS-Code/Pub/Figures/figure3_map_preterm.R")

#full national costs (figure 3)
source("PFAS-Code/Pub/Figures/full_natcost_figure.R")

#map of national release sites and pop density (Figure S4)
source("PFAS-Code/Pub/National Costs/nat_map.R")

#national costs (binary - Figure S5)
source("PFAS-Code/Pub/National Costs/nat_costs_binary.R")
