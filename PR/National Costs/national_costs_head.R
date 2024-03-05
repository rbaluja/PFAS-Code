#clean raw births data
if (nb_cbg == TRUE){
  source("PFAS-Code/PR/National Costs/nat_births.R") 
  births = cbg_births
  rm(cbg_births)
}else{
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character"))
}

#get watershed for each cont site
if (nat_run_cont_ws == TRUE){
  source("Code/PR/National Costs/nat_watersheds.R")  
}else{
  #load in watersheds
  load(modify_path("Data_Verify/GIS/National/nat_cont_watershed.RData"))
  cont_ws = wells_ws
  load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData"))
}

#assign cbg to sites
if (nat_reassn == TRUE){
  source("PFAS-Code/PR/National Costs/nat_assn.R")
}else{
  births = fread(modify_path("Data_Verify/National/births_sites_assigned5.csv"))
}

#run national costs (primary - Figure 4)
source("Code/PR/National Costs/nat_costs.R")

#map of national release sites and pop density (Figure S4)
source("Code/PR/National Costs/nat_map.R")

#national costs (binary - Figure S5)
source("Code/PR/National Costs/nat_costs_binary.R")