source("PFAS-Code/Pub/config.R")
#clean raw births data
births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character", tract = "character"))

#get watershed for each cont site
if (nat_run_cont_ws){
  source("PFAS-Code/Pub/Revision1/National/Nat_500/nat_watersheds.R")  
  cont_ws = get(load(modify_path("Data_Verify/RData/nat_cont_watershed.500RData")))
  births_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character"))
}else{
  #load in watersheds
  cont_ws = get(load(modify_path("Data_Verify/RData/nat_cont_watershed500.RData")))
  births_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
}

#assign cbg to sites
if (nat_reassn){
  source("PFAS-Code/Pub/Revision1/National/Nat_500/National Costs/nat_assn.R")
}else{
  load(modify_path("Data_Verify/National/births_sites_assigned500.RData"))
}

#run national costs (primary - Figure 3 part 1)
source("PFAS-Code/Pub/Revision1/National/Nat_500/National Costs/nat_costs.R")
