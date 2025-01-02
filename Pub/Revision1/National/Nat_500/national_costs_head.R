source("PFAS-Code/Pub/Revision1/ppt500/config.R")

#get watershed for each cont site
if (nat_run_cont_ws | !file.exists(modify_path(paste0("Data_Verify/RData/nat_cont_watershed", ppt, ".RData")))){
  source("PFAS-Code/Pub/Revision1/National/Nat_500/nat_watersheds.R")  
  cont_ws = get(load(modify_path(paste0("Data_Verify/RData/nat_cont_watershed", ppt, ".RData"))))
  births_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character"))
}else{
  #load in watersheds
  cont_ws = get(load(modify_path(paste0("Data_Verify/RData/nat_cont_watershed", ppt, ".RData"))))
  births_ws = get(load(modify_path("Data_Verify/GIS/nat_cbg_watershed.RData")))
  births = fread(modify_path("Data_Verify/National/births_cbg_cleaned_2010.csv"), colClasses = c(county = "character"))
}

#assign cbg to sites
if (nat_reassn){
  source("PFAS-Code/Pub/Revision1/National/Nat_500/nat_assn.R")
}else{
  load(modify_path(paste0("Data_Verify/National/births_sites_assigned", ppt, ".RData")))
}

#run national costs (primary - Figure 3 part 1)
source("PFAS-Code/Pub/Revision1/National/Nat_500/nat_costs.R")
