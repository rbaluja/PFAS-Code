#clean raw births data
if (nb_cbg == TRUE){
  source("PFAS-Code/PR/National Costs/nat_births.R") 
  births = cbg_births
  rm(cbg_births)
}else{
  births = fread("Data_Verify/National/births_cbg_cleaned_2010.csv", colClasses = c(county = "character", tract = "character"))
}

#get watershed for each cont site
if (nat_run_cont_ws == TRUE){
  source("Code/PR/National Costs/nat_watersheds.R")  
}


#run national costs (primary - Figure 4)
source("Code/PR/National Costs/nat_costs.R")

#map of national release sites and pop density (Figure S4)
source("Code/PR/National Costs/nat_map.R")

#national costs (binary - Figure S5)
source("Code/PR/National Costs/nat_costs_binary.R")