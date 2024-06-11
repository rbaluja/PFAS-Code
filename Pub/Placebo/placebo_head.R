source("PFAS-Code/Pub/config.R")
set.seed(1)
wbt_verbose(FALSE)
drop_states = FALSE
relaxed_up = FALSE

#data cleaning
source("PFAS-Code/Pub/Data/data_head.R")

nh_shape = tigris::states() %>% 
  dplyr::filter(STUSPS == "NH") %>% 
  st_transform(32110)

#fill sinks
wbt_breach_depressions(modify_path("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff"), modify_path("Data_Verify/GIS/filled_dem.tiff"))

#flow accumulation
wbt_d8_flow_accumulation(modify_path("Data_Verify/GIS/filled_dem.tiff"), modify_path("Data_Verify/GIS/flow_acc.tiff"))

#flow direction
wbt_d8_pointer(modify_path("Data_Verify/GIS/filled_dem.tiff"), modify_path("Data_Verify/GIS/flow_dir.tiff"))

#read in placebo functions
source("PFAS-Code/Pub/Placebo/placebo_functions.R")

if (rerun_placebos == TRUE){
  placebos_1 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_1, file = modify_path("Data_Verify/RData/placebos_1.RData"))
  
  placebos_2 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_2, file = modify_path("Data_Verify/RData/placebos_2.RData"))
  
  placebos_3 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_3, file = modify_path("Data_Verify/RData/placebos_3.RData"))
  
  placebos_4 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_4, file = modify_path("Data_Verify/RData/placebos_4.RData"))
  
  placebos_5 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_5, file = modify_path("Data_Verify/RData/placebos_5.RData"))
  
  placebos_6 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_6, file = modify_path("Data_Verify/RData/placebos_6.RData"))
  
  placebos_7 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_7, file = modify_path("Data_Verify/RData/placebos_7.RData"))
  
  placebos_8 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_8, file = modify_path("Data_Verify/RData/placebos_8.RData"))
  
  placebos_9 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_9, file = modify_path("Data_Verify/RData/placebos_9.RData"))
  
  placebos_10 = dplyr::bind_rows(pblapply(1:100, placebo, df, wells))
  save(placebos_10, file = modify_path("Data_Verify/RData/placebos_10.RData"))
  

  
  plac = NULL
  # Loop through the files
  for (i in 1:10) {
    #create a new environment for loading the file
    env = new.env()
    
    #load the file into the new environment
    load(modify_path(paste0("Data_Verify/RData/placebos_", i, ".RData")), envir = env)
    
    object_name = ls(env)
    
    current_placebo = get(object_name, env)
    
    # bind plac with current_placebo
    if(is.null(plac)) {
      plac = current_placebo
    } else {
      plac = plyr::rbind.fill(plac, current_placebo)
    }
  }
  
  #check for missing iterations, and redo them.
  n_na = length(which(is.na(plac$preterm)))
  while (n_na > 0){
    plac = plac %>%
      tidyr::drop_na(preterm)
  
    plac_na = dplyr::bind_rows(pblapply(1:n_na, placebo, df, wells))
    plac = plyr::rbind.fill(plac, plac_na)
    n_na = length(which(is.na(plac$preterm))) 
  }
  
  save(plac, file = modify_path("Data_Verify/RData/placebos_mort.RData") )
}else{
  load(modify_path("Data_Verify/RData/placebos_mort.RData") )
}

#This is the input to table S-5. It counts the number of runs with sig positive coef estimates
sink(modify_path2("Tables/falsification.tex"))
plac$pre_sig = as.numeric(plac$preterm/plac$preterm_se > 1.281552)
plac$vpre_sig = as.numeric(plac$vpreterm/plac$vpreterm_se > 2.326348)
plac$lbw_sig = as.numeric(plac$lbw/plac$lbw_se > 2.326348)
plac$llbw_sig = as.numeric(plac$llbw/plac$llbw_se > 1.644854)
plac$vlbw_sig = as.numeric(plac$vlbw/plac$vlbw_se > 2.326348)
plac$mort_sig = as.numeric(plac$mort/plac$mort_se > 2.326348)

print("Preterm")
sum(plac$pre_sig)/1000
print("Very Preterm")
sum(plac$vpre_sig)/1000
print("Low Birthweight")
sum(plac$lbw_sig)/1000
print("Moderately Low Birthweight")
sum(plac$llbw_sig)/1000
print("Extremely Low Birthweight")
sum(plac$vlbw_sig)/1000
print("Infant Mortality")
sum(plac$mort_sig)/1000
print("All Sig. Health Outcomes")
sum(as.numeric(plac$preterm/plac$preterm_se > 1.281552 & 
                 plac$vpreterm/plac$vpreterm_se > 2.326348 & 
                 plac$lbw/plac$lbw_se > 2.326348 & 
                 plac$llbw/plac$llbw_se > 1.644854 & 
                 plac$vlbw/plac$vlbw_se > 2.326348 & 
                 plac$mort/plac$mort_se > 2.326348))/1000

sink()

