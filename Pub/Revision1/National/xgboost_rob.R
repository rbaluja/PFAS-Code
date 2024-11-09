source("PFAS-Code/Pub/config.R")
xgb_out = terra::rast("Data Revisions/National/xgboost_rob/Outputs/prob_public.asc")
#based on the extent, it looks like the projection is EPSG:5070
terra::crs(xgb_out) = "EPSG:5070"
#plot to make sure
plot(xgb_out)
#yep

#create a new raster of indicator variables for a predicted probability of greater than 0.5
exp_rast = as.numeric(xgb_out > 0.5)
terra::writeRaster(exp_rast, file = "Data Revisions/National/xgboost_rob/Outputs/exp_rast_rob.tif", overwrite = TRUE)

#read in cbg-level natality data
if (run_cleaning == TRUE){
  source("PFAS-Code/Pub/Revision1/National/births_build.R")
}
births = fread("Data Revisions/National/births_cbg_cleaned_2010_rev.csv", colClasses = c("GEOID" = "character", "births" = "numeric"))
#read in state names
states = fread("Data_Verify_Pub/Supplemental/state_name_code_cross.csv", colClasses = "character")

#get shapefile for cbgs
cbg_fun = function(state){
  cbg_1 = tigris::block_groups(state = state, year = 2010)
  return(cbg_1)
}
cbg_shape = dplyr::bind_rows(pblapply(states$stusps, cbg_fun))
cbg_shape = cbg_shape %>% 
  dplyr::rename(GEOID = GEOID10)

#combine births with its shape
births = births %>% 
  left_join(cbg_shape %>% 
              dplyr::select(GEOID, geometry), by = "GEOID")

#get state
births = births %>% 
  dplyr::mutate(state = stringr::str_sub(GEOID, 1, 2))

births$state11 = ifelse(births$state %in% c("26", "27", "33", "36", "08", "23", "50", "06", "12", "38", "55"), 1, 0)

#find empty geometries
births = births %>% 
  dplyr::mutate(empty = sf::st_is_empty(geometry))
#great, all of them were matched correctly

#convert to sf object
births = sf::st_as_sf(births)

#extract the maximum of exp_rast for each cbg
births$cont = exactextractr::exact_extract(exp_rast, births, fun = "mean", default_value = 0)

#number of births affected
births$births_pfas = births$births * births$cont

#get cost figures
source("PFAS-Code/Pub/Revision1/National/xgboost_cost.R")
source("PFAS-Code/Pub/Revision1/National/xgboost_cost11.R")
#verification table
source("PFAS-Code/Pub/Revision1/National/xgb_verify_alg.R")