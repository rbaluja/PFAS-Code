#set working directory
setwd("~/Dropbox/PFAS Infants")

dir.create("Data_Verify/GIS/df")
dir.create("Data_Verify/GIS/df/df_pp")
dir.create("Data_Verify/GIS/df/df_watershed")
dir.create("Data_Verify/GIS/df/df_watershed/Shapes")


#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, 
             units, tidycensus)
options(modelsummary_format_numeric_latex = "mathmode")

#set up environment
natality_path = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/" #set path to natality data in Box Health
meters = 5000 #buffer for base spec
wind_dist= dist_allow = 10000 #wind distance cutoff
ppt = 1000 #cutoff for primary contamination site
run_cleaning = FALSE #clean natality data?
match_wells = FALSE #Re match natality data to wells?
domestic = FALSE #include individuals outside of PWS boundaries?
drop_far_down = TRUE
drop_far_up = FALSE
IV = FALSE #Run IV spec?
rerun_fs_clean = FALSE #clean first stage data?
drop_states = FALSE #running spec where we drop sites within meters of state border?
relaxed_up = FALSE #relaxed upgradient robustness spec?
GIS_create = FALSE #recreate watershed shapes?
create_figures = FALSE #output figures?
nat_run_cont_ws = FALSE#recreate national watershed shapes?
nat_reassn = FALSE #reassign national CBGs to release sites?
nat_redo_soil = FALSE #recalculate soil stats for national data?
oster_robust = FALSE #run Oster (2019) selection on unobservables?
false_test = FALSE #run falsification test?
census_key = "9f59b9fec9cffa85b5740734df3d81e7b617cf82"

#data cleaning
source("PFAS-Code/PR/Data/data_head.R")

if (GIS_create == TRUE){
  source("PFAS-Code/PR/GIS/gis_head.R")
}

#main analysis
source("PFAS-Code/PR/Main Analysis/main_analy_head.R")

#subset df to only those used in estimation
df = df[which(!is.na(df$dist) & df$dist <= 5000), ]
df = df %>% 
  dplyr::filter(!is.na(gestation) & 
                  !is.na(m_age) & 
                  !is.na(m_married) & 
                  !is.na(private_insurance) & 
                  !is.na(nbr_cgrtt) & 
                  !is.na(m_educ) & 
                  !is.na(f_educ) & 
                  !is.na(pm25) & 
                  !is.na(temp) & 
                  !is.na(p_manuf) & 
                  !is.na(n_hunits) & 
                  !is.na(med_hprice) & 
                  !is.na(well_elev) & 
                  !is.na(resid_elev) & 
                  !is.na(mr_04) & 
                  !is.na(mr_18) & 
                  !is.na(mr_21) & 
                  !is.na(mr_26) & 
                  !is.na(mr_27) & 
                  !is.na(mthr_wgt_dlv) & 
                  !is.na(mthr_pre_preg_wgt) & 
                  !is.na(m_height) & 
                  !is.na(tri5) & 
                  !is.na(county) & 
                  !is.na(year) & 
                  !is.na(month) & 
                  !is.na(birth_race_dsc_1) & 
                  !is.na(wic))

df$index = 1:nrow(df)
fwrite(df %>% as_tibble() %>% dplyr::select(id, index), "Data_Verify/GIS/natality_id_ws.csv")

df = df %>% st_as_sf(coords = c("lng", "lat"), crs = 4326)

df_watershed = function(i){
  
  #get location of test well i
  point_sf = df[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  # Run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = "Data_Verify/GIS/flow_acc.tiff", 
                       output = paste0("Data_Verify/GIS/df/df_pp/pp_site_", i, ".shp"), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = "Data_Verify/GIS/flow_dir.tiff", 
                pour_pts = paste0("Data_Verify/GIS/df/df_pp/pp_site_", i, ".shp"), 
                output = paste0("Data_Verify/GIS/df/df_watershed/watershed_", i, ".tiff"))
  
  #read in watershed
  ws = terra::rast(paste0("Data_Verify/GIS/df/df_watershed/watershed_", i, ".tiff"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("Data_Verify/GIS/df/df_watershed/Shapes/ws_shape_", i, ".shp"), overwrite = TRUE)
  
}

pblapply(1:nrow(df), df_watershed, cl = 4)


files = list.files("Data_Verify/GIS/df/df_watershed/Shapes/", pattern = "*.shp", recursive = T, full.names = T)

well_ws = function(f){
  w_ws1 = st_read(f)
  w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
  w_ws1$index = as.numeric(gsub("[^0-9]", "", f))
  return(w_ws1)
}

df_ws = dplyr::bind_rows(pblapply(files, well_ws, cl = 4))
df = df %>% as_tibble() %>% dplyr::select(!geometry) %>% left_join(df_ws)
save(df, file = paste0(natality_path, "[UA Box Health] natality_ws.RData"))


#delete folder
unlink("Data_Verify/GIS/df/", recursive = TRUE)
