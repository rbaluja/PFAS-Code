#set working directory
setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox)
options(modelsummary_format_numeric_latex = "mathmode")

#set up environment
meters = 5000
wind_dist= dist_allow = 10000
ppt = 1000
run_cleaning = FALSE
match_wells = FALSE
old_wells = FALSE
domestic = FALSE
system = FALSE
drop_dups = TRUE #needs to be false if calculating se on difference in theta
drop_far_down = TRUE
drop_far_up = FALSE
well_fd = test_fd = FALSE #flow line distance?
IV = TRUE
fa_resid = TRUE
drop_sites = FALSE
drop_states = FALSE
GIS_create = FALSE

#data cleaning
source("PFAS-Code/PR/Data/data_head.R")

if (GIS_create == TRUE){
  source("Code/PR/GIS/df_watershed.R") #mothers residence - NOTE: This takes a long time to run 
}else{
  #read in natality dataset with binary, flow accumulation, and catchment areas 
  load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality_ws.RData") 
}

#read in and set cont site watersheds 
load("New Hampshire/Data/RData/cont_watershed.RData")

#read in sites_ll to get right site number
rs_ll = fread("New Hampshire/Data/rs_ll.csv")
rs_ll$index = 1:nrow(rs_ll)

cont_ws = cont_ws %>% 
  left_join(rs_ll %>% dplyr::select(site, index))


#a well is downgradient if there is a site in its watershed
down_wells = st_intersection(cont_sites %>% st_transform(3437), df %>% st_as_sf(crs = 3437) %>% st_transform(3437))
down_wells = down_wells %>% as_tibble() %>% dplyr::select(id, site, pfas = sum_pfoa_pfos) 
dwells = unique(down_wells$id)

#only close down determines whether we include individuals on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
down_well_dist = function(w){
  dw = down_wells[which(down_wells$id == w), ]
  dw_ll = df %>% 
    as_tibble() %>%
    dplyr::filter(id == dw$id[1]) %>% 
    dplyr::select(lng = lng, lat = lat)
  
  rsdw_ll = rs_ll[which(rs_ll$site %in% dw$site), c("lng", "lat")]
  
  ds = distm(dw_ll, rsdw_ll)
  
  ind_nearest = which.min(ds)
  
  rsdw_site = rs_ll[which(rs_ll$site %in% dw$site), "site"]
  
  nearest_site = rsdw_site[ind_nearest]
  
  dw$n_sites_down5_resid = length(which(ds <= meters))
  dw = dw[which(dw$site == nearest_site[1]), ]
  dw$dist_down_resid = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_down_resid = pfas, site_down_resid = site)
  
  dw$down_resid = 1
  
  return(dw)
}

down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist))


#for calculating upgradient, first obtain set of wells in the catchment area of sites
up_wells = st_intersection(df %>% 
                             as_tibble() %>% 
                             dplyr::select(!geometry) %>% 
                             st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                             st_transform(3437), cont_ws %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, pfas = sum_pfoa_pfos)) %>% st_transform(3437))

up_wells = up_wells %>% as_tibble() %>% dplyr::select(id, site, pfas) 
uwells = unique(up_wells$id)

#only close up determines whether we include individuals on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w){
  dw = up_wells[which(up_wells$id == w), ]
  dw_ll = df %>% 
    as_tibble() %>%
    dplyr::filter(id == dw$id[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  rsdw_ll = rs_ll[which(rs_ll$site %in% dw$site), c("lng", "lat")]
  
  ds = distm(dw_ll, rsdw_ll)
  
  ind_nearest = which.min(ds)
  
  rsdw_site = rs_ll[which(rs_ll$site %in% dw$site), c("site", "pfas")]
  
  nearest_site = rsdw_site[ind_nearest]
  
  dw$n_sites_up5_resid = length(which(ds <= meters))
  dw = dw[which(dw$site == nearest_site[1]), ]
  
  dw$dist_up_resid = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_up_resid = pfas, site_up_resid = site)
  
  dw$up_resid = 1
  
  return(dw)
}

up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist))

df = df %>% 
  left_join(down_wells) %>% 
  left_join(up_wells)

df[is.na(df$down_resid), ]$down_resid = 0
df[is.na(df$up_resid), ]$up_resid = 0
df[is.na(df$n_sites_down5_resid), ]$n_sites_down5_resid = 0
df[is.na(df$n_sites_up5_resid), ]$n_sites_up5_resid = 0

#if both up and down, change up to 0
df[which(df$up_resid == 1 & df$down_resid == 1), ]$up_resid = 0

#for wells that arent down or up, find nearest site and use that 
well_dist = function(i){
  w = df[i, ]
  
  dists = distm(c(w$lng, w$lat), rs_ll[, c("lng", "lat")])
  
  ind = which.min(dists)
  
  #if neither down, nor up, set distance as that to the nearest site
  w$dist_near_resid = dists[ind]
  #if neither down, nor up, set pfas as that at the nearest site
  w$pfas_near_resid = rs_ll$pfas[ind]
  #get number of nearby sites
  w$n_sites_meters_resid = length(which(dists <= meters))
  
  w$site_near_resid = rs_ll$site[ind]
  
  return(w)
  
  
}
df1 = dplyr::bind_rows(pblapply(1:nrow(df), well_dist))

#fill in down, up, side variables
well_assgn = function(i, drop_far_down, drop_far_up){
  w = df1[i, ]
  down_far = 0
  
  #if distance for down well is less than meters, classify well as down, set pfas at level of relevant site
  d = w$dist_down_resid
  if (!is.na(d)){ #if there is a down site
    if (d < meters){ #if the down site is within the buffer, assign its values to the well
      w$pfas_resid = w$pfas_down_resid
      w$site_resid = w$site_down_resid
      w$dist_resid = w$dist_down_resid
      w$down_resid = 1
      return(w)
    }else if (d > meters & drop_far_down == TRUE){ #if the down site is outside the buffer, set values as missing (this will drop it in the regression)
      w$pfas_resid = NA
      w$site_resid = NA
      w$dist_resid = NA
      w$down_resid = NA
      return(w)
    }else{ #otherwise, d > meters and we reclassify. Set down_far to 1, so that we dont include in 
      w$down_resid = 0
      down_far = 1
    }
  }
  
  #if we get to this point, then there are either no down sites (or down sites are too far and running reclass spec)
  w$down_resid = 0
  
  #set up variables
  d_up = w$dist_up
  if (!is.na(d_up) & down_far == 0){ #if there is an up site (and no down sites)
    if (d_up < meters){ #if the up site is within the buffer, and all nearby sites are up, assign its values to the well
      w$pfas_resid = w$pfas_up_resid
      w$site_resid = w$site_up_resid
      w$dist_resid = w$dist_up_resid
      w$up_resid = 1
      return(w)
    }else if (d_up > meters & drop_far_up == TRUE){ #if the up site is outside the buffer, set values as missing (this will drop it in the regression)
      w$pfas_resid = NA
      w$site_resid = NA
      w$dist_resid = NA
      w$up_resid = NA
      return(w)
    }else{ #otherwise, d > meters and we reclassify
      w$up_resid = 0
    }
  }
  
  #if we get to this point, then the well has no nearby up or down (or we are reclassifying, or there is a nearby up site, but it is down of something)
  w$up = 0
  d_side = w$dist_near_resid
  if (d_side < meters){
    w$pfas_resid = w$pfas_near_resid
    w$dist_resid = w$dist_near_resid
    w$site_resid = w$site_near_resid
    return(w)
  }else{
    w$pfas_resid = NA
    w$dist_resid = NA
    w$site_resid = NA
    return(w)
  }
  
}

df2 = dplyr::bind_rows(pblapply(1:nrow(df1), well_assgn, drop_far_down, drop_far_up))
length(which(df2$down == 1 & df2$down_resid == 1))/length(which(df2$down == 1))

#subset to only rows which have a residence on the side or up
df2 = df2[which(df2$down_resid == 0), ]




pr_rside = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid + wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


lpr_rside = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                       m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                       pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                       mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                       mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                       m_height + tri5 + fa_resid + wind_exposure
                                     |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

mpr_rside = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                 m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                 pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                 mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                 mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                 m_height + tri5 + fa_resid + wind_exposure
                                               |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

vpr_rside = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                           m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                           pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                           mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                           m_height + tri5 + fa_resid+ wind_exposure
                                         |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


lbw_rside = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid + wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))
llbw_rside= fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                               m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                               pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                               mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                               mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                               m_height + tri5 + fa_resid + wind_exposure
                                             |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

mlbw_rside = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                         m_height + tri5 + fa_resid + wind_exposure
                                                       |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))

vlbw_rside = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                                   m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                                   pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                                   mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                                   mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                                   m_height + tri5 + fa_resid+ wind_exposure
                                                 |county + year^month + birth_race_dsc_1, data = df2, warn = F, notes = F, cluster = c("site", "year^month"))


save(lbw_rside, llbw_rside, mlbw_rside, vlbw_rside, pr_rside, lpr_rside, mpr_rside, vpr_rside, 
     file = "New Hampshire/Data/RData/side_robustness.RData")
