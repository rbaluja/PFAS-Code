# #read in and set well watersheds
load(modify_path("Data_Verify/GIS/wells_watershed.RData"))

#read in well_ll to get appropriate sys and well ids
well_ll = fread(modify_path("Data_Verify/GIS/wells_ll_ws.csv"))
wells_ws = wells_ws %>% left_join(well_ll)

#read in and set cont site watersheds 
load(modify_path("Data_Verify/GIS/cont_watershed.RData"))

#read in sites_ll to get right site number
rs_ll = fread(modify_path("Data_Verify/GIS/rs_ll_ws.csv"))

cont_ws = cont_ws %>% 
  left_join(rs_ll)

if (drop_states == TRUE){
  cont_ws = cont_ws[which(cont_ws$site %in% cont_sites$site), ]
}


######################
##### Downgradient matching

#a well is downgradient if there is a site in its watershed
#form intersection of cont sites with the watersheds of each well
#if a well is in the watershed of multiple sites, this returns multiple rows for that well
#if a well is in the watershed of no sites, then it doesnt show up in this 
down_wells = st_intersection(cont_sites %>% st_transform(3437), wells_ws %>% st_transform(3437))
#select well identifiers (sys_id, source) and site info (site, pfas)
down_wells = down_wells %>% as_tibble() %>% dplyr::select(sys_id, source, site, pfas = sum_pfoa_pfos) 
down_wells$sys_id = str_pad(as.character(down_wells$sys_id), 7, "left", "0")
down_wells$source = str_pad(as.character(down_wells$source), 3, "left", "0")
#set well as grouped indice across sys_id and source to iterate over
down_wells$well = down_wells %>% 
  dplyr::group_by(sys_id, source) %>%
  dplyr::group_indices(sys_id, source)
#grab unique wells with a cont site in its watershed
dwells = unique(down_wells$well)

#only close down determines whether we include individuals on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
#this function takes as input a well identifier and returns dataframe with that well, 
#its matched site, the number of down sites within 5km, the distance to the nearest down site,
#and the pfas level at its matched site
down_well_dist = function(w){
  #select all observations in down_well for well w
  dw = down_wells[which(down_wells$well == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  dw_ll = wells %>% 
    as_tibble() %>%
    dplyr::filter(source == dw$source[1] & sys_id == dw$sys_id[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  #get the lat longs for all sites in w's watershed
  rsdw_ll = rs_ll[which(rs_ll$site %in% dw$site), c("lng", "lat")]
  
  #get distance matrix for the well to each relevant release site 
  ds = distm(dw_ll, rsdw_ll)
  
  #get the index of the nearest relevant release site
  ind_nearest = which.min(ds)
  
  #get the site name for the nearest relevant release site
  rsdw_site = rs_ll[which(rs_ll$site %in% dw$site), "site"]
  nearest_site = as.character(rsdw_site[ind_nearest])
  
  #how many down sites are within 'meters' of the well?
  dw$n_sites_down5 = length(which(ds <= meters))
  #subset dw to only the nearest down site
  dw = dw[which(dw$site == nearest_site), ]
  #append distance to nearest site to dw
  dw$dist_down = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_down = pfas, site_down = site)
  
  dw$down = 1
  
  return(dw)
}

#apply down_well_dist (well to site matching, for down) to all wells with a cont site in its watershed
down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist))

######################
##### Upgradient matching

#for calculating upgradient, first obtain set of wells in the catchment area of sites
up_wells = st_intersection(wells %>% 
                             as_tibble() %>% 
                             dplyr::select(!geometry) %>% 
                             st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                             st_transform(3437), 
                           cont_ws %>% 
                             left_join(cont_sites %>% as_tibble() %>% 
                                         dplyr::select(site, pfas = sum_pfoa_pfos)) %>% 
                             st_transform(3437))

up_wells = up_wells %>% as_tibble() %>% dplyr::select(sys_id, source, site, pfas) 
up_wells$sys_id = str_pad(as.character(up_wells$sys_id), 7, "left", "0")
up_wells$source = str_pad(as.character(up_wells$source), 3, "left", "0")
up_wells$well = up_wells %>% 
  dplyr::group_by(sys_id, source) %>%
  dplyr::group_indices(sys_id, source)
uwells = unique(up_wells$well)

#only close up determines whether we include individuals on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w){
  #select all observations in up_well for well w
  uw = up_wells[which(up_wells$well == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  uw_ll = wells %>% 
    as_tibble() %>%
    dplyr::filter(source == uw$source[1] & sys_id == uw$sys_id[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  #get the lat longs for all sites for which w is in their watershed
  rsuw_ll = rs_ll[which(rs_ll$site %in% uw$site), c("lng", "lat")]
  
  #get distance matrix for the well to each relevant release site 
  ds = distm(uw_ll, rsuw_ll)
  
  #get distance matrix for the well to each relevant release site 
  ind_nearest = which.min(ds)
  
  #get the site name for the nearest relevant release site
  rsuw_site = rs_ll[which(rs_ll$site %in% uw$site), c("site")]
  nearest_site = as.character(rsuw_site[ind_nearest])
  
  #how many up sites are within 'meters' of the well?
  uw$n_sites_up5 = length(which(ds <= meters))
  #subset uw to only the nearest down site
  uw = uw[which(uw$site == nearest_site), ]
  #append distance to nearest site to uw
  uw$dist_up = ds[ind_nearest]
  
  uw = uw %>% 
    dplyr::rename(pfas_up = pfas, site_up = site)
  
  uw$up = 1
  
  return(uw)
}
#apply up_well_dist (well to site matching, for up) to all wells which are in the watershed of a cont site
up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist))

#bind wells with down_wells and up_wells to obtain the relevant up/down variables
wells = wells %>% 
  left_join(down_wells %>% dplyr::select(!well), by = c("sys_id", "source")) %>% 
  left_join(up_wells %>% dplyr::select(!well), by = c("sys_id", "source"))

#when NA, that means that they had no cont sites in their watershed (down) or 
#they were not in the watershed of any sites (up)
wells[is.na(wells$down), ]$down = 0
wells[is.na(wells$up), ]$up = 0
wells[is.na(wells$n_sites_down5), ]$n_sites_down5 = 0
wells[is.na(wells$n_sites_up5), ]$n_sites_up5 = 0

#get wind exposure
wells$wind_exposure = pbmapply(wind_function, wells$lng, wells$lat, rep(dist_allow, nrow(wells)))

#Find nearest release site for each well
well_dist = function(i){
  w = wells[i, ]
  
  dists = distm(c(w$lng, w$lat), rs_ll[, c("lng", "lat")])
  
  ind = which.min(dists)
  
  #set distance as that to the nearest site
  w$dist_near = dists[ind]
  #set pfas as that at the nearest site
  w$pfas_near = rs_ll$pfas[ind]
  #get number of nearby sites
  w$n_sites_meters = length(which(dists <= meters))
  
  w$site_near = rs_ll$site[ind]
  
  return(w)
  
  
}
wells1 = dplyr::bind_rows(pblapply(1:nrow(wells), well_dist))


#fill in down, up, side variables
well_assgn = function(i, drop_far_down, drop_far_up){
  w = wells1[i, ]
  down_far = 0
  
  #if distance for down well is less than meters, classify well as down, set pfas at level of relevant site
  d = w$dist_down
  if (!is.na(d)){ #if there is a down site
    w$up = 0
    if (d < meters){ #if the down site is within the buffer, assign its values to the well
      w$pfas = w$pfas_down
      w$site = w$site_down
      w$dist = w$dist_down
      w$down = 1
      w$up = 0
      return(w)
    }else if (d > meters & drop_far_down == TRUE){ #if the down site is outside the buffer, set values as missing (this will drop it in the regression)
      w$pfas = NA
      w$site = NA
      w$dist = NA
      w$down = NA
      return(w)
    }else{ #otherwise, d > meters and we reclassify. Set down_far to 1
      w$down = 0
      down_far = 1
    }
  }
  
  #if we get to this point, then there are either no down sites (or down sites are too far and running reclass spec)
  w$down = 0
    
  #set up variables
  d_up = w$dist_up
  if (!is.na(d_up) & down_far == 0 & relaxed_up == FALSE){ #if there is an up site (and no down sites)
    if (d_up < meters & (w$n_sites_up5 == w$n_sites_meters)){ #if the up site is within the buffer, and all nearby sites are up, assign its values to the well
      w$pfas = w$pfas_up
      w$site = w$site_up
      w$dist = w$dist_up
      w$up = 1
      return(w)
    }else if (d_up > meters & drop_far_up == TRUE){ #if the up site is outside the buffer, set values as missing (this will drop it in the regression)
      w$pfas = NA
      w$site = NA
      w$dist = NA
      w$up = NA
      return(w)
    }else{ #otherwise, d > meters and we reclassify
      w$up = 0
    }
  }
  
  #if we get to this point, then the well has no nearby up or down (or we are reclassifying, or there is a nearby up site, but it is down of something)
  w$up = 0
  d_side = w$dist_near
  if (d_side < meters){
    w$pfas = w$pfas_near
    w$dist = w$dist_near
    w$site = w$site_near
    return(w)
  }else{
    w$pfas = NA
    w$dist = NA
    w$site = NA
    return(w)
  }

}

wells2 = dplyr::bind_rows(pblapply(1:nrow(wells1), well_assgn, drop_far_down, drop_far_up))

df = df %>% 
  left_join(wells2 %>% 
              as_tibble() %>% 
              dplyr::select(sys_id, source, pfas, dist,  wind_exposure, site, up, down, n_sites = n_sites_meters, dist_down, dist_up)) 

df$updown = ifelse(df$down == 1 | df$up == 1, 1, 0)
