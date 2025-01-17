#a cbg is downgradient if there is a cont site within its watershed
#First, calculate the intersection of the cont sites and the birth watersheds
down_wells = st_intersection(cont_sites %>% st_transform(3437), births_ws %>% st_transform(3437))
down_wells = down_wells %>% as_tibble() %>% dplyr::select(geoid, site, pfas = sum_pfoa_pfos) 
dwells = unique(down_wells$geoid)

#get lat long in births df
births = births %>% 
  st_transform(4326) %>%
  dplyr::mutate(lng = sf::st_coordinates(.)[, 1],
         lat = sf::st_coordinates(.)[, 2]) %>% 
  st_transform(3437)

#only close down determines whether we include cbgs on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
down_well_dist = function(w){
  #select all observations in down_well for well w
  dw = down_wells[which(down_wells$geoid == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  dw_ll = births %>% 
    as_tibble() %>%
    dplyr::filter(geoid == dw$geoid[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  #get the lat longs for all sites in w's watershed
  rsdw_ll = cont_sites[which(cont_sites$site %in% dw$site & 
                               cont_sites$sum_pfoa_pfos %in% dw$pfas), c("lng", "lat", "site")]
  
  #distances
  ds = distm(dw_ll, rsdw_ll %>% as_tibble() %>% dplyr::select(lng, lat))
  
  #get the index of the nearest relevant release site
  ind_nearest = which.min(ds)
  
  #get the site name for the nearest relevant release site
  rsdw_site = rsdw_ll %>% 
    as_tibble() %>% 
    dplyr::select(site) %>%
    dplyr::filter(site %in% dw$site)
  nearest_site = as.character(rsdw_site[ind_nearest, "site"])
  
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

down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist))


#a cbg is upgradient if it lies in the watershed of all cont sites within 'meters'
up_wells = st_intersection(births %>% 
                             as_tibble() %>% 
                             dplyr::select(!geometry) %>% 
                             st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                             st_transform(3437), cont_ws %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, pfas = sum_pfoa_pfos)) %>% st_transform(3437))

up_wells = up_wells %>% as_tibble() %>% dplyr::select(geoid, site, pfas) 
uwells = unique(up_wells$geoid)

#only close up determines whether we include cbgs on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w){
  uw = up_wells[which(up_wells$geoid == w), ]
  uw_ll = births %>% 
    as_tibble() %>%
    dplyr::filter(geoid == uw$geoid[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  rsuw_ll = cont_sites[which(cont_sites$site %in% uw$site & 
                               cont_sites$sum_pfoa_pfos %in% uw$pfas), c("lng", "lat", "site")]
  
  ds = distm(uw_ll, rsuw_ll %>% as_tibble() %>% dplyr::select(lng, lat))
  
  ind_nearest = which.min(ds)
  
  rsuw_site = cont_sites$site[which(cont_sites$site %in% uw$site)]
  
  rsuw_site = rsuw_ll %>% 
    as_tibble() %>% 
    dplyr::select(site) %>%
    dplyr::filter(site %in% uw$site)
  
  nearest_site = as.character(rsuw_site[ind_nearest, "site"])
  
  uw$n_sites_up5 = length(which(ds <= meters))
  uw = uw[which(uw$site == nearest_site), ]
  uw$dist_up = ds[ind_nearest]
  
  uw = uw %>% 
    dplyr::rename(pfas_up = pfas, site_up = site)
  
  uw$up = 1
  
  return(uw)
}

up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist))

#join up and down wells with births to begin classification algorithm
births = births %>% 
  left_join(down_wells) %>% 
  left_join(up_wells)

#set missing variables to 0 (they were not present in upwells and/or downwells)
births[is.na(births$down), ]$down = 0
births[is.na(births$up), ]$up = 0
births[is.na(births$n_sites_down5), ]$n_sites_down5 = 0
births[is.na(births$n_sites_up5), ]$n_sites_up5 = 0


#Get chars of nearest release site
well_dist = function(i){
  w = births[i, ]
  
  dists = distm(c(w$lng, w$lat), cont_sites %>% as_tibble() %>% dplyr::select(lng, lat))
  
  ind = which.min(dists)
  
  #if neither down, nor up, set distance as that to the nearest site
  w$dist_near = dists[ind]
  #if neither down, nor up, set pfas as that at the nearest site
  w$pfas_near = cont_sites$sum_pfoa_pfos[ind]
  #get number of nearby sites
  w$n_sites_meters = length(which(dists <= 5100))
  
  w$site_near = cont_sites$site[ind]
  
  return(w)
  
  
}
births = dplyr::bind_rows(pblapply(1:nrow(births), well_dist))


#fill in down, up, side variables (giving 100 meter buffer in this spec)
well_assgn = function(i, drop_far_down = FALSE, drop_far_up = FALSE){
  w = births[i, ]
  down_far = 0
  
  #if distance for down well is less than meters, classify well as down, set pfas at level of relevant site
  d = w$dist_down
  if (!is.na(d)){ #if there is a down site
    w$up = 0
    if (d < meters + 100){ #if the down site is within the buffer, assign its values to the well
      w$pfas = w$pfas_down
      w$site = w$site_down
      w$dist = w$dist_down
      w$down = 1
      return(w)
    }else if (d > (meters + 100) & drop_far_down == TRUE){ #if the down site is outside the buffer, set values as missing (this will drop it in the regression)
      w$pfas = NA
      w$site = NA
      w$dist = NA
      w$down = NA
      return(w)
    }else{ #otherwise, d > meters and we reclassify. Set down_far to 1, so that we dont include in 
      w$down = 0
      down_far = 1
    }
  }
  
  #if we get to this point, then there are either no down sites (or down sites are too far and running reclass spec)
  w$down = 0
  
  #set up variables
  d_up = w$dist_up
  if (!is.na(d_up) & down_far == 0){ #if there is an up site (and no down sites)
    if (d_up < meters + 100){ #if the up site is within the buffer, assign its values to the well
      w$pfas = w$pfas_up
      w$site = w$site_up
      w$dist = w$dist_up
      w$up = 1
      return(w)
    }else if (d_up > (meters + 100) & drop_far_up == TRUE){ #if the up site is outside the buffer, set values as missing (this will drop it in the regression)
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
  if (d_side < meters + 100){
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

births = dplyr::bind_rows(pblapply(1:nrow(births), well_assgn))

save(births, file = modify_path("Data_Verify/National/births_sites_assigned.RData"))





