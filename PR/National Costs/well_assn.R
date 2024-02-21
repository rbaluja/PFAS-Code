down_wells = st_intersection(cont_sites %>% st_transform(3437), births_ws %>% st_transform(3437))
down_wells = down_wells %>% as_tibble() %>% dplyr::select(geoid, site, pfas = sum_pfoa_pfos) 
dwells = unique(down_wells$geoid)

#get lat long in births df
births = births %>% 
  st_transform(4326) %>%
  mutate(lng = sf::st_coordinates(.)[, 1],
         lat = sf::st_coordinates(.)[, 2]) %>% 
  st_transform(3437)

#only close down determines whether we include individuals on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
down_well_dist = function(w){
  dw = down_wells[which(down_wells$geoid == w), ]
  dw_ll = births %>% 
    as_tibble() %>%
    dplyr::filter(geoid == dw$geoid[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  rsdw_ll = cont_sites[which(cont_sites$site %in% dw$site & 
                               cont_sites$sum_pfoa_pfos %in% dw$pfas), c("lng", "lat")]
  
  ds = distm(dw_ll, rsdw_ll %>% as_tibble() %>% dplyr::select(lng, lat))
  
  ind_nearest = which.min(ds)
  
  rsdw_site = cont_sites$site[which(cont_sites$site %in% dw$site)]
  
  nearest_site = rsdw_site[ind_nearest]
  
  dw$n_sites_down = nrow(dw)
  dw = dw[which(dw$site == nearest_site[1]), ]
  dw$dist_down = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_down = pfas, site_down = site)
  
  dw$down = 1
  
  return(dw)
}

down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist))





up_wells = st_intersection(births %>% 
                             as_tibble() %>% 
                             dplyr::select(!geometry) %>% 
                             st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                             st_transform(3437), cont_ws %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, pfas = sum_pfoa_pfos)) %>% st_transform(3437))

up_wells = up_wells %>% as_tibble() %>% dplyr::select(geoid, site, pfas) 
uwells = unique(up_wells$geoid)

#only close up determines whether we include individuals on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w){
  dw = up_wells[which(up_wells$geoid == w), ]
  dw_ll = births %>% 
    as_tibble() %>%
    dplyr::filter(geoid == dw$geoid[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  rsdw_ll = cont_sites[which(cont_sites$site %in% dw$site & 
                               cont_sites$sum_pfoa_pfos %in% dw$pfas), c("lng", "lat")]
  
  ds = distm(dw_ll, rsdw_ll %>% as_tibble() %>% dplyr::select(lng, lat))
  
  ind_nearest = which.min(ds)
  
  rsdw_site = cont_sites$site[which(cont_sites$site %in% dw$site)]
  
  nearest_site = rsdw_site[ind_nearest]
  
  dw$n_sites_up = nrow(dw)
  dw = dw[which(dw$site == nearest_site[1]), ]
  dw$dist_up = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_up = pfas, site_up = site)
  
  dw$up = 1
  
  return(dw)
}

up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist))


births = births %>% 
  left_join(down_wells) %>% 
  left_join(up_wells)

births[is.na(births$down), ]$down = 0
births[is.na(births$up), ]$up = 0
births[is.na(births$n_sites_down), ]$n_sites_down = 0
births[is.na(births$n_sites_up), ]$n_sites_up = 0

#if both up and down, change up to 0
births[which(births$up == 1 & births$down == 1), ]$up = 0


#for wells that arent down or up, find nearest site and use that 
well_dist = function(i){
  w = births[i, ]
  
  dists = distm(c(w$lng, w$lat), cont_sites %>% as_tibble() %>% dplyr::select(lng, lat))
  
  ind = which.min(dists)
  
  #if neither down, nor up, set distance as that to the nearest site
  w$dist_near = dists[ind]
  #if neither down, nor up, set pfas as that at the nearest site
  w$pfas_near = cont_sites$sum_pfoa_pfos[ind]
  #get number of nearby sites
  w$n_sites_meters = length(which(dists <= 5010))
  
  w$site_near = cont_sites$site[ind]
  
  return(w)
  
  
}
births = dplyr::bind_rows(pblapply(1:nrow(births), well_dist))


#fill in down, up, side variables
well_assgn = function(i, drop_far_down = FALSE, drop_far_up = FALSE){
  w = births[i, ]
  down_far = 0
  
  #if distance for down well is less than meters, classify well as down, set pfas at level of relevant site
  d = w$dist_down
  if (!is.na(d)){ #if there is a down site
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

fwrite(births %>% as_tibble() %>% dplyr::select(geoid, births, down, up, pfas, dist, site, n_sites_meters), "Nat Data/births_sites_assigned5.csv")





