# #read in and set well watersheds
load("New Hampshire/Data/RData/wells_watershed.RData")

#read in well_ll to get appropriate sys and well ids
well_ll = fread("New Hampshire/Data/well_ll.csv")
well_ll$index = 1:nrow(well_ll)
wells_ws = wells_ws %>% left_join(well_ll %>% dplyr::select(sys_id, source, index))


#read in and set cont site watersheds 
load("New Hampshire/Data/RData/cont_watershed.RData")

#read in sites_ll to get right site number
rs_ll = fread("New Hampshire/Data/rs_ll.csv")
rs_ll$index = 1:nrow(rs_ll)

cont_ws = cont_ws %>% 
  left_join(rs_ll %>% dplyr::select(site, index))


#a well is downgradient if there is a site in its watershed
down_wells = st_intersection(cont_sites %>% st_transform(3437), wells_ws %>% st_transform(3437))
down_wells = down_wells %>% as_tibble() %>% dplyr::select(sys_id, source, site, pfas = sum_pfoa_pfos) 
down_wells$sys_id = str_pad(as.character(down_wells$sys_id), 7, "left", "0")
down_wells$source = str_pad(as.character(down_wells$source), 3, "left", "0")
down_wells$well = down_wells %>% 
  dplyr::group_by(sys_id, source) %>%
  dplyr::group_indices(sys_id, source)
dwells = unique(down_wells$well)

#only close down determines whether we include individuals on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
down_well_dist = function(w){
  dw = down_wells[which(down_wells$well == w), ]
  dw_ll = wells %>% 
    as_tibble() %>%
    dplyr::filter(source == dw$source[1] & sys_id == dw$sys_id[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  rsdw_ll = rs_ll[which(rs_ll$site %in% dw$site), c("lng", "lat")]
  
  ds = distm(dw_ll, rsdw_ll)
  
  ind_nearest = which.min(ds)
  
  rsdw_site = rs_ll[which(rs_ll$site %in% dw$site), "site"]
  
  nearest_site = rsdw_site[ind_nearest]
  
  dw$n_sites_down5 = length(which(ds <= meters))
  dw = dw[which(dw$site == nearest_site[1]), ]
  dw$dist_down = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_down = pfas, site_down = site)
  
  dw$down = 1
  
  return(dw)
}

down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist))

if (well_fd == TRUE){
  source("Code/Primary/Watersheds/wells_fd.R")
}


#for calculating upgradient, first obtain set of wells in the catchment area of sites
up_wells = st_intersection(wells %>% 
                             as_tibble() %>% 
                             dplyr::select(!geometry) %>% 
                             st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                             st_transform(3437), cont_ws %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, pfas = sum_pfoa_pfos)) %>% st_transform(3437))

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
  dw = up_wells[which(up_wells$well == w), ]
  dw_ll = wells %>% 
    as_tibble() %>%
    dplyr::filter(source == dw$source[1] & sys_id == dw$sys_id[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  rsdw_ll = rs_ll[which(rs_ll$site %in% dw$site), c("lng", "lat")]
  
  ds = distm(dw_ll, rsdw_ll)
  
  ind_nearest = which.min(ds)
  
  rsdw_site = rs_ll[which(rs_ll$site %in% dw$site), c("site", "pfas")]
  
  nearest_site = rsdw_site[ind_nearest]
  
  dw$n_sites_up5 = length(which(ds <= meters))
  dw = dw[which(dw$site == nearest_site[1]), ]
  
  dw$dist_up = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_up = pfas, site_up = site)
  
  dw$up = 1
  
  return(dw)
}

up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist))

wells = wells %>% 
  left_join(down_wells %>% dplyr::select(!well), by = c("sys_id", "source")) %>% 
  left_join(up_wells %>% dplyr::select(!well), by = c("sys_id", "source"))

wells[is.na(wells$down), ]$down = 0
wells[is.na(wells$up), ]$up = 0
wells[is.na(wells$n_sites_down5), ]$n_sites_down5 = 0
wells[is.na(wells$n_sites_up5), ]$n_sites_up5 = 0

#if both up and down, change up to 0
wells[which(wells$up == 1 & wells$down == 1), ]$up = 0

#get wind exposure
wells$wind_exposure = pbmapply(wind_function, wells$lng, wells$lat, rep(dist_allow, nrow(wells)))

#for wells that arent down or up, find nearest site and use that 
well_dist = function(i){
  w = wells[i, ]
  
  dists = distm(c(w$lng, w$lat), rs_ll[, c("lng", "lat")])
  
  ind = which.min(dists)
  
  #if neither down, nor up, set distance as that to the nearest site
  w$dist_near = dists[ind]
  #if neither down, nor up, set pfas as that at the nearest site
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
    if (d < meters){ #if the down site is within the buffer, assign its values to the well
      w$pfas = w$pfas_down
      w$site = w$site_down
      w$dist = w$dist_down
      w$down = 1
      return(w)
    }else if (d > meters & drop_far_down == TRUE){ #if the down site is outside the buffer, set values as missing (this will drop it in the regression)
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

if (well_fd == TRUE){
  wells2[is.na(wells2$well_fd), ]$well_fd = 0 
  df = df %>% 
    left_join(wells2 %>% 
                as_tibble() %>% 
                dplyr::select(sys_id, source, pfas, dist,  wind_exposure, site, up, down, n_sites = n_sites_meters, dist_down, dist_up, well_fd))  
}else{
  df = df %>% 
    left_join(wells2 %>% 
                as_tibble() %>% 
                dplyr::select(sys_id, source, pfas, dist,  wind_exposure, site, up, down, n_sites = n_sites_meters, dist_down, dist_up)) 
}

df$updown = ifelse(df$down == 1 | df$up == 1, 1, 0)
