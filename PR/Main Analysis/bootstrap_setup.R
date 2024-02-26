#read in watersheds for test wells
load("Data_Verify/GIS/fs_test_watershed.RData")
fs_cont = fread("Data_Verify/Contamination/cleaned_contwell.csv")

#read in and set cont site watersheds 
load("Data_Verify/GIS/cont_watershed.RData")

#read in sites_ll to get right site number
rs_ll = fread("Data_Verify/GIS/rs_ll_ws.csv")

cont_ws = cont_ws %>% 
  left_join(rs_ll)

#This follows the same general algorithm given in binary.R, where instead of looking at drinking water wells
#it instead looks at test wells. For any questions on code, see relevant comments in binary.R
#a well is downgradient if there is a site in its watershed
down_wells = st_intersection(cont_sites %>% 
                               dplyr::select(-any_of("index")) %>%
                               st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(3437), 
                             test_ws %>% st_transform(3437))
down_wells = down_wells %>% as_tibble() %>% dplyr::select(index, site, pfas = sum_pfoa_pfos) 
dwells = unique(down_wells$index)

#only close down determines whether we include individuals on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
down_well_dist = function(w){
  #select all observations in down_well for well w
  dw = down_wells[which(down_wells$index == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  dw_ll = fs_cont %>% 
    as_tibble() %>%
    dplyr::filter(index == dw$index[1]) %>% 
    dplyr::select(c("well_lng", "well_lat"))
  
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

down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist, cl = 3))

#for calculating upgradient, first obtain set of wells in the catchment area of sites
up_wells = st_intersection(fs_cont %>% 
                             st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
                             st_transform(3437), 
                           cont_ws %>% 
                             left_join(cont_sites %>% 
                                         as_tibble() %>% 
                                         dplyr::select(site, pfas = sum_pfoa_pfos)) %>% 
                             st_transform(3437) %>% 
                             dplyr::select(!index))

up_wells = up_wells %>% as_tibble() %>% dplyr::select(index, site, pfas) 
uwells = unique(up_wells$index)

#only close up determines whether we include individuals on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w){
  #select all observations in up_well for well w
  uw = up_wells[which(up_wells$index == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  uw_ll = fs_cont %>% 
    as_tibble() %>%
    dplyr::filter(index == uw$index[1]) %>% 
    dplyr::select(c("well_lng", "well_lat"))
  
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

up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist, cl = 3))

fs_cont = fs_cont %>% 
  left_join(down_wells, by = c("index")) %>% 
  left_join(up_wells, by = c("index"))

#when NA, that means that they had no cont sites in their watershed (down) or 
#they were not in the watershed of any sites (up)
fs_cont[is.na(fs_cont$down), ]$down = 0
fs_cont[is.na(fs_cont$up), ]$up = 0
fs_cont[is.na(fs_cont$n_sites_down5), ]$n_sites_down5 = 0
fs_cont[is.na(fs_cont$n_sites_up5), ]$n_sites_up5 = 0

#find nearest site and its chars
fs_cont_dist = function(i){
  w = fs_cont[i, ]
  
  dists = distm(c(w$well_lng, w$well_lat), rs_ll[, c("lng", "lat")])
  
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
fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), fs_cont_dist, cl = 3))

#fill in down, up, side variables
fs_cont_assgn = function(i, drop_far_down, drop_far_up){
  w = fs_cont[i, ]
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

fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), fs_cont_assgn, drop_far_down, drop_far_up))

######
###Soil variables at the test well

#soil porosity
fs_cont_fa= fs_cont %>% 
  st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
  st_transform(32110) %>% 
  st_buffer(10) %>% 
  st_transform(4326)

sp = terra::rast("Data_Verify/Soil/por_gNATSGO/por_gNATSGO_US.tif")

fs_sp = exactextractr::exact_extract(sp, fs_cont_fa)

fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), flowacc, fs_sp, fs_cont, "sp"))

#available water capacity
awc = terra::rast("Data_Verify/Soil/awc_gNATSGO/awc_gNATSGO_US.tif")

fs_awc = exactextractr::exact_extract(awc, fs_cont_fa)

fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), flowacc, fs_awc, fs_cont, "awc"))


#get wind exposure
fs_cont$wind_exposure = pbmapply(wind_function, fs_cont$well_lng, fs_cont$well_lat, rep(dist_allow, nrow(fs_cont)))

fs_cont$wellpfas = fs_cont$pfos + fs_cont$pfoa
fs_cont$domestic = ifelse(fs_cont$watervapusage == "DOMESTIC", 1, 0)
fs_cont$t = fs_cont$year - 2010
fs_cont$updown = ifelse((fs_cont$down == 1 | fs_cont$up == 1) & !is.na(fs_cont$up) & !is.na(fs_cont$down), 1, 0)



#get soil characteristics at drinking wells
#soil porosity
wells_fa= wells %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(32110) %>% 
  st_buffer(10) %>% 
  st_transform(4326)

wells_sp = exactextractr::exact_extract(sp, wells_fa)

wells = dplyr::bind_rows(pblapply(1:nrow(wells_fa), flowacc, wells_sp, wells_fa, "sp"))

#available water capacity
wells_awc = exactextractr::exact_extract(sp, wells_fa)

wells = dplyr::bind_rows(pblapply(1:nrow(wells_fa), flowacc, wells_awc, wells, "awc"))

df = df %>% left_join(wells %>% as_tibble() %>% dplyr::select(sys_id, source, sp, awc)) 
