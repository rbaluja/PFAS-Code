dem = terra::rast(modify_path("Data_Verify/Supplemental/LiDAR-Derived Bare Earth DEM - NH.tiff"))
z_raster = terra::init(dem, fun=0)

rs_ll = fread(modify_path("Data_Verify/GIS/rs_ll_ws.csv"))

#get lat long of each grid
ll = dplyr::bind_rows(pblapply(1:ncell(z_raster), function(x) data.frame(xyFromCell(z_raster, x))))
ll$index = 1:nrow(ll)

#get distance from ll to nearest cont site
ll_dist= st_distance(ll %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_transform(32110), 
                     cont_sites %>% st_transform(32110))
ll$csite_dist = apply(ll_dist, 1, min)

#subset to only those within 5km of a site
ll =ll[which(ll$csite_dist <= 5000), ]

ll$ws_file = 1:nrow(ll)



#soil porosity
ll =ll %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326, remove = F) %>% 
  st_transform(32110) %>% 
  st_buffer(10) %>% 
  st_transform(4326)

sp = terra::rast(modify_path("Data_Verify/Soil/por_gNATSGO/por_gNATSGO_US.tif"))
fs_sp = exactextractr::exact_extract(sp, ll)
ll = dplyr::bind_rows(pblapply(1:nrow(ll), flowacc,fs_sp,ll, "sp", cl = n_cores))

#available water capacity
awc = terra::rast(modify_path("Data_Verify/Soil/awc_gNATSGO/awc_gNATSGO_US.tif"))
fs_awc = exactextractr::exact_extract(awc, ll)
ll = dplyr::bind_rows(pblapply(1:nrow(ll), flowacc, fs_awc, ll, "awc", cl = n_cores))

#clay content
clay = terra::rast(modify_path("Data_Verify/Soil/isric/NH_mean_clay.tif"))
fs_clay = exactextractr::exact_extract(clay, ll)
ll = dplyr::bind_rows(pblapply(1:nrow(ll), flowacc, fs_clay, ll, "clay", cl = n_cores))

#sand content
sand = terra::rast(modify_path("Data_Verify/Soil/isric/NH_mean_sand.tif"))
fs_sand = exactextractr::exact_extract(sand, ll)
ll = dplyr::bind_rows(pblapply(1:nrow(ll), flowacc, fs_sand, ll, "sand", cl = n_cores))

#silt content
silt = terra::rast(modify_path("Data_Verify/Soil/isric/NH_mean_silt.tif"))
fs_silt = exactextractr::exact_extract(silt, ll)
ll = dplyr::bind_rows(pblapply(1:nrow(ll), flowacc, fs_silt, ll, "silt", cl = n_cores))


#get up and down for grid points
if (!file.exists(modify_path("Data_Verify/GIS/grid_ll_watershed.RData"))){
  source("PFAS-Code/PR/Figures/Grid Contamination/grid_cont_gis.R")
}else{
  load(modify_path("Data_Verify/GIS/grid_ll_watershed.RData")) 
}

#This follows the same general algorithm given in binary.R, where instead of looking at drinking water wells
#it instead looks at test wells. For any questions on code, see relevant comments in binary.R
#a well is downgradient if there is a site in its watershed
down_wells = st_intersection(cont_sites %>% 
                               dplyr::select(-any_of("index")) %>%
                               st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(3437), 
                             grid_ll_ws %>% st_transform(3437))
down_wells = down_wells %>% as_tibble() %>% dplyr::select(ws_file, site, pfas = sum_pfoa_pfos) 
dwells = unique(down_wells$ws_file)

#only close down determines whether we include individuals on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
down_well_dist = function(w){
  #select all observations in down_well for well w
  dw = down_wells[which(down_wells$ws_file == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  dw_ll =ll %>% 
    as_tibble() %>%
    dplyr::filter(ws_file == dw$ws_file[1]) %>% 
    dplyr::select(c("x", "y"))
  
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

down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist, cl = n_cores))

#for calculating upgradient, first obtain set of wells in the catchment area of sites
up_wells = st_intersection(ll %>% 
                             st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
                             st_transform(3437), 
                           cont_ws %>% 
                             left_join(cont_sites %>% 
                                         as_tibble() %>% 
                                         dplyr::select(site, pfas = sum_pfoa_pfos)) %>% 
                             st_transform(3437) %>% 
                             dplyr::select(!index))

up_wells = up_wells %>% as_tibble() %>% dplyr::select(ws_file, site, pfas) 
uwells = unique(up_wells$ws_file)

#only close up determines whether we include individuals on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w){
  #select all observations in up_well for well w
  uw = up_wells[which(up_wells$ws_file == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  uw_ll =ll %>% 
    as_tibble() %>%
    dplyr::filter(ws_file == uw$ws_file[1]) %>% 
    dplyr::select(c("x", "y"))
  
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

up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist, cl = n_cores))

ll =ll %>% 
  left_join(down_wells, by = c("ws_file")) %>% 
  left_join(up_wells, by = c("ws_file"))

#when NA, that means that they had no cont sites in their watershed (down) or 
#they were not in the watershed of any sites (up)
ll[is.na(ll$down), ]$down = 0
ll[is.na(ll$up), ]$up = 0
ll[is.na(ll$n_sites_down5), ]$n_sites_down5 = 0
ll[is.na(ll$n_sites_up5), ]$n_sites_up5 = 0

#find nearest site and its chars
ll_dist = function(i){
  w =ll[i, ]
  
  dists = distm(c(w$x, w$y), rs_ll[, c("lng", "lat")])
  
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
ll = dplyr::bind_rows(pblapply(1:nrow(ll), ll_dist, cl = n_cores))

#fill in down, up, side variables
ll_assgn = function(i, drop_far_down, drop_far_up){
  w =ll[i, ]
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

ll = dplyr::bind_rows(pblapply(1:nrow(ll), ll_assgn, FALSE, FALSE, cl = n_cores))


ll$wind_exposure = pbmapply(wind_function,ll$x,ll$y, rep(dist_allow, nrow(ll)))

ll$updown = ifelse((ll$down == 1 |ll$up == 1) & !is.na(ll$up) & !is.na(ll$down), 1, 0)

ll$pred_pfas = 0.769612 + 7.206569 * ll$down + 0.003181 * ll$sp + 
  -0.003632 * ll$awc + -0.006685 * ll$clay + -0.001860 * ll$sand + 0.002208 * ll$silt + 
  0.587664 * asinh(ll$pfas) + -0.420210 * log(ll$dist) + 
  -0.226273 * ll$updown + -0.002080 * ll$sp * ll$down +   0.000853 * ll$awc * ll$down + 
  -0.005056 * ll$clay * ll$down + -0.002351 * ll$sand * ll$down +  0.007307 * ll$silt * ll$down + 
  -0.745619 * log(ll$dist) * ll$down

nind = which(is.na(ll$pred_pfas))
ll[nind, ]$pred_pfas = 1.767553+ 5.543289 * ll[nind, ]$down + 
  0.671526 * asinh(ll[nind, ]$pfas) + -0.630132* log(ll[nind, ]$dist) + 
  -0.308053 * ll[nind, ]$updown + 
  -0.583208  * log(ll[nind, ]$dist) * ll[nind, ]$down


z_raster[ll$index] =ll$pred_pfas


