load("New Hampshire/Data/RData/fs_test_watershed.RData")
fs_cont = fread("New Hampshire/Data/Contamination/cleaned_contwell_122023.csv")

fs_cont$index = 1:nrow(fs_cont)

#a well is downgradient if there is a site in its watershed
down_wells = st_intersection(cont_sites %>% st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% st_transform(3437), test_ws %>% st_transform(3437))
down_wells = down_wells %>% as_tibble() %>% dplyr::select(index, site, pfas = sum_pfoa_pfos) 
down_wells$test_n = down_wells %>% 
  dplyr::group_by(index) %>%
  dplyr::group_indices(index)
dwells = unique(down_wells$test_n)

#only close down determines whether we include individuals on down wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
down_well_dist = function(w){
  dw = down_wells[which(down_wells$test_n == w), ]
  dw_ll = fs_cont %>% 
    as_tibble() %>%
    dplyr::filter(index== dw$index[1]) %>% 
    dplyr::select(c("well_lng", "well_lat"))
  
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

down_wells = dplyr::bind_rows(pblapply(dwells, down_well_dist, cl = 3))

if (test_fd == TRUE){
  source("Code/Primary/Watersheds/first_stage_fd.R")
}


#for calculating upgradient, first obtain set of wells in the catchment area of sites
up_wells = st_intersection(fs_cont %>% 
                             st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
                             st_transform(3437), cont_ws %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, pfas = sum_pfoa_pfos)) %>% st_transform(3437) %>% dplyr::select(!index))

up_wells = up_wells %>% as_tibble() %>% dplyr::select(index, site, pfas) 
up_wells$test_n = up_wells %>% 
  dplyr::group_by(index) %>%
  dplyr::group_indices(index)
uwells = unique(up_wells$test_n)

#only close up determines whether we include individuals on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w){
  dw = up_wells[which(up_wells$test_n == w), ]
  dw_ll = fs_cont %>% 
    as_tibble() %>%
    dplyr::filter(index== dw$index[1]) %>% 
    dplyr::select(c("well_lng", "well_lat"))
  
  rsdw_ll = rs_ll[which(rs_ll$site %in% dw$site), c("lng", "lat")]
  
  ds = distm(dw_ll, rsdw_ll)
  
  ind_nearest = which.min(ds)
  
  rsdw_site = rs_ll[which(rs_ll$site %in% dw$site), "site"]
  
  nearest_site = rsdw_site[ind_nearest]
  
  dw$n_sites_up5 = length(which(ds <= meters))
  dw = dw[which(dw$site == nearest_site[1]), ]
  dw$dist_up = ds[ind_nearest]
  
  dw = dw %>% 
    dplyr::rename(pfas_up = pfas, site_up = site)
  
  dw$up = 1
  
  return(dw)
}     

up_wells = dplyr::bind_rows(pblapply(uwells, up_well_dist, cl = 3))

fs_cont = fs_cont %>% 
  left_join(down_wells, by = c("index")) %>% 
  left_join(up_wells, by = c("index"))

fs_cont[is.na(fs_cont$down), ]$down = 0
fs_cont[is.na(fs_cont$up), ]$up = 0
fs_cont[is.na(fs_cont$n_sites_down5), ]$n_sites_down5 = 0
fs_cont[is.na(fs_cont$n_sites_up5), ]$n_sites_up5 = 0

#if both up and down, change up to 0
fs_cont[which(fs_cont$up == 1 & fs_cont$down == 1), ]$up = 0

#get wind exposure
fs_cont$wind_exposure = pbmapply(wind_function, fs_cont$well_lng, fs_cont$well_lat, rep(dist_allow, nrow(fs_cont)))

#for wells that arent down or up, find nearest site and use that 
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
    if (d_up < meters & w$n_sites_up5 == w$n_sites_meters){ #if the up site is within the buffer, assign its values to the well
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

if (soil_well == TRUE){
  #soil porosity
  fs_cont_fa= fs_cont %>% 
    st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
    st_transform(32110) %>% 
    st_buffer(10) %>% 
    st_transform(4326)
  
  sp = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/por_gNATSGO/por_gNATSGO_US.tif")
  
  fs_sp = exactextractr::exact_extract(sp, fs_cont_fa)
  
  fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), flowacc, fs_sp, fs_cont, "sp"))
  
  #available water capacity
  fs_cont_fa= fs_cont %>% 
    st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
    st_transform(32110) %>% 
    st_buffer(10) %>% 
    st_transform(4326)
  
  awc = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/awc_gNATSGO/awc_gNATSGO_US.tif")
  
  fs_awc = exactextractr::exact_extract(awc, fs_cont_fa)
  
  fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), flowacc, fs_awc, fs_cont, "awc"))
  
  
  #field content
  fs_cont_fa= fs_cont %>% 
    st_as_sf(coords = c("well_lng", "well_lat"), crs = 4326) %>% 
    st_transform(32110) %>% 
    st_buffer(10) %>% 
    st_transform(4326)
  
  fc = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/fc_gNATSGO/fc_gNATSGO_US.tif")
  
  fs_fc = exactextractr::exact_extract(fc, fs_cont_fa)
  
  fs_cont = dplyr::bind_rows(pblapply(1:nrow(fs_cont), flowacc, fs_fc, fs_cont, "fc"))
}else{#getting soil chars at release site
  
  #soil porosity
  rs_fa= cont_sites %>% 
    st_transform(32110) %>% 
    st_buffer(10) %>% 
    st_transform(4326)
  
  sp = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/por_gNATSGO/por_gNATSGO_US.tif")
  
  rs_sp = exactextractr::exact_extract(sp, rs_fa)
  
  cont_sites = dplyr::bind_rows(pblapply(1:nrow(rs_fa), flowacc, rs_sp, rs_fa, "sp"))
  
  #available water capacity
  awc = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/awc_gNATSGO/awc_gNATSGO_US.tif")
  
  rs_awc = exactextractr::exact_extract(sp, cont_sites)
  
  cont_sites = dplyr::bind_rows(pblapply(1:nrow(rs_fa), flowacc, rs_awc, cont_sites, "awc"))
  fs_cont = fs_cont %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(awc, sp, site))
}



#run regressions
fs_cont$wellpfas = fs_cont$pfos + fs_cont$pfoa
fs_cont$domestic = ifelse(fs_cont$watervapusage == "DOMESTIC", 1, 0)
fs_cont$t = fs_cont$year - 2010
fs_cont$updown = ifelse((fs_cont$down == 1 | fs_cont$up == 1) & !is.na(fs_cont$up) & !is.na(fs_cont$down), 1, 0)
w_reg = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                        updown + wind_exposure + domestic + temp + pm25 + med_inc +
                        p_manuf + n_hunits + med_hprice + elevation + tri5 + t, data = fs_cont) 

w_reg_nat = fixest::feols(asinh(wellpfas) ~ down * poly(sp, awc, degree = 1, raw = TRUE) + asinh(pfas) + log(dist)*down + 
                        updown, data = fs_cont) 

w_reg_nos = fixest::feols(asinh(wellpfas) ~ down + asinh(pfas) + log(dist)*down + 
                            updown, data = fs_cont) 


if (soil_well == TRUE){
  #get soil characterists at drinking wells
  #soil porosity
  wells_fa= wells %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
    st_transform(32110) %>% 
    st_buffer(10) %>% 
    st_transform(4326)
  
  sp = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/por_gNATSGO/por_gNATSGO_US.tif")
  
  wells_sp = exactextractr::exact_extract(sp, wells_fa)
  
  wells = dplyr::bind_rows(pblapply(1:nrow(wells_fa), flowacc, wells_sp, wells_fa, "sp"))
  
  #available water capacity
  awc = terra::rast("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/Soil/awc_gNATSGO/awc_gNATSGO_US.tif")
  
  wells_awc = exactextractr::exact_extract(sp, wells_fa)
  
  wells = dplyr::bind_rows(pblapply(1:nrow(wells_fa), flowacc, wells_awc, wells, "awc"))
  
  df = df %>% left_join(wells %>% as_tibble() %>% dplyr::select(sys_id, source, sp, awc)) 
}else{
  df = df %>% left_join(cont_sites %>% as_tibble() %>% dplyr::select(awc, sp, site))
}



df$domestic = 0
df$elevation = df$well_elev
df$t = as.numeric(df$year) - 2010
df$pred_pfas = predict(w_reg, df)
