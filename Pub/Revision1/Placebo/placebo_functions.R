placebo = function(i, df, wells){
  psites = st_sample(nh_shape, 41)
  
  psites = do.call(rbind, st_geometry(psites)) %>% 
    as_tibble() %>% setNames(c("lng","lat")) %>% 
    st_as_sf(coords = c("lng", "lat"), crs = 32110)
  
  psites = psites %>% 
    st_transform(4326)
  
  psites$site = 1:nrow(psites)
  

  #create directories for watershed calculation
  dir.create("Data_Verify_Revision/GIS/placebo")
  dir.create("Data_Verify_Revision/GIS/placebo/cont_pp")
  dir.create("Data_Verify_Revision/GIS/placebo/cont_watershed")
  dir.create("Data_Verify_Revision/GIS/placebo/cont_watershed/Shapes")
  #calculate watershed for each placebo site
  lapply(1:nrow(psites), placebo_ws, psites)
  
  #get watershed dataframe
  cont_ws = combine_placebo_ws()
  
  df = bin_spec(df, wells, cont_ws, psites)
  
  #get distance to nearest site
  X = st_distance(df %>% st_transform(32110), psites %>% st_transform(32110))
  min_distances = apply(X, 1, min)
  df$csite_dist = min_distances
  
  regr = tryCatch({
    placebo_res(df)
  }, error = function(e) {
    #if an error occurs, set regr to a predefined data frame
    boot_coefs = data.frame(matrix(ncol = 44, nrow = 1))
    colnames(boot_coefs) = c("m_age", "m_age_se", 
                             "m_married", "m_married_se",
                             "private_insurance", "private_insurance_se",
                             "nbr_cgrtt", "nbr_cgrtt_se",
                             "m_educ", "m_educ_se",
                             "f_educ", "f_educ_se",
                             "mr_04", "mr_04_se",
                             "mr_18", "mr_18_se",
                             "mr_08", "mr_08_se",
                             "mr_21", "mr_21_se",
                             "mr_26", "mr_26_se",
                             "mr_27", "mr_27_se",
                             "mthr_wgt_dlv", "mthr_wgt_dlv_se",
                             "mthr_pre_preg_wgt", "mthr_pre_preg_wgt_se",
                             "m_height", "m_height_se",
                             "med_hprice", "med_hprice_se",
                             "med_inc", "med_inc_se", 
                             "rural", "rural_se", 
                             "well_elev", "well_elev_se", 
                             "resid_elev, resid_elev_se", 
                             "temp", "temp_se", 
                             "pm25", "pm25_se")
    return(boot_coefs)
  })
  
  #delete intermediate files
  unlink("Data_Verify_Revision/GIS/placebo/", recursive = TRUE)
  
  return(regr)
  
}


placebo_ws = function(i, psites){
  point_sf = psites[i, "geometry"]
  temp_point_path = tempfile(fileext = ".shp")
  st_write(point_sf, temp_point_path, quiet = TRUE)
  
  #run snap pour points
  wbt_snap_pour_points(pour_pts = temp_point_path, 
                       flow_accum = "Data_Verify_Revision/GIS/flow_acc.tiff", 
                       output = paste0("Data_Verify_Revision/GIS/placebo/cont_pp/pp_site_", i, ".shp"), 
                       snap_dist = 0.007569 * 5)
  #calculate watershed
  wbt_watershed(d8_pntr = "Data_Verify_Revision/GIS/flow_dir.tiff", 
                pour_pts = paste0("Data_Verify_Revision/GIS/placebo/cont_pp/pp_site_", i, ".shp"), 
                output = paste0("Data_Verify_Revision/GIS/placebo/cont_watershed/watershed_", i, ".tiff"))
  #read in watershed
  ws = terra::rast(paste0("Data_Verify_Revision/GIS/placebo/cont_watershed/watershed_", i, ".tiff"))
  
  #transform watershed to a polygon
  ws_poly = as.polygons(ws)
  #save shapefile of watershed
  writeVector(ws_poly, paste0("Data_Verify_Revision/GIS/placebo/cont_watershed/Shapes/ws_shape_", i, ".shp"), overwrite = TRUE)
}

combine_placebo_ws = function(){
  files = list.files("Data_Verify_Revision/GIS/placebo/cont_watershed/Shapes", pattern = "*.shp", recursive = T, full.names = T)
  files = files[!endsWith(files, ".xml")]
  
  cont_ws = function(f){
    w_ws1 = st_read(f)
    w_ws1 = w_ws1 %>% st_transform(3437) %>% dplyr::summarise(geometry = st_union(geometry))
    w_ws1$site = as.numeric(gsub("[^0-9]", "", f))
    return(w_ws1)
  }
  
  cont_ws = dplyr::bind_rows(lapply(files, cont_ws))
  
  return(cont_ws)
}


bin_spec = function(df, wells, cont_ws, psites){
  # #read in and set well watersheds
  load("Data_Verify/GIS/wells_watershed.RData")
  
  #read in well_ll to get appropriate sys and well ids
  well_ll = fread("Data_Verify/GIS/wells_ll_ws.csv")
  wells_ws = wells_ws %>% left_join(well_ll)
  
  #a well is downgradient if there is a site in its watershed
  down_wells = st_intersection(psites %>% st_transform(3437), wells_ws %>% st_transform(3437))
  down_wells$sys_id = str_pad(as.character(down_wells$sys_id), 7, "left", "0")
  down_wells$source = str_pad(as.character(down_wells$source), 3, "left", "0")
  down_wells$well = down_wells %>% 
    dplyr::group_by(sys_id, source) %>%
    dplyr::group_indices(sys_id, source)
  dwells = unique(down_wells$well)
  
  
  down_wells = dplyr::bind_rows(lapply(dwells, down_well_dist, down_wells, psites, wells))
  
  
  #for calculating upgradient, first obtain set of wells in the catchment area of sites
  up_wells = st_intersection(wells %>% 
                               as_tibble() %>% 
                               dplyr::select(!geometry) %>% 
                               st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                               st_transform(3437), cont_ws %>% st_transform(3437))
  
  up_wells = up_wells %>% as_tibble() %>% dplyr::select(sys_id, source, site) 
  up_wells$sys_id = str_pad(as.character(up_wells$sys_id), 7, "left", "0")
  up_wells$source = str_pad(as.character(up_wells$source), 3, "left", "0")
  
  up_wells = up_wells %>% left_join(well_ll %>% 
                                      dplyr::mutate(sys_id = str_pad(as.character(sys_id), 7, "left", "0"), 
                                                    source = str_pad(as.character(source), 3, "left", "0")) %>%
                                      dplyr::select(sys_id, source, index))
  up_wells$well = up_wells %>% 
    dplyr::group_by(sys_id, source) %>%
    dplyr::group_indices(sys_id, source)
  uwells = unique(up_wells$well)
  up_wells = dplyr::bind_rows(lapply(uwells, up_well_dist, up_wells, psites, wells))
  
  
  
  wells = wells %>% 
    left_join(down_wells %>% 
                as_tibble() %>% 
                dplyr::select(!c(well, index, site, geometry)), by = c("sys_id", "source")) %>% 
    left_join(up_wells %>% 
                dplyr::select(!c(well, site, index)), by = c("sys_id", "source"))
  
  wells[is.na(wells$down), ]$down = 0
  wells[is.na(wells$up), ]$up = 0
  wells[is.na(wells$n_sites_down5), ]$n_sites_down5 = 0
  wells[is.na(wells$n_sites_up5), ]$n_sites_up5 = 0
  
  
  wells1 = dplyr::bind_rows(lapply(1:nrow(wells), well_dist, wells, psites))
  
  
  wells2 = dplyr::bind_rows(lapply(1:nrow(wells1), well_assgn, drop_far_down, drop_far_up, wells1))
  
  df = df %>% 
    left_join(wells2 %>% 
                as_tibble() %>% 
                dplyr::select(sys_id, source, dist, up, down, n_sites = n_sites_meters, dist_down, dist_up, site)) 
  
  df$updown = ifelse(df$down == 1 | df$up == 1, 1, 0)
  
  return(df)
  
}


down_well_dist = function(w, down_wells, psites, wells){
  #select all observations in down_well for well w
  dw = down_wells[which(down_wells$well == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  dw_ll = wells %>% 
    as_tibble() %>%
    dplyr::filter(source == dw$source[1] & sys_id == dw$sys_id[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  #get the lat longs for all sites in w's watershed
  rsdw_site = psites[which(psites$site %in% dw$site), "site"]
  
  #get distance matrix for the well to each relevant release site 
  ds = as.numeric(st_distance(dw_ll %>% 
                                st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                                st_transform(32110), rsdw_site %>% st_transform(32110)))
  
  #get the index of the nearest relevant release site
  ind_nearest = which.min(ds)

  nearest_site = rsdw_site %>% 
    as_tibble() %>% 
   dplyr::select(site)
  nearest_site = as.character(nearest_site[ind_nearest, "site"])
  
  #how many down sites are within 'meters' of the well?
  dw$n_sites_down5 = length(which(ds <= meters))
  #subset dw to only the nearest down site
  dw = dw[which(dw$site == nearest_site), ]
  #append distance to nearest site to dw
  dw$dist_down = ds[ind_nearest]
  
  dw$site_down = as.numeric(nearest_site)
  
  dw$down = 1
  
  return(dw)
}


#only close up determines whether we include individuals on up wells that are further than meters in analysis
#if 1, then we do include them if they have a nearby well that is closer than meters (they will be on the side)
#if 0, then we do not include them
up_well_dist = function(w, up_wells, psites, wells){
  #select all observations in up_well for well w
  uw = up_wells[which(up_wells$well == w), ]
  #filter wells (from NHDES_PWS.R) to only include that well and its coordinates
  uw_ll = wells %>% 
    as_tibble() %>%
    dplyr::filter(source == uw$source[1] & sys_id == uw$sys_id[1]) %>% 
    dplyr::select(c("lng", "lat"))
  
  #get the lat longs for all sites for which w is in their watershed
  rsuw_ll = psites[which(psites$site %in% uw$site), ]
  
  #get distance matrix for the well to each relevant release site 
  ds = as.numeric(st_distance(uw_ll %>% 
                                st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                                st_transform(32110), rsuw_ll %>% st_transform(32110)))
  
  #get distance matrix for the well to each relevant release site 
  ind_nearest = which.min(ds)
  
  #get the site name for the nearest relevant release site
  nearest_site = rsuw_ll %>% 
    as_tibble() %>% 
    dplyr::select(site)
  nearest_site = as.character(nearest_site[ind_nearest, "site"])
  
  #how many up sites are within 'meters' of the well?
  uw$n_sites_up5 = length(which(ds <= meters))
  #subset uw to only the nearest down site
  uw = uw[which(uw$site == nearest_site), ]
  #append distance to nearest site to uw
  uw$dist_up = ds[ind_nearest]
  
  uw$up = 1
  
  uw$site_up = as.numeric(nearest_site)
  return(uw)
}

#for wells that arent down or up, find nearest site and use that 
well_dist = function(i, wells, psites){
  w = wells[i, ]
  
  dists = as.numeric(st_distance(w %>% 
                                   as_tibble() %>% 
                                   dplyr::select(!geometry) %>% 
                                   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
                                   st_transform(32110), psites %>% st_transform(32110)))
  
  ind = which.min(dists)
  
  #if neither down, nor up, set distance as that to the nearest site
  w$dist_near = dists[ind]
  #get number of nearby sites
  w$n_sites_meters = length(which(dists <= meters))
  w$site_near = psites$site[ind]
  
  return(w)
  
  
}


#fill in down, up, side variables
well_assgn = function(i, drop_far_down, drop_far_up, wells1){
  w = wells1[i, ]
  down_far = 0
  
  #if distance for down well is less than meters, classify well as down, set pfas at level of relevant site
  d = w$dist_down
  if (!is.na(d)){ #if there is a down site
    w$up = 0
    if (d < meters){ #if the down site is within the buffer, assign its values to the well
      w$dist = w$dist_down
      w$down = 1
      w$site = w$site_down
      return(w)
    }else if (d > meters & drop_far_down == TRUE){ #if the down site is outside the buffer, set values as missing (this will drop it in the regression)
      w$dist = NA
      w$down = NA
      w$site = NA
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
    if (d_up < meters & w$n_sites_up5 == w$n_sites_meters){ #if the up site is within the buffer, and all nearby sites are up, assign its values to the well
      w$dist = w$dist_up
      w$up = 1
      w$site = w$site_up
      return(w)
    }else if (d_up > meters & drop_far_up == TRUE){ #if the up site is outside the buffer, set values as missing (this will drop it in the regression)
      w$dist = NA
      w$up = NA
      w$site = NA
      return(w)
    }else{ #otherwise, d > meters and we reclassify
      w$up = 0
    }
  }
  
  #if we get to this point, then the well has no nearby up or down (or we are reclassifying, or there is a nearby up site, but it is down of something)
  w$up = 0
  d_side = w$dist_near
  if (d_side < meters){
    w$dist = w$dist_near
    w$site = w$site_near
    return(w)
  }else{
    w$dist = NA
    w$site = NA
    return(w)
  }
  
}


placebo_res = function(df){
  boot_coefs = data.frame(matrix(ncol = 44, nrow = 1))
  colnames(boot_coefs) = c("m_age", "m_age_se", 
                           "m_married", "m_married_se",
                           "private_insurance", "private_insurance_se",
                           "nbr_cgrtt", "nbr_cgrtt_se",
                           "m_educ", "m_educ_se",
                           "f_educ", "f_educ_se",
                           "mr_04", "mr_04_se",
                           "mr_18", "mr_18_se",
                           "mr_08", "mr_08_se",
                           "mr_21", "mr_21_se",
                           "mr_26", "mr_26_se",
                           "mr_27", "mr_27_se",
                           "mthr_wgt_dlv", "mthr_wgt_dlv_se",
                           "mthr_pre_preg_wgt", "mthr_pre_preg_wgt_se",
                           "m_height", "m_height_se",
                           "med_hprice", "med_hprice_se",
                           "med_inc", "med_inc_se", 
                           "rural", "rural_se", 
                           "well_elev", "well_elev_se", 
                           "resid_elev, resid_elev_se", 
                           "temp", "temp_se", 
                           "pm25", "pm25_se")
  
  
  dreg = fixest::feols(down ~ m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height+ med_hprice + med_inc + rural + well_elev + resid_elev + temp + pm25
                       |county + year^month + birth_race_dsc_1, 
                       data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  boot_coefs[1, "m_age"] = dreg$coefficients["m_age"]
  boot_coefs[1, "m_age_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["m_age", "m_age"])
  
  boot_coefs[1, "m_married"] = dreg$coefficients["m_married"]
  boot_coefs[1, "m_married_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["m_married", "m_married"])
  
  boot_coefs[1, "private_insurance"] = dreg$coefficients["private_insurance"]
  boot_coefs[1, "private_insurance_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["private_insurance", "private_insurance"])
  
  boot_coefs[1, "nbr_cgrtt"] = dreg$coefficients["nbr_cgrtt"]
  boot_coefs[1, "nbr_cgrtt_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["nbr_cgrtt", "nbr_cgrtt"])
  
  boot_coefs[1, "m_educ"] = dreg$coefficients["m_educ"]
  boot_coefs[1, "m_educ_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["m_educ", "m_educ"])
  
  boot_coefs[1, "f_educ"] = dreg$coefficients["f_educ"]
  boot_coefs[1, "f_educ_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["f_educ", "f_educ"])
  
  boot_coefs[1, "mr_04"] = dreg$coefficients["mr_04"]
  boot_coefs[1, "mr_04_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mr_04", "mr_04"])
  
  boot_coefs[1, "mr_18"] = dreg$coefficients["mr_18"]
  boot_coefs[1, "mr_18_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mr_18", "mr_18"])
  
  boot_coefs[1, "mr_08"] = dreg$coefficients["mr_08"]
  boot_coefs[1, "mr_08_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mr_08", "mr_08"])
  
  boot_coefs[1, "mr_21"] = dreg$coefficients["mr_21"]
  boot_coefs[1, "mr_21_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mr_21", "mr_21"])
  
  boot_coefs[1, "mr_26"] = dreg$coefficients["mr_26"]
  boot_coefs[1, "mr_26_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mr_26", "mr_26"])
  
  boot_coefs[1, "mr_27"] = dreg$coefficients["mr_27"]
  boot_coefs[1, "mr_27_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mr_27", "mr_27"])
  
  boot_coefs[1, "mthr_wgt_dlv"] = dreg$coefficients["mthr_wgt_dlv"]
  boot_coefs[1, "mthr_wgt_dlv_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mthr_wgt_dlv", "mthr_wgt_dlv"])
  
  boot_coefs[1, "mthr_pre_preg_wgt"] = dreg$coefficients["mthr_pre_preg_wgt"]
  boot_coefs[1, "mthr_pre_preg_wgt_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["mthr_pre_preg_wgt", "mthr_pre_preg_wgt"])
  
  boot_coefs[1, "m_height"] = dreg$coefficients["m_height"]
  boot_coefs[1, "m_height_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["m_height", "m_height"])
  
  boot_coefs[1, "med_hprice"] = dreg$coefficients["med_hprice"]
  boot_coefs[1, "med_hprice_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["med_hprice", "med_hprice"])
  
  boot_coefs[1, "med_inc"] = dreg$coefficients["med_inc"]
  boot_coefs[1, "med_inc_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["med_inc", "med_inc"])
  
  boot_coefs[1, "rural"] = dreg$coefficients["rural"]
  boot_coefs[1, "rural_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["rural", "rural"])
  
  boot_coefs[1, "well_elev"] = dreg$coefficients["well_elev"]
  boot_coefs[1, "well_elev_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["well_elev", "well_elev"])
  
  boot_coefs[1, "resid_elev"] = dreg$coefficients["resid_elev"]
  boot_coefs[1, "resid_elev_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["resid_elev", "resid_elev"])
  
  boot_coefs[1, "temp"] = dreg$coefficients["temp"]
  boot_coefs[1, "temp_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["temp", "temp"])
  
  boot_coefs[1, "pm25"] = dreg$coefficients["pm25"]
  boot_coefs[1, "pm25_se"] = sqrt(vcov(dreg, cluster = c("site", "year^month"))["pm25", "pm25"])
  
  
  
  return(boot_coefs)
  
  
  
  
}
