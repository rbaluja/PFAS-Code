placebo = function(i, df, wells, iv = FALSE){
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
    if(!iv){
      placebo_res_corr(df)
    }else
      placebo_res_iv(df)
  }, error = function(e) {
    if (!iv){
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
    }else{
      boot_coefs = data.frame(matrix(ncol = 40, nrow = 1))
      colnames(boot_coefs) = c("pre_d", "pre_d_se", 
                               "pre_u", "pre_u_se",
                               "mpre_d", "mpre_d_se",
                               "mpre_u", "mpre_u_se",
                               "vpre_d", "vpre_d_se",
                               "vpre_u", "vpre_u_se",
                               "epre_d", "epre_d_se",
                               "epre_u", "epre_u_se",
                               "lbw_d", "lbw_d_se",
                               "lbw_u", "lbw_u_se",
                               "lbwft_d", "lbwft_d_se",
                               "lbwft_u", "lbwft_u_se",
                               "mlbw_d", "mlbw_d_se",
                               "mlbw_u", "mlbw_u_se",
                               "vlbw_d", "vlbw_d_se",
                               "vlbw_u", "vlbw_u_se",
                               "elbw_d", "elbw_d_se",
                               "elbw_u", "elbw_u_se",
                               "mort_d", "mort_d_se",
                               "mort_u", "mort_u_se")
    }
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
  load("Data_Verify_Pub/GIS/wells_watershed.RData")
  
  #read in well_ll to get appropriate sys and well ids
  well_ll = fread("Data_Verify_Pub/GIS/wells_ll_ws.csv")
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


placebo_res_corr = function(df){
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
                           "resid_elev", "resid_elev_se", 
                           "temp", "temp_se", 
                           "pm25", "pm25_se")
  

  
  dreg_m_age = fixest::feols(down ~ m_age,
                       data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_m_married = fixest::feols(down ~ m_married,
                            data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_private_insurance = fixest::feols(down ~ private_insurance,
                                  data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_nbr_cgrtt = fixest::feols(down ~ nbr_cgrtt,
                            data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_m_educ = fixest::feols(down ~ m_educ,
                        data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_f_educ = fixest::feols(down ~ f_educ,
                        data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mr04 = fixest::feols(down ~ mr_04,
                      data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mr18 = fixest::feols(down ~ mr_18,
                            data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mr08 = fixest::feols(down ~ mr_08,
                            data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mr21 = fixest::feols(down ~ mr_21,
                            data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mr26 = fixest::feols(down ~ mr_26,
                            data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mr27 = fixest::feols(down ~ mr_27,
                            data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mthr_wgt_dlv = fixest::feols(down ~ mthr_wgt_dlv,
                              data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_mthr_pre_preg_wgt = fixest::feols(down ~ mthr_pre_preg_wgt,
                                  data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_m_height = fixest::feols(down ~ m_height,
                          data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_med_hprice = fixest::feols(down ~ med_hprice,
                           data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  dreg_med_inc = fixest::feols(down ~ med_inc,
                       data = df, warn = F, notes = F, cluster = c("site", "year^month"))
 
  dreg_rural = fixest::feols(down ~ rural,
                      data = df, warn = F, notes = F, cluster = c("site", "year^month"))
 
  dreg_well_elev = fixest::feols(down ~ well_elev,
                          data = df, warn = F, notes = F, cluster = c("site", "year^month"))
 
  dreg_resid_elev = fixest::feols(down ~ resid_elev,
                           data = df, warn = F, notes = F, cluster = c("site", "year^month"))
 
  dreg_temp = fixest::feols(down ~ temp,
                    data = df, warn = F, notes = F, cluster = c("site", "year^month"))
 
  dreg_pm25 = fixest::feols(down ~ pm25,
                     data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  
  boot_coefs[1, "m_age"] = dreg_m_age$coefficients["m_age"]
  boot_coefs[1, "m_age_se"] = sqrt(vcov(dreg_m_age, cluster = c("site", "year^month"))["m_age", "m_age"])
  
  boot_coefs[1, "m_married"] = dreg_m_married$coefficients["m_married"]
  boot_coefs[1, "m_married_se"] = sqrt(vcov(dreg_m_married, cluster = c("site", "year^month"))["m_married", "m_married"])
  
  boot_coefs[1, "private_insurance"] = dreg_private_insurance$coefficients["private_insurance"]
  boot_coefs[1, "private_insurance_se"] = sqrt(vcov(dreg_private_insurance, cluster = c("site", "year^month"))["private_insurance", "private_insurance"])
  
  boot_coefs[1, "nbr_cgrtt"] = dreg_nbr_cgrtt$coefficients["nbr_cgrtt"]
  boot_coefs[1, "nbr_cgrtt_se"] = sqrt(vcov(dreg_nbr_cgrtt, cluster = c("site", "year^month"))["nbr_cgrtt", "nbr_cgrtt"])
  
  boot_coefs[1, "m_educ"] = dreg_m_educ$coefficients["m_educ"]
  boot_coefs[1, "m_educ_se"] = sqrt(vcov(dreg_m_educ, cluster = c("site", "year^month"))["m_educ", "m_educ"])
  
  boot_coefs[1, "f_educ"] = dreg_f_educ$coefficients["f_educ"]
  boot_coefs[1, "f_educ_se"] = sqrt(vcov(dreg_f_educ, cluster = c("site", "year^month"))["f_educ", "f_educ"])
  
  boot_coefs[1, "mr_04"] = dreg_mr04$coefficients["mr_04"]
  boot_coefs[1, "mr_04_se"] = sqrt(vcov(dreg_mr04, cluster = c("site", "year^month"))["mr_04", "mr_04"])
  
  boot_coefs[1, "mr_18"] = dreg_mr18$coefficients["mr_18"]
  boot_coefs[1, "mr_18_se"] = sqrt(vcov(dreg_mr18, cluster = c("site", "year^month"))["mr_18", "mr_18"])
  
  boot_coefs[1, "mr_08"] = dreg_mr08$coefficients["mr_08"]
  boot_coefs[1, "mr_08_se"] = sqrt(vcov(dreg_mr08, cluster = c("site", "year^month"))["mr_08", "mr_08"])
  
  boot_coefs[1, "mr_21"] = dreg_mr21$coefficients["mr_21"]
  boot_coefs[1, "mr_21_se"] = sqrt(vcov(dreg_mr21, cluster = c("site", "year^month"))["mr_21", "mr_21"])
  
  boot_coefs[1, "mr_26"] = dreg_mr26$coefficients["mr_26"]
  boot_coefs[1, "mr_26_se"] = sqrt(vcov(dreg_mr26, cluster = c("site", "year^month"))["mr_26", "mr_26"])
  
  boot_coefs[1, "mr_27"] = dreg_mr27$coefficients["mr_27"]
  boot_coefs[1, "mr_27_se"] = sqrt(vcov(dreg_mr27, cluster = c("site", "year^month"))["mr_27", "mr_27"])
  
  boot_coefs[1, "mthr_wgt_dlv"] = dreg_mthr_wgt_dlv$coefficients["mthr_wgt_dlv"]
  boot_coefs[1, "mthr_wgt_dlv_se"] = sqrt(vcov(dreg_mthr_wgt_dlv, cluster = c("site", "year^month"))["mthr_wgt_dlv", "mthr_wgt_dlv"])
  
  boot_coefs[1, "mthr_pre_preg_wgt"] = dreg_mthr_pre_preg_wgt$coefficients["mthr_pre_preg_wgt"]
  boot_coefs[1, "mthr_pre_preg_wgt_se"] = sqrt(vcov(dreg_mthr_pre_preg_wgt, cluster = c("site", "year^month"))["mthr_pre_preg_wgt", "mthr_pre_preg_wgt"])
  
  boot_coefs[1, "m_height"] = dreg_m_height$coefficients["m_height"]
  boot_coefs[1, "m_height_se"] = sqrt(vcov(dreg_m_height, cluster = c("site", "year^month"))["m_height", "m_height"])
  
  boot_coefs[1, "med_hprice"] = dreg_med_hprice$coefficients["med_hprice"]
  boot_coefs[1, "med_hprice_se"] = sqrt(vcov(dreg_med_hprice, cluster = c("site", "year^month"))["med_hprice", "med_hprice"])
  
  boot_coefs[1, "med_inc"] = dreg_med_inc$coefficients["med_inc"]
  boot_coefs[1, "med_inc_se"] = sqrt(vcov(dreg_med_inc, cluster = c("site", "year^month"))["med_inc", "med_inc"])
  
  boot_coefs[1, "rural"] = dreg_rural$coefficients["rural"]
  boot_coefs[1, "rural_se"] = sqrt(vcov(dreg_rural, cluster = c("site", "year^month"))["rural", "rural"])
  
  boot_coefs[1, "well_elev"] = dreg_well_elev$coefficients["well_elev"]
  boot_coefs[1, "well_elev_se"] = sqrt(vcov(dreg_well_elev, cluster = c("site", "year^month"))["well_elev", "well_elev"])
  
  boot_coefs[1, "resid_elev"] = dreg_resid_elev$coefficients["resid_elev"]
  boot_coefs[1, "resid_elev_se"] = sqrt(vcov(dreg_resid_elev, cluster = c("site", "year^month"))["resid_elev", "resid_elev"])
  
  boot_coefs[1, "temp"] = dreg_temp$coefficients["temp"]
  boot_coefs[1, "temp_se"] = sqrt(vcov(dreg_temp, cluster = c("site", "year^month"))["temp", "temp"])
  
  boot_coefs[1, "pm25"] = dreg_pm25$coefficients["pm25"]
  boot_coefs[1, "pm25_se"] = sqrt(vcov(dreg_pm25, cluster = c("site", "year^month"))["pm25", "pm25"])
  
  
  
  return(boot_coefs)
  
  
  
  
}


placebo_res_iv = function(df){
  boot_coefs = data.frame(matrix(ncol = 40, nrow = 1))
  colnames(boot_coefs) = c("pre_d", "pre_d_se", 
                           "pre_u", "pre_u_se",
                           "mpre_d", "mpre_d_se",
                           "mpre_u", "mpre_u_se",
                           "vpre_d", "vpre_d_se",
                           "vpre_u", "vpre_u_se",
                           "epre_d", "epre_d_se",
                           "epre_u", "epre_u_se",
                           "lbw_d", "lbw_d_se",
                           "lbw_u", "lbw_u_se",
                           "lbwft_d", "lbwft_d_se",
                           "lbwft_u", "lbwft_u_se",
                           "mlbw_d", "mlbw_d_se",
                           "mlbw_u", "mlbw_u_se",
                           "vlbw_d", "vlbw_d_se",
                           "vlbw_u", "vlbw_u_se",
                           "elbw_d", "elbw_d_se",
                           "elbw_u", "elbw_u_se",
                           "mort_d", "mort_d_se",
                           "mort_u", "mort_u_se")
  
  
  preterm = fixest::feols(I(gestation < 37) ~  1
                          |county + year^month + birth_race_dsc_1
                          |updown + down ~
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                            m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                          , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  mpreterm = fixest::feols(I(gestation < 37 & gestation >= 32) ~  1
                           |county + year^month + birth_race_dsc_1
                           |updown + down ~
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                             m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                           , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  vpreterm = fixest::feols(I(gestation < 32 & gestation >= 28) ~  1
                           |county + year^month + birth_race_dsc_1
                           |updown + down ~
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                             m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                           , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  epreterm = fixest::feols(I(gestation < 28) ~  1
                           |county + year^month + birth_race_dsc_1
                           |updown + down ~
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                             m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                           , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  lbw = fixest::feols(I(bweight < 2500) ~  1
                      |county + year^month + birth_race_dsc_1
                      |updown + down ~
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                        m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                      , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  lbw_ft = fixest::feols(I(bweight < 2500) ~  1
                         |county + year^month + birth_race_dsc_1
                         |updown + down ~
                           m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                           m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                           mthr_wgt_dlv +mthr_pre_preg_wgt + 
                           m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                         , data = df[which(df$gestation >= 37), ], warn = F, notes = F, cluster = c("site", "year^month"))
  
  mlbw = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  1
                       |county + year^month + birth_race_dsc_1
                       |updown + down ~
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                       , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  vlbw = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  1
                       |county + year^month + birth_race_dsc_1
                       |updown + down ~
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                       , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  elbw = fixest::feols(I(bweight < 1000) ~  1
                       |county + year^month + birth_race_dsc_1
                       |updown + down ~
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                         m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                       , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  
  
  mort_tab = fixest::feols(death ~  1
                           |county + year^month + birth_race_dsc_1
                           |updown + down ~
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + 
                             m_educ + f_educ + mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height+ med_hprice + med_inc + well_elev + resid_elev + temp + pm25
                           , data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  boot_coefs[1, "pre_d"] = preterm$coefficients["fit_down"]
  boot_coefs[1, "pre_d_se"] = sqrt(vcov(preterm, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "pre_u"] = preterm$coefficients["fit_updown"]
  boot_coefs[1, "pre_u_se"] = sqrt(vcov(preterm, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "mpre_d"] = mpreterm$coefficients["fit_down"]
  boot_coefs[1, "mpre_d_se"] = sqrt(vcov(mpreterm, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "mpre_u"] = mpreterm$coefficients["fit_updown"]
  boot_coefs[1, "mpre_u_se"] = sqrt(vcov(mpreterm, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "vpre_d"] = vpreterm$coefficients["fit_down"]
  boot_coefs[1, "vpre_d_se"] = sqrt(vcov(vpreterm, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "vpre_u"] = vpreterm$coefficients["fit_updown"]
  boot_coefs[1, "vpre_u_se"] = sqrt(vcov(vpreterm, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "epre_d"] = epreterm$coefficients["fit_down"]
  boot_coefs[1, "epre_d_se"] = sqrt(vcov(epreterm, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "epre_u"] = epreterm$coefficients["fit_updown"]
  boot_coefs[1, "epre_u_se"] = sqrt(vcov(epreterm, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "lbwft_d"] = lbw_ft$coefficients["fit_down"]
  boot_coefs[1, "lbwft_d_se"] = sqrt(vcov(lbw_ft, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "lbwft_u"] = lbw_ft$coefficients["fit_updown"]
  boot_coefs[1, "lbwft_u_se"] = sqrt(vcov(lbw_ft, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "lbw_d"] = lbw$coefficients["fit_down"]
  boot_coefs[1, "lbw_d_se"] = sqrt(vcov(lbw, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "lbw_u"] = lbw$coefficients["fit_updown"]
  boot_coefs[1, "lbw_u_se"] = sqrt(vcov(lbw, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "mlbw_d"] = mlbw$coefficients["fit_down"]
  boot_coefs[1, "mlbw_d_se"] = sqrt(vcov(mlbw, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "mlbw_u"] = mlbw$coefficients["fit_updown"]
  boot_coefs[1, "mlbw_u_se"] = sqrt(vcov(mlbw, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "vlbw_d"] = vlbw$coefficients["fit_down"]
  boot_coefs[1, "vlbw_d_se"] = sqrt(vcov(vlbw, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "vlbw_u"] = vlbw$coefficients["fit_updown"]
  boot_coefs[1, "vlbw_u_se"] = sqrt(vcov(vlbw, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  boot_coefs[1, "elbw_d"] = elbw$coefficients["fit_down"]
  boot_coefs[1, "elbw_d_se"] = sqrt(vcov(elbw, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "elbw_u"] = elbw$coefficients["fit_updown"]
  boot_coefs[1, "elbw_u_se"] = sqrt(vcov(elbw, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  
  boot_coefs[1, "mort_d"] = mort_tab$coefficients["fit_down"]
  boot_coefs[1, "mort_d_se"] = sqrt(vcov(mort_tab, cluster = c("site", "year^month"))["fit_down", "fit_down"])
  boot_coefs[1, "mort_u"] = mort_tab$coefficients["fit_updown"]
  boot_coefs[1, "mort_u_se"] = sqrt(vcov(mort_tab, cluster = c("site", "year^month"))["fit_updown", "fit_updown"])
  
  
  
  return(boot_coefs)
  
}


placebo_res = function(df){
  bc1 = placebo_res_corr(df)
  bc2 = placebo_res_iv(df)
  return(list(bc1, bc2))
}