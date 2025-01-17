#read in functions
source('Code/triangle_functions.R')


################
#Read in Data###
################
sf_use_s2(TRUE)
huc8_shape = st_read('Data/Groundwater/WBD_01_HU2_Shape/Shape/WBDHU10.shp') %>% 
  st_transform(4326)


cont_sites = read_xlsx('Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
  dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
  dplyr::select(`Site name`, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, lat = Latitude, 
         date = `Date Sampled`, lng = Longitude, industry = Industry, 
         pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
         sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
         sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
         total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & total_pfas >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

#well data
gw_level = read.delim('Data/Groundwater/nh_gwlevels') %>%
  dplyr::select(agency_cd, site_no, sl_lev_va, site_tp_cd, sl_datum_cd )
gw_level$sl_lev_va = as.numeric(gw_level$sl_lev_va)
gw_level = gw_level %>% tidyr::drop_na(sl_lev_va) #gets rid of missing data
gw_level = subset(gw_level, gw_level$sl_datum_cd == 'NAVD88'  ) #set the reference datum. 

#find average depth for each well
gw_level$gw_depth = ave(gw_level$sl_lev_va, gw_level$agency_cd, gw_level$site_no )


#keep only one observation for each well
gw_level = gw_level[, c(1, 2, ncol(gw_level))] #keep agency, well id, and mean measurement 
gw_level = gw_level %>% distinct()


#load latitude/longitude information for well id
gw_loc = read_delim('Data/Groundwater/nh_gwloc', 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE)
gw_loc = gw_loc[-1, ]
gw_loc$site_no = as.character(gw_loc$site_no)

gw_loc = gw_loc[, c(1, 2, 7, 8)] #keep agency, well id, and lat-long. 

#merge lat-long onto depth data by well id and site number
gw_level = gw_level %>% 
  left_join(gw_loc) %>% 
  dplyr::rename(lng = dec_long_va, 
                lat = dec_lat_va) %>% 
  dplyr::mutate(lng = as.numeric(lng), 
                lat = as.numeric(lat))
gw_level = gw_level %>%
  tidyr::drop_na()

rm(gw_loc)
gw_level = gw_level %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) 

gw = gw_level %>% 
  st_coordinates()



##############################################################
###Intersection of cont_sites with meter buffer with huc8#####
##############################################################

#gives a list of each intersection
#change from GEOS (Planar CRS's) to S2 (Spherical CRS's) for this calculation 
#(have to turn it on whenever doing sf calculation with non-planar CRS, and turn it off when working in meters)
sf_use_s2(TRUE)
points = st_intersects(cont_sites, huc8_shape)
sf_use_s2(FALSE)


cont_sites_buff = cont_sites %>% 
  st_transform(3488) %>%
  st_buffer(meters) %>% 
  st_transform(4326)


inter_finder = function(i){
  a = cont_sites_buff[i, ] %>% 
    st_intersection(huc8_shape[points[[i]], ])
  return(a)
}
sf_use_s2(TRUE)
#save dataframe of the intersecting geometry of each contamination site and it's huc
cont_huc_inter = dplyr::bind_rows(pblapply(1:nrow(cont_sites), inter_finder, cl = 8))





##########################################################
###Find OLS Plane elev = a_1 * lat + a_2 * lng + a_3######
##########################################################
if (rerun_gw == T){
  ols_list = pblapply(1:nrow(cont_sites), grad_dir, cl = 8)  
}else{
  if (ppt != 500){
    load(paste0("~/Desktop/PFAS Infants/New Hampshire/Data/RData/ols_list_t", n_triangles,
                "_km", meters/1000, "_huc10.RData"))
  }else{
    load(paste0("~/Desktop/PFAS Infants/New Hampshire/Data/RData/ols_list_t", n_triangles,
                "_km", meters/1000, "_huc10_500ppt.RData"))
  }
  
}


dir_list = vector(mode = 'list', length = length(ols_list))
for(i in 1:length(ols_list)){
  dir_list[[i]] = -1* ols_list[[i]][1:2]
}
for ( i in 1:length(ols_list)){
  dir = unlist(dir_list[[i]])
  dir_list[[i]] = (dir/sqrt(sum(dir^2)))
}


#######################
###Building Triangles##
#######################
triangles = dplyr::bind_rows(pblapply(1:nrow(cont_sites), triangle_maker, n_triangles, cl = 8))


#merge these triangles with the contamination data
cont_sites$id = 1:nrow(cont_sites)
cont_sites = cont_sites %>% 
  as_tibble() %>% 
  dplyr::select(!geometry) %>%
  left_join(triangles) %>% 
  dplyr::select(-id)

cont_sites = st_as_sf(cont_sites, crs = 4326) 
