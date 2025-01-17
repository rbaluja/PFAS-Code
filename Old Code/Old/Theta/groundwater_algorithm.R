#read in functions
source("Code/Primary/Theta/theta_functions.R")


################
#Read in Data###
################
sf_use_s2(TRUE)
huc_shape = st_read(paste0('Data/Groundwater/WBD_01_HU2_Shape/Shape/WBDHU', huc, '.shp')) %>% 
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
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

#add in elevation
# cont_sites$c_elevation = get_elev_point(as.data.frame(cont_sites %>% 
#                                                         as_tibble() %>%
#                                                         dplyr::select(c(lng, lat))), prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")$elevation




##############################################################
###Intersection of cont_sites with meter buffer with huc8#####
##############################################################

#gives a list of each huc that a contamination site is in
#change from GEOS (Planar CRS's) to S2 (Spherical CRS's) for this calculation 
#(have to turn it on whenever doing sf calculation with non-planar CRS, and turn it off when working in meters)
sf_use_s2(TRUE)
points = st_intersects(cont_sites, huc_shape)
sf_use_s2(FALSE)


cont_sites_buff = cont_sites %>% 
  st_transform(32110) %>% #NH meters projected CRS
  st_buffer(meters) %>% 
  st_transform(4326)


inter_finder = function(i){
  a = cont_sites_buff[i, ] %>% 
    st_intersection(huc_shape[points[[i]], ])
  return(a)
}
sf_use_s2(TRUE)
#save dataframe of the intersecting geometry of each contamination site's buffer and it's huc
cont_huc_inter = dplyr::bind_rows(pblapply(1:nrow(cont_sites), inter_finder, cl = 8))





##########################################################
###Find OLS Plane elev = a_1 * lat + a_2 * lng + a_3######
##########################################################
#This is obtained by running "Current_Code/PFAS/ols_list.R" on the HPC
load("~/Desktop/PFAS Infants/New Hampshire/Data/RData/olsl_elev_huc10_1000ppt_10km2.RData")



dir_list = vector(mode = 'list', length = length(ols_list))
for(i in 1:length(ols_list)){
  lng = as.numeric(ols_list[[i]][2])
  lat = as.numeric(ols_list[[i]][3])
  
  if (abs(lng) < 100000 & abs(lat) < 100000){
    dir_list[[i]] = -1* c(lng, lat) 
  }else{
    dir_list[[i]] = "singular"
  }
}
for (i in 1:length(ols_list)){
  dir = unlist(dir_list[[i]])
  if (dir[1] != "singular"){
    dir_list[[i]] = (dir/sqrt(sum(dir^2))) 
  }
}


#get theta_gw
cont_huc_inter$theta_gw = sapply(1:nrow(cont_sites), theta_fun)

#add huc8 code
cont_huc_inter$huc8 = str_sub(cont_huc_inter$huc10, 1, 8)

#add this back to cont_sites_buff
cont_sites_buff = cont_sites_buff %>% 
  left_join(cont_huc_inter %>% 
              as_tibble() %>% 
              dplyr::select(site, theta_gw, huc8))

