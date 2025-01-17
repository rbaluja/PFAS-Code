#function representing the algorithm for assigning gw elevation at a point
gw_fun = function(i, x){
  
  x_i = x[ , i]
  min_i = which(x_i == min(x_i))[1]
  
  if (x_i[min_i] <= 1609){ #if nearest measurement is not more than a mile away then assign its table elevation 
    depth = mean(gw_level$gw_depth[min_i])
  }else if (x_i[min_i] > 1609 & x_i[min_i] <= gw_dist_allowance ){ #if nearest measurement is between 1 and gw_dist_allowance away, take weighted average of all wells in that range
    s =  which(x_i > 1609 & x_i<= gw_dist_allowance)
    d = gw_level[s , ]
    d$weight = 1/(x_i[s]^2)
    depth = sum(d$weight * d$gw_depth)/(sum(d$weight))
  } else{ #drop if nearest measurement is further than gw_dist_allowance away
    depth = NA
  }
  return(depth)
}


#this function returns the coefficients of the OLS plane of interest
#this function returns the coefficients of the OLS plane of interest
grad_dir = function(i){
  points = st_sample(cont_huc_inter$geometry[i], size = 20000) #sample 20000 points from the intersection of the circle around site i and the huc8 shape
  elev_points = do.call(rbind, st_geometry(points)) %>% 
    as_tibble() %>% setNames(c("lng","lat")) #save these points in a dataframe
  
  #elevation algorithm
  e1 =  get_elev_point(elev_points, prj = "EPSG:4326", "aws", z = 14)
  elev_points$elev = e1$elevation
  elev_points = elev_points %>%
    tidyr::drop_na()
  
  
  a_1 = elev_points$lng - cont_sites$lng[i]
  a_2 = elev_points$lat- cont_sites$lat[i]
  b = elev_points$elev
  A = data.frame(gw_level = b, lng = a_1, lat = a_2)
  
  r = fixest::feols(gw_level ~ lng + lat, data = A)
  
  #saving coefficients
  return(r$coefficients)
  
  
}







theta_fun = function(i){
  
  #If flat region, then this algorithm does not work
  if (dir_list[[i]][1] != "singular"){
    sf_use_s2(TRUE)
    
    ###Step 1: Find point that is meters away on line of steepest descent####
    
    #gives a point in the direction of steepest descent
    for_lat = cont_sites$lat[i] + dir_list[[i]][2]
    for_lng = cont_sites$lng[i] + dir_list[[i]][1]
    
    #getting end point of line dist meters away
    point_df = data.frame(x = c(cont_sites$lng[i], for_lng), y = c(cont_sites$lat[i], for_lat), cont = c(1, 0), linestring_id = c(1, 1))
    
    point_sdf = point_df %>%
      st_as_sf(coords = c('x', 'y'), crs = 4326)
    
    cont_point = point_sdf[which(point_sdf$cont == 1), 3]
    cont_point_coords = cont_point %>% st_coordinates()
    
    #Make line segment from cont site to end point
    line = sf_line(obj = point_df) %>%
      st_set_crs(4326) %>%
      st_transform(32110)
    

    sf_use_s2(FALSE)
    ratio = dist/st_length(line) 
    
    #this is a point dist meters away in the direction of fastest decrease
    m1 = st_linesubstring(line, from = 0, to = ratio) %>%
      st_endpoint() %>% #st_linesubstring maps from [0, 1]
      st_transform(4326) 
    
    sf_use_s2(TRUE)
    m1_coords = m1 %>% st_coordinates()
    
    
    
    #This is the angle between the positive x-axis and the line originating from the 
    #cont site in the direction of fastest decrease
    theta = atan2(m1_coords[2] - cont_point_coords[2], m1_coords[1] - cont_point_coords[1])
    
    return(theta)
  }else{
    return("singular")
  }
  
}




epa_dist = function(i){
  xi = dom_dist[ , i ]
  d = df_dom[i, ]
  
  if (min(xi) <= 500){
    
    j = which(xi == min(xi))[1]
    a = cont_dom[j, ]
    d$pfoa = a$pfoa
    d$pfos = a$pfos 
    d$pfna = a$pfna
    d$pfbs = a$pfbs
    d$genx = a$genx
    d$pfhxs = a$pfhxs
    
    
  }else{
    d$pfoa = NA
    d$pfos = NA
    d$pfna = NA
    d$pfbs = NA
    d$genx = NA
    d$pfhxs = NA
  }
  
  return(d)
}
