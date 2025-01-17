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
grad_dir = function(i){
  points = st_sample(cont_huc_inter$geometry[i], size = 50000) #sample 50000 points from the intersection of the circle around site i and the huc8 shape
  elev_points = do.call(rbind, st_geometry(points)) %>% 
    as_tibble() %>% setNames(c("lat","lng")) #save these points in a dataframe
  
  #water table algorithm
  x = distm(elev_points, gw, fun = distHaversine) #gets distance from the sampled points and the table measurements
  x = t(x)
  elev_points$gw_level = unlist(lapply(1:50000, gw_fun, x)) #uses table algorithm to assign table elevation
  elev_points = elev_points %>%
    tidyr::drop_na()
  
  
  a_1 = elev_points$lat
  a_2 = elev_points$lng
  a_3 = rep(1, length(a_1))
  b = elev_points$gw_level
  
  A = matrix(c(a_1, a_2, a_3), nrow = length(a_1), ncol = 3)
  
  #solving for optimal coefficients
  theta = solve(t(A) %*% A) %*% t(A) %*% b #solve for OLS coefficients
  
  #saving coefficients
  return(theta)
  
  
}

##############################
###Main Functions##############
##############################

#n is the number of triangles 
triangle_maker = function(i, n){
  sf_use_s2(TRUE)
  
  if (n %% 2 != 0){
    stop('Number of triangles must be even')
  }
  ###Step 1: Find point that is 10 miles away on line of steepest descent####
  
  
  #gives a point in the direction of steepest descent
  for_lat = cont_sites$lat[i] + dir_list[[i]][1]
  for_lng = cont_sites$lng[i] + dir_list[[i]][2]
  
  #getting end point of line dist meters away
  point_df = data.frame(x = c(cont_sites$lng[i], for_lng), y = c(cont_sites$lat[i], for_lat), cont = c(1, 0), linestring_id = c(1, 1))
  
  point_sdf = point_df %>%
    st_as_sf(coords = c('x', 'y'), crs = 4326)
  
  cont_point = point_sdf[which(point_sdf$cont == 1), 3]
  cont_point_coords = cont_point %>% st_coordinates()
  
  line = sf_line(obj = point_df) %>%
    st_set_crs(4326) %>%
    st_transform(3488)
  
  sf_use_s2(FALSE)
  ratio = dist/st_length(line)
  
  #this is a point dist meters away in the direction of fastest decrease
  m1 = st_linesubstring(line, from = 0, to = ratio) %>%
    st_endpoint() 
  
  m1 = m1 %>%
    st_transform(4326) 
  
  sf_use_s2(TRUE)
  
  m1_coords = m1 %>% st_coordinates()
  
  
  ###Step 2: Find angle of triangle between m1, cont_point, and x-axis.
  #########  Use this angle to shift each of the angles an appropriate amount.
  
  #use m1 to figure out the amount to correct for in theta
  theta = atan2(m1_coords[2] - cont_point_coords[2], m1_coords[1] - cont_point_coords[1])
  
  
  
  
  ###Step 3: Find six ref points in terms of this angle and the cont_point
  
  
  #use unit circle to create reference points for the triangle (the standard unit circle radians are given at the bottom of each point)
  #Do this first in terms of meters to get constant radius, then transform to lat long
  cp_3488 = point_sdf %>% 
    st_transform(3488) %>% 
    filter(cont == 1) %>% 
    st_coordinates()
  
  #build edge points
  x = 1
  for (j in 1:n){
    x = x + 2
    a = list('x' = (cp_3488[1] + dist * cos(theta + (pi * x)/n)), 'y' = cp_3488[2] + dist * sin(theta + (pi * x)/n)) %>%
      as.data.frame() %>% 
      st_as_sf(coords = c('x', 'y'), crs = 3488) %>% 
      st_transform(4326)
    
    assign(paste0('a_', j), a)
  }
  
  
  ###Step 4: Create and save triangles as polygons of the points
  
  #key: t1 is triangle with center direction of fastest decrease, then remaining triangles follow counter-clockwise
  l = vector(mode = 'list', length = n)
  for (j in 1:n){
    if (j == 1){
      tri = rbind(cont_point, eval(parse(text = paste0('a_', n))), a_1, cont_point)
      tri = tri %>%
        st_set_crs(4326) %>%
        st_transform(32632) %>% #helps with casting
        dplyr::summarise() %>%
        st_cast('POLYGON') %>% 
        st_transform(4326)
      l[[j]] = tri
    }else{
      tri = rbind(cont_point, eval(parse(text = paste0('a_', j))), eval(parse(text = paste0('a_', (j-1)))), cont_point)
      tri = tri %>%
        st_set_crs(4326) %>%
        st_transform(32632) %>% #helps with casting
        dplyr::summarise() %>%
        st_cast('POLYGON') %>% 
        st_transform(4326)
      l[[j]] = tri
    }
  }
  
  l = bind_rows(setNames(l, seq_along(l)), .id = "triangle")
  l$id = i
  
  l = l %>% st_intersection(cont_huc_inter[i, ]) %>% 
    dplyr::select(triangle, geometry, id)
  
  
  return(l)
  
}
