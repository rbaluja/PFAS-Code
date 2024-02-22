inner_wind_function = function(i, lng, lat, dist_allow){
  
  dist_site = distm(c(lng, lat), c(c_sites$lng[i], c_sites$lat[i]))
  
  if (dist_site <= dist_allow){
    
    theta = atan2(lat - c_sites$lat[i], lng - c_sites$lng[i])  
    
    w = wind %>% 
      dplyr::filter(ID == i)
    
    n = length(which(abs(atan2(sin(theta - w$norm_radians), cos(theta -w$norm_radians))) <= pi/4))
    
    return(n/365 * (1 - dist_site/dist_allow))
    
  }else{
    return(0)
  }
  
}

wind_function = function(lng, lat, dist_allow){
  
  exposure = sum(sapply(1:nrow(c_sites), inner_wind_function, lng, lat, dist_allow))
  
  return(exposure)
  
}


