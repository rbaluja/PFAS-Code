flowacc = function(i, d, w, option){
  d2 = d[[i]]
  d2 = d2[!is.na(d2$value), ]
  w2 = w[i, ]
  
  if (option == "well"){
    w2$fa_well = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "resid"){
    w2$fa_resid = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "sp"){
    w2$sp = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "awc"){
    w2$awc = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "fc"){
    w2$fc = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }
  
  return(w2)
}


distNAfun = function(i, d, w, option){
  d2 = d[[i]]
  w2 = w[i, ]
  
  if (option == "well"){
    w2$well_fd = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }else if (option == "resid"){
    w2$resid_fd = as.numeric((d2$value %*% d2$coverage_fraction)/sum(d2$coverage_fraction)) 
  }
  
  return(w2)
}
