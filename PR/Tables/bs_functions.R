linear_bootstrap = function(boot_coefs, outcome, reg){
  
  #subtract off mean, divide by dof
  bs_sd = sqrt(sum((boot_coefs[[outcome]] - reg$coefficients["pred_pfas"])^2)/(nrow(boot_coefs) - 1))
  
  return(bs_sd)
}

quintile_bootstrap = function(boot_coefs, reg_data){
  p2_sd = sqrt(sum((boot_coefs$preterm2 - reg_data[2, "pre_coef"])^2)/(nrow(boot_coefs) - 1))
  p3_sd = sqrt(sum((boot_coefs$preterm3 - reg_data[3, "pre_coef"])^2)/(nrow(boot_coefs) - 1))
  p4_sd = sqrt(sum((boot_coefs$preterm4 - reg_data[4, "pre_coef"])^2)/(nrow(boot_coefs) - 1))
  p5_sd = sqrt(sum((boot_coefs$preterm5 - reg_data[5, "pre_coef"])^2)/(nrow(boot_coefs) - 1))
  
  lp2_sd = sqrt(sum((boot_coefs$lpreterm2 - reg_data[2, "lpre_coef"])^2)/(nrow(boot_coefs) - 1))
  lp3_sd = sqrt(sum((boot_coefs$lpreterm3 - reg_data[3, "lpre_coef"])^2)/(nrow(boot_coefs) - 1))
  lp4_sd = sqrt(sum((boot_coefs$lpreterm4 - reg_data[4, "lpre_coef"])^2)/(nrow(boot_coefs) - 1))
  lp5_sd = sqrt(sum((boot_coefs$lpreterm5 - reg_data[5, "lpre_coef"])^2)/(nrow(boot_coefs) - 1))
  
  mp2_sd = sqrt(sum((boot_coefs$mpreterm2 - reg_data[2, "mpre_coef"])^2)/(nrow(boot_coefs) - 1))
  mp3_sd = sqrt(sum((boot_coefs$mpreterm3 - reg_data[3, "mpre_coef"])^2)/(nrow(boot_coefs) - 1))
  mp4_sd = sqrt(sum((boot_coefs$mpreterm4 - reg_data[4, "mpre_coef"])^2)/(nrow(boot_coefs) - 1))
  mp5_sd = sqrt(sum((boot_coefs$mpreterm5 - reg_data[5, "mpre_coef"])^2)/(nrow(boot_coefs) - 1))
  
  vp2_sd = sqrt(sum((boot_coefs$vpreterm2 - reg_data[2, "vpre_coef"])^2)/(nrow(boot_coefs) - 1))
  vp3_sd = sqrt(sum((boot_coefs$vpreterm3 - reg_data[3, "vpre_coef"])^2)/(nrow(boot_coefs) - 1))
  vp4_sd = sqrt(sum((boot_coefs$vpreterm4 - reg_data[4, "vpre_coef"])^2)/(nrow(boot_coefs) - 1))
  vp5_sd = sqrt(sum((boot_coefs$vpreterm5 - reg_data[5, "vpre_coef"])^2)/(nrow(boot_coefs) - 1))
  
  lbw2_sd = sqrt(sum((boot_coefs$lbw2 - reg_data[2, "lbw_coef"])^2)/(nrow(boot_coefs) - 1))
  lbw3_sd = sqrt(sum((boot_coefs$lbw3 - reg_data[3, "lbw_coef"])^2)/(nrow(boot_coefs) - 1))
  lbw4_sd = sqrt(sum((boot_coefs$lbw4 - reg_data[4, "lbw_coef"])^2)/(nrow(boot_coefs) - 1))
  lbw5_sd = sqrt(sum((boot_coefs$lbw5 - reg_data[5, "lbw_coef"])^2)/(nrow(boot_coefs) - 1))
  
  llbw2_sd = sqrt(sum((boot_coefs$llbw2 - reg_data[2, "llbw_coef"])^2)/(nrow(boot_coefs) - 1))
  llbw3_sd = sqrt(sum((boot_coefs$llbw3 - reg_data[3, "llbw_coef"])^2)/(nrow(boot_coefs) - 1))
  llbw4_sd = sqrt(sum((boot_coefs$llbw4 - reg_data[4, "llbw_coef"])^2)/(nrow(boot_coefs) - 1))
  llbw5_sd = sqrt(sum((boot_coefs$llbw5 - reg_data[5, "llbw_coef"])^2)/(nrow(boot_coefs) - 1))
  
  mlbw2_sd = sqrt(sum((boot_coefs$mlbw2 - reg_data[2, "mlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  mlbw3_sd = sqrt(sum((boot_coefs$mlbw3 - reg_data[3, "mlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  mlbw4_sd = sqrt(sum((boot_coefs$mlbw4 - reg_data[4, "mlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  mlbw5_sd = sqrt(sum((boot_coefs$mlbw5 - reg_data[5, "mlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  
  vlbw2_sd = sqrt(sum((boot_coefs$vlbw2 - reg_data[2, "vlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  vlbw3_sd = sqrt(sum((boot_coefs$vlbw3 - reg_data[3, "vlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  vlbw4_sd = sqrt(sum((boot_coefs$vlbw4 - reg_data[4, "vlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  vlbw5_sd = sqrt(sum((boot_coefs$vlbw5 - reg_data[5, "vlbw_coef"])^2)/(nrow(boot_coefs) - 1))
  
  mort2_sd = sqrt(sum((boot_coefs$mort2 - reg_data[2, "mort_coef"])^2)/(nrow(boot_coefs) - 1))
  mort3_sd = sqrt(sum((boot_coefs$mort3 - reg_data[3, "mort_coef"])^2)/(nrow(boot_coefs) - 1))
  mort4_sd = sqrt(sum((boot_coefs$mort4 - reg_data[4, "mort_coef"])^2)/(nrow(boot_coefs) - 1))
  mort5_sd = sqrt(sum((boot_coefs$mort5 - reg_data[5, "mort_coef"])^2)/(nrow(boot_coefs) - 1))
  
  return(list(p2_sd = p2_sd, p3_sd = p3_sd, p4_sd = p4_sd, p5_sd = p5_sd, 
              lp2_sd = lp2_sd, lp3_sd = lp3_sd, lp4_sd = lp4_sd, lp5_sd = lp5_sd, 
              mp2_sd = mp2_sd, mp3_sd = mp3_sd, mp4_sd = mp4_sd, mp5_sd = mp5_sd, 
              vp2_sd = vp2_sd, vp3_sd = vp3_sd, vp4_sd = vp4_sd, vp5_sd = vp5_sd, 
              lbw2_sd = lbw2_sd, lbw3_sd = lbw3_sd, lbw4_sd = lbw4_sd, lbw5_sd = lbw5_sd,
              llbw2_sd = llbw2_sd, llbw3_sd = llbw3_sd, llbw4_sd = llbw4_sd, llbw5_sd = llbw5_sd,
              mlbw2_sd = mlbw2_sd, mlbw3_sd = mlbw3_sd, mlbw4_sd = mlbw4_sd, mlbw5_sd = mlbw5_sd,
              vlbw2_sd = vlbw2_sd, vlbw3_sd = vlbw3_sd, vlbw4_sd = vlbw4_sd, vlbw5_sd = vlbw5_sd, 
              mort2_sd = mort2_sd, mort3_sd = mort3_sd, mort4_sd = mort4_sd, mort5_sd = mort5_sd))
}


#linear covariance bootstrap
cov_boot = function(boot_coefs, outcome1, reg1, outcome2, reg2){
  #subtract off mean, divide by dof
  bs_sd = sum((boot_coefs[[outcome1]] - reg1$coefficients["pred_pfas"]) * (boot_coefs[[outcome2]] - reg2$coefficients["pred_pfas"]))/(nrow(boot_coefs) - 1)
  return(bs_sd)
}
