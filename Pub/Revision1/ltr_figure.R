one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

lower_cbound = function(out, reg_data){
  d_name = paste0(out, "_down")
  dse_name = paste0(out, "_dse")
  dltr_name = paste0(out, "_dltr")
  dltr_se_name = paste0(out, "_dltr_se")
  cov_name = paste0(out, "_cov")
  
  return(reg_data[])
}

reg_data = data.frame(matrix(ncol = 46, nrow = 0))
colnames(reg_data) = c('ges_length', 
                       'pre_down', 'pre_dse', 'pre_dltr', 'pre_dltr_se', "pre_cov",
                       'mpre_down', 'mpre_dse', 'mpre_dltr', 'mpre_dltr_se', "mpre_cov",
                       'vpre_down', 'vpre_dse', 'vpre_dltr', 'vpre_dltr_se', "vpre_cov",
                       'epre_down', 'epre_dse', 'epre_dltr', 'epre_dltr_se', "epre_cov",
                       'lbw_down','lbw_dse', 'lbw_dltr', 'lbw_dltr_se', "lbw_cov",
                       'mlbw_down','mlbw_dse', 'mlbw_dltr', 'mlbw_dltr_se', "mlbw_cov",
                       'vlbw_down','vlbw_dse', 'vlbw_dltr', 'vlbw_dltr_se', "vlbw_cov",
                       'elbw_down','elbw_dse', 'elbw_dltr', 'elbw_dltr_se', "elbw_cov",
                       'mort_down','mort_dse', 'mort_dltr', 'mort_dltr_se', "mort_cov")

index = 1
for (ges_length in c(6, 12, 18, 24, 30, 36, 42, 48, 54, 60)){
  
  preterm = fixest::feols(I(gestation < 37) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 +fa_resid + wind_exposure 
                          |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  mpreterm = fixest::feols(I(gestation < 37 & gestation >= 32) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 +fa_resid + wind_exposure 
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  vpreterm = fixest::feols(I(gestation < 32 & gestation >= 28) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 +fa_resid + wind_exposure 
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  epreterm = fixest::feols(I(gestation < 28) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 +fa_resid + wind_exposure 
                           |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  #birthweight
  lbw = fixest::feols(I(bweight < 2500) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 +fa_resid + wind_exposure 
                      |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  mlbw = fixest::feols(I(bweight >= 1500 & bweight < 2500) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 +fa_resid + wind_exposure 
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  vlbw = fixest::feols(I(bweight >= 1000 & bweight < 1500) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 +fa_resid + wind_exposure 
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  elbw = fixest::feols(I(bweight < 1000) ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 +fa_resid + wind_exposure 
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  #mortality
  mort = fixest::feols(death ~  (updown + down)*I(m_months_res > ges_length) +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 +fa_resid + wind_exposure 
                       |county + year^month + birth_race_dsc_1, data = df, warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  
  pre_v = vcov(preterm, cluster = c("site", "year^month"))
  mpre_v = vcov(mpreterm, cluster = c("site", "year^month"))
  vpre_v = vcov(vpreterm, cluster = c("site", "year^month"))
  epre_v = vcov(epreterm, cluster = c("site", "year^month"))
  
  lbw_v = vcov(lbw, cluster = c("site", "year^month"))
  mlbw_v = vcov(mlbw, cluster = c("site", "year^month"))
  vlbw_v = vcov(vlbw, cluster = c("site", "year^month"))
  elbw_v = vcov(elbw, cluster = c("site", "year^month"))
  
  mort_v = vcov(mort, cluster = c("site", "year^month"))
  
  
  reg_data[index, "ges_length"] = ges_length
  reg_data[index, "pre_down"] = preterm$coeftable["down", 1]/mean(df$gestation < 37)
  reg_data[index, "pre_dse"] = sqrt(pre_v["down", "down"])/mean(df$gestation < 37)
  reg_data[index, "pre_dltr"] = preterm$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$gestation < 37)
  reg_data[index, "pre_dltr_se"] =  sqrt(pre_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$gestation < 37)
  reg_data[index, "pre_cov"] =  pre_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$gestation < 37)
  
  reg_data[index, "mpre_down"] = mpreterm$coeftable["down", 1]/mean(df$gestation < 37 & df$gestation >= 32)
  reg_data[index, "mpre_dse"] = sqrt(mpre_v["down", "down"])/mean(df$gestation < 37 & df$gestation >= 32)
  reg_data[index, "mpre_dltr"] = mpreterm$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$gestation < 37 & df$gestation >= 32)
  reg_data[index, "mpre_dltr_se"] =  sqrt(mpre_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$gestation < 37 & df$gestation >= 32)
  reg_data[index, "mpre_cov"] =  mpre_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$gestation < 37 & df$gestation >= 32)
  
  reg_data[index, "vpre_down"] = vpreterm$coeftable["down", 1]/mean(df$gestation < 32 & df$gestation >= 28)
  reg_data[index, "vpre_dse"] = sqrt(vpre_v["down", "down"])/mean(df$gestation < 32 & df$gestation >= 28)
  reg_data[index, "vpre_dltr"] = vpreterm$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$gestation < 32 & df$gestation >= 28)
  reg_data[index, "vpre_dltr_se"] = sqrt(vpre_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$gestation < 32 & df$gestation >= 28)
  reg_data[index, "vpre_cov"] =  vpre_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$gestation < 32 & df$gestation >= 28)
  
  reg_data[index, "epre_down"] = epreterm$coeftable["down", 1]/mean(df$gestation < 28)
  reg_data[index, "epre_dse"] = sqrt(epre_v["down", "down"])/mean(df$gestation < 28)
  reg_data[index, "epre_dltr"] = epreterm$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$gestation < 28)
  reg_data[index, "epre_dltr_se"] = sqrt(epre_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$gestation < 28)
  reg_data[index, "epre_cov"] =  epre_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$gestation < 28)
  
  reg_data[index, "lbw_down"] = lbw$coeftable["down", 1]/mean(df$bweight < 2500)
  reg_data[index, "lbw_dse"] = sqrt(lbw_v["down", "down"])/mean(df$bweight < 2500)
  reg_data[index, "lbw_dltr"] = lbw$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$bweight < 2500)
  reg_data[index, "lbw_dltr_se"] = sqrt(lbw_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$bweight < 2500)
  reg_data[index, "lbw_cov"] = lbw_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$bweight < 2500)
  
  reg_data[index, "mlbw_down"] = mlbw$coeftable["down", 1]/mean(df$bweight >= 1500 & df$bweight < 2500)
  reg_data[index, "mlbw_dse"] = sqrt(mlbw_v["down", "down"])/mean(df$bweight >= 1500 & df$bweight < 2500)
  reg_data[index, "mlbw_dltr"] = mlbw$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$bweight >= 1500 & df$bweight < 2500)
  reg_data[index, "mlbw_dltr_se"] = sqrt(mlbw_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$bweight >= 1500 & df$bweight < 2500)
  reg_data[index, "mlbw_cov"] = mlbw_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$bweight >= 1500 & df$bweight < 2500)
  
  reg_data[index, "vlbw_down"] = vlbw$coeftable["down", 1]/mean(df$bweight >= 1000 & df$bweight < 1500)
  reg_data[index, "vlbw_dse"] = sqrt(vlbw_v["down", "down"])/mean(df$bweight >= 1000 & df$bweight < 1500)
  reg_data[index, "vlbw_dltr"] = vlbw$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$bweight >= 1000 & df$bweight < 1500)
  reg_data[index, "vlbw_dltr_se"] = sqrt(vlbw_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$bweight >= 1000 & df$bweight < 1500)
  reg_data[index, "vlbw_cov"] = vlbw_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$bweight >= 1000 & df$bweight < 1500)
  
  reg_data[index, "elbw_down"] = elbw$coeftable["down", 1]/mean(df$bweight < 1000)
  reg_data[index, "elbw_dse"] = sqrt(elbw_v["down", "down"])/mean(df$bweight < 1000)
  reg_data[index, "elbw_dltr"] = elbw$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$bweight < 1000)
  reg_data[index, "elbw_dltr_se"] = sqrt(elbw_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$bweight < 1000)
  reg_data[index, "elbw_cov"] = elbw_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$bweight < 1000)
  
  reg_data[index, "mort_down"] = mort$coeftable["down", 1]/mean(df$death)
  reg_data[index, "mort_dse"] = sqrt(mort_v["down", "down"])/mean(df$death)
  reg_data[index, "mort_dltr"] = mort$coeftable[paste0("down:I(m_months_res > ", ges_length, ")"), 1]/mean(df$death)
  reg_data[index, "mort_dltr_se"] = sqrt(mort_v[paste0("down:I(m_months_res > ", ges_length, ")"), paste0("down:I(m_months_res > ", ges_length, ")")])/mean(df$death)
  reg_data[index, "mort_cov"] = mort_v[paste0("down"), paste0("down:I(m_months_res > ", ges_length, ")")]/mean(df$death)
  
  
  
  print(index)
  print(reg_data[index, ])
  index = index + 1
}

ltr_fun = function(out, reg_data){
  d_name = paste0(out, "_down")
  dse_name = paste0(out, "_dse")
  dltr_name = paste0(out, "_dltr")
  dltr_se_name = paste0(out, "_dltr_se")
  cov_name = paste0(out, "_cov")
  
  reg_data$sig_d = as.factor(ifelse((reg_data[, d_name]/reg_data[, dse_name]) > 1.645, "sig", "nsig"))
  reg_data$sig_dltr = as.factor(ifelse((reg_data[, d_name] + reg_data[, dltr_name])/
    (sqrt(reg_data[, dse_name]^2 + reg_data[, dltr_se_name]^2 + 2 * reg_data[, cov_name])) > 1.645, "sig", "nsig"))
  a = ggplot() +
    geom_point(data = reg_data, aes(x = ges_length + 1, y = get(d_name) + get(dltr_name), color = "Long-Term Resident", shape = sig_dltr), size = 6) +
    geom_errorbar(data = reg_data, aes(x = ges_length + 1, 
                                       ymin = (get(d_name) + get(dltr_name)) - 
                                         1.96 * (sqrt(get(dse_name)^2 + get(dltr_se_name)^2 + 2 * get(cov_name))),
                                       ymax = (get(d_name) + get(dltr_name)) + 
                                         1.96 * (sqrt(get(dse_name)^2 + get(dltr_se_name)^2 + 2 * get(cov_name))), color = "Long-Term Resident"), 
                  width = 1, size = 2) + 
    geom_point(data = reg_data, aes(x = ges_length - 1, y = get(d_name), color = "Short-Term Resident", shape = sig_d), size = 6) +
    geom_errorbar(data = reg_data, aes(x = ges_length - 1, 
                                       ymin = get(d_name) - 1.96 * get(dse_name),
                                       ymax = get(d_name) + 1.96 * get(dse_name), color = "Short-Term Resident"), 
                  width = 1, size = 2) + 
    theme_minimal(base_size = 28) + 
    labs(x = "Long-Term Resident Cutoff (Months)", y = "Effect Size", color = element_blank()) + 
    scale_x_continuous(labels = seq(from = 6, to = 60, by = 6), breaks = seq(from = 6, to = 60, by = 6)) + 
    scale_y_continuous(labels = scales::label_percent(), limits = c(-4, 5), breaks = c(-4, -2, 0, 2, 4)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    theme(legend.position = "bottom", 
          legend.key.size = unit(5, "lines"), 
          legend.text = element_text(size = 32)) + 
    scale_color_manual(values = c("Long-Term Resident" = "dodgerblue3", "Short-Term Resident" = "firebrick4")) + 
    guides(size = "none", shape = "none") + 
    scale_shape_manual(values = c("sig" = 15, "nsig" = 20))

  return(a)
  }

plots = lapply(c("pre", "lbw", "mpre", "mlbw", "vpre", "vlbw", "epre", "elbw"), ltr_fun, reg_data = reg_data)
plots[[1]] = plots[[1]] + ylab("Any (< 37 Weeks)") + xlab("")
plots[[2]] = plots[[2]] + ylab("Any (< 2500g)")+ xlab("")
plots[[3]] = plots[[3]] + ylab("Moderately (32-36 Weeks)")+ xlab("")
plots[[4]] = plots[[4]] + ylab("Moderately (1500-2499g)")+ xlab("")
plots[[5]] = plots[[5]] + ylab("Very (28-31 Weeks)")+ xlab("")
plots[[6]] = plots[[6]] + ylab("Very (1000-1499g)")+ xlab("")
plots[[7]] = plots[[7]] + ylab("Extremely (< 28 Weeks)")
plots[[8]] = plots[[8]] + ylab("Extremely (< 1000g)")
cfig = ggpubr::ggarrange(plotlist = plots, nrow = 4, ncol = 2, common.legend = T, legend = "bottom")
ggsave(cfig, file = modify_path3("Figures Revision/ltr_figure.png"), width = 10000, height = 7000, units = "px", dpi = 300)


mort_fig = ltr_fun("mort", reg_data)
mort_fig = mort_fig + ylab("Infant Mortality") + guides(color = "none")
ggsave(mort_fig, file = modify_path3("Figures Revision/ltr_mort_figure.png"), width = 7500, height = 1750, units = "px", dpi = 300)
