#first, get predicted pfas in levels:
df$pred_pfas_ppt = sinh(df$pred_pfas)
df$year_month = paste0(df$year, "-", df$month)

#fit a cubic spline in predicted pfas (ppt)
spline_fig = function(outcome, df, short = FALSE){
  if (outcome == "pre"){
    m = gam(I(gestation < 37) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
             n_sites + wind_exposure + 
             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
             mthr_wgt_dlv +mthr_pre_preg_wgt + 
             m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  if (outcome == "mpre"){
    m = gam(I(gestation < 37 & gestation >= 32) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  if (outcome == "vpre"){
    m = gam(I(gestation < 32 & gestation >= 28) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  if (outcome == "epre"){
    m = gam(I(gestation < 32 & gestation >= 28) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  
  
  if (outcome == "lbw"){
    m = gam(I(bweight < 2500) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  if (outcome == "mlbw"){
    m = gam(I(bweight < 2500 & bweight >= 1500) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  if (outcome == "vlbw"){
    m = gam(I(bweight < 1500 & bweight >= 1000) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  if (outcome == "elbw"){
    m = gam(I(bweight < 1000) ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  if (outcome == "mort"){
    m = gam(death ~ s(pred_pfas_ppt, bs = "cr") + I(asinh(pfas)) + 
              n_sites + wind_exposure + 
              m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
              pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
              mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
              mthr_wgt_dlv +mthr_pre_preg_wgt + 
              m_height + tri5 + fa_resid + as.factor(county) + as.factor(birth_race_dsc_1) + as.factor(year_month), data = df )
  }
  
  
  
  m = getViz(m)
  o = plot(sm(m, select = 1))
  o = o + l_fitLine(colour = "dodgerblue3", size = 2) +
    l_ciLine(mul = 5, colour = "firebrick4", linetype = 2) + 
    theme_classic() + 
    labs(y = outcome, x = "Predicted PFAS (ppt)") + 
    theme_minimal(base_size = 28)
  
  if (!short){
    o = o +
      scale_x_continuous(breaks = seq(from = 0, to = 80000, by = 20000), labels = c("0", "20,000", "40,000", "60,000", "80,000"))
  }else{
    o = o + scale_x_continuous(breaks = seq(from = 0, to = 20000, by = 5000), labels = c("0", "5,000", "10,000", "15,000", "20,000"), limits = c(0, 20000)) 
  }
  return(o)
}

plots = lapply(c("pre", "lbw", "mpre", "mlbw", "vpre", "vlbw", "epre", "elbw"), spline_fig, df)
plots[[1]] = plots[[1]] + ylab("Any (< 37 Weeks)") + xlab("") + ggtitle("Preterm") + theme(plot.title = element_text(hjust = 0.5))
plots[[2]] = plots[[2]] + ylab("Any (< 2500g)")+ xlab("") + ggtitle("Low Birthweight") + theme(plot.title = element_text(hjust = 0.5))
plots[[3]] = plots[[3]] + ylab("Moderately (32-36 Weeks)")+ xlab("")
plots[[4]] = plots[[4]] + ylab("Moderately (1500-2499g)")+ xlab("")
plots[[5]] = plots[[5]] + ylab("Very (28-31 Weeks)")+ xlab("")
plots[[6]] = plots[[6]] + ylab("Very (1000-1499g)")+ xlab("")
plots[[7]] = plots[[7]] + ylab("Extremely (< 28 Weeks)")
plots[[8]] = plots[[8]] + ylab("Extremely (< 1000g)")

p = gridPrint(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]], ncol = 2, nrow = 4)
ggsave(p, file = "Figures Revision/spline_figure_ppt.png", width = 7500, height = 7500, units = "px", dpi = 300)


plots = lapply(c("pre", "lbw", "mpre", "mlbw", "vpre", "vlbw", "epre", "elbw"), spline_fig, df, short = TRUE)
plots[[1]] = plots[[1]] + ylab("Any (< 37 Weeks)") + xlab("") + ggtitle("Preterm") + theme(plot.title = element_text(hjust = 0.5))
plots[[2]] = plots[[2]] + ylab("Any (< 2500g)")+ xlab("") + ggtitle("Low Birthweight") + theme(plot.title = element_text(hjust = 0.5))
plots[[3]] = plots[[3]] + ylab("Moderately (32-36 Weeks)")+ xlab("")
plots[[4]] = plots[[4]] + ylab("Moderately (1500-2499g)")+ xlab("")
plots[[5]] = plots[[5]] + ylab("Very (28-31 Weeks)")+ xlab("")
plots[[6]] = plots[[6]] + ylab("Very (1000-1499g)")+ xlab("")
plots[[7]] = plots[[7]] + ylab("Extremely (< 28 Weeks)")
plots[[8]] = plots[[8]] + ylab("Extremely (< 1000g)")

p = gridPrint(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]], plots[[8]], ncol = 2, nrow = 4)
ggsave(p, file = "Figures Revision/spline_figure_ppt20.png", width = 7500, height = 7500, units = "px", dpi = 300)

mort_plot = spline_fig("mort", df) + ylab("Infant Mortality")
mort_p = gridPrint(mort_plot, ncol = 1)
ggsave(mort_p, file = "Figures Revision/mort_spline_ppt.png", width = 3750, height = 1750, units = "px", dpi = 300)

mort_plot = spline_fig("mort", df, short = TRUE) + ylab("Infant Mortality")
mort_p = gridPrint(mort_plot, ncol = 1)
ggsave(mort_p, file = "Figures Revision/mort_spline_ppt20.png", width = 3750, height = 1750, units = "px", dpi = 300)



#histogram of support
ppt_hist = ggplot(df, aes(x = pred_pfas_ppt)) + 
  geom_histogram(binwidth = 1000, fill = "dodgerblue3", color = "black") + 
  theme_classic() + 
  labs(x = "Predicted PFAS (ppt)", y = "Number of Births") + 
  theme_minimal(base_size = 28) + 
  scale_x_continuous(breaks = seq(from = 0, to = 80000, by = 20000), labels = c("0", "20,000", "40,000", "60,000", "80,000")) +
  ylim(0, 2500)
ggsave(ppt_hist, file = "Figures Revision/ppt_hist.png", width = 3750, height = 1750, units = "px", dpi = 300)

ppt_hist20 = ggplot(df, aes(x = pred_pfas_ppt)) + 
  geom_histogram(binwidth = 1000, fill = "dodgerblue3", color = "black") + 
  theme_classic() + 
  labs(x = "Predicted PFAS (ppt)", y = "Number of Births") + 
  theme_minimal(base_size = 28) + 
  scale_x_continuous(breaks = seq(from = 0, to = 20000, by = 5000)) + 
  xlim(0, 20000) + 
  ylim(0, 2500) 
ggsave(ppt_hist20, file = "Figures Revision/ppt_hist20.png", width = 3750, height = 1750, units = "px", dpi = 300)


length(which(!is.na(df$pred_pfas_ppt) & df$pred_pfas_ppt > 20000))/length(which(!is.na(df$pred_pfas_ppt)))

                                                                          