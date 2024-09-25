#extract significant predictors of downgradient
sig_pred = which(as.numeric(abs(r1_down[["Downgradient"]]$coefficients/r1_down[["Downgradient"]]$se)) > 1.96)
sig_names = r1_down[["Downgradient"]]$coeftable[sig_pred, 1]
#m_age, nbr_cgrtt, m_educ, mr_21, mr_26, mr_27, m_height
