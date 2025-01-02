load(modify_path("Data_Verify/National/births_sites_assigned.RData"))

births = 
  births %>% 
  dplyr::mutate(tract = stringr::str_pad(tract, pad = "0", width = 6, side = "left")) %>% 
  dplyr::mutate(GEOID = paste0(county, tract, cbg))

births_ll = births %>% 
  st_transform(st_crs(xgb_out))

#get the predicted exposure prob from the xgboost model for each point in w_ll
births_ll$xgb_prob = terra::extract(xgb_out, births_ll)$prob_public

verd = list()
verd[["down1"]] = lm(down ~ xgb_prob, data = births_ll)
verd[["down2"]] = lm(down ~ I(xgb_prob > 0.315), data = births_ll)
verd[["down3"]] = lm(down ~ I(xgb_prob > 0.5), data = births_ll)
verd[["down4"]] = lm(down ~ I(xgb_prob > 0.75), data = births_ll)

modelsummary::modelsummary(verd,
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("xgb_prob" = "Prob. of PFAS at Well", 
                                        "I(xgb_prob > 0.315)TRUE" = "I(Prob of PFAS > 0.315)", 
                                        "I(xgb_prob > 0.5)TRUE" = "I(Prob of PFAS > 0.5)", 
                                        "I(xgb_prob > 0.75)TRUE" = "I(Prob of PFAS > 0.75)"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/verd_rout.tex")) 

veru = list()
veru[["up1"]] = lm(up ~ xgb_prob, data = births_ll)
veru[["up2"]] = lm(up ~ I(xgb_prob > 0.315), data = births_ll)
veru[["up3"]] = lm(up ~ I(xgb_prob > 0.5), data = births_ll)
veru[["up4"]] = lm(up ~ I(xgb_prob > 0.75), data = births_ll)

modelsummary::modelsummary(veru,
                           stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01), #gives one sided test stars, when it has right sign
                           fmt = modelsummary::fmt_significant(2, scientific = F), 
                           coef_map = c("xgb_prob" = "Prob. of PFAS at Well", 
                                        "I(xgb_prob > 0.315)TRUE" = "I(Prob of PFAS > 0.315)", 
                                        "I(xgb_prob > 0.5)TRUE" = "I(Prob of PFAS > 0.5)", 
                                        "I(xgb_prob > 0.75)TRUE" = "I(Prob of PFAS > 0.75)"),
                           gof_map = c("nobs", "r.squared"), 
                           output = modify_path2("Tables/Revisions/veru_rout.tex")) 

