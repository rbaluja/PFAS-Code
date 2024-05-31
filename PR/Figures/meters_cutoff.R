#set working directory
setwd("~/Dropbox/PFAS Infants")

#load in helper functions
source("PFAS-Code/PR/env_functions.R")
source("PFAS-Code/PR/Main Analysis/watershed_functions.R")
flowacc = function(i, d, w, option){
  d2 = d[[i]]
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

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets)
options(modelsummary_format_numeric_latex = "mathmode")

#set up environment
wind_dist= dist_allow = 10000
ppt = 1000
run_cleaning = FALSE
match_wells = FALSE
old_wells = FALSE
domestic = FALSE
system = FALSE
drop_dups = TRUE #needs to be false if calculating se on difference in theta
drop_far_down = TRUE
drop_far_up = FALSE
well_fd = test_fd = FALSE #flow line distance?
IV = FALSE
fa_resid = FALSE
drop_states = FALSE
relaxed_up = FALSE
code_check = FALSE
n_cores = 1
natality_path = "/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/"

index = 1
if (code_check == FALSE){
  load(paste0(natality_path, "[UA Box Health] birth_records_matched_mortality.RData"))  
  
}else{
  cont_sites = read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
    dplyr::filter(State == 'New Hampshire' & `Matrix Type` == 'Groundwater') %>% 
    dplyr::select(site = `Site name`, lat = Latitude, 
                  date = `Date Sampled`, lng = Longitude, industry = Industry, 
                  sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`) %>%
    dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= ppt) %>% #cut to 1000ppt by Bo meeting 4/21/23
    st_as_sf(coords = c('lng', 'lat'), remove = F) %>% # transform into a spatial dataframe
    st_set_crs('+proj=longlat +datum=WGS84')
  
  #two sites are repeated, remove them
  cont_sites = cont_sites[which(!cont_sites$site %in% c("Former Aerotronic Site", "Gilson Road Site")), ]
  
  load(modify_path("Data_Verify/fake_natality.RData"))
  
  #get covariates for birth records
  source("PFAS-Code/PR/Data/birth_covars.R")
  
  #well location and service area data (NHDES)
  source("PFAS-Code/PR/Data/NHDES_PWS.R")
  #match residences to water wells
  source("PFAS-Code/PR/Data/natality_wells.R")
  
  #get elevation at relevant well and residence
  source("PFAS-Code/PR/Data/elev_setup.R")
}
#get flow accumulation at residence
# #read in flow accumulation raster
if (!file.exists(modify_path("Data_Verify/GIS/cont_fa_sum_buffed.tiff"))){
  stop("Need to run preliminaries first to get flow accumulation raster")
}
cont_fa = terra::rast(modify_path("Data_Verify/GIS/cont_fa_sum_buffed.tiff"))
df_inter_fa= df %>% as_tibble() %>%  
  dplyr::select(!geometry) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F) %>% 
  st_transform(32110) %>% 
  st_buffer(10) %>% 
  st_transform(4326)

df_fa = exactextractr::exact_extract(cont_fa, df_inter_fa)

df = dplyr::bind_rows(pblapply(1:nrow(df), flowacc, df_fa, df, "resid"))

one_sp = function(tval, pval){
  if (tval < 0){
    return(1 - pval/2)
  }else{
    return(pval/2)
  }
}

dfs = df
reg_data = data.frame(matrix(ncol = 46, nrow = 0))
colnames(reg_data) = c('meters', 
                       'pre_down', 'pre_dse', 'pre_updown', 'pre_udse', 'pre_down_p',
                       'lpre_down', 'lpre_dse', 'lpre_updown', 'lpre_udse', 'lpre_down_p',
                       'mpre_down', 'mpre_dse', 'mpre_updown', 'mpre_udse', 'mpre_down_p',
                       'vpre_down', 'vpre_dse', 'vpre_updown', 'vpre_udse', 'vpre_down_p',
                       'lbw_down','lbw_dse', "lbw_updown", "lbw_udse", 'lbw_down_p',
                       'llbw_down','llbw_dse', "llbw_updown", "llbw_udse",'llbw_down_p',
                       'vlbw_down','vlbw_dse', "vlbw_updown", "vlbw_udse",'vlbw_down_p',
                       'elbw_down','elbw_dse', "elbw_updown", "elbw_udse", 'elbw_down_p', 
                       'mort_down','mort_dse', "mort_updown", "mort_udse", 'mort_down_p')
for (meters in 3:10 * 1000){
  
  dist = meters
  
  df = dfs
  
  #obtain theta info for Northeastern contamination data
  source("PFAS-Code/PR/Data/pfas_lab_sites.R")
  
  #well location and service area data (NHDES)
  source("PFAS-Code/PR/Data/NHDES_PWS.R")
  
  #set up wind
  source("PFAS-Code/PR/Data/wind.R")
  
  #binary setup
  source("PFAS-Code/PR/Main Analysis/binary.R")
  
  preterm_any = fixest::feols(I(gestation < 37) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                                m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                                pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                                mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                                mthr_wgt_dlv +mthr_pre_preg_wgt + 
                                m_height + tri5 + fa_resid + wind_exposure
                              |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                              warn = F, notes = F, cluster = c("site", "year^month"))
  
  lpreterm = fixest::feols(I(gestation < 37 & gestation >= 32) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                           warn = F, notes = F, cluster = c("site", "year^month"))
  
  mpreterm = fixest::feols(I(gestation < 32 & gestation >= 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                           warn = F, notes = F, cluster = c("site", "year^month"))
  
  vpreterm = fixest::feols(I(gestation < 28) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                             m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                             pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                             mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                             mthr_wgt_dlv +mthr_pre_preg_wgt + 
                             m_height + tri5 + fa_resid + wind_exposure
                           |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                           warn = F, notes = F, cluster = c("site", "year^month"))
  
  
  
  lbw_any = fixest::feols(I(bweight < 2500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                            m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                            pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                            mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                            mthr_wgt_dlv +mthr_pre_preg_wgt + 
                            m_height + tri5 + fa_resid + wind_exposure
                          |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                          warn = F, notes = F, cluster = c("site", "year^month"))
  
  lbw = fixest::feols(I(bweight < 2500 & bweight >= 1500) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                        m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                        pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                        mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                        mthr_wgt_dlv +mthr_pre_preg_wgt + 
                        m_height + tri5 + fa_resid + wind_exposure
                      |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                      warn = F, notes = F, cluster = c("site", "year^month"))
  
  vlbw = fixest::feols(I(bweight < 1500 & bweight >= 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid + wind_exposure
                       |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                       warn = F, notes = F, cluster = c("site", "year^month"))
  
  elbw = fixest::feols(I(bweight < 1000) ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid + wind_exposure
                       |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                       warn = F, notes = F, cluster = c("site", "year^month"))
  
  mort = fixest::feols(death ~  updown + down +  I(pfas/10^3) + dist  + n_sites + 
                         m_age + m_married  + private_insurance  + nbr_cgrtt  + m_educ + f_educ +
                         pm25 + temp +med_inc+ p_manuf + n_hunits + med_hprice  + well_elev + resid_elev + csite_dist + wic+
                         mr_04 + mr_18 + mr_08 + mr_21 + mr_26 + mr_27 + 
                         mthr_wgt_dlv +mthr_pre_preg_wgt + 
                         m_height + tri5 + fa_resid + wind_exposure
                       |county + year^month + birth_race_dsc_1, data = df[which(df$dist <= meters), ], 
                       warn = F, notes = F, cluster = c("site", "year^month"))

  

  pre_v = vcov(preterm_any, cluster = c("site", "year^month"))
  lpre_v = vcov(lpreterm, cluster = c("site", "year^month"))
  mpre_v = vcov(mpreterm, cluster = c("site", "year^month"))
  vpre_v = vcov(vpreterm, cluster = c("site", "year^month"))
  
  lbw_v = vcov(lbw_any, cluster = c("site", "year^month"))
  llbw_v = vcov(lbw, cluster = c("site", "year^month"))
  vlbw_v = vcov(vlbw, cluster = c("site", "year^month"))
  elbw_v = vcov(elbw, cluster = c("site", "year^month"))
  
  mort_v = vcov(mort, cluster = c("site", "year^month"))
  
  
  reg_data[index, "meters"] = meters
  reg_data[index, "pre_down"] = preterm_any$coeftable["down", 1]
  reg_data[index, "pre_dse"] = sqrt(pre_v["down", "down"])
  reg_data[index, "pre_updown"] = preterm_any$coeftable["updown", 1]
  reg_data[index, "pre_udse"] =  sqrt(pre_v["updown", "updown"])
  reg_data[index, "pre_down_p"] = one_sp(preterm_any$coeftable["down", "t value"], 
                                         preterm_any$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "lpre_down"] = lpreterm$coeftable["down", 1]
  reg_data[index, "lpre_dse"] = sqrt(lpre_v["down", "down"])
  reg_data[index, "lpre_updown"] = lpreterm$coeftable["updown", 1]
  reg_data[index, "lpre_udse"] =  sqrt(lpre_v["updown", "updown"])
  reg_data[index, "lpre_down_p"] = one_sp(lpreterm$coeftable["down", "t value"], 
                                          lpreterm$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "mpre_down"] = mpreterm$coeftable["down", 1]
  reg_data[index, "mpre_dse"] = sqrt(mpre_v["down", "down"])
  reg_data[index, "mpre_updown"] = mpreterm$coeftable["updown", 1]
  reg_data[index, "mpre_udse"] = sqrt(mpre_v["updown", "updown"])
  reg_data[index, "mpre_down_p"] = one_sp(mpreterm$coeftable["down", "t value"], 
                                          mpreterm$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "vpre_down"] = vpreterm$coeftable["down", 1]
  reg_data[index, "vpre_dse"] = sqrt(vpre_v["down", "down"])
  reg_data[index, "vpre_updown"] = vpreterm$coeftable["updown", 1]
  reg_data[index, "vpre_udse"] = sqrt(vpre_v["updown", "updown"])
  reg_data[index, "vpre_down_p"] = one_sp(vpreterm$coeftable["down", "t value"], 
                                          vpreterm$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "lbw_down"] = lbw_any$coeftable["down", 1]
  reg_data[index, "lbw_dse"] = sqrt(lbw_v["down", "down"])
  reg_data[index, "lbw_updown"] = lbw$coeftable["updown", 1]
  reg_data[index, "lbw_udse"] = sqrt(lbw_v["updown", "updown"])
  reg_data[index, "lbw_down_p"] = one_sp(lbw_any$coeftable["down", "t value"], 
                                         lbw_any$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "llbw_down"] = lbw$coeftable["down", 1]
  reg_data[index, "llbw_dse"] = sqrt(llbw_v["down", "down"])
  reg_data[index, "llbw_updown"] = lbw$coeftable["updown", 1]
  reg_data[index, "llbw_udse"] = sqrt(llbw_v["updown", "updown"])
  reg_data[index, "llbw_down_p"] = one_sp(lbw$coeftable["down", "t value"], 
                                          lbw$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "vlbw_down"] = vlbw$coeftable["down", 1]
  reg_data[index, "vlbw_dse"] = sqrt(vlbw_v["down", "down"])
  reg_data[index, "vlbw_updown"] = vlbw$coeftable["updown", 1]
  reg_data[index, "vlbw_udse"] = sqrt(vlbw_v["updown", "updown"])
  reg_data[index, "vlbw_down_p"] = one_sp(vlbw$coeftable["down", "t value"], 
                                          vlbw$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "elbw_down"] = elbw$coeftable["down", 1]
  reg_data[index, "elbw_dse"] = sqrt(elbw_v["down", "down"])
  reg_data[index, "elbw_updown"] = elbw$coeftable["updown", 1]
  reg_data[index, "elbw_udse"] = sqrt(elbw_v["updown", "updown"])
  reg_data[index, "elbw_down_p"] = one_sp(elbw$coeftable["down", "t value"], 
                                         elbw$coeftable["down", "Pr(>|t|)"])
  
  reg_data[index, "mort_down"] = mort$coeftable["down", 1]
  reg_data[index, "mort_dse"] = sqrt(mort_v["down", "down"])
  reg_data[index, "mort_updown"] = mort$coeftable["updown", 1]
  reg_data[index, "mort_udse"] = sqrt(mort_v["updown", "updown"])
  reg_data[index, "mort_down_p"] = one_sp(mort$coeftable["down", "t value"], 
                                          mort$coeftable["down", "Pr(>|t|)"])

  
  
  
  print(index)
  print(reg_data[index, ])
  index = index + 1
  
}



adjustment_km = .1
reg_data$km = reg_data$meters/1000

reg_data$pre_dlower = reg_data$pre_down - 1.96 * reg_data$pre_dse
reg_data$pre_dupper = reg_data$pre_down + 1.96 * reg_data$pre_dse

reg_data$pre_udlower = reg_data$pre_updown - 1.96 * reg_data$pre_udse
reg_data$pre_udupper = reg_data$pre_updown + 1.96 * reg_data$pre_udse


reg_data$lpre_dlower = reg_data$lpre_down - 1.96 * reg_data$lpre_dse
reg_data$lpre_dupper = reg_data$lpre_down + 1.96 * reg_data$lpre_dse

reg_data$lpre_udlower = reg_data$lpre_updown - 1.96 * reg_data$lpre_udse
reg_data$lpre_udupper = reg_data$lpre_updown + 1.96 * reg_data$lpre_udse

reg_data$mpre_dlower = reg_data$mpre_down - 1.96 * reg_data$mpre_dse
reg_data$mpre_dupper = reg_data$mpre_down + 1.96 * reg_data$mpre_dse

reg_data$mpre_udlower = reg_data$mpre_updown - 1.96 * reg_data$mpre_udse
reg_data$mpre_udupper = reg_data$mpre_updown + 1.96 * reg_data$mpre_udse

reg_data$vpre_dlower = reg_data$vpre_down - 1.96 * reg_data$vpre_dse
reg_data$vpre_dupper = reg_data$vpre_down + 1.96 * reg_data$vpre_dse

reg_data$vpre_udlower = reg_data$vpre_updown - 1.96 * reg_data$vpre_udse
reg_data$vpre_udupper = reg_data$vpre_updown + 1.96 * reg_data$vpre_udse

# Create adjusted datasets
reg_data_upgradient = reg_data %>% 
  mutate(theta_cutoff_adjusted =km - adjustment_km)

reg_data_downgradient = reg_data %>% 
  mutate(theta_cutoff_adjusted = km + adjustment_km)


reg_data_upgradient$pre_down_p_label = ifelse(reg_data_upgradient$pre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$pre_down_p))
reg_data_upgradient$lpre_down_p_label = ifelse(reg_data_upgradient$lpre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$lpre_down_p))
reg_data_upgradient$mpre_down_p_label =  ifelse(reg_data_upgradient$mpre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$mpre_down_p))
reg_data_upgradient$vpre_down_p_label =  ifelse(reg_data_upgradient$vpre_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$vpre_down_p))


p_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = pre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = pre_dlower, ymax = pre_dupper), width = 0.1) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = pre_dupper, label = pre_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Any (<37 Weeks)") + 
  xlab("") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24), 
        plot.title = element_text(hjust = 0.5, size = 34)) +
  guides(color = "none", fill = "none")+ ggtitle("Preterm") + 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE) + ylim(c(-0.02, 0.12))


lp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = lpre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = lpre_dlower, ymax = lpre_dupper), width = 0.1) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = lpre_dupper, label = lpre_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Moderately (32-36 Weeks)") + 
  xlab("") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25)+  guides(color = FALSE)+ ylim(c(-0.02, 0.12))

mp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = mpre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = mpre_dlower, ymax = mpre_dupper), width = 0.1) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = mpre_dupper, label = mpre_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Very (28-31 Weeks)") + 
  xlab("") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25)+  guides(color = FALSE)+ ylim(c(-0.02, 0.12))


vp_combined = ggplot() +
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = vpre_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = vpre_dlower, ymax = vpre_dupper), width = 0.1) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = vpre_dupper, label = vpre_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Extremely (<28 Weeks)") + 
  theme_minimal() + 
  xlab("Buffer (km)") +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 26), 
        axis.title.x = element_text(size = 28), 
        legend.text = element_text(size = 24),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = guide_legend(title = ""), fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + 
  scale_x_continuous(breaks = 1:10)+ ylim(c(-0.02, 0.12))

reg_data$lbw_dlower = reg_data$lbw_down - 1.96 * reg_data$lbw_dse
reg_data$lbw_dupper = reg_data$lbw_down + 1.96 * reg_data$lbw_dse

reg_data$lbw_udlower = reg_data$lbw_updown - 1.96 * reg_data$lbw_udse
reg_data$lbw_udupper = reg_data$lbw_updown + 1.96 * reg_data$lbw_udse

reg_data$llbw_dlower = reg_data$llbw_down - 1.96 * reg_data$llbw_dse
reg_data$llbw_dupper = reg_data$llbw_down + 1.96 * reg_data$llbw_dse

reg_data$llbw_udlower = reg_data$llbw_updown - 1.96 * reg_data$llbw_udse
reg_data$llbw_udupper = reg_data$llbw_updown + 1.96 * reg_data$llbw_udse

reg_data$vlbw_dlower = reg_data$vlbw_down - 1.96 * reg_data$vlbw_dse
reg_data$vlbw_dupper = reg_data$vlbw_down + 1.96 * reg_data$vlbw_dse

reg_data$vlbw_udlower = reg_data$vlbw_updown - 1.96 * reg_data$vlbw_udse
reg_data$vlbw_udupper = reg_data$vlbw_updown + 1.96 * reg_data$vlbw_udse

reg_data$elbw_dlower = reg_data$elbw_down - 1.96 * reg_data$elbw_dse
reg_data$elbw_dupper = reg_data$elbw_down + 1.96 * reg_data$elbw_dse

reg_data$elbw_udlower = reg_data$elbw_updown - 1.96 * reg_data$elbw_udse
reg_data$elbw_udupper = reg_data$elbw_updown + 1.96 * reg_data$elbw_udse

# Create adjusted datasets
reg_data_upgradient = reg_data %>% 
  mutate(theta_cutoff_adjusted = km - adjustment_km)

reg_data_downgradient = reg_data %>% 
  mutate(theta_cutoff_adjusted = km + adjustment_km)

reg_data_upgradient$lbw_down_p_label = ifelse(reg_data_upgradient$lbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$lbw_down_p))
reg_data_upgradient$llbw_down_p_label = ifelse(reg_data_upgradient$llbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$llbw_down_p))
reg_data_upgradient$vlbw_down_p_label = ifelse(reg_data_upgradient$vlbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$vlbw_down_p))
reg_data_upgradient$elbw_down_p_label = ifelse(reg_data_upgradient$elbw_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$elbw_down_p))

lbw_combined = ggplot(reg_data, aes(x = km)) + 
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = lbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = lbw_dlower, ymax = lbw_dupper), width = 0.1) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = lbw_dupper, label = lbw_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Any (<2500g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24), 
        plot.title = element_text(hjust = 0.5, size = 34)) +
  guides(color = "none", fill = "none")+ ggtitle("Low Birthweight") + 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+ ylim(c(-0.02, 0.12))


llbw_combined = ggplot(reg_data, aes(x = km)) + 
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = llbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = llbw_dlower, ymax = llbw_dupper), width = 0.1) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = llbw_dupper, label = llbw_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Moderately (1500-2499g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+ ylim(c(-0.02, 0.12))

vlbw_combined = ggplot(reg_data, aes(x = km)) + 
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = vlbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = vlbw_dlower, ymax = vlbw_dupper), width = 0.1) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = vlbw_dupper, label = vlbw_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Very (1000-1499g)") + 
  theme_minimal() + 
  xlab("") + 
  theme(legend.position = "bottom", 
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + guides(color = FALSE)+ ylim(c(-0.02, 0.12))


elbw_combined = ggplot(reg_data, aes(x = km)) + 
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = elbw_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = elbw_dlower, ymax = elbw_dupper), width = 0.1,) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = elbw_dupper, 
                                            label = elbw_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Extremely (<1000g)") + 
  theme_minimal() + 
  xlab("Buffer (km)") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 26), 
        axis.title.x = element_text(size = 28), 
        legend.text = element_text(size = 24),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + 
  scale_x_continuous(breaks = 1:10) + ylim(c(-0.02, 0.12))

#infant mortality
reg_data$mort_dlower = reg_data$mort_down - 1.96 * reg_data$mort_dse
reg_data$mort_dupper = reg_data$mort_down + 1.96 * reg_data$mort_dse

# Create adjusted datasets
reg_data_upgradient = reg_data %>% 
  mutate(theta_cutoff_adjusted = km - adjustment_km)

reg_data_downgradient = reg_data %>% 
  mutate(theta_cutoff_adjusted = km + adjustment_km)

reg_data_upgradient$mort_down_p_label = ifelse(reg_data_upgradient$mort_down_p < 0.001, "<0.001", sprintf("%.3f", reg_data_upgradient$mort_down_p))

mort_fig = ggplot(reg_data, aes(x = km)) + 
  geom_point(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = mort_down)) +
  geom_errorbar(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, ymin = mort_dlower, ymax = mort_dupper), width = 0.1,) +
  geom_text(data = reg_data_upgradient, aes(x = theta_cutoff_adjusted, y = mort_dupper, 
                                            label = mort_down_p_label), nudge_y = 0.01, size = 7) +
  ylab("Infant Mortality") + 
  theme_minimal() + 
  xlab("Buffer (km)") +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = 26), 
        axis.title.x = element_text(size = 28), 
        legend.text = element_text(size = 24),
        axis.title.y = element_text(size = 28), 
        axis.text.y = element_text(size = 24)) +
  guides(color = "none", fill = "none")+ 
  geom_hline(yintercept = 0, color = "black", size = 0.25) + 
  scale_x_continuous(breaks = 1:10) + ylim(c(-0.02, 0.12))



figure_s3 = (p_combined | lbw_combined) / (lp_combined | llbw_combined) / (mp_combined | vlbw_combined) / (vp_combined | elbw_combined)
ggsave(modify_path3("Figures/Robustness/cutoff_figure.png"), figure_s3, width = 7000, height = 7000, units = "px", dpi = 300)
ggsave(modify_path3("Figures/Robustness/cutoff_figure_mort.png"), mort_fig, width = 3500, height = 1750, units = "px")


