k = 1
gw_dist_allowance = 3000
ppt = 1000
t_option = "Theta"
huc = 10


coef_df = data.frame(matrix(ncol = 9, nrow = 0))
colnames(coef_df) = c("km", "ges_d_coef", "ges_d_se", 
                      "ges_other_coef", "ges_other_se", 
                      "bweight_d_coef", "bweight_d_se", 
                      "bweight_other_coef", "bweight_other_se")
for (meters in 3:15 * 1000){
  dist = meters
  
  #obtain triangle info for Northeastern contamination data
  source('Code/Theta/groundwater_algorithm.R')
  
  #well location and service area data (NHDES)
  source('Code/Theta/source_service_cleaning.R')
  
  #load in primary dataframe
  load("/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality_wdem_elev.RData")
  
  #run analysis
  source("Code/Theta/mult_site_setup.R")
  
  coef_df[k, ] = c(meters/1000, ssg$coeftable[2, 1], ssg$coeftable[2, 2], 
                   ssg$coeftable[3, 1], ssg$coeftable[3, 2],
                   ssb$coeftable[2, 1], ssb$coeftable[2, 2],
                   ssb$coeftable[3, 1], ssb$coeftable[3, 2])
  
  print(paste0("Just finished ", k, "th iteration"))
  
  
  k = k + 1
  
}

coef_df$ges_d_low = coef_df$ges_d_coef - 1.645 * coef_df$ges_d_se
coef_df$ges_d_high = coef_df$ges_d_coef + 1.645 * coef_df$ges_d_se
coef_df$bweight_d_low = coef_df$bweight_d_coef - 1.645 * coef_df$bweight_d_se
coef_df$bweight_d_high = coef_df$bweight_d_coef + 1.645 * coef_df$bweight_d_se

colors = c("Other PFAS"= "olivedrab4", "Downstream PFAS" = "steelblue")
ggplot(coef_df, aes(x = km)) + geom_smooth(aes(y = ges_d_coef, color = "Downstream PFAS"), se = F) + 
  geom_ribbon(aes(ymin = ges_d_low, ymax = ges_d_high, x = km, fill = 'Confidence Band'), alpha = 0.1, fill = 'blue') +
  ylab('Treatment Effect') + xlab('Kilometers') + 
  geom_smooth(aes(y = ges_other_coef, color = "Other PFAS"), se = F) + 
  ggtitle("Length of Gestation (Weeks)") + theme_minimal() + 
  labs(color = "") +
  scale_color_manual(values = colors) + theme(legend.position = c(0.8, 0.2))
  
  
colors = c("Other PFAS"= "olivedrab4", "Downstream PFAS" = "steelblue")
ggplot(coef_df, aes(x = km)) + geom_smooth(aes(y = bweight_d_coef, color = "Downstream PFAS"), se = F) + 
  geom_ribbon(aes(ymin = bweight_d_low, ymax = bweight_d_high, x = km, fill = 'Confidence Band' ), alpha = 0.1, fill = 'blue') +
  ylab('Treatment Effect') + xlab('Kilometers')  + 
  geom_smooth(aes(y = bweight_other_coef, color = "Other PFAS"), se = F) + 
  ggtitle("Infant Birth Weight (Grams)") + theme_minimal() + 
  labs(color = "") +
  scale_color_manual(values = colors) + theme(legend.position = c(0.8, 0.2))




