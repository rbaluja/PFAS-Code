#clear memory
rm(list = ls())
#restart R
.rs.restartR()

#set working directory
if (file.exists('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH')){
  setwd('~/Documents/Projects/Current_Projects/PFAS Infant Health/NH') 
}else{
  setwd('/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH')
}

#load in helper functions
source("Code/Primary/env_functions.R")
source("Code/Primary/Watersheds/watershed_functions.R")

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




index = 1
load("/Users/robert/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] birth_records_matched122023.RData") 
dfs = df
reg_data = data.frame(matrix(ncol = 4, nrow = 0))
colnames(reg_data) = c('meters', "total_n", "n_up", "n_down")
for (meters in seq(3000, 10000, by = 1000)){
  
  dist = meters
  
  df = dfs
  
  #obtain theta info for Northeastern contamination data
  source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/Watersheds/groundwater_algorithm.R")
  
  #well location and service area data (NHDES)
  source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/Watersheds/source_service_cleaning.R")
  
  #set up wind
  source("/Users/robert/Documents/GitHub/PFAS_IH/Primary/wind.R")
  
  #binary setup
  source("Code/Primary/Watersheds/binary.R")

  df = df %>% 
    dplyr::filter(!is.na(gestation) & 
                    !is.na(m_age) & 
                    !is.na(m_married) & 
                    !is.na(private_insurance) & 
                    !is.na(nbr_cgrtt) & 
                    !is.na(m_educ) & 
                    !is.na(f_educ) & 
                    !is.na(pm25) & 
                    !is.na(temp) & 
                    !is.na(p_manuf) & 
                    !is.na(n_hunits) & 
                    !is.na(med_hprice) & 
                    !is.na(well_elev) & 
                    !is.na(resid_elev) & 
                    !is.na(mr_04) & 
                    !is.na(mr_18) & 
                    !is.na(mr_21) & 
                    !is.na(mr_26) & 
                    !is.na(mr_27) & 
                    !is.na(mthr_wgt_dlv) & 
                    !is.na(mthr_pre_preg_wgt) & 
                    !is.na(m_height) & 
                    !is.na(tri5) & 
                    !is.na(county) & 
                    !is.na(year) & 
                    !is.na(month) & 
                    !is.na(birth_race_dsc_1) & 
                    !is.na(wic))
  
  df$updown = ifelse(df$up == 1 | df$down == 1, 1, 0)
  df = df[df$dist <= meters & !is.na(df$dist), ]
  
  total_n = nrow(df)
  n_up = length(which(df$up == 1))
  n_down = length(which(df$down == 1))
  
  reg_data[index, "meters"] = meters
  reg_data[index, "total_n"] = total_n
  reg_data[index, "n_up"] = n_up
  reg_data[index, "n_down"] = n_down
  
  print(index)
  index = index + 1
  
}

reg_data$km = reg_data$meters/1000

reg_data_long = reg_data %>%
  tidyr::pivot_longer(cols = c(total_n, n_up, n_down),
               names_to = "Category",
               values_to = "Value")

ggplot(reg_data_long, aes(x = km, y = Value, color = Category)) + 
  geom_line(size = 1.5) +
  ylab('Sample Size') + 
  xlab('Buffer (km)') + 
  theme_minimal() + guides(color = "none") + 
  annotate("text", x = 4.4, y = 12000, label = "Total Sample", angle = 15, size = 5) + 
  annotate("text", x = 4.9, y = 7200, label = "Downgradient", angle = 4, size = 5)+ 
  annotate("text", x = 4.5, y = 1100, label = "Upgradient", angle = 3, size = 5) + 
  theme(axis.text = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 12, face = "bold")) + 
  scale_x_continuous(breaks = 1:10)
