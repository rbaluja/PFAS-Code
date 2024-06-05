source("PFAS-Code/Pub/config.R")
index = 1
if (!code_check){
  load(paste0(natality_path, "[UA Box Health] birth_records_matched_still.RData")) 
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
  source("PFAS-Code/Pub/Data/birth_covars.R")

  #well location and service area data (NHDES)
  source("PFAS-Code/Pub/Data/NHDES_PWS.R")
  #match residences to water wells
  source("PFAS-Code/Pub/Data/natality_wells.R")
  
  #get elevation at relevant well and residence
  source("PFAS-Code/Pub/Data/elev_setup.R")
}
dfs = df
reg_data = data.frame(matrix(ncol = 4, nrow = 0))
colnames(reg_data) = c('meters', "total_n", "n_up", "n_down")
for (meters in seq(3000, 10000, by = 1000)){
  
  dist = meters
  
  df = dfs
  
  #obtain theta info for Northeastern contamination data
  source("PFAS-Code/Pub/Data/pfas_lab_sites.R")
  
  #well location and service area data (NHDES)
  source("PFAS-Code/Pub/Data/NHDES_PWS.R")
  
  #set up wind
  source("PFAS-Code/Pub/Data/wind.R")
  
  #binary setup
  source("PFAS-Code/Pub/Main Analysis/binary.R")
  


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

figure_s2b = ggplot(reg_data_long, aes(x = km, y = Value, color = Category)) + 
  geom_line(size = 2) +
  ylab('Sample Size') + 
  xlab('Buffer (km)') + 
  theme_minimal() + 
  guides(color = "none") + 
  annotate("text", x = 6, y = 14000, label = "Total Sample", angle = 25, size = 16) + 
  annotate("text", x = 5.4, y = 8500, label = "Downgradient", angle = 5, size = 16) + 
  annotate("text", x = 7.6, y = 3500, label = "Upgradient", angle = 0, size = 16) + 
  theme(axis.text = element_text(size = 35), 
        axis.title = element_text(size = 35)) + 
  scale_x_continuous(breaks = 1:10) +
  geom_point(data = subset(reg_data_long, km == 5), aes(x = km, y = Value, color = Category), size = 7)

ggsave(modify_path3("Figures/figure_s2b.png"), figure_s2b, width = 4421, height = 2937, units = "px")
