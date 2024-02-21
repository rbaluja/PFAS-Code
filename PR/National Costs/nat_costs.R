
if (nat_reassn == TRUE){
  births = read.csv("Nat Data/births_cbg_cleaned_2010.csv")
  #add state column to cbgs
  births$county = stringr::str_pad(as.character(births$county), 5, "left", "0")
  births$state = stringr::str_sub(births$county, 1, 2)
  
  births$geoid = paste0(births$county, births$tract, births$cbg)
  fwrite(births %>% dplyr::select(geoid, lng, lat), "Nat Data/cbg_ll.csv")
  
  births = births %>% 
    st_as_sf(coords = c("lng", "lat"), crs = 4326)
  
  #load in contamination data
  cont_sites = read_xlsx('New Hampshire/Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
    dplyr::filter(`Matrix Type` == 'Groundwater' & State != "Alaska") %>% 
    dplyr::select(`Site name`, State, Latitude, Longitude, Industry, 
                  `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                  `Max PFOA+PFOS from a single sample (ppt)`, 
                  `Max Total PFAS from a single sample (ppt)`, 
                  `PFAS Level (ppt)`) %>% 
    dplyr::rename(site = `Site name`, state = State, lat = Latitude, 
                  date = `Date Sampled`, lng = Longitude, industry = Industry, 
                  pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                  sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                  sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                  total_pfas = `PFAS Level (ppt)`) %>%
    dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
    st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
    st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
    st_transform(4326)
  
  #subset cbgs (births) to only those within 5km of a cont site
  csite_buff = cont_sites %>% 
    st_transform(5070) %>% #get to albers projection for meters
    st_buffer(meters)
  
  #only keep cbgs within a buffer
  births = st_intersection(births %>% st_transform(5070), csite_buff)
  
  #some duplicates, for now, only keep birth columns and drop dups
  births = births %>% 
    dplyr::select(county, tract, cbg, births, state, geoid, geometry) %>% 
    unique()
  
  #load in watersheds
  load("New Hampshire/Data/RData/nat_cont_watershed.RData")
  cont_ws = wells_ws
  load("New Hampshire/Data/RData/nat_cbg_watershed.RData")
  
  #merge watersheds
  #first do cont sites
  cont_ws = cont_ws %>% 
    left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, state, pfas = sum_pfoa_pfos))
  
  #now do births
  births_ws = wells_ws %>%  #this is how the original object was named. It isnt actually wells
    left_join(births %>% as_tibble() %>% dplyr::select(geoid, births))
  
  
  source("Code/PR/National Costs/well_assn.R") 
}else{
  births = fread("Nat Data/births_sites_assigned5.csv")
}
bll = fread("Nat Data/cbg_ll.csv")
bll = bll[which(bll$geoid %in% births$geoid), ]
 
#soil stuff
if (nat_redo_soil == TRUE){
  source("Code/PR/National Costs/soil.R") 
}else{
  births = fread("Nat Data/nat_births_fcleaned5.csv")
}

cont_sites = read_xlsx('New Hampshire/Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
  dplyr::filter(`Matrix Type` == 'Groundwater' & State != "Alaska") %>% 
  dplyr::select(`Site name`, State, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`) %>% 
  dplyr::rename(site = `Site name`, state = State, lat = Latitude, 
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)

#subsetting to states with initiatives
bs = births %>% 
  left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, state)) %>% 
  dplyr::filter(state %in% c("Michigan", 
                             "Minnesota", 
                             "New Hampshire", 
                             "New York", 
                             "Colorado", 
                             "Maine", 
                             "Vermont", 
                             "California", 
                             "Florida", 
                             "North Dakota", 
                             "Wisconsin"))



bs$updown = ifelse(bs$up == 1 | bs$down == 1, 1, 0)
#taken from national first stage. See daily note (01/23/23) for why these are only vars
bs$pred_pfas = 3.367947 + 4.800807 * bs$down + 0.003696 * bs$sp + 
  -0.005859 * bs$awc + 0.595943 * asinh(bs$pfas) + -0.870116 * log(bs$dist) + 
  -0.414740 * bs$updown + -0.002030 * bs$sp * bs$down + 0.001334 * bs$awc * bs$down + 
  -0.396678 * log(bs$dist) * bs$down

#for those with missing soil data, use modified regression for imputation
nind = which(is.na(bs$pred_pfas))
bs[nind, ]$pred_pfas = 4.876906 + 3.696133 * bs[nind, ]$down + 
  0.604114 * asinh(bs[nind, ]$pfas) + -0.932331 * log(bs[nind, ]$dist) + 
  -0.450470 * bs[nind, ]$updown + 
  -0.355169 * log(bs[nind, ]$dist) * bs[nind, ]$down


#getting impacts in states with initiatives
bs$add_vpre = bs$pred_pfas * bs$births * 0.0039
sum(bs$add_vpre) #930.7396
#standard error: 
bs$add_vpre_se = bs$pred_pfas * bs$births * 0.0016
sum(bs$add_vpre_se)#381.8419 births se
#cost se: 381.8419 * 204083 = 77927440
#very preterm cost: 930.7396 * 204083 = 189948130
bs$lower_vpre = bs$pred_pfas * bs$births *(0.0039 - 1.96 * 0.0016) # 182.3295 * 204083 = 81,292,009
bs$upper_vpre = bs$pred_pfas * bs$births *(0.0039 + 1.96 * 0.0016) # 1679.15 * 204083 = 748,652,722

bs$add_mpre = bs$pred_pfas * bs$births * 0.00053
sum(bs$add_mpre) #126.4851
#standard error: 
bs$add_mpre_se = bs$pred_pfas * bs$births * 0.00050
sum(bs$add_mpre_se)#119.3256 births se
#cost se: 119.3256 * 205041 = 24466640
#moderately preterm cost: 126.4851 * 205041 =  25934631
bs$lower_mpre = bs$pred_pfas * bs$births *(0.00053 - 1.96 * 0.00050) # -107.393 *241,769.18 =  -25964318
bs$upper_mpre = bs$pred_pfas * bs$births *(0.00053 + 1.96 * 0.00050) # 360.3633 * 241,769.18 = 87124740

bs$add_lpre = bs$pred_pfas * bs$births * 0.0062
sum(bs$add_lpre) #1479.637
#standard error: 
bs$add_lpre_se = bs$pred_pfas * bs$births *  0.006
sum(bs$add_lpre_se)#1431.907 births se
#cost se: 1431.907 * 36728.05 = 52591152
#late preterm cost: 1479.637 * 36728.05 = 54344182


#birthweight
bs$add_vlbw = bs$pred_pfas * bs$births * 0.0048
sum(bs$add_vlbw) #1145.526
#standard error: 
bs$add_vlbw_se = bs$pred_pfas * bs$births * 0.0023
sum(bs$add_vlbw_se)#548.8977 births se
#cost se: 548.8977 * 5133739.83 = 2,817,897,985
#very low birthweight cost: 1145.526 * 5133739.83 = 5880832453
bs$lower_vlbw = bs$pred_pfas * bs$births *(0.0048 - 1.96 * 0.0023) # 69.68615 *  5133739.83 = 357,750,564
bs$upper_vlbw = bs$pred_pfas * bs$births *(0.0048 + 1.96 * 0.0023) #  2221.365 * 5133739.83 = 11,403,909,977

bs$add_mlbw = bs$pred_pfas * bs$births * 0.00191
sum(bs$add_mlbw) #812.4429
#standard error: 
bs$add_mlbw_se = bs$pred_pfas * bs$births *0.00094
sum(bs$add_mlbw_se) #224.3321 births se
#cost se: 224.3321 * 1634411.22 = 366,650,901
#moderately low birthweight cost: 455.8238 * 1634411.22 = 745003533
bs$lower_mlbw = bs$pred_pfas * bs$births *(0.00191 - 1.96 * 0.00094) # 16.13282 *  1634411.22 = 26,367,662
bs$upper_mlbw = bs$pred_pfas * bs$births *(0.00191 + 1.96 * 0.00094) #  895.5147 * 1634411.22 = 1,463,639,273

bs$add_llbw = bs$pred_pfas * bs$births * 0.0042
sum(bs$add_llbw) #1786.524
bs$add_lbw = bs$pred_pfas * bs$births * 0.0109
sum(bs$add_lbw) #4636.454


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Very Preterm", "Mod. Preterm", "Late Preterm"), 2), 
                 levels = c("Very Preterm", "Mod. Preterm", "Late Preterm")),
  Value = c(931, 126, 1480, 0.19, 0.03, 0.05), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")), # Axis assignment
  se = c("(382)", "(119)", "(1432)", "(0.08)", "(0.02)", "(0.05)")
)

# Scaling factor for right axis values
#scale_factor = max(data$Value[data$Axis == "Left"]) / max(data$Value[data$Axis == "Right"])
scale_factor = 5000/12

data$Axis = factor(data$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data$Weeks = factor(data$Weeks, 
                    levels = c("Very Preterm", "Mod. Preterm", "Late Preterm"),
                    labels = c("Very Preterm", "Mod. Preterm", "Late Preterm"))


# Updated ggplot code
p_costs = ggplot(data, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern( # Use geom_bar_pattern for patterns
    stat="identity", 
    position=position_dodge(), 
    aes(y=ifelse(Axis=="↑ Births", Value, Value * scale_factor), alpha = 0.5, pattern = Axis),
    pattern_fill = "white", # Set the color of the pattern
    pattern_density = 0.1, # Adjust density of the pattern lines
    pattern_spacing = 0.02, # Adjust spacing of the pattern lines
    pattern_key_scale_factor = 0.9 # Adjust the scale of the pattern in the legend
  ) +
  scale_fill_manual(values=c("↑ Births" = "blue", "Cost" = "red")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor, name="Annual Cost ($ Billion)"), # Adjusting secondary axis
    limits = c(NA, 5000) # Set the upper limit to a higher value
  )  +
  theme_minimal() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 20, face = "bold"), 
        axis.text.y = element_text(size = 20, face = "bold"), 
        legend.text = element_text(size = 20, face = "bold"), 
        axis.text.x = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25)) +
  guides(alpha = "none", fill = "none", pattern = "none") +
  scale_pattern_manual(values = c("none", "stripe"))

p_costs = p_costs + geom_text(aes(label=round(Value, digits=2), 
                                  y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 350), # Adjust the offset as needed
                              position=position_dodge(width=0.9), 
                              vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                              size=7, 
                              fontface = "bold")
p_costs = p_costs + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor) + 100), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=5, 
                                fontface = "bold")
p_costs



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Very Low Birthweight", "Mod. Low Birthweight"), 2), 
                 levels = c("Very Low Birthweight", "Mod. Low Birthweight")),
  Value = c(1146, 456, 5.88, 0.75), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Right", "Right")), # Axis assignment
  se = c("(549)", "(224)", "(2.82)", "(0.37)")
)

# Scaling factor for right axis values
#scale_factor_bw = max(data_bw$Value[data_bw$Axis == "Left"]) / max(data_bw$Value[data_bw$Axis == "Right"])
scale_factor_bw = 5000/12

data_bw$Axis = factor(data_bw$Axis, levels = c("Left", "Right"), labels = c("↑ Births", "Cost"))
data_bw$Weeks = factor(data_bw$Weeks, 
                       levels = c("Very Low Birthweight", "Mod. Low Birthweight"),
                       labels = c("Very Low Birthweight", "Mod. Low Birthweight"))


# Updated ggplot code
lbw_cost = ggplot(data_bw, aes(x=Weeks, y=Value, fill=Axis)) +
  geom_bar_pattern( # Use geom_bar_pattern to apply patterns
    stat="identity", 
    position=position_dodge(),
    aes(y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw), alpha = 0.5, pattern = Axis),
    pattern_fill = "black", # Set color of the pattern
    pattern_density = 0.1, # Adjust density of pattern lines
    pattern_spacing = 0.02, # Adjust spacing of pattern lines
    pattern_key_scale_factor = 0.9 # Adjust scale of pattern in legend
  ) +
  scale_fill_manual(values=c("↑ Births" = "blue", "Cost" = "red")) +
  scale_y_continuous(
    "Annual Additional Births",
    sec.axis = sec_axis(~./scale_factor_bw, name="Annual Cost ($ Billion)"), # Adjusting secondary axis
    limits = c(NA, 5000) # Set the upper limit to a higher value
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 20, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 22, face = "bold"), 
        panel.grid.major = element_line(color = "grey60", size = 0.5),
        panel.grid.minor = element_line(color = "grey60", size = 0.25)) + 
  guides(alpha = "none") + # Adjust guide for patterns
  scale_pattern_manual(values = c("none", "stripe"))
lbw_cost = lbw_cost + geom_text(aes(label=round(Value, digits=2), 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 350), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=7, 
                                fontface = "bold")

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 100), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=5, 
                                fontface = "bold")

lbw_cost

p_costs = p_costs + guides(pattern = "none")
p_costs / lbw_cost


#extrapolated costs
cs = cont_sites %>%   
  dplyr::filter(state %in% c("Michigan", 
                             "Minnesota", 
                             "New Hampshire", 
                             "New York", 
                             "Colorado", 
                             "Maine", 
                             "Vermont", 
                             "California", 
                             "Florida", 
                             "North Dakota", 
                             "Wisconsin"))

csite_buff = cs %>% 
  st_transform(5070) %>% #get to albers projection for meters
  st_buffer(meters)


#bring in cbg pops
cbg_pop = fread("New Hampshire/Data/Supplemental/cbg_pop.csv")
states = tigris::states() %>% 
  as_tibble() %>% 
  dplyr::select(state_name = NAME, state = GEOID) %>% 
  dplyr::filter(state_name %in% c("Michigan", 
                                  "Minnesota", 
                                  "New Hampshire", 
                                  "New York", 
                                  "Colorado", 
                                  "Maine", 
                                  "Vermont", 
                                  "California", 
                                  "Florida", 
                                  "North Dakota", 
                                  "Wisconsin")) %>% 
  dplyr::mutate(state = as.numeric(state))

cbg_pop_sub = cbg_pop %>% 
  left_join(states) %>% 
  dplyr::filter(state_name %in% c("Michigan", 
                                  "Minnesota", 
                                  "New Hampshire", 
                                  "New York", 
                                  "Colorado", 
                                  "Maine", 
                                  "Vermont", 
                                  "California", 
                                  "Florida", 
                                  "North Dakota", 
                                  "Wisconsin"))

#ratio of us to 11 states population
sum(cbg_pop$pop)/sum(cbg_pop_sub$pop)





