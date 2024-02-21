#set working directory
if (file.exists('~/Documents/Projects/Current_Projects/PFAS Infant Health')){
  setwd('~/Documents/Projects/Current_Projects/PFAS Infant Health') 
}else{
  setwd('/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health')
}

#set environmental variables
run_cont_ws = FALSE
meters = 5000
reassn = FALSE
redo_soil = FALSE

#load in helper functions
source("NH/Code/Primary/env_functions.R")
source("NH/Code/Primary/Watersheds/watershed_functions.R")

#load necessary packages
load_library(sfheaders, lwgeom, dplyr, geosphere, sp, readxl, sf, raster, plyr, 
             pbapply, tigris, terra, readr, data.table, stringr, elevatr, gmodels, 
             rgdal, modelsummary, kableExtra, ggplot2, patchwork, pBrackets, whitebox, ggpattern)


if (reassn == TRUE){
  #load in births data
  if (file.exists("~/Documents/Projects/Current_Projects/PFAS Infant Health/Nat Data/births_cbg_cleaned_2010.csv")){
    births = read.csv("~/Documents/Projects/Current_Projects/PFAS Infant Health/Nat Data/births_cbg_cleaned_2010.csv")
  }else{
    births = read.csv("/Users/robert/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/Nat Data/births_cbg_cleaned_2010.csv")
  }
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
  load("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/New Hampshire/Data/RData/nat_cont_watershed.RData")
  cont_ws = wells_ws
  load("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/NH/New Hampshire/Data/RData/nat_cbg_watershed.RData")
  
  #merge watersheds
  #first do cont sites
  cont_ws = cont_ws %>% 
    left_join(cont_sites %>% as_tibble() %>% dplyr::select(site, state, pfas = sum_pfoa_pfos))
  
  #now do births
  births_ws = wells_ws %>%  #this is how the original object was named. It isnt actually wells
    left_join(births %>% as_tibble() %>% dplyr::select(geoid, births))
  
  
  source("Code/National Costs/well_assn.R") 
}else{
  births = fread("Nat Data/births_sites_assigned5.csv")
}
bll = fread("Nat Data/cbg_ll.csv")
bll = bll[which(bll$geoid %in% births$geoid), ]

#soil stuff
if (redo_soil == TRUE){
  source("NH/Code/National Costs/soil.R") 
}else{
  births = fread("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Projects/Current_Projects/PFAS Infant Health/Nat Data/nat_births_fcleaned5.csv")
}

cont_sites = read_xlsx('NH/New Hampshire/Data/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx', sheet = 2) %>% 
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
bs$upordown = as.numeric(bs$up == 1 | bs$down == 1)


#getting impacts in states with initiatives
add_vpre = sum(bs[which(bs$down == 1), ]$births) * 0.0068 #133.84
#standard error: 
add_vpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0022 #43.30
#very preterm cost: 133.84 * 445852.20 = 59672858
#cost se: 43.30 * 445852.20 = 19305400

add_mpre = sum(bs[which(bs$down == 1), ]$births) * -0.0053 #-104.3216
#standard error: 
add_mpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.0046 #90.53547
#very preterm cost: -104.3216 * 241769.18 = -25221748
#cost se: 90.53547 * 241769.18 = 21888686

add_lpre = sum(bs[which(bs$down == 1), ]$births) * 0.020 #393.6325
#standard error: 
add_lpre_se =sum(bs[which(bs$down == 1), ]$births) * 0.011 #216.50
#very preterm cost: 393.6325 * 36728.05 = 14457354
#cost se:216.50 * 36728.05 = 7951623




#birthweight
add_vlbw = sum(bs[which(bs$down == 1), ]$births) * 0.0076 #149.5803
#standard error: 
add_vlbw_se =sum(bs[which(bs$down == 1), ]$births) * 0.0028 #55.10855
#very preterm cost: 149.5803 * 5133739.83 = 767906344
#cost se: 55.10855 * 5133739.83 = 282912958/10^9

add_mlbw = sum(bs[which(bs$down == 1), ]$births) * 0.00052 #10.23444
#standard error: 
add_mlbw_se =sum(bs[which(bs$down == 1), ]$births) * 0.00373 #73.41245
#very preterm cost: 10.23444 * 1634411.22 = 16727284
#cost se: 73.41245 * 1634411.22 = 119986132/10^9


#social cost figure
data = data.frame(
  Weeks = factor(rep(c("Very Preterm", "Mod. Preterm", "Late Preterm"), 2), 
                 levels = c("Very Preterm", "Mod. Preterm", "Late Preterm")),
  Value = c(134, -104, 394, 0.06, -0.03, 0.01), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Left", "Right", "Right", "Right")), # Axis assignment
  se = c("(43)", "(91)", "(217)", "(0.02)", "(0.02)", "(0.008)")
)

# Scaling factor for right axis values
#scale_factor = max(data$Value[data$Axis == "Left"]) / max(data$Value[data$Axis == "Right"])
scale_factor = 500/1

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
    limits = c(NA, 500) # Set the upper limit to a higher value
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
                                  y= ifelse(Axis=="↑ Births", ifelse(Weeks == "Mod. Preterm", Value + 100, Value), 
                                            ifelse(Weeks == "Mod. Preterm", (Value + 0.02) * scale_factor, Value * scale_factor)) + 35), # Adjust the offset as needed
                              position=position_dodge(width=0.9), 
                              vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                              size=7, 
                              fontface = "bold")
p_costs = p_costs + geom_text(aes(label=se, 
                                  y= ifelse(Axis=="↑ Births", ifelse(Weeks == "Mod. Preterm", Value + 100, Value), 
                                            ifelse(Weeks == "Mod. Preterm", (Value + 0.02) * scale_factor, Value * scale_factor)) + 10), # Adjust the offset as needed
                              position=position_dodge(width=0.9), 
                              vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                              size=5, 
                              fontface = "bold")
p_costs



#Birthweight
data_bw = data.frame(
  Weeks = factor(rep(c("Very Low Birthweight", "Mod. Low Birthweight"), 2), 
                 levels = c("Very Low Birthweight", "Mod. Low Birthweight")),
  Value = c(150, 10, 0.77, 0.02), # Combined values for both axes
  Axis = factor(c("Left", "Left", "Right", "Right")), # Axis assignment
  se = c("(55)", "(73)", "(0.28)", "(0.12)")
)

# Scaling factor for right axis values
#scale_factor_bw = max(data_bw$Value[data_bw$Axis == "Left"]) / max(data_bw$Value[data_bw$Axis == "Right"])
scale_factor_bw = 500/1

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
    limits = c(NA, 500) # Set the upper limit to a higher value
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
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 25), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=7, 
                                fontface = "bold")

lbw_cost = lbw_cost + geom_text(aes(label=se, 
                                    y=ifelse(Axis=="↑ Births", Value, Value * scale_factor_bw) + 5), # Adjust the offset as needed
                                position=position_dodge(width=0.9), 
                                vjust=0, # Vertically justifies text to the bottom, making it appear above the bar
                                size=5, 
                                fontface = "bold")

lbw_cost

p_costs = p_costs + guides(pattern = "none")
p_costs / lbw_cost
