#well location and service area data (NHDES)
source("PFAS-Code/Pub/Data/NHDES_PWS.R")

#note, this comes from running 'PFAS-Code/Pub/Data/natality_data.R' and "PFAS-Code/Pub/Data/birth_covars.R"
load(paste0(natality_path, "[UA Box Health] birth_records_wdem_prematch.RData"))

#assign service districts to residences
df = df %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F) %>%
  st_transform(32110) %>%
  st_join(sa %>% 
            st_transform(32110), join = st_within, largest = T)

df = df %>% 
  dplyr::mutate(sys_id = as.character(sys_id))


df[which(is.na(df$sys_id)), ]$sys_id = 'Domestic Well' #Assume those living outside the piped areas are on domestic wells

#make list of the dataframes by sys_id
serviced_list = df %>% 
  dplyr::group_by(sys_id) %>% 
  dplyr::group_split()


#algorithm for assigning well service (assume that for those in a service area, the nearest well is the one that services them)
well_assigner = function(i, l){
  
  #get all birth records corresponding to the ith group (system)
  sl = l[[i]]
  #transform to lat long data
  sl = sl %>% 
    st_transform(4326)
  
  #if the assigned system is not in sys_skip (created in NHDES_PWS.R, no gw well systems) and not domestic well
  if (!(sl$sys_id[1] %in% sys_skip) & sl$sys_id[1] != 'Domestic Well'){
    
    #grab lat long for each birth record
    sl_coord = sl %>% 
      st_coordinates()
    
    #subset wells to only those in the right system
    w = wells %>% 
      dplyr::filter(as.character(sys_id) == sl$sys_id[1]) %>%
      as_tibble() %>%
      dplyr::select(lng, lat, source)
    
    #get distance from each residence to the wells in the assigned system
    distance = distm(sl_coord, w[, c("lng", "lat")])
    
    w$index = 1:nrow(w)
    #for each row in distance, obtain the column index with the minimum valye
    #assign this index to sl (so we can match the right well)
    sl$index = as.vector(apply(distance, 1, FUN = which.min))
    
    #join sl with w by index to get the nearest well in the right service area
    sl = sl %>% 
      left_join(w %>% dplyr::rename(well_lat = lat, well_lng = lng), by = 'index') %>% 
      dplyr::select(-c(index))
  }else if (sl$sys_id[1] == 'Domestic Well'){
    #if sl is outside a service area, then assign its well as its residence
    sl_coord = sl %>% 
      st_coordinates()
    sl$well_lat = sl_coord[, 2]
    sl$well_lng = sl_coord[, 1]
  }else{
    sl$well_lat = NA
    sl$well_lng = NA
  }
  
  return(sl)
}

#apply the above well assigning function to unique service area by birth record list, bind rows to get df back
df = dplyr::bind_rows(pblapply(1:length(serviced_list), well_assigner, serviced_list))

df$gw = ifelse(!is.na(df$well_lat) & df$sys_id != "Domestic Well", 1, 0)
df$sw = ifelse(is.na(df$well_lat) & df$sys_id != "Domestic Well", 1, 0)
df$dw = ifelse(df$sys_id == "Domestic Well", 1, 0)




df$college = ifelse(df$max_educ >= 6, 1, 0)
df[which(df$n_prenatal == 99), ]$n_prenatal = NA
df$ind_prenatal = ifelse(df$n_prenatal >= 15 & !is.na(df$n_prenatal), 1, 0)
df$old = ifelse(df$m_age > 40, 1, 0)
df$young = ifelse(df$m_age < 20, 1, 0)
df$no_hs = ifelse(df$max_educ < 3, 1, 0)
df$med_hprice = df$med_hprice/10^4
df$med_inc = df$med_inc/10^3
df$group = ifelse(df$gw == 1, "GW", ifelse(df$sw == 1, "SW", "DW"))
df2 = df %>% 
  dplyr::filter(!is.na(gestation) & 
                  !is.na(bweight) &
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


#merge in death records for infant mortality
path = paste0(natality_path, "[UA Box Health] VR2210_Deliverable/dr_6264_deliverable.xlsx")
df_d = read_excel(path, sheet = 3)

df2 = df2 %>% 
  left_join(df_d %>% 
              dplyr::rename(age_death = DECD_AGE_YR, 
                            manner_death = CERTFR_MANNER_DTH_CD, 
                            id = BRTH_CERT_FILE_NBR))

df2$death = as.numeric(!is.na(df2$age_death))

df2$preterm = as.numeric(df2$gestation < 37)
df2$mpreterm = as.numeric(df2$gestation >= 32 & df2$gestation < 37)
df2$vpreterm = as.numeric(df2$gestation < 32 & df2$gestation >= 28)
df2$epreterm = as.numeric(df2$gestation < 28)

df2$lbw = as.numeric(df2$bweight < 2500)
df2$mlbw = as.numeric(df2$bweight >= 1500 & df2$bweight < 2500)
df2$vlbw = as.numeric(df2$bweight < 1500 & df2$bweight >= 1000)
df2$elbw = as.numeric(df2$bweight < 1000)

df2 = df2 %>% 
  dplyr::select(`Maternal Age` = m_age, 
                `College` = college,
                `Less than High School` = no_hs,
                `Maternal Marital Status` = m_married, 
                `Maternal Tobacco Use` = cig, 
                White = white,
                `Median Housing Price` = med_hprice,
                `Median Income` = med_inc,
                group,
                `WIC` = wic, 
                `Private Insurance` = private_insurance, 
                `Months in Residence` = m_months_res, 
                `Younger than 20` = young, 
                `Older than 40` = old,
                `Prenatal Care Visits` = n_prenatal,
                `Pre-Pregancy Diabetes` = mr_04, 
                `Gestational Diabetes` = mr_18, 
                Hypertension = mr_08, 
                `Gestational Hypertension` = mr_23, 
                Eclampsia = mr_10, 
                `Preterm` = preterm, 
                `Moderately Preterm` = mpreterm,
                `Very Preterm` = vpreterm,
                `Extremely Preterm` = epreterm,
                `Low Birthweight` = lbw,
                `Moderately Low Birthweight` = mlbw,
                `Very Low Birthweight` = vlbw,
                `Extremely Low Birthweight` = elbw,
                `Infant Mortality` = death
                
  )
df2 = as.data.frame(df2)


datasummary_balance(~group, 
                    data = df2, 
                    na.rm = T, 
                    fmt = modelsummary::fmt_significant(2, scientific = F, 
                                                        zero.print = T, drop0trailing = F, 
                                                        nsmall = 2), 
                    output = modify_path2("Tables/Revisions/balance_gw_sw_dw.tex")) 
