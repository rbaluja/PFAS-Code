cont = fread("Data/Contamination/NH_tests.csv") 

#translate all results to ppt
cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "UG/KG" | cont$DETLIMU == "UG/KG DRY", ]$DETLIM

cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM

cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "UG/L" | cont$DETLIMU == "UG/L DRY", ]$DETLIM

cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1 = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM = 1000 * cont[cont$DETLIMU == "NG/G" | cont$DETLIMU == "NG/G DRY", ]$DETLIM

cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$NUMRESULT1 = 1000000 * cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$DETLIM = 1000000 * cont[cont$DETLIMU == "MG/KG" | cont$DETLIMU == "MG/KG DRY", ]$DETLIM

cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$NUMRESULT1 = 1000000 * cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$NUMRESULT1
cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$DETLIM = 1000000 * cont[cont$DETLIMU == "MG/L" | cont$DETLIMU == "MG/L DRY", ]$DETLIM

#put any tests with a result that was lower than the detection limit to zero. Based on my reading, this detection limit it that which has α = 0.01
#where α is in relation to a test for a value strictly positive. 
cont[cont$QUALIFIER == "<", ]$NUMRESULT1 = cont[cont$QUALIFIER == "<", ]$DETLIM/sqrt(2) #NEED TO FIX
cont[is.na(cont$NUMRESULT1), ]$NUMRESULT1 = 0


cont = cont %>% 
  rename_all(tolower) %>% 
  tidyr::drop_na(numresult1) %>%
  dplyr::group_by(activityid, wshedparmname, startdate, stationid1, latdecdeg, longdecdeg) %>%
  dplyr::mutate(numresult1 = max(numresult1, na.rm = T)) %>% #if more than one test at the same time, take max
  ungroup() %>%
  dplyr::distinct(activityid, wshedparmname, startdate, stationid1, latdecdeg, longdecdeg, numresult1, .keep_all = TRUE) %>% 
  tidyr::pivot_wider(id_cols = c(activityid, startdate, town, stationid1, latdecdeg, longdecdeg, watervapusage), 
                     names_from = wshedparmname, 
                     values_from = numresult1) #pivots dataset to create frame containing each test for all chemicals

#turn NA to 0 (they didnt test for that chemical)
cont[, 8:ncol(cont)][is.na(cont[, 8:ncol(cont)])] = 0


cont = cont %>%
  dplyr::rename(genx = `2,3,3,3-TETRAFLUORO-2-(HEPTAFLUOROPROPOXY)PROPANOIC ACID - HFPO-DA - GENXACID`, 
                pfna = `PERFLUORONONANOIC ACID - PFNA`, 
                pfhxs = `PERFLUOROHEXANE SULFONIC ACID - PFHXS`, 
                pfbs = `PERFLUOROBUTANE SULFONIC ACID - PFBS`, 
                pfoa = `PERFLUOROOCTANOIC ACID - PFOA`,
                pfos = `PERFLUOROOCTANE SULFONIC ACID - PFOS`, 
                lat = latdecdeg, 
                lng = longdecdeg)

#set date
cont$date = as.Date(sapply(strsplit(cont$startdate, " "), "[", 1), format = "%m/%d/%Y")

#take earliest test result for each well
cont = cont %>% 
  dplyr::group_by(stationid1) %>% 
  dplyr::filter(date == min(date))

#some still have multiples, so subset to the needed columns and take means over those
cont = cont %>% 
  dplyr::select(stationid1, lng, lat, watervapusage, pfoa, pfos, pfna, pfbs, genx, pfhxs) %>% 
  dplyr::mutate(pfoa = mean(pfoa), 
                pfos = mean(pfos), 
                pfna = mean(pfna), 
                pfbs = mean(pfbs), 
                genx = mean(genx), 
                pfhxs= mean(pfhxs)) %>% 
  unique()

#break into public and domestic and merge with dataframe
cont_pub = cont %>% 
  dplyr::filter(watervapusage == "PUBLIC WATER SUPPLY")
#extract well and source id
cont_pub$sys_id = ""
cont_pub$well_id = ""

cont_pub[nchar(cont_pub$stationid1) == 10, ]$sys_id = 
  str_sub(cont_pub[nchar(cont_pub$stationid1) == 10, ]$stationid1, 1, 7)

cont_pub[nchar(cont_pub$stationid1) == 10, ]$well_id = 
  str_sub(cont_pub[nchar(cont_pub$stationid1) == 10, ]$stationid1, 8, 10)

cont_pub[which(cont_pub$sys_id == ""), ]$sys_id = 
  sapply(strsplit(cont_pub[which(cont_pub$sys_id == ""), ]$stationid1, "_"), "[", 1)
cont_pub[which(cont_pub$well_id == ""), ]$well_id = 
  sapply(strsplit(cont_pub[which(cont_pub$well_id == ""), ]$stationid1, "_"), "[", 2)

#fix a few stragglers
cont_pub[which(cont_pub$sys_id == "1618400-001"), ]$well_id = "001"
cont_pub[which(cont_pub$sys_id == "1618400-001"), ]$sys_id = "1618400"



df_pub = df %>% 
  dplyr::filter(sys_id != "Domestic Well") %>% 
  left_join(cont_pub, by = c("sys_id", "source" = "well_id"))


#do the same for domestic wells
cont_dom = cont %>% 
  dplyr::filter(watervapusage == "DOMESTIC") 


df_dom = df%>% 
  dplyr::filter(sys_id == "Domestic Well")

dom_dist = distm(df_dom %>% 
                   as_tibble() %>% 
                   dplyr::select(well_lng, well_lat), 
                 cont_dom %>% 
                   ungroup() %>%
                   tidyr::drop_na(lat) %>%
                   dplyr::select(lng, lat))
dom_dist = t(dom_dist)

ddom  = dplyr::bind_rows(pblapply(1:nrow(df_dom), epa_dist))


#bind these back together
df = rbind.fill(df_pub %>% st_transform(st_crs(ddom)), ddom)

df$pfoa_v = ifelse(df$pfoa >= 4, 1, 0)
df$pfos_v = ifelse(df$pfos >= 4, 1, 0)
df$hazard_v = ifelse(df$genx/10 + df$pfhxs/9 + df$pfna/10 + df$pfbs/2000 > 1, 1, 0)

  
fixest::feols(gestation ~ hazard_v + pfoa_v + pfos_v + I(sys_id == "Domestic Well"), data = df)



#set up data with indicators for above/over
cont = cont %>% 
  dplyr::mutate(GenX = ifelse(genx >= 10, 1, 0), 
                PFHxS = ifelse(pfhxs >= 9, 1, 0), 
                PFNA = ifelse(pfna >= 10, 1, 0), 
                PFBS = ifelse(pfbs >= 2000, 1, 0), 
                PFOA = ifelse(pfoa >= 4, 1, 0), 
                PFOS = ifelse(pfos >= 4, 1, 0))

library(corrplot)

corrplot(cor(contind), order = "hclust", 
         tl.col = "black", tl.srt = 45, type = "upper", method = "ellipse")



#pull out hazard index violations 
df = cont[cont$genx/10 +  cont$pfhxs/9 + cont$pfbs/2000 + cont$pfna/10 >= 1, ]

length(unique(df$stationid1))
df$genx_ind = ifelse(df$genx >= 10, 1, 0)
df$pfhxs_ind = ifelse(df$pfhxs >= 9, 1, 0)
df$pfbs_ind = ifelse(df$pfbs >= 2000, 1, 0)
df$pfna_ind = ifelse(df$pfna >= 10, 1, 0)

plyr::count(df[, 79:82])
