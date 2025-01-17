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

#put any tests with a result that was lower than the detection limit to the detection limit/sqrt(2). Based on my reading, this detection limit it that which has α = 0.01
#where α is in relation to a test for a value strictly positive. 
cont[cont$QUALIFIER == "<", ]$NUMRESULT1 = cont[cont$QUALIFIER == "<", ]$DETLIM/sqrt(2) #NEED TO FIX
cont[is.na(cont$NUMRESULT1), ]$NUMRESULT1 = 0


cont = cont %>% 
  rename_all(tolower) %>% 
  dplyr::group_by(activityid, wshedparmname, wellusage, startdate, stationid1, latdecdeg, longdecdeg) %>%
  dplyr::filter(wellusage == "WITHDRAWAL/RETURN" | wellusage == "RESIDENTIAL/PRIVATE") %>%
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
cont$year = lubridate::year(cont$date)

#extract well and source id
cont$source_id = sapply(strsplit(cont$stationid1, "_"), "[", 1)
cont$well_id = sapply(strsplit(cont$stationid1, "_"), "[", 2)

#only work with public water for now, and subset data to columns that matter
c= cont %>% 
  dplyr::filter(watervapusage == "PUBLIC WATER SUPPLY") %>% 
  dplyr::select(c(source_id, well_id, genx, pfna, pfhxs, pfbs, pfoa, pfos))

#filling in odd coding

c$well_id = ifelse(nchar(c$source_id) == 10 & is.na(c$well_id), substr(c$source_id, 8, 10), c$well_id)
c$source_id = ifelse(nchar(c$source_id) == 10, substr(c$source_id, 1, 7), c$source_id)

c$well_id = ifelse(endsWith(c$source_id, "PWDPH1") & is.na(c$well_id), "001", c$well_id)
c$source_id = ifelse(endsWith(c$source_id, "PWDPH1"), substr(c$source_id, 1, 7), c$source_id)

c[c$source_id == "1618400-001", "well_id"] = "001"
c[c$source_id == "1618400-001", "source_id"] = "1618400"
c[c$source_id == "201709003DW06", "well_id"] = "003"
c[c$source_id == "201709003DW06", "source_id"] = "2017090"
c[c$source_id == "199501031DW1", "well_id"] = "031"
c[c$source_id == "199501031DW1", "source_id"] = "1995010"

#take max for each well
c = c %>% 
  dplyr::group_by(source_id, well_id) %>% 
  summarize_at(vars(genx:pfos), max)




ct = fread("Data/Contamination/ocur_tables.csv") #obtained at https://www.des.nh.gov/resource-center/publications?keys=hb1766&purpose=Reports&subcategory=PFAS
ct[ct == "ND"] = 0 #not above lab limit. Need to figure that out
ct$sys_id = sapply(strsplit(ct$source, "-"), "[", 1)
ct$well_id = sapply(strsplit(ct$source, "-"), "[", 2)
ct = ct %>% 
  left_join(wells)
c = ct %>% 
  left_join()

c = c %>% 
  plyr::rbind.fill(ct %>% dplyr::select(source_id, well_id, pfoa, pfos, pfna, pfhxs)) %>% 
  dplyr::group_by(source_id, well_id) %>% 
  summarize_at(vars(genx:pfos), max, na.rm = T)
c[c == -Inf] = 0


#merge this with df
df = df %>% 
  left_join(c, by = c("sys_id" = "source_id", "source" = "well_id"))

#replace NA with 0 across appropriate columns
df = df %>% 
  dplyr::mutate_at(vars(genx:pfos), as.numeric) %>%
  dplyr::mutate_at(vars(t4_n_sites:pfos), function(x) ifelse(is.na(x), 0, x))

reg_cont_pub = fixest::feols(preterm ~ year + age + married + genx + pfna + pfhxs + pfbs + pfoa + pfos, data = df)
#can also include indicators here for an RD around the thresholds in the EPA's new rules



###################
###Domestic Well###
###################
#This strategy will compare houses who tested their domestic well for contamination and had a positive result to those who had a nondetect
#If there is a site "close enough" to a domestic well that was contaminated, then I will consider the domestic well to have the same level of contamination
dom_well = df %>% 
  dplyr::filter(sys_id == "Domestic Well") %>% 
  dplyr::select(!c(genx, pfna, pfhxs, pfbs, pfoa, pfos)) %>% st_transform(3437)
dom_cont = cont %>% 
  dplyr::filter(watervapusage == "DOMESTIC") %>% 
  tidyr::drop_na(lng) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE) %>% 
  st_transform(3857) %>% #put it into meters for the buffer (500 meters for now)
  st_buffer(500) %>% 
  st_transform(st_crs(dom_well))

dom_assigner = function(i){
  loc_cont_inter = st_intersection(dom_well[i, ], dom_cont %>% 
                                     dplyr::select(c(lat, lng, genx, pfna, pfhxs, pfbs, pfoa, pfos, date))) 
  if (nrow(loc_cont_inter) > 0){
    l = loc_cont_inter %>% as_tibble() %>% dplyr::select(!geometry)
    d = geosphere::distm(l[1, c("well_lng", "well_lat")], l[ , c("lng", "lat")])
    m = which(d == min(d))
    l = loc_cont_inter[m, ]
    if (length(m) == 1){
      return(l)
    }else if (length(m) > 1){
      md = which((as.Date(paste0(l$month[1], "/01/", l$year[1]), format = "%m/%d/%Y") - l$date) == min(as.Date(paste0(l$month[1], "/01/", l$year[1]), format = "%m/%d/%Y") - l$date))
      l = loc_cont_inter[md, ]
      return(l %>% dplyr::select(!date))
    }
  }else{
    return(dom_well[i, ] %>% dplyr::mutate(genx = NA, pfna = NA, pfhxs = NA, pfbs = NA, pfoa = NA, pfos = NA))
  }
}

#this is acting weird for some reason, just run a naive loop
#dom_a = pblapply(1:nrow(dom_well), dom_assigner, cl = 4)
dom_a = vector(mode = "list", length = nrow(dom_well))
for (i in 1:nrow(dom_well)){
  dom_a[[i]] = dom_assigner(i)
  if ( i %% 5000 == 0){
    print(i)
  }
}  
dom_a = dplyr::bind_rows(dom_a)
