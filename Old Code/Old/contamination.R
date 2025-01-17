library(sfheaders)
library(lwgeom)
library(dplyr)
library(geosphere)
library(sp)
library(readxl)
library(sf)
library(raster)
library(plyr)
library(pbapply)
library(tigris)
library(terra)
library(data.table)

setwd('/Users/robert/Dropbox/PFAS Infants/New Hampshire/')

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
cont[cont$QUALIFIER == "<", ]$NUMRESULT1 = 0 #need to fix this


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

#summary stats

#GENX

#get number of times chemical is above threshold on its own
length(which(cont$genx >= 10))
a = cont[cont$genx >= 10, ]
summary(a$date)
b = a %>% distinct(stationid1, .keep_all = TRUE) #number of unique wells
length(which(b$watervapusage != "DOMESTIC"))#how many of these are public wells

b = a[which(a$pfhxs < 9 & a$pfna < 10 & a$pfbs < 2000 ), ] #subset to only those tests where this chemical was only from HI that binds
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))

b = a[which(a$pfhxs < 9 & a$pfna < 10 & a$pfbs < 2000 & a$pfoa < 4 & a$pfos < 4), ] #subset to only binding regulation
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))


#PFHXS

#get number of times chemical is above threshold on its own
length(which(cont$pfhxs >= 9))
a = cont[cont$pfhxs >= 9, ]
summary(a$date)
b = a %>% distinct(stationid1, .keep_all = TRUE)#number of unique wells
length(which(b$watervapusage != "DOMESTIC"))#how many of these are public wells

b = a[which(a$genx < 10 & a$pfna < 10 & a$pfbs < 2000 ), ]#subset to only those tests where this chemical was only from HI that binds
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))

b = a[which(a$genx < 10 & a$pfna < 10 & a$pfbs < 2000 & a$pfoa < 4 & a$pfos < 4), ]#subset to only binding regulation
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))



#PFNA


length(which(cont$pfna >= 10))
a = cont[cont$pfna >= 10, ]
summary(a$date)
b = a %>% distinct(stationid1, .keep_all = TRUE)#number of unique wells
length(which(b$watervapusage != "DOMESTIC"))#how many of these are public wells

b = a[which(a$genx < 10 & a$pfhxs < 9 & a$pfbs < 2000 ), ]#subset to only those tests where this chemical was only from HI that binds
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))

b = a[which(a$genx < 10 & a$pfhxs < 9 & a$pfbs < 2000 & a$pfoa < 4 & a$pfos < 4), ]#subset to only binding regulation
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))



#PFBS


length(which(cont$pfbs >= 2000))
a = cont[cont$pfbs >= 2000, ]
summary(a$date)
b = a %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))


b = a[which(a$genx < 10 & a$pfna < 10 & a$pfhxs < 9 ), ]
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))


b = a[which(a$genx < 10 & a$pfna < 10 & a$pfhxs < 9 & a$pfoa < 4 & a$pfos < 4), ]
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))




#PFOA

length(which(cont$pfoa >= 4))
a = cont[cont$pfoa >= 4, ]
summary(a$date)
b = a %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))

b = a[which(a$genx < 10 & a$pfhxs < 9 & a$pfbs < 2000 & a$pfna < 10 & a$pfos < 4), ]#subset to only binding regulation
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))




length(which(cont$pfos >= 4))
a = cont[cont$pfos >= 4, ]
summary(a$date)
b = a %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))

b = a[which(a$genx < 10 & a$pfhxs < 9 & a$pfbs < 2000 & a$pfna < 10 & a$pfoa < 4), ]#subset to only binding regulation
b = b %>% distinct(stationid1, .keep_all = TRUE)
length(which(b$watervapusage != "DOMESTIC"))


#set up data with indicators for above/over
contind = cont %>% 
  dplyr::mutate(GenX = ifelse(genx >= 10, 1, 0), 
                PFHxS = ifelse(pfhxs >= 9, 1, 0), 
                PFNA = ifelse(pfna >= 10, 1, 0), 
                PFBS = ifelse(pfbs >= 2000, 1, 0), 
                PFOA = ifelse(pfoa >= 4, 1, 0), 
                PFOS = ifelse(pfos >= 4, 1, 0)) %>% 
  dplyr::select(GenX, PFHxS, PFNA, PFBS, PFOA, PFOS) 

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
