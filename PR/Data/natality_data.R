#read in birth data
path = paste0(natality_path, "[UA Box Health] VR2210_Deliverable/dr_6264_deliverable.xlsx")
df = read_excel(path, sheet = 2)
rm(path)

#drop observations with missing key variables (lose 1538 observations)
df = df %>% 
  dplyr::filter(CHLD_BRTH_WGT != 9999 & CLNC_EST_GEST_AGE != 99) %>% 
  dplyr::filter(!is.na(CHLD_BRTH_WGT) & !is.na(CLNC_EST_GEST_AGE)) %>%
  dplyr::filter(APGAR_FIVE_MIN <= 10 & !is.na(APGAR_FIVE_MIN))

#drop individuals with missing lat long (lose 5320 observations)
df = df %>%
  dplyr::filter(!is.na(POINT_X) & POINT_X != 0) %>% 
  dplyr::rename(lng = POINT_X, lat = POINT_Y)

#clean up a few choice variables
df = df %>% 
  dplyr::mutate(m_married = ifelse(MTHR_MARTL_STATUS == "Y", 1, 0)) %>% #create married dummy var
  dplyr::mutate(white = ifelse(BIRTH_RACE_DSC_1 == "White", 1, 0)) %>% #race indicator for white
  dplyr::mutate(private_insurance = ifelse(PAYMT_MTHD_DLVRY == 5, 1, 0)) %>% #indicator for private insurance for method of payment
  dplyr::mutate(bweight = ifelse(BRTH_WGT_UNIT == 1, as.numeric(CHLD_BRTH_WGT), 
                                 (16 * as.numeric(CHLD_BRTH_WGT) + as.numeric(CHLD_BRTH_WGT_OZ)) * 28.3495)) %>% #convert pounds/ounces to grams
  dplyr::mutate(single_birth = ifelse(BRTH_PLRLTY == 1, 1, 0)) %>% #add indicator for one or more than one birth at event
  dplyr::mutate(m_transport_before = ifelse(MTHR_TRANS_PRIOR_BRTH == "Y", 1, 0)) %>% #pre-birth transport
  dplyr::mutate(m_transport_after = ifelse(MTHR_TRANS_AFTER_BRTH == "Y", 1, 0)) %>% #post-birth transport
  dplyr::mutate(c_transport = ifelse(CHLD_TRANS == "Y", 1, 0)) %>% 
  dplyr::mutate(stillbrn = ifelse(CHLD_DEAD_LIVE == 2, 1, 0)) %>% 
  dplyr::mutate(cig = ifelse(TBCCO_USE == "Y", 1, 0)) %>%
  dplyr::mutate(m_months_res = ifelse(!is.na(MTHR_RES_CUR_YRS), 
                                      MTHR_RES_CUR_YRS * 12 + ifelse(!is.na(MTHR_RES_CUR_MTH), MTHR_RES_CUR_MTH, 0), 
                                      ifelse(!is.na(MTHR_RES_CUR_MTH), MTHR_RES_CUR_MTH, NA))) %>%
  dplyr::mutate(wic = ifelse(MTHR_WIC_FOOD == "Y", 1, 0)) %>%
  dplyr::rename(id = ST_FILE_NBR, 
         year = CHLD_BRTH_YR, 
         month = CHLD_BRTH_MONTH, 
         zipcode = MTHR_RES_ZIPCD, 
         m_age = MTHR_AGE, 
         m_educ = MTHR_EDUC, 
         f_educ = FTHR_EDUC, 
         gestation = CLNC_EST_GEST_AGE, 
         n_prenatal = NBR_PRNTL_VISITS, 
         apgar5 = APGAR_FIVE_MIN) 

#maximum education
df$max_educ = ifelse((!is.na(df$m_educ) & !is.na(df$f_educ) & df$m_educ >= df$f_educ) | 
                       (is.na(df$f_educ) & !is.na(df$m_educ)), df$m_educ,
                     ifelse((!is.na(df$m_educ) & !is.na(df$f_educ) & df$m_educ < df$f_educ) | 
                              (!is.na(df$f_educ) & is.na(df$m_educ)), df$f_educ, NA))

#make all column names lowercase
df = df %>% 
  dplyr::rename_all(tolower)

#Fill NAs in natality data
df[which(df$mthr_wgt_dlv == 999), ]$mthr_wgt_dlv = NA
df[which(df$mthr_pre_preg_wgt == 999), ]$mthr_pre_preg_wgt = NA
df[which(df$n_prenatal == 99), ]$n_prenatal = NA

#get height in inches
df$m_height = 12*df$mthr_hght_ft + df$mthr_hght_in

df$lbweight = ifelse(df$bweight < 2500, 1, 0)
df$preterm = ifelse(df$gestation <= 36, 1, 0)

#filling in conditions
df = df %>%
  tibble::rowid_to_column("row") %>% #keep track of each original observation in the data
  dplyr::mutate(x_split = str_extract_all(med_risks, "..")) %>% #create x_split variable: this is a list containing each pair of sequential characters in med_risks
  tidyr::unnest(x_split) %>% #this creates a separate row for each element of x_split (which is why we kept track of the original row number)
  dplyr::distinct(row, x_split, .keep_all = TRUE) %>% #remove doubles in med_risks
  dplyr::mutate(value = 1) %>%
  tidyr::spread(key = x_split, value = value, fill = 0) %>% #get names from x_split and values from value. In this case, value = 1 when they have that med risk, and we fill it with 0 if they dont
  rename_with(~ ifelse(str_detect(., "^[0-9]"), paste0("mr_", .), .)) %>% #rename resulting columns
  dplyr::select(-row)

#turn df spatial
df = df %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326, remove = FALSE)

#get cbg of residence
source("PFAS-Code/PR/Data/cbg_births.R")
