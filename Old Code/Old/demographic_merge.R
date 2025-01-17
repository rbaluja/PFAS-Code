#first read in and aggregate weather data
w = fread("Data/Supplemental/nh_cbg_weather.csv") %>% 
  dplyr::rename(geoid = location)
w$year = as.numeric(str_sub(w$date, 1, 4))

w = w %>% 
  group_by(geoid, year) %>% 
  dplyr::mutate(temp = (tmin + tmax)/2) %>% 
  dplyr::summarize(temp = mean(temp, na.rm = T))


#read in and bind pollution data
pm = fread("Data/Supplemental/nh_cbg_pm25.csv")

env = left_join(w, pm)

#read in census vars (at tract level)
dem = fread("Data/Supplemental/tract_stats.csv")
dem = dem %>% 
  dplyr::mutate(tract = as.character(tract)) %>%
  dplyr::mutate(county = dplyr::case_when(
    county < 10 ~ paste0("00", county), 
    county >= 10 ~ paste0("0", county)), 
    tract = dplyr::case_when(
      nchar(as.character(tract)) == 3 ~ paste0("000", tract), 
      nchar(as.character(tract)) == 4 ~ paste0("00", tract), 
      nchar(as.character(tract)) == 5 ~ paste0("0", tract), 
      nchar(as.character(tract)) == 6 ~ tract
    )) %>% 
  dplyr::mutate(geoid = paste0("33", county, tract))

#join environmental and demographic data
env$t_geoid = str_sub(env$geoid, 1, 11)

d = env %>% 
  left_join(dem, by = c("t_geoid" = "geoid"))

#####
##assign block group by lat long

#read in blocks shapefile
load("/Users/nhdata/Library/CloudStorage/Box-Box/NH Supplemental Data/cbg_tigris.RData")

#assign block of residence
df = df %>% 
  st_transform(st_crs(cbg_shape)) %>%
  st_join(cbg_shape, join = st_within, largest = T)

df = df %>% 
  dplyr::rename(geoid = GEOID) %>%
  left_join(d %>% dplyr::mutate(year = as.character(year), 
                                geoid = as.character(geoid)))

df$county = paste0("33", df$COUNTYFP)

#read in county gdp
rgdp = fread("Data/Supplemental/CAGDP9/CAGDP9_NH_2001_2021.csv") %>% 
  dplyr::filter(LineCode == 90 & GeoFIPS != 33000)

rgdp = rgdp %>% 
  tidyr::pivot_longer(cols = dplyr::starts_with("2"), 
               names_to = "year", 
               values_to = "man_rgdp")
rgdp$man_rgdp = ifelse(rgdp$man_rgdp == "(D)", NA, rgdp$man_rgdp)
#need to extrapolate to fill in missing data (withheld to protect confidentiality)
rgdp = rgdp %>%
  dplyr::mutate(man_rgdp = as.numeric(man_rgdp)) %>%
  dplyr::group_by(GeoFIPS) %>% 
  dplyr::mutate(man_rgdp = dplyr::case_when(
    is.na(man_rgdp) ~ imputeTS::na_interpolation(man_rgdp), 
    !is.na(man_rgdp) ~ man_rgdp
  ))

df = df %>% 
  dplyr::mutate(county = as.numeric(county)) %>%
  left_join(rgdp %>% 
              dplyr::select(GeoFIPS, year,  man_rgdp) %>% 
              dplyr::rename(county = GeoFIPS))



save(df, file = "/Users/nhdata/Library/CloudStorage/Box-Box/[UA Box Health] Economics/[UA Box Health] natality_wdem062323.RData")
