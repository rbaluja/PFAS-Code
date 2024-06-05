#get population at block group
states = fread('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv')
sc = as.character(states$st)
sc = ifelse(nchar(sc) < 2, paste0("0", sc), sc)
cbg_ll = fread(paste0("https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG", sc[1], ".txt"))
for (i in 2:length(sc)){
  cll = fread(paste0("https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG", sc[i], ".txt"))
  cbg_ll = rbind(cbg_ll, cll)
}

cbg_ll = cbg_ll %>% 
  dplyr::rename(state = STATEFP, 
                county = COUNTYFP, 
                pop = POPULATION, 
                lat = LATITUDE, 
                lng = LONGITUDE, 
                tract = TRACTCE, 
                cbg = BLKGRPCE) %>%
  mutate(state = as.character(state), 
         county = as.character(county)) %>% 
  mutate(state = ifelse(nchar(state) < 2, paste0("0", state), state), 
         county = ifelse(nchar(county) == 1, paste0("00", county), 
                         ifelse(nchar(county) == 2, paste0("0", county), county)))
cbg_ll$county = paste0(cbg_ll$state, cbg_ll$county)



counties = tigris::counties()
url = "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.xlsx?v=7498.4"
download.file(url, modify_path("Data_Verify/Supplemental/nat_pops.csv"))

c_pop = readxl::read_xlsx(modify_path("Data_Verify/Supplemental/nat_pops.csv"))
c_pop = janitor::row_to_names(c_pop, 4)

counties = counties %>% left_join(c_pop, by = c("GEOID" = "FIPStxt"))

#move square meters to square miles
counties = counties %>% 
  dplyr::mutate(ALAND = ALAND/2589988)

counties$pop_dens = as.numeric(counties$CENSUS_2020_POP)/as.numeric(counties$ALAND)
counties$log_pd = log(counties$pop_dens)

cont_sites = readxl::read_xlsx(modify_path('Data_Verify/Contamination/PFAS Project Lab Known Contamination Site Database for sharing 10_09_2022.xlsx'), sheet = 2) %>% 
  dplyr::filter(`Matrix Type` == 'Groundwater') %>% 
  dplyr::select(`Site name`, Latitude, Longitude, Industry, 
                `Date Sampled`,`Max PFOA (ppt)`, `Max PFOS (ppt)`, 
                `Max PFOA+PFOS from a single sample (ppt)`, 
                `Max Total PFAS from a single sample (ppt)`, 
                `PFAS Level (ppt)`, State) %>% 
  dplyr::rename(site = `Site name`, lat = Latitude, state = State,
                date = `Date Sampled`, lng = Longitude, industry = Industry, 
                pfoa = `Max PFOA (ppt)`, pfos = `Max PFOS (ppt)`, 
                sum_pfoa_pfos = `Max PFOA+PFOS from a single sample (ppt)`, 
                sum_pfas = `Max Total PFAS from a single sample (ppt)`, 
                total_pfas = `PFAS Level (ppt)`) %>%
  dplyr::filter(industry != 'Unknown' & sum_pfoa_pfos >= 1000) %>% #cut to 1000ppt by Bo meeting 4/21/23
  st_as_sf(coords = c('lng', 'lat'), remove = F) %>%
  st_set_crs('+proj=longlat +datum=WGS84' ) %>% 
  st_transform(4326)


state_abb = c("MI", "MN", "NH", "NY", "CO", "ME", "VT", "CA", "FL", "ND", "WI")
states_keep = c("Michigan", 
                "Minnesota", 
                "New Hampshire", 
                "New York", 
                "Colorado", 
                "Maine", 
                "Vermont", 
                "California", 
                "Florida", 
                "North Dakota", 
                "Wisconsin")
figure_s6 = ggplot() +
  geom_sf(data = counties, aes(fill = log_pd), color = NA, alpha = 0.6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Log Population \nDensity") +
  geom_point(data = cont_sites %>% dplyr::filter(state %in% states_keep), aes(x = lng, y = lat, size = sum_pfoa_pfos/10^3), alpha = 0.6) +
  scale_size_continuous(name = "PFAS (ppb)") + 
  theme_minimal() +
  xlim(-130, -60) +
  ylim(23, 50) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.95, -0.1),  # Bottom right corner
        legend.justification = c(1, 0),   # Anchor point of the legend
        legend.background = element_blank(), 
        legend.box.margin = margin(0, 0, 0, 0), 
        plot.margin = margin(0, 0, 0, 0)) + 
  xlab("") + ylab("")

ggsave(modify_path3("Figures/National Costs/nat_map.png"), figure_s6, width = 2694, height = 2355, units = "px")
