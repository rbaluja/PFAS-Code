# Impact of Maternal Exposure to PFAS
Robert Baluja, Wesley Howden, Bo Guo, Ashley Langer, Derek Lemoine  
  
### Code Summary

**Execution file**: Code/nh_head.R  
	- Set parameters for model    
	- Outline of code  

**Files ran within the execution file (in order):**    

1. **Code/natality_data.R**  

  **inputs:** Raw natality data from NH  

  **outputs:** Cleaned spatial dataframe of the data provided from NH   


2. **Code/groundwater_algorithm.R**  

  **inputs:** HUC8 shapefile, Northeastern PFAS lab contamination source data, USGS groundwater levels  

  **outputs:** pxq spatial dataframe where p = number of contamination sites and q = number of triangles. Data consist of the site name, lat/lng, industry, year of confirmed testing, levels of confirmed pfoa and pfos, the triangle and its shape.  

  **summary:** Subset contamination data to only include sites with confirmed contamination in groundwater that exceeds 1000 ppt (spoke with Bo about this on 4/21/23) and with a known industry. For each of source site, find the direction of groundwater flow as follows: select a large number of random points from the intersection of the HUC8 shape that the source lies in and a circle of radius *meters* around the source, then fit the OLS solution to the linear mapping of lat and lng to groundwater elevation. The returned coefficients are the direction of fastest increase (under the linearity and locality assumptions), and -1 x the coefficients is then the direction of fastest decrease. With the direction of fastest decrease determined for each source, this file then creates *n_triangles* with the center of triangle 1 being the line of fastest decrease, and *meters* meters long. The remaining triangles are labelled in a counter-clockwise fashion. Merge the resulting data with the contamination source data.  
  

3. **Code/source_service_cleaning.R**  

  **inputs:** Shapefile of areas piped to public water, csv containing the location and characteristics of all wells in service by a CWS   

  **outputs:** Shapefile of all areas serviced by public water and the particular system id (sa), the same as sa but also including each source and its lat/lng (wells), systems that do not have any groundwater sources (sys_skip), dataframe with all (if any) source triangles each well falls into

  **summary:** Subset systems to only those that are classified as a CWS and provide water service (some of them only provide sewer to some areas - so those areas do not have access to public water and should be treated as the domestic well population in the analysis). Subset sources that come from groundwater. Merge these two files. Create a 1-meter buffer around each well and determine which (if any) contamination triangles it falls in, save the resulting dataframe as well_triangles. Determine which service providers do not use any groundwater wells by comparing the list of CWS providers and the list of sources by providers. If a provider does not have any groundwater sources, then include it in sys_skip.  
  

4. **Code/natality_wells.R**  

  **inputs:** natality dataframe (df), sa  

  **outputs:** dataframe that merges sa and df (df)

  **summary:** Spatial merge between df and sa determines which service provider provides water to each individual. If the merge is empty for a given individual, then I assume that they receive their drinking water from a private well (whose location is the same as their residence). For those individuals with a nonempty merge, I take the source that is geographically closest to them as the one which provides them with their water (NOTE: This is not exactly correct, but my understanding is that it isn't too bad of a simplification). Drop all individuals who live in a service area that obtains their water from non-groundwater sources (or who only purchases groundwater from another location).   
  

5. **Code/wind.R**  

  **inputs:** Northeastern contamination dataset, wind direction data (monthly era5 reanalysis)

  **outputs:** Primary dataframe (df), merged with the wind data to additionally include the number of sites that its drinking water source is downwind of at some time of the year, the sum of the number of months it is downwind of some site, and the distance weighted sum of the contamination at all of these sites

  **summary:** Use wind data to compute montly direction of prevailing wind, as measured by the u and v components. Use the rWind package to convert these to a direction measured in degrees. Build a triangle around each of these montly prevailing directions at a site, where *w_width* is the width of the eventual triangle. Use the resulting spatial dataframe of monthly wind triangles for each site to determine which, if any, site-month triangles a given water source lies in. Merge this information, and the distance weighted sum of contamination amounts at these sites, with the primary dataframne. Repeat this exercise for the individuals on domestic wells. 
  
  
6. **Code/triangle_strat.R**  

  **inputs:** wells, cont_sites

  **outputs:** wide dataframe of each source and the number of sites that it lies in for each triangle and the sum of pfas exposure by each triangle (wt_summary), the same for each domestic well (all residences outside of a service areas, domw_summary). These data are merged with df seperately. Also runs the triangle regression (t_reg).   

  **summary:** Obtain the list of intersections between a given well and the contamination triangles. Add to this the distance between the well and the site. Reformat data to aggregate this information by triangle number and well. Do this seperately for domestic and public wells. Merge this info with the natality dataframe by source resulting in the original natality data with additional columns for the number of sites whose triangles their water source lies in and the sum of pfas found, grouped by the triangles. Uses these data to perform the triangle analysis.  


7. **Code/cont_data_df.R**  

  **inputs:** pfas contamination testing data for public wells from NHDES (/Data/Contamination/NH_tests.csv), contamination data from tests housed as pdf's on NHDES website (https://www.des.nh.gov/resource-center/publications?keys=hb1766&purpose=Reports&subcategory=PFAS)

  **outputs:**  wells wth contamination, as confirmed by a test (c), merged with the primary dataframe (df) for those individuals that are on public water. I then do the same for domestic wells that are tested and found to have contamination in the testing data (dom_a). Uses this info for regressions (reg_cont_pub & reg_cont_dom)

  **summary:**  Read in and clean testing data. In particular, mutate the information contained in the data such that it is all measured in the same units (ppt), replace non detects with sqrt(2)/detection limit NOTE: THIS NEEDS TO BE UPDATED - what do we feel comfortable using to impute those data (Options include quantile regression and Bayesian methods - among others). Bind these data with the primary dataset. Return regression of interest for individuals only on public wells. For individuals on domestic wells: take a 500 meter buffer around each of the domestic wells that tested  for PFAS (and that have geo info). If there is a single well buffer than an individual lives in, then I assume that their water was contaminated in the same way as that well, if there are multiple wells, then I take the closest one, if there are multiple tests for the clostest one, then I take the test that is closest in time to the date of birth, if there are no wells sufficiently close that were tested, then I drop that individual. Use these information for the regression of interest (this will compare houses who tested their domestic well for contamination and had a positive result to those who had a nondetect and are sufficiently close)



  


  
  
