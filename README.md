### PFAS in Drinking Water Wells Cause Preterm and Low-Weight Births
#### Baluja, Guo, Howden, Langer, Lemoine

### Software Dependencies
- GDAL
- PROJ
- GEOS
- Whitebox Tools

### Machine Spec
This was run on an iMac with a 3.8 GHz 8-Core Intel Core i7 processor and 128GB of RAM. I used R Version 4.4.0 (2024-04-24)

### Code Summary

#### Prior to Running Execution file: 
#### 1. PR/preliminaries.R
#### Parameters  
   * code_check
   * redo_GIS
   * rerun_fs_clean: Clean first stage testing data?
   * rerun_bs: Run bootstrap (only matters if it has been run, and the output saved, previously)
   * ppt (used to determine which contamination site cutoff to use to select which watersheds to build)
   * wind_dist
   * meters
   * drop_states (drop sources near state border - Always keep FALSE for preliminaries)
   * census_key: Census API key

1. ##### PR/GIS/gis_head.R
   Note: This only runs if either redo_GIS is true, or one of the (primary) outputs from the below files is missing from the working directory
   **PR/GIS/cont_watershed.R**
   Note: This is only run if GIS_create is true
    * Input: NH DEM, PFAS Lab contamination sites
    * Output: Shapefile of watersheds for all contamination sites ("Data_Verify/GIS/cont_watershed.RData"), crosswalk from site index to site name (Data_Verify/GIS/rs_ll_ws.csv)
    * This file creates the watershed shapes for all contamination sites in the data. To do so, it first reads in the DEM and fills in any sinks/depressions in the data. It uses this filled raster to calculate flow accumulation and direction rasters for the state. It then iterates through each site, snapping its point to the cell with highest flow accumulation within a 3 cell radius. It uses this snapped point, along with the flow direction raster, to calculate its watershed. The resulting raster is transformed to a polygon and saved to memory. After iterating through each site, all polygons are read into memory, combined into a single file, and saved to memory.  

    * **PR/GIS/test_wells_watershed.R**
    * Input: Flow accumulation raster (Data_Verify/GIS/flow_acc.tiff), flow direction raster (Data_Verify/GIS/flow_dir.tiff), cleaned test well data
    * Output: Shapefile of the watersheds for all test wells used in the first stage (Data_Verify/GIS/fs_test_watershed.RData), crosswalk from well index to source and system id (Data_Verify/GIS/wells_ll_ws.csv)
    * This file follows the same framework as cont_watershed.R, applied instead to the test wells

    * **PFAS-Code/PR/GIS/wells_watershed.R**
    * Input: Flow accumulation raster (Data_Verify/GIS/flow_acc.tiff), flow direction raster (Data_Verify/GIS/flow_dir.tiff), wells (taken from PR/Data/NHDES_PWS.R)
    * Output: Data_Verify/GIS/wells_watershed.RData
    * This file follows the same framework as cont_watershed.R, applied instead to the drinking water wells

    * **PR/GIS/cont_flowacc.R**
    * Input: PFAS Lab contamination sites, flow direction raster (Data_Verify/GIS/flow_dir.tiff), NH DEM
    * Weighted flow accumulation raster (Data_Verify/GIS/cont_fa_sum_buffed.tiff)
    * This file first reads into memory the contamination sites. For each site, it uses the flow direction raster to calculate the direction that water would flow, when starting from that point. This information is stored as a raster, where cells off this path are labeled as NA and those on the path have a value of 1. The non-NA values are then buffered by 500 meters, and the values are replaced with the PFAS level at the relevant site. After this is completed for all sites, the individual rasters are read into memory, the NA values are replaced with zeros, and then the sum across all site-specific rasters is applied to obtain a single raster with the total weighted flow accumulation throughout space from all the 41 sites.
   
2. * **PR/Data/cont_cleaning.R**
    Note: This only runs if rerun_fs_clean is true or Data_Verify/Contamination/cleaned_contwell.csv is missing
    * Input: New Hampshire PFAS test results (Data_Verify/Contamination/NH_tests.csv), weather data (Data_Verify/Supplemental/nh_cbg_weather.csv), PM2.5 data (Data_Verify/Supplemental/nh_cbg_pm25.csv), NH DEM, TRI facilities (Data_Verify/Supplemental/tri_nh.csv)
    * Output: first stage test data (Data_Verify/Contamination/cleaned_contwell.csv)
    * This file begins by reading in the test well data (fs_cont) and translating all values to parts-per-trillion. It then imputes missing data by detection limit$/\sqrt2$. Remaining data with no value in the test are either non detects, below the detection limit with no listed limit, or just blank - all are put at zero. For each testing activity and contaminant, the maximum tested level is kept, and the dataframe is pivoted around the test events. For each unique test well, the maximal test is kept, with remaining duplicates being dealt with by taking the earlier test. After this, demographic and environmental covariates are added to fs_test.

#### 2. PR/Bootstrap/bootstrap_iv.R  
   * This file creates three files: RData/bootstrap.RData, RData/bootstrap_quant.RData, RData/bootstrap_sb.RData. These files contain 10,000 rows, corresponding to bootsrap iterations of the IV specifications.
   * The file first reads in all data (PR/Data/data_head.R), assigns downgradient, upgradient, and to-the-side for each birth (PR/Main Analysis/binary.R), calculates flow accumulation at the residence (PR/Main Analysis/flow_accumulation.R), and then assigns downgradient, upgradient, and to-the-side for each test well, along with their soil variables and wind exposure for the first stage regression. It then adds soil variables to the birth record data, through the location of their drinking water well


#### 3. Additional files  
Note: These should each be run with a clean environment  
* **PR/Robustness/gw_verification.R**
  * This calculates the statistics from Appendix M  
* **PR/Robustness/ny_spec.R**
  * This calculates the results used for the New York section (Appendix N)
* **PR/Placebo/placebo_head.R**
  * This calculates the number of false positives under the placebo test (Table S-8)
* **PR/GIS/cs_downstream.R**
  * This directly calculates the necessary GIS files and creates Figure S-1
* **PR/Figures/pop_matching_cutoff.R**
  * This creates Figure S-2b
* **PR/Figures/meters_cutoff.R**
  * This creates Figure S-3
* **PFAS-Code/PR/Figures/quintiles_pfas.R**
  * This creates Figure Figure S-5 and Table S-12

### New Hampshire impacts: PR/infant_health_head.R  
### Parameters:
- natality_path: File path to UA Box Drive folder
- meters: Buffer for defining ``nearby sites''
- wind_dist: Maximum distance assumed for wind transport from Saint Gobain Plastics
- ppt: Lower threshold of PFOA + PFOS for a primary release site
- run_cleaning: Reclean natality data
- match_wells: Rematch residences to water wells
- drop_far_down: When a well is downgradient of a release site further than 'meters' away, drop them?
- drop_far_up: When a well is upgradient of a release site further than 'meters' away, drop them?
- IV: Run IV spec?
- drop_states: Remove all contamination sites within 'meters' of a state border? Used for robustness figure
- relaxed_up: Remove upgradient classification? Used for robustness figure
- create_figures: Build figures used in text
- census_key: Census API key used for fetching tract and CBG-level covariates
- tables: Create tables?
- figures: create figures?
- n_cores: number of cores used to parallelize tasks
- rob_app_fig: Create and save Figure S-4
- bs_cov: bootstrap and save to memory the covariance matrix for IV

#### Files ran within the execution file (in order):

1. **PR/Data/data_head.R**
    * **PR/Data/pfas_lab_sites.R**
    * Input: PFAS Lab contamination sites
    * Output: cont_sites, cont_sites_buff
    * This file reads into memory the primary contamination sites (cont_sites), removes two duplicate sites, buffers the sites by 'meters' (cont_sites_buff). If running drop_states, then it further calculates the distance from each site to the nearest state border, and drops any sites within 'meters' of such. 

    * **PR/Data/NHDES_PWS.R**
    * Input: PWS service areas (Data_Verify/Groundwater/NH_Confidential_Data/Secure_Data/Water_and_Sewer_Lines.shp), groundwater well locations (Data_Verify/Groundwater/NH_Confidential_Data/Secure_Data/Public_Water_Supply_Wells.shp)
    * Output: wells, sys_skip
    * This file first reads into memory the service area shapes (sa), subsetting these data to only community water systems which have water service (not just sewer). It then reads into memory the water source locations (wells), subsetting this dataset to only active groundwater wells associated with an active water community water system. Wells is then merged with sa to obtain a spatial dataframe with the lat long of each source, and the geometry of the system-level service area. After this is completed, PR/Data/no_wells.R is run to obtain a list of all systems which do not have active local groundwater sources (sys_skip). To do so, it reads in the service areas (sa_nowells), and subsets it to community water systems with water service. It then reads in the active groundwater water source information (w). sys_skip is defined as the systems which are not contained in the set of systems in w. 

    * **PR/Data/wind.R**
    * Input: cont_sites, GRIDMET daily wind direction raster (Data_Verify/Wind/agg_met_th_1979_CurrentYear_CONUS.nc)
    * Output: wind, wind_function, inner_wind_function
    * This file first sets up the wind function, which for a given lat long and site index (always equal to one, since only Saint Gobain contributes wind contamination), finds the distance from the site to the point. If the distance is less than wind_dist, then it finds the number of days in 2015 that the angular distance between the point and the direction of prevailing wind on that day was less than $\pi/4$. The file then reads the single contamination site (c_sites) and transforms cont_sites to a SpatRaster (sites - to be used with the sf package). Then it reads in the GRIDMET daily direction data, calculates the prevailing daily direction at SG, transforms this direction to radians mapped between $[-\pi, \pi]$, and subsets the data to only days in 2015. The resulting data (wind) is dataframe with 365 rows (one per day in 2015) with its corresponding daily direction. 

    * **PR/Data/natality_data.R**
    Note: This only runs if run_cleaning is true
    * Input: natality data paste0(natality_path, "[UA Box Health] VR2210_Deliverable/dr_6264_deliverable.xlsx")
    * Output: cleaned natality data with CBG of residence (df)
    * This file reads in the natality data and cleans up key variables and drops those with missing geographical and health outcome information. It then uses the tigris package to assign the CBG of residence to each birth residence

    * **PR/Data/birth_covars.R**
    Note: This only runs if run_cleaning is true
    * Input: weather data (Data_Verify/Supplemental/nh_cbg_weather.csv), pm2.5 data (Data_Verify/Supplemental/nh_cbg_pm25.csv), tract-level Census covariates (Data_Verify/Supplemental/tract_stats.csv), TRI facilities (Data_Verify/Supplemental/tri_nh.csv)
    * Output: natality dataframe with covariates attached paste0(natality_path, "[UA Box Health] birth_records_wdem_prematch.RData")
    * This file reads in combines all necessary covariates to the birth records. It further finds the distance from each residence to the nearest contamination site (csite_dist)

    * **PR/Data/natality_wells.R**
     Note: This only runs if match_wells is true
     * Input: service areas (sa), natality data (df), sys_skip, wells
     * Output: natality data matched to drinking water wells
     * This file begins by finding the water system servicing each residence. It then breaks df into a list, grouped by water system. For each water system, well_assigner operates on the subset of df which was assigned to that system. If that system is in sys_skip, then it returns that subset of df with well_lat = well_lng = NA (to drop them). If the system is "Domestic Well" (i.e., residences outside of a service area), then it sets well_lat = residence_lat and well_lng = residence_lng. Otherwise, the system is one with at least one groundwater well. It then finds the nearest well in the assigned system to each residence and sets well_lat & well_lng equal to the coordinates for the relevant wells. After completing this algorithm for each system, it then drops all birth records with missing well coordinates (records assigned systems in sys_skip). 

    * **PR/Data/elev_setup.R**
    Note: This only runs if match_wells is true
    * Input: NH DEM, wells, matched natality data
    * Output: natality data with matched well elevation and elevation at their residence (df)
    * This file reads in the DEM and extracts the elevation at the relevant cells for the drinking water wells and residence in the birth records. Both of these are added to the natality data (df). 


2. **PR/Main Analysis/main_analy.R** 
    * **PR/Main Analysis/binary.R**
    * Input: well watersheds (Data_Verify/GIS/wells_watershed.RData), crosswalk from well index to source and system id (Data_Verify/GIS/wells_ll_ws.csv), contamination site watersheds (Data_Verify/GIS/cont_watershed.RData), crosswalk from site index to site name (Data_Verify/GIS/rs_ll_ws.csv)
    * Output: Natality data with treatment assignment by drinking water well (used in binary spec)
    * This file first reads in the well and contamination site watershed shapes saved in the GIS section. It then classifies all downgradient wells by finding which wells have a contamination site in their watershed (it takes the intersection of cont_sites and the well watershed shapes) - there are 282 such wells. For each of these water wells, it then finds the nearest such site and assigns the site variables to this well (pfas_down, dist_down, site_down). A similar logic is applied when assigning upgradient wells, where now, a well obtains up information when it lies in the watershed of a contamination site (it finds the intersection of wells, with its geometry set as its lat long and the contamination site watersheds) - there are 697 such wells. For each of these water wells, it then finds the nearest such site and assigns the site variables to this well (pfas_up, dist_up, site_up). Wells is then merged with down_wells and up_wells, with missing information set to 0. Wind exposure is calculated using wind_function (from Data/wind.R). Then for each well, characteristics of the nearest contamination site, and the number of sites within 'meters' are calculated and appends to wells (wells1). Once this is done, sites can be matched to release sites - well_assgn takes as input each well index and assigns the well to a site according to algorithm discussed in the paper (wells2). The resulting dataframe is merged with df to assign treatment statuses to the birth records. 

    * **PR/Main Analysis/flow_accumulation.R**
    * Input: Weighted flow accumulation raster from sites (Data_Verify/GIS/cont_fa_sum_buffed.tiff), natality data (df)
    * Output: Natality data with fa_resid (weighted flow accumulation at their residence)
    * This file reads in the weighted flow accumulation raster, built in the GIS section, extracts the value at the cell for each residence in the data, and assigns this value as fa_resid. 

    * **PR/Main Analysis/first_stage.R**
    Note: This only runs if IV is true
    * Input: fs_cont (Data_Verify/Contamination/cleaned_contwell.csv), test well watersheds (Data_Verify/GIS/fs_test_watershed.RData), contamination site watersheds (cont_ws), soil porosity raster (Data_Verify/Soil/por_gNATSGO/por_gNATSGO_US.tif), available water capacity raster (Data_Verify/Soil/awc_gNATSGO/awc_gNATSGO_US.tif)
    * Output: First stage regression (w_reg), natality data with predicted PFAS level
    * This file follows the same logic as in binary.R to assign contamination sites to test wells. After completing such exercise, it then assigns soil properties to each well and runs the first stage regressions. After this, it assigns soil properties to the water wells, merges this with the natality data by well identifier, and uses this information to assign predicted pfas to each birth record.
  
3. **PR/Tables/tables.R**
   * This file needs the bootstrapped standard errors to have been calculated to run, as well as the robustness checks (drop_states, relaxed_up in main_analy.R and PR/Robustness/resid_side_comparison.R). It writes to file each of the tables used in the main text. The following files are ran within this file:
   * PFAS-Code/PR/Robustness/oster_selection.R

4. **PR/Figures/figures_head.R**
   * This file creates all figures in the main text and SI, outside of the national cost ones (see last section of README for prerequisites. 

#### National Costs
**PR/national_costs_head.R**
- nat_run_cont_ws: Recreate national watershed files?
- nat_reassn: Reassign CBG's to national release sites?
- nat_redo_soil: Recalculate soil statistics for national data?

 * **PR/National Costs/nat_births.R**
 Note: This only runs if nb_cbg is true
 * Input: Raw CDC Wonder natality data (Data_Verify/National/nat_births10.txt), state name to number crosswalk (https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv), Census CBG population counts
 * Output: CBG by county population weighted births (Data_Verify/National/births_cbg_cleaned_2010.csv)
 * This file uses CBG-level population counts to form CBG-county weights to downscale the county-level population counts to the CBG-level.

 * **PR/National Costs/nat_watersheds.R**
 Note: This only runs if nat_run_cont_ws is true
 * This file creates the watersheds of each CBG centroid and primary release site in the 11 states used in calculating national impact costs

 * **PR/National Costs/nat_assn.R**
 Note: This only runs if nat_reassn is true
 * This file cleans up the identifiers in the births and contamination datasets, then call well_assn.R. well_assn.R follows the same logic as the primary specification (Main Analysis/binary.R) in assigning CBG centroids to contamination sites, and in assigning up, down, and side classifications. 

 * **PR/National Costs/nat_costs**
 * This file uses the assigned natality data, the estimated first stage equation (Main Analysis/first_stage.R), and the linear IV results to calculate estimated impacts at the CBG level. It then creates Figure 3a

 * **PR/Figures/figure3_map.R**
 * This file creates and saves to disk Figure 3b

 * **PR/National Costs/nat_map.R**
 * This file creates and saves to disk Figure S-6

 * **PR/National Costs/nat_costs_binary.R**
 * This file recreates Figure 3a with the binary downgradient estimates. It creates and saves Figure S-7




