#calculate watersheds
source("PFAS-Code/PR/GIS/cont_watershed.R") #PFAS Lab sites
source("PFAS-Code/PR/GIS/test_wells_watershed.R") #first stage test wells- NOTE: This takes a long time to run
source("PFAS-Code/PR/GIS/wells_watershed.R") #PWS wells

#calculate flow accumulation
source("PFAS-Code/PR/GIS/cont_flowacc.R")