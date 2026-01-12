setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/Extreme heat and cold")

library(terra)
library(tidyterra)
library(dplyr)
library(stringr)


##################################################
#set.nc paths
tmin_file <- "data/raw/tasmin/tasmin_hadukgrid_uk_1km_day_20241201-20241231.nc"
tmax_file <- "data/raw/tasmax_hadukgrid_uk_1km_day_20241201-20241231.nc"

#read .nc files
tmin = rast(tmin_file)
tmax = rast(tmax_file)


##################################################################
#also extracting the date 
date_data = data.frame( date = as.Date(as.POSIXct(tmin@pntr[["time"]])))

date_data = date_data %>% 
  mutate(layers_date_id = as.character(as.numeric(str_extract(date, "\\d\\d$"))))


###############################################################################
#Read Birmingham boundary (shapefile / geopackage)
bham_sf = st_read("data/external/boundaries/boundaries-lsoa-2021-birmingham/boundaries-lsoa-2021-birmingham.shp") # or .shp

###########################################
#get the associated LSOA21CD for each row id
#before running the extraction 

bham_id_lsoa21 = bham_sf %>%
  st_drop_geometry() %>%
  mutate(ID = row_number()) %>%             
  select(ID, LSOA21CD, LSOA21NM) 

########################################

#convert to terra for raster ops
bham = terra::vect(bham_sf)


#point R at the folder that contains proj.db
list.files(.libPaths(), pattern = "proj\\.db$", recursive = TRUE, full.names = TRUE)
Sys.setenv(PROJ_LIB = "C:/Users/TMPACGAG/AppData/Local/Programs/R/R-4.4.2/library/sf/proj")
Sys.getenv("PROJ_LIB")


#Reproject LSOAs to match raster CRS
bham_r = terra::project(bham, crs(tmin))

# Dissolve to single boundary for fast crop/mask
bham_boundary = aggregate(bham_r)

# Crop + mask rasters to Birmingham
tmin_bham = mask(crop(tmin, bham_boundary), bham_boundary)
tmax_bham = mask(crop(tmax, bham_boundary), bham_boundary)


plot(tmin_bham)
plot(tmax_bham)


# Extract daily mean per LSOA (area-weighted)
# In terra, exact=TRUE means: for each polygon, it computes the coverage fraction of every raster cell 
# that intersects it (partial cells on the boundary get fractional weights), and when you supply fun = mean, 
# terra returns the area-weighted mean for each layer (each date).

tmin_ex = extract(tmin_bham, bham_r, fun = mean, na.rm = TRUE, exact = TRUE)
tmax_ex = extract(tmax_bham, bham_r, fun = mean, na.rm = TRUE, exact = TRUE)


# Compute tmean at LSOA/day level (wide)
#copies the whole tmin_ex to keep the same id 
tmean_ex = tmin_ex
#turn into matrix and calucalte the mean
tmean_ex[,-1] = (as.matrix(tmin_ex[,-1]) + as.matrix(tmax_ex[,-1])) / 2


##################################################################################

#rename the colnames
colnames(tmean_ex) = gsub("tasmin", "tasmean", colnames(tmean_ex))

#covert back to a dataframe and pviot it to long format 
tmean_long = as.data.frame(tmean_ex) %>% 
  left_join(bham_id_lsoa21, by = ("ID")) %>% 
  pivot_longer(cols = c(-ID, -LSOA21CD,-LSOA21NM),
               names_to = "layers",
               values_to = "tasmean"
               ) %>% 
  mutate(layers_date_id = str_extract(layers, "\\d+$")) %>% 
  left_join(date_data, by = ("layers_date_id"))
  













