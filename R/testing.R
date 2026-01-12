setwd("C:/Users/TMPACGAG/OneDrive - Birmingham City Council/Documents/R projects/PHM/Extreme heat and cold")


install.packages("raster")


raster("C:\Users\TMPACGAG\Downloads\tas_hadukgrid_uk_1km_ann_202401-202412.nc")

b = raster("tasmin_hadukgrid_uk_1km_day_20241201-20241231.nc")

f <- "tasmin_hadukgrid_uk_1km_day_20241201-20241231.nc"
b = brick(f, varname = "tasmin")


plot(b[[1]], main = "tasmin: 2024-12-08")


# number of days (31)
nlayers(b)  

# get a subset of dates/times 
getZ(b)[1:5]   

#Plot one day (use subset not [[ ]])

d10 <- subset(b, 10)

plot(d10, main = paste("tasmin", as.Date(getZ(b)[10])))

#Plot the monthly mean map

m <- calc(b, mean, na.rm = TRUE)
plot(m, main = "Mean tasmin (Dec 2024)")


##############################################################

# Birmingham-ish lon/lat 
#Create a point in latitude/longitude (WGS84)
#Your raster b is not in lat/long. It’s in a UK projection (meters). So we must convert the point.
pt_ll <- SpatialPoints(data.frame(x = -1.90, y = 52.48),
                       proj4string = CRS("+proj=longlat +datum=WGS84"))


# convert point to raster CRS
#Take my Birmingham point in lon/lat and re-express it in the same coordinate system as the raster
pt <- spTransform(pt_ll, crs(b))

#Extract values from every layer at that point
vals <- extract(b, pt)                 # returns the raster value at that point for each layer

#getZ(b) reads the time metadata attached to your RasterBrick.
dates <- as.Date(getZ(b))


#Combine them into a “normal dataset”
ts <- data.frame(date = dates, tasmin = as.numeric(vals))
ts

plot(ts$date, ts$tasmin, type="l", xlab="Date", ylab="tasmin (°C)")



