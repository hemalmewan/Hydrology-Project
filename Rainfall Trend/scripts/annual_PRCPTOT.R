## Annual PRCPTOT indices

##import required libraries
library(terra)
library(zoo)


##Total Precipitation per Month Jan-DEC Locations 357

##define threshold value
wet_threshold<-1

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/annual_PRCPTOT/"


r<-rast(url) ##convert raster object

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations

##Extract daily rainfall at point
rain_values<-terra::extract(r,pts)
##Remove ID column
rain_values<-rain_values[,-1]

annual_PRCPTOT<-function(precip_vale,threshold){
   wet_days<-sum(precip_vale[precip_vale>=threshold])
   return(wet_days)
}

annual_wet_days<-apply(rain_values,1,annual_PRCPTOT,threshold=wet_threshold)

pts$annual_PRCPTOT<-annual_wet_days
##rasterize output
annual_PRCPTOT_raster<-rasterize(pts,r[[1]],field="annual_PRCPTOT")

## Save raster
file_name <- paste0(save_url, "PRCPTOT_1951.tif")
writeRaster(annual_PRCPTOT_raster, file_name, overwrite = TRUE)

cat("Saved:", file_name, "\n")

# Save to files
saveRDS(annual_wet_days, "C:/Hydrology-Project/Rainfall Trend/scripts/annual_PRCPTOT.rds")
