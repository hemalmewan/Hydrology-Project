
##read the 99th percentile value in the file
p99 <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/p99_threshold.rds")

print(p99)  # check value


##R95  indices

##import required libraries
library(terra)

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/R99p/"


r<-rast(url) ##convert raster object

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations


##Extract daily rainfall at point
rain_values<-terra::extract(r,pts)
##Remove ID column
rain_values<-rain_values[,-1]


##define the customize function
R99p<-function(daily_precip,threshold){
  wet<-sum(daily_precip[as.numeric(daily_precip)>threshold])
  return(wet)
}

##Apply above function for each station
extreme_wet_days<-apply(rain_values,1,R99p,threshold=p99)


pts$R99p<-extreme_wet_days
##rasterize output
R99p_raster<-rasterize(pts,r[[1]],field="R99p")

## Save raster
file_name <- paste0(save_url, "R99p_1951.tif")
writeRaster(R99p_raster, file_name, overwrite = TRUE)

cat("Saved:", file_name, "\n")


##plot raster file
tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)

rasters<-rast(rasters)


# Plot with scientific color scale
plot(rasters, 
     main = "R99p for 1951",
     col = hcl.colors(30, "YlOrRd"))

