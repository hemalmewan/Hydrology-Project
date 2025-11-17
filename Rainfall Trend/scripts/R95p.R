
##read the 95th percentile value in the file
p95 <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/p95_threshold.rds")

print(p95)  # check value


##R95  indices

##import required libraries
library(terra)

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/R95/"


r<-rast(url) ##convert raster object


points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations

##Extract daily rainfall at point
rain_values<-terra::extract(r,pts)
##Remove ID column
rain_values<-rain_values[,-1]


##define the customize function
R95p<-function(daily_precip,threshold){
  wet<-sum(daily_precip[as.numeric(daily_precip)>threshold])
  return(wet)
}

##Apply above function for each station
very_wet_days<-apply(rain_values,1,R95,threshold=p95)

pts$R95p<-very_wet_days
##rasterize output
R95p_raster<-rasterize(pts,r[[1]],field="R95p")

## Save raster
file_name <- paste0(save_url, "R95p_1951.tif")
writeRaster(R95p_raster, file_name, overwrite = TRUE)

cat("Saved:", file_name, "\n")


##plot raster file
tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)



rasters<-rast(rasters)
rasters

# Plot with scientific color scale
plot(rasters, 
     main = "R95p for 1951",
     col = hcl.colors(30, "YlOrRd"))


# Save to files
saveRDS(very_wet_days, "C:/Hydrology-Project/Rainfall Trend/scripts/R95_threshold.rds")
