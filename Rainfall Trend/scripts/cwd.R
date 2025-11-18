##CWD indices

##import required libraries
library(terra)


##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/CWD_1951/"


r<-rast(url) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31

r


points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations


# Define threshold for wet day
wet_threshold <- 1  # mm

##Extract daily rainfall at point
rain_values<-terra::extract(r,pts)
##Remove ID column
rain_values<-rain_values[,-1]


##define the customize function
CWD<-function(daily_precip,threshold){
  wet<-as.numeric(daily_precip)>=threshold
  if(all(!wet)) return(0) ##no wet days all are dry days
  rle_wet<-rle(wet)
  max_cwd<-max(rle_wet$lengths[rle_wet$values])
  
  return(max_cwd)
}

##Apply above function for each station
cwd_values<-apply(rain_values,1,CWD,threshold=wet_threshold)
pts$CWD<-cwd_values
##rasterize output
CWD_raster<-rasterize(pts,r[[1]],field="CWD")

## Save raster
file_name <- paste0(save_url, "CWD_1951.tif")
writeRaster(CWD_raster, file_name, overwrite = TRUE)

cat("Saved:", file_name, "\n")


##plot raster file
tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

# Extract month number from file names
months <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(tif.files)))

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)

rasters<-rast(rasters)

# Plot with scientific color scale
plot(rasters, 
     main = "Consecutive Wet Days (WET) – 1951",
     col = hcl.colors(30, "YlOrRd"))



par(mfrow=c(1,1))
