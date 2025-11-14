##CDD indices

##import required libraries
library(terra)

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/CDD_1951/"


r<-rast(url) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations


# Define threshold for dry day
dry_threshold <- 1  # mm

##Extract daily rainfall at point
rain_values<-terra::extract(r,pts)
##Remove ID column
rain_values<-rain_values[,-1]


##define the customize function
CDD<-function(daily_precip,threshold=1){
   dry<-as.numeric(daily_precip)<threshold
   if(all(!dry)) return(0) ##no dry days
   rle_dry<-rle(dry)
   max_cdd<-max(rle_dry$lengths[rle_dry$values])
   
   return(max_cdd)
}

##Apply above function for each station
cdd_values<-apply(rain_values,1,CDD,threshold=dry_threshold)

pts$CDD<-cdd_values
##rasterize output
CDD_raster<-rasterize(pts,r[[1]],field="CDD")

## Save raster
file_name <- paste0(save_url, "CDD_1951.tif")
writeRaster(CDD_raster, file_name, overwrite = TRUE)

cat("Saved:", file_name, "\n")


##plot raster file

tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

# Extract month number from file names
months <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(tif.files)))

# Order files by month number
tif.files <- tif.files[order(months)]

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)

rasters<-rast(rasters)

rasters

# Plot with scientific color scale
plot(rasters, 
     main = "Consecutive Dry Days (CDD) – 1951",
     col = hcl.colors(30, "YlOrRd"))
  



