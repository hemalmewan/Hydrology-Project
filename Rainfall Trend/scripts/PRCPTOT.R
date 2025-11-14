##PRCPTOT indices

##import required libraries
library(terra)
library(zoo)


##Total Precipitation per Month Jan-DEC Locations 357

##define threshold value
threshold<-1

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/PRCPTOT_1951/"


r<-rast(url) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations

## Loop for each month (1 to 12)
for(month in 1:12){
  
  ## Identify daily bands for this month
  monthly_index <- which(format(dates, "%m") == sprintf("%02d", month))
  monthly_precipitation <- r[[monthly_index]]
  
  ## Extract precipitation at stations
  vals <- extract(monthly_precipitation, pts)
  vals <- vals[,-1, drop = FALSE]  # remove ID column, keep matrix
  
  ## Compute PRCPTOT per station
  PRCPTOT <- apply(vals, 1, function(x) sum(x[x >= threshold], na.rm = TRUE))
  
  ## Attach to points
  pts$PRCPTOT <- PRCPTOT
  
  ## Rasterize output
  template <- monthly_precipitation[[1]]
  PRCPTOT_raster <- rasterize(pts, template, field = "PRCPTOT")
  
  ## Save raster
  file_name <- paste0(save_url, "_PRCPTOT_1951_", sprintf("%02d", month), ".tif")
  writeRaster(PRCPTOT_raster, file_name, overwrite = TRUE)
  
  cat("Saved:", file_name, "\n")
}




##plot these 12 raster files

tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

# Extract month number from file names
months <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(tif.files)))


# Order files by month number
tif.files <- tif.files[order(months)]

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)


##set the grid size as 4 rows and 3 columns
par(mfrow=c(4,3))

month_names=month.abb
for (month in 1:12) {
  plot(rasters[[month]], main = paste("PRCPTOT 1951 -", month_names[month])
       ,col = hcl.colors(30, "YlOrRd"))
  
}


