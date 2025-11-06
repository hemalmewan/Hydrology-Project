##import library
library(terra)

##url(path) for the nc file in 1951 daily precipitation 
url<-"C:/UOC pdf/4th Year/DS 4007-Research/sptiao_tempo/rainFallTrend/Rainfall Input/NCDF/rainfall_1952_daily.nc"
##url(path) for the 1951 precipitation data
url_1<-"C:/UOC pdf/4th Year/DS 4007-Research/sptiao_tempo/rainFallTrend/Rainfall Input/drf_1952_new2.csv"
##save the output raster files for 1951(Rx1day)
save_url<-"C:/UOC pdf/4th Year/DS 4007-Research/sptiao_tempo/rainFallTrend/Rainfall Input/Rx1day_1952/"


r<-rast(url) ##convert raster object
date<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31

points <- read.csv(url_1)  ##read file
pts <- vect(points, geom = c("lon", "lat"), crs = crs(r)) ##convert the lat lon spatial locations

##loop through all the months january to December
for(month_i in 1:12){
  
  ##define end date and start date
  monthly_index <- which(format(dates, "%m") == sprintf("%02d", month_i))
  
  ##Daily precipitation data for each month
  monthly_precipitation<-r[[monthly_index]]
  
  ##daily precipitation data for 1951 each month each location
  vals<-extract(monthly_precipitation,pts)
  
  ##get the maximum precipitation for each location for each month
  monthly_max_prepitation<-apply(vals[,-1],1,max,na.rm=TRUE)
  
  pts$Rx1day<-monthly_max_prepitation
  
  ##get the first raster layer for each month
  ras_layer<-monthly_precipitation[[1]]
  ##make the Rx1day as the raster layer for each month and each location
  monthly_Rx1day<-rasterize(pts,ras_layer,field="Rx1day")
  
  # Output filename
  file_name <- paste0(save_url, "Rx1day_1952_", month_i, ".tif")
  
  ##save the raster file for each month 
  writeRaster(monthly_Rx1day,file_name,overwrite = TRUE)
  
}



##plot these 12 raster files

tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

# Extract month number from file names
months <- as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", basename(tif.files)))

# Order files by month number
tif.files <- tif.files[order(months)]

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)

rasters

##set the grid size as 4 rows and 3 columns
par(mfrow=c(4,3))

month_names=month.abb
for (month in 1:12) {
  plot(rasters[[month]], main = paste("Rx1day 1952 -", month_names[month]))
  
}

par(mfrow=c(1,1))

