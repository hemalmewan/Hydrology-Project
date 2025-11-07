##import reuqired libraries
library(terra)
library(zoo)


##get the user input 
paste("Enter the Required d value:",d<-as.integer(readline()))
print(d)

##validate the range of the d value it should be between 1 to 10
if(d<1 | d>10){
  print("Error!!,d must be between 1 and 10")
  
}


##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Input/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Input/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Input/Rxdday_1951/"


r<-rast(url) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations


##loop through all the months January to December
for (month in 1:12) {
  ##define end date and start date for each month
  monthly_index<-which(format(dates,"%m")==sprintf("%02d",month))
  
  
  ##Daily precipitation data for each month
  monthly_precipitation<-r[[monthly_index]]
  
  ##Daily precipitation data in each month each location
  vals<-extract(monthly_precipitation,pts)
  vals_mat<-as.matrix(vals[,-1,drop=FALSE]) ##remove ID column
  
  
  ## Compute the rolling window days
  if(ncol(vals_mat) >= d){
    roll_sum <- apply(vals_mat, 1, function(x) rollapply(x, width = d, FUN = sum, align = "left", na.rm = TRUE))
    roll_sum <- as.matrix(roll_sum)
    monthly_Rxdday <- apply(roll_sum, 2, max, na.rm = TRUE)
  }else{
    monthly_Rxdday <- rep(NA, nrow(vals))
  }
  
  
  ##Attach the result as points
  pts$Rxdday<-monthly_Rxdday
  
  ##create the raster layers
  templete<-monthly_precipitation[[1]]
  Rxdday_raster<-rasterize(pts,templete,field="Rxdday")
  
  ##Save raster
  file_name<-paste0(save_ur l,"Rx",d,"day_1951_",sprintf("%02d",month),".tif")
  writeRaster(Rxdday_raster,file_name,overwrite=TRUE)
  
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
  plot(rasters[[month]], main = paste("Rx3day 1951 -", month_names[month]))
  
}

par(mfrow=c(1,1))


