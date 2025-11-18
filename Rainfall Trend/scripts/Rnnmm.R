##Rnnmm indices

##import required libraries
library(terra)

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/Rnnmm/"


r<-rast(url) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31



points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations


# Define threshold for dry day
dry_threshold <- 10  # mm

##define the user define function for climate indics Rnnmm
Rnnmm<-function(daily_precip,threshold){
  no_days<-sum(daily_precip>threshold)
  return(no_days)
}


##loo[ through all the months Jan-Dec]
for (month in 1:12) {
  ##define end date and start date for each month
  monthly_index<-which(format(dates,"%m")==sprintf("%02d",month))
  
  ##Daily precipitation data for each month
  monthly_precipitation<-r[[monthly_index]]
  
  ##Daily precipitation data in each month each location
  vals<-extract(monthly_precipitation,pts)
  vals_mat<-as.matrix(vals[,-1,drop=FALSE]) ##remove ID column
  
  monthly_days<-apply(vals_mat,1,Rnnmm,threshold=dry_threshold)
  
  ##Attach the result as points
  pts$Rnnmm<-monthly_days
  
  ##create the raster layers
  templete<-monthly_precipitation[[1]]
  Rnnmm_raster<-rasterize(pts,templete,field="Rnnmm")
  
  ##Save raster
  file_name<-paste0(save_url,"R",dry_threshold,"mm_1951_",sprintf("%02d",month),".tif")
  writeRaster(Rnnmm_raster,file_name,overwrite=TRUE)
  
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
rasters<-rast(rasters)


library(terra)
library(viridis)


val_range <- range(values(rasters), na.rm = TRUE)

par(mfrow = c(4, 3),
    mar = c(2, 2, 3, 2),
    oma = c(0, 0, 4, 0))

month_names <- month.abb

for (i in 1:12) {
  plot(rasters[[i]], col = viridis(30), zlim = val_range,
       main = paste0("R",dry_threshold,"mm(1951)-", month_names[i]),
       axes = FALSE, box = FALSE)
}
mtext("Monthly R",dry_threshold,"nn— Year 1951",
      outer = TRUE, cex = 1.5, font = 2)

