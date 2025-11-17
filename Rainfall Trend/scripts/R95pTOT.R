##R95pTOT indices

library(terra)

##read the annual PRCPTOT percentile value in the file
annual_PRCPTOT <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/annual_PRCPTOT.rds")
##read the R95p value in the file
R95p <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/R95_threshold.rds")

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/R95pTOT/"

r<-rast(url) ##convert raster object

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations

##calculate the indices
R95pTOT<-(100*R95p)/annual_PRCPTOT

pts$R95pTOT<-R95pTOT
##rasterize output
R95pTOT_raster<-rasterize(pts,r[[1]],field="R95pTOT")

## Save raster
file_name <- paste0(save_url, "R95pTOT_1951.tif")
writeRaster(R95pTOT_raster, file_name, overwrite = TRUE)

cat("Saved:", file_name, "\n")

##plot raster file
tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)

rasters<-rast(rasters)

# Plot with scientific color scale
plot(rasters, 
     main = "R95pTOT – 1951",
     col = hcl.colors(30, "YlOrRd"))
