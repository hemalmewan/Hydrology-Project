##R99pTOT indices

library(terra)

##read the annual PRCPTOT percentile value in the file
annual_PRCPTOT <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/annual_PRCPTOT.rds")
##read the R99p value in the file
R99<- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/R99_threshold.rds")



##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
##save output url
save_url<-"C:/Hydrology-Project/Rainfall Trend/indices/R99pTOT/"

r<-rast(url) ##convert raster object

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations

##calculate the indices
R99pTOT<-(100*R99)/annual_PRCPTOT

pts$R99pTOT<-R99pTOT
##rasterize output
R99pTOT_raster<-rasterize(pts,r[[1]],field="R99pTOT")

## Save raster
file_name <- paste0(save_url, "R99pTOT_1951.tif")
writeRaster(R99pTOT_raster, file_name, overwrite = TRUE)

cat("Saved:", file_name, "\n")

##plot raster file
tif.files<-list.files(save_url,pattern = "\\.tif$",full.names = TRUE)

##Load them as a SpatRaster files
rasters<-lapply(tif.files,rast)

rasters<-rast(rasters)

# Plot with scientific color scale
plot(rasters, 
     main = "R99pTOT – 1951",
     col = hcl.colors(30, "YlOrRd"))
