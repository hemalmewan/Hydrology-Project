## R pipeline for each climate indices

##import required libraries
library(terra)
library(zoo)


##input the year
year<-as.integer(readline("Enter Year (e.g, 1951) :"))
cat("Selected Year:", year, "\n")

##input rolling window
d <- as.integer(readline("Enter Rolling Window n (1-10): "))
stopifnot(d >= 1 & d <= 10)

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/"
##output directory
output_dir <- "C:/Hydrology-Project/Rainfall Trend/indices/"

## Build file names dynamically based on year
nc_file <- paste0(url, "rainfall_", year, "_daily.nc")
station_csv <- paste0(url_1, "drf_", year, "_new2.csv")
dir.create(output_dir, showWarnings = FALSE)



## --- LOAD DATA ---
r<-rast(nc_file) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31


points<-read.csv(station_csv) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations

threshold <- 1   # rainfall threshold for PRCPTOT & CDD

##define the customize function for CDD climate indices
compute_CDD <- function(){
  vals <- extract(r, pts)[,-1]
  CDD <- function(x){
    dry <- as.numeric(x) < threshold
    dry[is.na(dry)]<-FALSE ##handle missing values
    
    if(all(!dry)) return(0) ##if no rainy days
    runs <- rle(dry)
    max(runs$lengths[runs$values])
  }
  cdd_vals <- apply(vals, 1, CDD)
  pts$CDD <- cdd_vals
  
  cdd_dir <- paste0(output_dir, "CDD_", year, "/")
  dir.create(cdd_dir, showWarnings = FALSE)
  
  fname <- paste0(cdd_dir, "CDD_", year, ".tif")
  writeRaster(rasterize(pts, r[[1]], field="CDD"), fname, overwrite=TRUE)
  cat("Saved:", fname, "\n")
}

##define the customize function for PRCPTOT climate indices
compute_PRCPTOT <- function() {
  prcp_dir <- paste0(output_dir, "PRCPTOT_", year, "/")
  dir.create(prcp_dir, showWarnings = FALSE)
  
  for(month in 1:12){
    monthly_index <- which(format(dates, "%m") == sprintf("%02d", month))
    vals <- extract(r[[monthly_index]], pts)[,-1, drop=FALSE]
    prcptot <- apply(vals, 1, function(x) sum(x[x >= threshold], na.rm = TRUE))
    
    pts$PRCPTOT <- prcptot
    PRCPTOT_raster <- rasterize(pts, r[[1]], field="PRCPTOT")
    fname <- paste0(prcp_dir, "PRCPTOT_", year, "_", sprintf("%02d", month), ".tif")
    writeRaster(PRCPTOT_raster,fname, overwrite=TRUE)
    cat("Saved:", fname, "\n")
  }
}


##define the customize function for Rxdday climate indices
compute_Rxdday <- function(d){
  rx_dir <- paste0(output_dir, "Rx", d, "day_", year, "/")
  dir.create(rx_dir, showWarnings = FALSE)
  
  for(month in 1:12){
    monthly_index <- which(format(dates, "%m") == sprintf("%02d", month))
    vals <- as.matrix(extract(r[[monthly_index]], pts)[,-1,drop=FALSE])
    
    if(ncol(vals) >= d){
      roll_sum <- t(apply(vals, 1, function(x) rollapply(x, width=d, sum, align="left", na.rm=TRUE)))
      rx_vals <- apply(roll_sum, 1, max, na.rm=TRUE)
    } else {
      rx_vals <- rep(NA, nrow(vals))
    }
    
    pts$Rxdday <- rx_vals
    out_r <- rasterize(pts, r[[1]], field="Rxdday")
    fname <- paste0(rx_dir, "Rx", d, "day_", year, "_", sprintf("%02d", month), ".tif")
    writeRaster(out_r, fname, overwrite=TRUE)
    cat("Saved:", fname, "\n")
  }
}

##----------------------RUN PIPELINE---------------------
compute_PRCPTOT()
compute_Rxdday(d)
compute_CDD()

cat("\n Pipeline Completed Successfully.\n")





####--------------------------------------PLOT THESE INDICES----------------------------------
library(tmap)
##-------------------------RXDDAY-----------------------------------

index <- "Rx1day"
for(month in 1:12){
  file <- paste0(output_dir, index, "_", year, "/", index, "_", year, "_", sprintf("%02d", month), ".tif")
  r <- rast(file)
  
  print(
    tm_shape(r) +
      tm_raster(palette = "-Spectral", title = paste0(index, " (", month.abb[month], " ", year, ")")) +
      tm_layout(legend.outside = TRUE)
  )
}


##----------------------------PRCPTOT----------------------------------------------
index<-"PRCPTOT"
for(month in 1:12){
  file <- paste0(output_dir, index, "_", year, "/", index, "_", year, "_", sprintf("%02d", month), ".tif")
  r <- rast(file)
  
  print(
    tm_shape(r) +
      tm_raster(palette = "-Spectral", title = paste0(index, " (", month.abb[month], " ", year, ")")) +
      tm_layout(legend.outside = TRUE)
  )
}


##----------------------------CDD---------------------------------------------------
index<-"CDD"
file <- paste0(output_dir, index, "_", year, "/", index, "_", year, ".tif")
r <- rast(file)
  
print(
    tm_shape(r) +
      tm_raster(palette = "-Spectral", title = paste0(index,"(",year, ")")) +
      tm_layout(legend.outside = TRUE)
)


