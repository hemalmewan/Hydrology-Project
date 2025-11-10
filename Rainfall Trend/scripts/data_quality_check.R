### Check the quality of the yearly climate raster data

##input the year
year<-as.integer(readline("Enter Year (e.g, 1951) :"))
cat("Selected Year:", year, "\n")

##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/"

## Build file names dynamically based on year
nc_file <- paste0(url, "rainfall_", year, "_daily.nc")

## --- LOAD DATA ---
r<-rast(nc_file) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31


##---------------------------------------Count the number of raster layers-------------------------------
print(nlyr(r))

##---------------------------------------Check the missing values per raster layer----------------------------------------------
na_count<-sapply(1:nlyr(r), function(i){
    sum(is.na(values(r[[i]])))
  
})

summary(na_count)

##--------------------------------------ensure all raster layers has same extent and resolution---------------------------------

##check the extent
extent_check<-all(sapply(1:nlyr(r), function(i) ext(r[[i]])==ext(r[[i]])))
print(extent_check) # TRUE means all layers have same extent

##check the resolution
res_check<-all(sapply(1:nlyr(r),function(i) res(r[[i]])==res(r[[i]])))
print(res_check) # TRUE means all layers have same cell size


##--------------------------------------------------check the Outliers and Summary Stat----------------------------------------------------------

##check the summary statistics
stats<-sapply(1:nlyr(r),function(i){
  vals<-values(r[[i]])
  c(min=min(vals,na.rm=TRUE),
    max=max(vals,na.rm=TRUE),
    mean=mean(vals,na.rm=TRUE))
})

summary_stat<-t(stats)

head(summary_stat)

##check outliers
suspicious_cell<-sapply(1:nlyr(r), function(i){
  vals<-values(r[[i]])
  which(vals<0) ## negative
  
})

# Count suspicious cells per day
sapply(suspicious_cell, length)

##----------------------------------------------------Meta Data---------------------------
print(r)


