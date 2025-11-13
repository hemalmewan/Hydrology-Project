library(terra)
library(reshape2)
library(ggplot2)


##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
##path of the csv file 
url_1<-"C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"


r<-rast(url) ##convert raster object
dates<-as.Date(time(r)) ##time range 1951-01-01 to 1951-12-31

points<-read.csv(url_1) ##read csv file 
pts<-vect(points,geom=c("lon","lat"),crs=crs(r)) ##convert the lat lon as the spatial locations


monthly_index<-which(format(dates,"%m")==sprintf("%02d",1))
monthly_precipitation<-r[[monthly_index]]

##Daily precipitation data in each month each location
vals<-extract(monthly_precipitation,pts)

# Get all columns except ID
day_cols <- setdiff(names(vals), "ID")

# Extract the numeric part of column names
day_nums <- as.numeric(gsub("[^0-9]", "", day_cols))  # removes everything except digits

vals_long <- melt(vals, id.vars="ID", measure.vars=day_cols,
                  variable.name="day", value.name="precipitation")

# Assign the correct numeric day
vals_long$day <- rep(day_nums, each = nrow(vals))

# Create proper dates for January
vals_long$date <- as.Date(vals_long$day - 1, origin="1951-01-01")



# Boxplot per location
ggplot(vals_long, aes(x=factor(ID), y=precipitation)) +
  geom_boxplot(fill="lightblue") +
  labs(x="Location ID", y="Daily Precipitation (mm)", title="January Daily Precipitation") +
  theme_minimal()

# Time series per location
ggplot(vals_long, aes(x=date, y=precipitation, color=factor(ID))) +
  geom_line() +
  geom_point() +
  labs(x="Date", y="Daily Precipitation (mm)", title="January Daily Precipitation Time Series") +
  theme_minimal()
