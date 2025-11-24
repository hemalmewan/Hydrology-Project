library(terra)
library(reshape2)
library(ggplot2)

##-------------------------------
## File paths
##-------------------------------
nc_path <- "C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
csv_path <- "C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"

##-------------------------------
## Load raster + stations
##-------------------------------
r <- rast(nc_path)
points <- read.csv(csv_path)

# Extract year from filename (automatic)
year <- 1951    # you can replace this later with automatic parsing

##-------------------------------
## Create date sequence for the year
##-------------------------------
dates <- seq(
  as.Date(paste0(year, "-01-01")),
  as.Date(paste0(year, "-12-31")),
  by = "day"
)

# Rename raster layers with dates
names(r) <- as.character(dates)



##-------------------------------
## Convert CSV points to spatial
##-------------------------------
pts <- vect(points, geom=c("lon","lat"), crs=crs(r))

##-------------------------------
## Extract daily precipitation at all stations
##-------------------------------
vals <- extract(r, pts)

##-------------------------------
## Melt into long format
##-------------------------------
vals_long <- melt(
  vals[1,],
  id.vars = "ID",
  measure.vars = day_cols,
  variable.name = "date",
  value.name = "precipitation"
)

vals_long

## Convert date column
vals_long$date <- as.Date(vals_long$date)

## Optionally merge station names (if exists)
if("station_id" %in% names(points)) {
  vals_long$station_id <- points$station_id[match(vals_long$ID, seq_len(nrow(points)))]
} else {
  vals_long$station_id <- paste0("Station_", vals_long$ID)
}

##-------------------------------
## Yearly time-series plot per station
##-------------------------------
ggplot(vals_long, aes(x = date, y = precipitation)) +
  geom_line(color="blue") +
  facet_wrap(~ station_id, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(
    title = paste("Yearly Daily Precipitation -", year),
    x = "Date",
    y = "Daily Precipitation (mm)"
  )


####------------------------------------------Monthly Distribution of each location--------------------------------
## Identify daily bands for this month
monthly_index <- which(format(dates, "%m") == sprintf("%02d",1))
monthly_precipitation <- r[[monthly_index]]



## Extract precipitation at stations
vals <- extract(monthly_precipitation, pts)
vals <- vals[,-1, drop = FALSE]  # remove ID column, keep matrix




