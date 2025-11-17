library(terra)

wet_threshold <- 1 ##threshold value

# Load station info
points <- read.csv("C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv")
pts <- vect(points, geom=c("lon","lat"), crs="EPSG:4326")

# Initialize single dimensional outputs
total_wet_days_30yr <- 0              # store the sum of all the wet days across 30 years
wet_values_30yr <- numeric(0)         # store the precipitaion values across 30 years

# Your function (unchanged)
percentile <- function(daily_precip, threshold) {
  wet_day_count  <- sum(as.numeric(daily_precip) >= threshold)
  wet_values     <- daily_precip[as.numeric(daily_precip) >= threshold]
  
  return(list(
    wet_day_count = wet_day_count,
    wet_values    = wet_values
  ))
}

# Loop through all years
for (year in 1951:1980) {
  
  cat("Processing year:", year, "\n")
  
  # Load rainfall netCDF for the year
  ncfile <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_", year, "_daily.nc")
  r <- rast(ncfile)
  
  # Extract rainfall for all stations
  rain_values <- extract(r, pts)
  rain_values <- rain_values[, -1]   # remove ID column
  
  # Apply your function for each station
  res <- apply(rain_values, 1, percentile, threshold = wet_threshold)
  
  # Sum wet days for all stations for this year
  total_wet_days_30yr <- total_wet_days_30yr +
    sum(sapply(res, function(x) x$wet_day_count))
  
  # Collect ALL wet-day rainfall values from ALL stations
  wet_values_30yr <- c(
    wet_values_30yr,
    unlist(lapply(res, function(x) x$wet_values))
  )
}

cat("Finished 30-year wet-day accumulation.\n")

##calculate the 95th percentile for the baseline time period(30 years)
p95 <- quantile(wet_values_30yr, 0.95, na.rm = TRUE)

##calculate the 99th percentile for the baseline time period(30 years)
p99<-quantile(wet_values_30yr,0.99,na.rm=TRUE)


# Save to files
saveRDS(p95, "C:/Hydrology-Project/Rainfall Trend/scripts/p95_threshold.rds")
saveRDS(p99, "C:/Hydrology-Project/Rainfall Trend/scripts/p99_threshold.rds")
