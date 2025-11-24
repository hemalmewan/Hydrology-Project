## -----------------------------------------------------------
## Load required libraries
## -----------------------------------------------------------
library(terra)
library(moments)

## -----------------------------------------------------------
## File paths
## -----------------------------------------------------------
url  <- "C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc"
url_1 <- "C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv"
save_url <- "C:/Hydrology-Project/Rainfall Trend/indices/CDD_1951/"

## -----------------------------------------------------------
## Read raster and points
## -----------------------------------------------------------
r <- rast(url)                    # rainfall raster (daily layers)
points <- read.csv(url_1)         # CSV of lat-lon
pts <- vect(points, geom=c("lon","lat"), crs = crs(r))

## -----------------------------------------------------------
## Extract rainfall at station locations
## -----------------------------------------------------------
rain_values <- terra::extract(r, pts)
rain_values <- rain_values[,-1]   # remove ID column returned by extract()

## -----------------------------------------------------------
## Extract dates for each raster layer
## -----------------------------------------------------------
dates<- seq(as.Date("1951-01-01"), as.Date("1951-12-31"), by = "day")
months <- as.numeric(format(dates, "%m"))


## -----------------------------------------------------------
## Define seasonal month groups
## -----------------------------------------------------------
winter_months       <- c(12, 1, 2)
pre_monsoon_months  <- c(3, 4, 5)
monsoon_months      <- c(6, 7, 8, 9)
post_monsoon_months <- c(10, 11)


## -----------------------------------------------------------
## Stats function (mean, min, max, SD, skewness, kurtosis)
## -----------------------------------------------------------
season_stats <- function(daily_data) {
  mean_val <- mean(daily_data, na.rm=TRUE)
  min_val  <- min(daily_data, na.rm=TRUE)
  max_val  <- max(daily_data, na.rm=TRUE)
  sd_val   <- sd(daily_data, na.rm=TRUE)
  skew_val <- skewness(daily_data, na.rm=TRUE)
  kurt_val <- kurtosis(daily_data, na.rm=TRUE)
  
  return(c(mean_val, min_val, max_val, sd_val, skew_val, kurt_val))
}

## -----------------------------------------------------------
## Seasonal subsets
## -----------------------------------------------------------
winter_cols       <- which(months %in% winter_months)
pre_cols          <- which(months %in% pre_monsoon_months)
monsoon_cols      <- which(months %in% monsoon_months)
post_cols         <- which(months %in% post_monsoon_months)

## -----------------------------------------------------------
## Apply statistics row-wise (each location)
## -----------------------------------------------------------
winter_stats       <- apply(rain_values[, winter_cols], 1, season_stats)
pre_monsoon_stats  <- apply(rain_values[, pre_cols], 1, season_stats)
monsoon_stats      <- apply(rain_values[, monsoon_cols], 1, season_stats)
post_monsoon_stats <- apply(rain_values[, post_cols], 1, season_stats)
annual_stats       <- apply(rain_values, 1, season_stats)

## -----------------------------------------------------------
## Prepare final output table
## -----------------------------------------------------------
location_names <- if("Location" %in% colnames(points)) points$Location else 1:nrow(points)

final_output <- data.frame(
  Location = location_names,
  
  Winter_mean  = winter_stats[1,],
  Winter_min   = winter_stats[2,],
  Winter_max   = winter_stats[3,],
  Winter_sd    = winter_stats[4,],
  Winter_skew  = winter_stats[5,],
  Winter_kurt  = winter_stats[6,],
  
  PreMonsoon_mean     = pre_monsoon_stats[1,],
  PreMonsoon_min      = pre_monsoon_stats[2,],
  PreMonsoon      = pre_monsoon_stats[3,],
  PreMonsoon      = pre_monsoon_stats[4,],
  PreMonsoon     = pre_monsoon_stats[5,],
  PreMonsoon    = pre_monsoon_stats[6,],
  
  Monsoon_mean     = monsoon_stats[1,],
  Monsoon_min      = monsoon_stats[2,],
  Monsoon_max      = monsoon_stats[3,],
  Monsoon_sd       = monsoon_stats[4,],
  Monsoon_skew     = monsoon_stats[5,],
  Monsoon_kurt     = monsoon_stats[6,],
  
  PostMonsoon_mean    = post_monsoon_stats[1,],
  PostMonsoon_min     = post_monsoon_stats[2,],
  PostMonsoon_max     = post_monsoon_stats[3,],
  PostMonsoon_sd      = post_monsoon_stats[4,],
  PostMonsoon_skew    = post_monsoon_stats[5,],
  PostMonsoon_kurt    = post_monsoon_stats[6,],
  
  Annual_mean  = annual_stats[1,],
  Annual_min   = annual_stats[2,],
  Annual_max   = annual_stats[3,],
  Annual_sd    = annual_stats[4,],
  Annual_skew  = annual_stats[5,],
  Annual_kurt  = annual_stats[6,]
)

View(final_output)

## -----------------------------------------------------------
## Save to CSV (optional)
## -----------------------------------------------------------
write.csv(final_output, paste0(save_url, "Seasonal_Stats_1951.csv"), row.names = FALSE)

## Done
print("Seasonal + annual statistics calculated successfully!")
