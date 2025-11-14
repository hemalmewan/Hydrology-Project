library(terra)

# List all daily raster files
files <- list.files("C:/Hydrology-Project/Rainfall Trend/yearly-rasters/rainfall_1951_day_tif", pattern = "\\.tif$", full.names = TRUE)

# Extract numeric day index from filenames
day_numbers <- as.numeric(gsub(".*day_([0-9]+)\\.tif$", "\\1", basename(files)))


# Order files numerically
files <- files[order(day_numbers)]

# Read them as a SpatRaster stack (one layer per day)
r_stack <- rast(files)

##assign actual dates
dates <- seq(as.Date("1951-01-01"), as.Date("1951-12-31"), by = "day")
time(r_stack) <- dates


# Write to NetCDF
writeCDF(r_stack, 
         filename = "C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_1951_daily.nc",
         overwrite = TRUE,
         varname = "precip_day",
         unit = "mm/day",
         longname = "Daily precipitation",
         compression = 4)

cat("NetCDF file created successfully!\n")



