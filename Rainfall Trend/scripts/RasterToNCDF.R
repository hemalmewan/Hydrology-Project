library(terra)

# Loop through all years 1951 to 1981
for (year in 1951:1981) {
  
  # Define raster folder
  folder <- paste0("C:/Hydrology-Project/Rainfall Trend/yearly-rasters/rainfall_", year, "_day_tif/")
  
  # List all daily raster files
  files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Extract numeric day index
  day_numbers <- as.numeric(gsub(".*day_([0-9]+)\\.tif$", "\\1", basename(files)))
  
  # Order files numerically
  files <- files[order(day_numbers)]
  
  # Read all rasters as a SpatRaster
  r_stack <- rast(files)
  
  # Create correct date sequence
  dates <- seq(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")), by="day")
  
  # Write NetCDF — include time here, not in SpatRaster
  out_nc <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/rainfall_", year, "_daily.nc")
  
  writeCDF(r_stack,
           filename = out_nc,
           overwrite = TRUE,
           varname = "Day",
           unit = "mm/day",
           longname = "Daily precipitation",
           time = dates,
           tname = "time",
           tunits = "days since 1950-01-01",
           compression = 4)
  
  cat("Created NetCDF for year:", year, "\n")
}

cat("All NetCDF files created successfully!\n")

