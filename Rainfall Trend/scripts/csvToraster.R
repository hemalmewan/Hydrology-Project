library(sp)
library(raster)
library(viridis)

# Function to check if a year is a leap year
is_leap_year <- function(year) {
  return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
}

# Function to get number of days in a year
days_in_year <- function(year) {
  return(ifelse(is_leap_year(year), 366, 365))
}



# Base directory for CSV files and output
base_csv_dir <- "C:/Hydrology-Project/Rainfall Trend/CSV files/"
base_out_dir <- "C:/Hydrology-Project/Rainfall Trend/yearly-rasters/"

# Loop through each year
for(year in 1951:1980) {
  
  cat("\n========================================\n")
  cat("Processing Year:", year, "\n")
  cat("========================================\n")
  
  # Construct CSV filename (adjust pattern to match your files)
  csv_file <- paste0(base_csv_dir, "drf_", year, "_new2.csv")
  
  # Check if file exists
  if(!file.exists(csv_file)) {
    cat("WARNING: File not found:", csv_file, "\n")
    next
  }
  
  # Read CSV
  data <- read.csv(csv_file)
  
  # Convert to spatial points
  coordinates(data) <- ~lon+lat
  proj4string(data) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Extract coordinates
  coords <- coordinates(data)
  
  # Identify all Day columns
  day_cols <- grep("^Day_", names(data), value = TRUE)
  n_days <- length(day_cols)
  
  # Expected number of days for this year
  expected_days <- days_in_year(year)
  
  cat("Year", year, "is", ifelse(is_leap_year(year), "a LEAP year", "a regular year"), "\n")
  cat("Expected days:", expected_days, "| Found days:", n_days, "\n")
  
  # Create output directory for this year
  outdir <- paste0(base_out_dir, "Daily_Rasters_", year, "/")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  # Loop through each day
  for(i in 1:n_days) {
    
    day_col <- day_cols[i]
    
    # Extract day number
    day_num <- as.numeric(sub("Day_", "", day_col))
    
    # Create spatial points for this day
    data_day <- data.frame(lon = coords[,1], 
                           lat = coords[,2], 
                           rainfall = data[[day_col]])
    
    # Remove NA values
    data_day <- data_day[complete.cases(data_day), ]
    
    # Skip if no valid data
    if(nrow(data_day) == 0) {
      cat("  WARNING: No valid data for", day_col, "\n")
      next
    }
    
    # Convert to spatial points
    coordinates(data_day) <- ~lon+lat
    proj4string(data_day) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # Create SpatialPixelsDataFrame
    spdf <- SpatialPixelsDataFrame(points = data_day, 
                                   data = data.frame(rainfall = data_day$rainfall), 
                                   tolerance = 0.25)
    
    # Convert to raster
    r <- raster(spdf)
    crs(r) <- crs(spdf)
    
    # Create filename
    filename <- sprintf("%sDay_%03d_%d.tif", outdir, day_num, year)
    
    # Save raster
    writeRaster(r, filename, overwrite = TRUE)
    
    # Progress indicator (every 30 days)
    if(day_num %% 30 == 0) {
      cat("  Processed", day_num, "days...\n")
    }
  }
  
  cat("\nYear", year, "completed! Processed", n_days, "days\n")
  cat("Rasters saved to:", outdir, "\n")
}

cat("\n========================================\n")
cat("ALL YEARS PROCESSED SUCCESSFULLY!\n")
cat("========================================\n")





