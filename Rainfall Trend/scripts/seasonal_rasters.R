## -----------------------------------------------------------
## 1. PREPARATION
## -----------------------------------------------------------
library(terra)

## -----------------------------------------------------------
## 2. DEFINE SEASONS
## -----------------------------------------------------------
season_list <- list(
  "winter"       = c(1, 2, 12),
  "pre_monsoon"  = c(3, 4, 5),
  "monsoon"      = c(6, 7, 8, 9),
  "post_monsoon" = c(10, 11)
)

## -----------------------------------------------------------
## 3. CALCULATION LOOP
## -----------------------------------------------------------
for (year in 1951:1980) {
  
  # --- Dynamic File Paths ---
  url   <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/Daily_nc_", year, ".nc")
  url_1 <- paste0("C:/Hydrology-Project/Rainfall Trend/CSV files/drf_", year, "_new2.csv")
  
  # Check if files exist to avoid crashing
  if(!file.exists(url) || !file.exists(url_1)) {
    warning(paste("Files missing for year:", year, "- Skipping."))
    next
  }
  
  cat("\nProcessing Year:", year, "-------------------------\n")
  
  r <- rast(url)
  points <- read.csv(url_1)
  pts <- vect(points, geom=c("lon","lat"), crs = crs(r))
  
  # Extract values
  rain_values <- terra::extract(r, pts)
  rain_values <- rain_values[,-1] 
  
  # --- FIX: Date Sequence (Corrected Parentheses) ---
  dates  <- seq(from = as.Date(paste0(year, "-01-01")), 
                to   = as.Date(paste0(year, "-12-31")), 
                by   = "day")
  months <- as.numeric(format(dates, "%m"))
  
  # Loop through seasons
  for (season_name in names(season_list)) {
    
    cat("  > Season:", season_name, "...")
    
    # Subset data
    season_data <- rain_values[, which(months %in% season_list[[season_name]]), drop = FALSE]
    
    # Calculate Total
    season_total <- rowSums(season_data, na.rm = TRUE)
    
    # Rasterize
    pts$precip_val <- season_total
    season_raster <- rasterize(pts, r[[1]], field = "precip_val", fun = mean)
    
    # --- FIX: Dynamic Save Directory & Creation ---
    # Example: C:/.../seasonal_rasters/winter/
    save_dir <- paste0("C:/Hydrology-Project/Rainfall Trend/seasonal_rasters/seasonal_precip_",year,"/",season_name, "/")
    
    # Create directory if it doesn't exist
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }
    
    # --- FIX: Dynamic Filename with Year ---
    # Example: Total_Precip_winter_1952.tif
    out_name <- paste0(save_dir, "Total_Precip_", season_name, "_", year, ".tif")
    
    writeRaster(season_raster, out_name, overwrite = TRUE)
    cat(" Saved.\n")
  }
}

cat("\nDone! All seasonal rasters (1951-1980) created.")
