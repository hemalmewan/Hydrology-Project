library(terra)

# Read data
rain <- read.csv("C:/UOC pdf/4th Year/DS 4007-Research/sptiao_tempo/rainFallTrend/Rainfall Input/drf_1953_new2.csv")

# Extract coordinates
coords <- rain[, c("lon", "lat")]

# Loop through each day (starting from 3rd column)
for (i in 4:ncol(rain)) {
  day_col <- names(rain)[i]
  rainfall_day <- rain[[i]]
  
  # Create spatial points
  pts <- vect(data.frame(coords, rainfall = rainfall_day), geom = c("lon", "lat"), crs = "EPSG:4326")
  
  # Create a raster grid
  r_template <- rast(ext(pts), resolution =1)  # adjust resolution if needed
  crs(r_template) <- "EPSG:4326"
  
  # Rasterize rainfall
  r_rain <- rasterize(pts, r_template, field = "rainfall", fun = mean)
  
  # Save raster
  filename <- paste0("C:/UOC pdf/4th Year/DS 4007-Research/sptiao_tempo/rainFallTrend/Rainfall Input/rainfall_1953_day_tif/rainfall_1953_day_", i - 3, ".tif")
  writeRaster(r_rain, filename, overwrite = TRUE)
  
  cat("Saved:", filename, "\n")
}



