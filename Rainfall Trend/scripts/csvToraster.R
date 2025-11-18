library(terra)

## Loop through the years 1954 to 1981
for (year in 1954:1981) {
  
  # Read data
  url <- paste0("C:/Hydrology-Project/Rainfall Trend/CSV files/drf_", year, "_new2.csv")
  rain <- read.csv(url)
  
  # Extract coordinates
  coords <- rain[, c("lon", "lat")]
  
  # Create output directory for this year
  out_dir <- paste0("C:/Hydrology-Project/Rainfall Trend/yearly-rasters/rainfall_", year, "_day_tif")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # Loop through each day (starting from 4th column)
  for (i in 4:ncol(rain)) {
    
    day_col <- names(rain)[i]
    rainfall_day <- rain[[i]]
    
    # Create spatial points
    pts <- vect(data.frame(coords, rainfall = rainfall_day),
                geom = c("lon", "lat"), 
                crs = "EPSG:4326")
    
    # Create a raster template
    r_template <- rast(ext(pts), resolution = 0.25)
    crs(r_template) <- "EPSG:4326"
    
    # Rasterize rainfall
    r_rain <- rasterize(pts, r_template, field = "rainfall", fun = mean)
    
    # Save raster
    filename <- paste0(out_dir, "/rainfall_", year, "_day_", i - 3, ".tif")
    writeRaster(r_rain, filename, overwrite = TRUE)
    
    cat("Saved:", filename, "\n")
  }
}



##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/yearly-rasters/rainfall_1954_day_tif/rainfall_1954_day_1.tif"

r<-rast(url)

time(r)

precip<-values(r)


precip


# Plot with scientific color scale
plot(r, 
     main = "Consecutive Dry Days (CDD) – 1954",
     col = hcl.colors(30, "YlOrRd"))
