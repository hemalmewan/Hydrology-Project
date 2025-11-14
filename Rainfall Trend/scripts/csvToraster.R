library(terra)

# Read data
rain <- read.csv("C:/Hydrology-Project/Rainfall Trend/CSV files/drf_1951_new2.csv")

# Extract coordinates
coords <- rain[, c("lon", "lat")]


# Loop through each day (starting from 3rd column)
for (i in 4:ncol(rain)) {
  day_col <- names(rain)[i]
  rainfall_day <- rain[[i]]
  
  # Create spatial points
  pts <- vect(data.frame(coords, rainfall = rainfall_day), geom = c("lon", "lat"), crs = "EPSG:4326")
  
  # Create a raster grid
  r_template <- rast(ext(pts), resolution =0.25)  # adjust resolution if needed
  crs(r_template) <- "EPSG:4326"
  
  # Rasterize rainfall
  r_rain <- rasterize(pts, r_template, field = "rainfall", fun = mean)
  
  # Save raster
  filename <- paste0("C:/Hydrology-Project/Rainfall Trend/yearly-rasters/rainfall_1951_day_tif/rainfall_1951_day_", i - 3, ".tif")
  writeRaster(r_rain, filename, overwrite = TRUE)
  
  cat("Saved:", filename, "\n")
}


##path of the nc file
url<-"C:/Hydrology-Project/Rainfall Trend/yearly-rasters/rainfall_1951_day_tif/rainfall_1951_day_1.tif"

r<-rast(url)

precip<-values(r)


precip

# Plot with scientific color scale
plot(r, 
     main = "Consecutive Dry Days (CDD) – 1951",
     col = hcl.colors(30, "YlOrRd"))
