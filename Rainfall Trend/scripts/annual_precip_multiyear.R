## multi year raster viewer
library(terra)

years <- 1951:1960
annual_result <- list()  ##store annual raster for each selected year

for (year_index in seq_along(years)) {
  year <- years[year_index]
  nc_path <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/Daily_nc_", year, ".nc")
  
  ##check whether file exists or not
  if (file.exists(nc_path)) {
    ##load the raster file for each year
    r <- rast(nc_path)
    
    ## calculate annual precipitation row-sum for each location
    annual_sum <- app(r, sum, na.rm = TRUE)
    
    ## store in list
    annual_result[[as.character(year)]] <- annual_sum
  }
}

## convert list to SpatRaster collection for plotting
annual_stack <- rast(annual_result)

## plot all years
plot(
  annual_stack,
  main = paste("Annual Precipitation", names(annual_result)),
  nc = 3
)
