## multi year raster viewer
library(terra)

years <- 1951:1956
monthly_result <- list()   ## store monthly rasters for each year

for (year_index in seq_along(years)) {
  year <- years[year_index]
  nc_path <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/Daily_nc_", year, ".nc")
  
  ## check whether file exists or not
  if (file.exists(nc_path)) {
    
    ## load the raster file for each year
    r <- rast(nc_path)
    
    ## create date sequence for the layers
    start_date <- as.Date(paste0(year, "-01-01"))
    d <- seq(start_date, by = "day", length.out = nlyr(r))
    
    ## get month as numeric (01–12)
    months <- format(d, "%m")
    
    ## calculate monthly sums (row-sum across layers)
    monthly_sum <- tapp(r, months, fun = sum, na.rm = TRUE)
    
    ## store in list
    monthly_result[[as.character(year)]] <- monthly_sum
  }
}

## -------------------------------
## Example: plot a single year's monthly maps
## -------------------------------
year_to_plot <- "1953"

if (year_to_plot %in% names(monthly_result)) {
  plot(
    monthly_result[[year_to_plot]],
    main = paste("Monthly Precipitation", year_to_plot),
    nc = 4
  )
}

