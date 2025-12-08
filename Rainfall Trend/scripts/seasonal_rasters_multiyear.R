library(terra)

season_list <- list(
  "Winter"       = c(1, 2, 12),
  "Pre-monsoon"  = c(3, 4, 5),
  "Monsoon"      = c(6, 7, 8, 9),
  "Post-monsoon" = c(10, 11)
)

years <- 1951:1956

## ----------------------------------------------------------
## Function to process a single year (used inside lapply)
## ----------------------------------------------------------
process_year <- function(year) {
  
  nc_path <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/Daily_nc_", year, ".nc")
  
  if (!file.exists(nc_path)) {
    return(NULL)
  }
  
  ## load data
  r <- rast(nc_path)
  
  ## date sequence
  start_date <- as.Date(paste0(year, "-01-01"))
  d <- seq(start_date, by = "day", length.out = nlyr(r))
  months <- as.integer(format(d, "%m"))
  
  ## compute seasonal rasters using lapply
  season_layers <- lapply(names(season_list), function(season_name) {
    
    sel_months <- season_list[[season_name]]
    idx <- which(months %in% sel_months)
    
    if (length(idx) == 0) return(NULL)
    
    sum(r[[idx]], na.rm = TRUE)
  })
  
  ## name the list elements
  names(season_layers) <- names(season_list)
  
  ## convert to SpatRaster
  rast(season_layers)
}

## ----------------------------------------------------------
## Apply the function to all years
## ----------------------------------------------------------
seasonal_result <- lapply(years, process_year)
names(seasonal_result) <- as.character(years)

year_to_plot <- "1955"

if (!is.null(seasonal_result[[year_to_plot]])) {
  plot(
    seasonal_result[[year_to_plot]],
    main = paste("Seasonal Precipitation", year_to_plot),
    nc = 2
  )
}
