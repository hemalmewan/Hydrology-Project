library(terra)


##loop through all the years (i.e 1981 to 2024)

for (year in 1981:2024) {
  # 1. Define folder and list files
  folder <- paste0("C:/Hydrology-Project/PCP_data_ghana_ethi/PCP_Ghana/",year,"/")
  files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  
  
  # 2. Load all layers initially (Result: 730 layers)
  r_temp <- rast(files)
  
  
  # 3. FIX: SUBSET TO KEEP ONLY BAND 1
  # Keep odd layers (1, 3, 5...) to drop the metadata band
  indices <- seq(1, nlyr(r_temp), by = 2) 
  r_final <- r_temp[[indices]]            
  
  
  # 4. Define the output path
  out_nc <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/ghana/Daily_nc_",year,".nc")
  
  # Ensure the directory actually exists before writing
  dir_path <- dirname(out_nc)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # 5. Define Dates
  dates <- seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), by="day")
  
  
  # 6. Write NetCDF (Pass 'time' HERE instead of modifying r_final)
  writeCDF(r_final, 
           filename = out_nc, 
           overwrite = TRUE, 
           varname = "precip", 
           unit = "mm", 
           longname = "Daily precipitation", 
           compression = 4,
           time = dates)  # <--- This is the fix!

  cat(paste0("Success! Created 365-layer NetCDF for ",year))
  
}

cat("Finish Processing all the year!!!!!!!!!!")



