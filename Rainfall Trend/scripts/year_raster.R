##convert the all daily rasters into one year

library(terra)

folder_path<-"C:/Hydrology-Project/Rainfall Trend/NCDF/"
for(year in 1951:1980){
  url<-paste0(folder_path,"Daily_nc_",year,".nc")
  
  r_daily <- rast(url) ##load the nc file
  
  
  r_annual <- sum(r_daily, na.rm = TRUE)
  output_dir<-"C:/Hydrology-Project/Rainfall Trend/year_data/"
  
  # Save to disk
  writeRaster(r_annual, filename = paste0(output_dir,"Annual_Precip_",year,".tif"), overwrite = TRUE)
}

