library(DBI)
library(RPostgres)


##Define the Database connection parameters
dbname<-"shinydb"
host<-"localhost"
port<-5432
user<-"shinyuser"
password<-"shiny_password"

##nc files local folder path
base_url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/"

##Establish Connection
con<-dbConnect(
  RPostgres::Postgres(),
  dbname=dbname,
  host=host,
  port=port,
  user=user,
  password=password
)


#drop the existing raster storage table from the databse
dbExecute(con,"DROP TABLE IF EXISTS raster_storage;")

##create the databse table
dbExecute(con,
        "CREATE TABLE IF NOT EXISTS raster_storage(
            country TEXT,
            year INTEGER,
            filename TEXT,
            file_data BYTEA,
            PRIMARY KEY (country,year)
        )"  
)

##define all the countries
countries<-c("india","ghana","ethiopia")


##loop through all the countries
for(country in countries){
  ##full path to the country folder
  country_path<-file.path(base_url,country)
  
  ##check whether folder exist or not
  if(dir.exists(country_path)){
    
    ##fetch all the nc files from this folder
    nc_files<-list.files(country_path, pattern = "^Daily_nc_[0-9]{4}\\.nc$", full.names = TRUE)
    
    print(paste("Processing country:",country,"-Found",length(nc_files),"files."))
    
    ##loop through each nc file for selected country
    for (file_path in nc_files) {
      ##extract the filename and year
      fname<-basename(file_path)
      
      ##extract the specific year from the filename
      year_val<-as.integer(gsub("\\D","",fname))
      
      ##read file as RAW binary
      raw_data<-readBin(file_path,"raw",n=file.info(file_path)$size)
      
      ##prepare query
      query<-"INSERT INTO raster_storage (country,year,filename,file_data) VALUES ($1,$2,$3,$4)"
      
      ##Execute Insert usign trycatch 
      tryCatch({
        dbExecute(con,query,params=list(country,year_val,fname,list(raw_data)))
        print(paste("Uploaded:",country,year_val))
      },error=function(e){
          warning(paste("Failed to upload:",fname,"-",e$message))
        }
      
      )
      
    }
                         
  }else{
   warning(paste("Folder not found for:",country,"at",country_path))
  }
}

##disconnect the database connection
dbDisconnect(con)
##prompt massage 
print("Upload process completed!!!!!!!!!!!")


query<-"SELECT file_data FROM raster_storage WHERE year=$1"
result<-dbGetQuery(con,query,params=list(1952))
result




