library(DBI)
library(RPostgres)

##specify the database parameters
dbname<-"shinydb"
host<-"localhost"
port<-5432
user<-"shinyuser"
password<-"shiny_password"

##url path for nc files
url<-"C:/Hydrology-Project/Rainfall Trend/NCDF/"

##estabilish the localhost connection
con<-dbConnect(
  RPostgres::Postgres(),
  dbname=dbname,
  host=host,
  port=5432,
  user=user,
  password=password
  
)

##create the table to store the nc files
dbExecute(con,
     "CREATE TABLE IF NOT EXISTS raster_storage (
         year INTEGER,
         filename TEXT,
         file_data BYTEA
     );
")

##loop through the all years and upload to the database
for(year in 1951:1980){
   ## read the nc file path
  file_path<-paste0(url,"/Daily_nc_",year,".nc")
  
  ##check whather nc file is exist or not
  if(file.exists(file_path)){
     ##read the file as a RAW bytes
    raw_data<-readBin(file_path,"raw",n=file.info(file_path)$size)
    
    ## prepare the query
    query<-"INSERT INTO raster_storage (year,filename,file_data) VALUES ($1,$2,$3)"
    
    ##store the nc files into the database
    dbExecute(con,query,params=list(year,basename(file_path),list(raw_data)))
    
    print(paste("Uploaded the nc file in year:",year))
  }else{
    warning(paste("File Missing:",file_path))
  }
}

##disconnect the connection
dbDisconnect(con)

query<-"SELECT file_data FROM raster_storage WHERE year=$1"
result<-dbGetQuery(con,query,params=list(1952))
result$file_data[[1]]
