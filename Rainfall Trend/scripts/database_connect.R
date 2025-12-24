library(RPostgreSQL)
library(DBI)

##specify the parameters
dbname<-"shinydb"
host<-"localhost"
port<-5432
user<-"shinyuser"
password<-"shiny_password"

##url path for the csv files
url<-"C:/Hydrology-Project/Rainfall Trend/CSV files"

##connect to the postgresql database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname=dbname,
  host=host,
  port=port,
  user=user,
  password=password
)



##loop through the each year
for (year in 1951:2007) {
  ##file path to  the csv file
  file_path<-paste0(url,"/drf_",year,"_new2.csv")
  
  ##check the csv exists or not
  if(file.exists(file_path)){
    
    ##read the csv file
    data<-read.csv(file_path)
    
    ##write the csv file to the SQL table
    table_name<-paste0("Daily_precipitation_",year)
    ##write to the database
    dbWriteTable(con,table_name,data,overwrite=TRUE,row.names=FALSE)
    
    print(paste("Successfully uploaded:",table_name))
  }else{
    warning(paste("File not found:",file_path))
  }
 
  
}



