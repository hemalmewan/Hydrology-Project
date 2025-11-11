library(shiny)
library(shinydashboard)
library(terra)
library(tmap)
library(reshape)

tmap_mode("view")  # Enable interactive maps

ui <- dashboardPage(
  dashboardHeader(title = "Rainfall Viewer Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster Viewer", tabName = "raster", icon = icon("globe")),
      menuItem("Data Quality", tabName = "quality", icon = icon("chart-line")),
      menuItem("Climate Indice",tabName = "climate_indice",icon =icon("cloud-rain")),
      
      fileInput("ncfile", "Upload Yearly NetCDF File (.nc)", accept = ".nc"),
      fileInput("stationfile", "Upload Station Coordinates (.csv)", accept = ".csv"),
      
      radioButtons("viewType", "Select Raster Type:",
                   choices = c("Daily" = "daily", "Monthly" = "monthly"),
                   selected = "daily"),
      
      uiOutput("date_or_month_selector")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      #---- TAB 1: Raster Viewer ----
      tabItem(
        tabName = "raster",
        fluidRow(
          box(width = 12, title = "Rainfall Map", status = "primary", solidHeader = TRUE,
              tmapOutput("map", height = "800px")
          )
        )
      ),
      
      #---- TAB 2: Data Quality ----
      tabItem(
        tabName = "quality",
        fluidRow(
          box(width = 12, title ="NetCDF Metadata", verbatimTextOutput("meta"))
        ),
        fluidRow(
          box(width = 4, title ="Select Station",
              selectInput("station", "Station ID:", choices = NULL)
          ),
          box(width = 8, title ="Monthly Time Series Plot",
              plotOutput("timeseries_plot", height ="350px"))
        ),
        fluidRow(
          box(width = 12, title ="Monthly Precipitation Distribution",
              plotOutput("dist_plot", height ="350px"))
        )
      ),
      
      ##------------------------Climate Indices------------------------
      tabItem(
        tabName = "climate_indice",
        fluidRow(
          box(width =4,title ="Select Climate Index",status="primary",solidHeader =TRUE,
              selectInput("climate_index","Climate Index:",
                          choices =c("PRCPTOT","CDD","RxDday"))
              
          ),
          box(width =8,title ="Index Parameters",status ="warning",solidHeader =TRUE,
              uiOutput("index_parameters")
              )
          
        ),
        fluidRow(
           box(width =12,title ="Climate Index Result",status ="success",solidHeader = TRUE,
               plotOutput("index_plot",height ="400px")
               )
        )
        
        
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  ## ----------------------Load Raster--------------------------
  r_daily <- reactive({
    req(input$ncfile)
    rast(input$ncfile$datapath)
  })
  
  ##------------------------Load csv----------------------------
  stations<-reactive({
    req(input$stationfile)
    read.csv(input$stationfile$datapath)
    
  })
  
  ##--------------------------Extract the cordinates-----------------------------
  pts<-reactive({
    req(stations())
    vect(stations(),geom=c("lon","lat"),crs=crs(r_daily()))
  })
  
  observeEvent(stations(), {
    updateSelectInput(session, "station", choices = stations()$station_id)
  })
  
  ##-----------------------------Meta Data of the raster files-----------------------
  output$meta<-renderPrint({
    req(r_daily())
    r_daily() ##display meta data
    
  })
  
  ##--------------------------------Extract dates----------------------------------------------
  daily_dates <- reactive({
    req(r_daily())
    as.Date(time(r_daily())) ##time range Jan 01 to Dec 31
  })
  
  ##------------------------------------Aggregate to Monthly (Sum)--------------------------------
  r_monthly <- reactive({
    req(r_daily())
    r <- r_daily()
    dates <- daily_dates()
    month_group <- format(dates, "%Y-%m")
    
    r_m <- tapp(r, month_group, sum, na.rm = TRUE)
    names(r_m) <- unique(month_group)
    r_m
  })
  
  ##---------------------------------------Month Labels--------------------------------------------
  month_labels <- reactive({
    req(r_monthly())
    ym <- names(r_monthly())
    pretty_names <- format(as.Date(paste0(ym, "-01")), "%B %Y")
    names(pretty_names) <- ym 
    pretty_names
  })
  
  ##-----------------------------------------Extract Monthly Data for Stations------------------
  monthly_values<-reactive({
    req(r_monthly(),pts(),stations())
    
    vals<-extract(r_monthly(),pts()) ##return location ID and monthly columns
    vals<-vals[,-1] ##remove ID col
    
    df<-cbind(StationID=stations()$station_id,vals)
    
    df_long<-reshape::melt(df,id.vars="StationID",variable.names="Month",value.name="Rain")
    
    # Clean Month column:
    df_long$Month <- as.character(df_long$Month)         # ensure character
    df_long$Month <- gsub("^X", "", df_long$Month)       # remove 'X' prefix if present
    df_long$Month <- gsub("\\.", "-", df_long$Month)     # change 2025.01 → 2025-01
    
    # Convert to Date safely
    df_long$Month <- as.Date(paste0(df_long$Month, "-01"), format="%Y-%m-%d")
    
    df_long
  })
  
  ##-----------------------Time Series Plot for Selected Station-----------------------
  output$timeseries_plot<-renderPlot({
     req(input$station)
    df<-monthly_values()[monthly_values()$StationID==input$station,]
    
    plot(df$Month,df$Rain,type="o",pch=16,col="blue",
         xlab="Month",ylab="Rainfall (mm)",
         main=paste("Monthly Precipitation - Station:",input$station))
    
   })
  
  ##---------------------------------------Dynamic UI---------------------------------------------
  output$date_or_month_selector <- renderUI({
    req(r_daily())
    if (input$viewType == "daily") {
      selectInput("selected_day", "Select Date:", choices = as.character(daily_dates()))
    } else {
      selectInput("selected_month", "Select Month:", choices = month_labels())
    }
  })
  
  ##-----------------------------------------Render Map (tmap v4 syntax)-------------------------------
  output$map <- renderTmap({
    req(r_daily())
    
    if (input$viewType == "daily") {
      req(input$selected_day)
      idx <- which(as.character(daily_dates()) == input$selected_day)
      r_show <- r_daily()[[idx]]
      title_txt <- paste("Daily Rainfall:", input$selected_day)
      
    } else {
      req(input$selected_month)
      
      # Find the raster layer name based on selected label
      ym <- names(month_labels())[month_labels() == input$selected_month]
      r_show <- r_monthly()[[ym]]
      title_txt <- paste("Monthly Rainfall:",input$selected_month)
    }
    
    tm_shape(r_show) +
      tm_raster(
        col.scale = tm_scale(values = "-Spectral"),
        col.legend = tm_legend(title = title_txt)
      ) +
      tm_layout(legend.outside = TRUE)
  })
  
  ###------------------------------------Climate Indices------------------------
  output$index_parameters<-renderUI({
     req(input$climate_index)
    
    if(input$climate_index=="RxDday"){
       ##Rolling window input
      numericInput("rolling_window","Rolling Window(days):",value = 5,min =1,max = 10)
    }
    else if(input$climate_index %in% c("PRCPTOT","CDD")){
      ##Threshold input
      numericInput("threshold","Threshold (mm):",value =1,min =0,max =200)
      
    }
  })
  
  
}

shinyApp(ui, server)
