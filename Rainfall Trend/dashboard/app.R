library(shiny)
library(shinydashboard)
library(terra)
library(tmap)
library(reshape)
library(zoo)

tmap_mode("view")  # Enable interactive maps

ui <- dashboardPage(
  dashboardHeader(title = "Rainfall Viewer Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster Viewer", tabName = "raster", icon = icon("globe")),
      menuItem("Data Quality", tabName = "quality", icon = icon("chart-line")),
      menuItem("Climate Indice", tabName = "climate_indice", icon = icon("cloud-rain")),
      
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
          box(width = 4, title = "Select Climate Index", status = "primary", solidHeader = TRUE,
              selectInput("climate_index", "Climate Index:",
                          choices = c("PRCPTOT", "CDD", "RxDday")),
              
              uiOutput("index_parameters"),
              uiOutput("index_month_selector"),
              actionButton("compute_index", "Compute Index", icon = icon("cogs")),
              downloadButton("download_index", "Download Result")
          ),
          box(width = 8, title = "Climate Index Result", status = "success", solidHeader = TRUE,
              plotOutput("index_plot", height = "600px")
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
  stations <- reactive({
    req(input$stationfile)
    read.csv(input$stationfile$datapath)
  })
  
  ##--------------------------Extract the coordinates-----------------------------
  pts <- reactive({
    req(stations())
    vect(stations(), geom = c("lon", "lat"), crs = crs(r_daily()))
  })
  
  observeEvent(stations(), {
    updateSelectInput(session, "station", choices = stations()$station_id)
  })
  
  ##-----------------------------Meta Data of the raster files-----------------------
  output$meta <- renderPrint({
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
  monthly_values <- reactive({
    req(r_monthly(), pts(), stations())
    
    vals <- extract(r_monthly(), pts()) ##return location ID and monthly columns
    vals <- vals[,-1] ##remove ID col
    
    df <- cbind(StationID = stations()$station_id, vals)
    
    df_long <- reshape::melt(df, id.vars = "StationID", variable.names = "Month", value.name = "Rain")
    
    # Clean Month column:
    df_long$Month <- as.character(df_long$Month)
    df_long$Month <- gsub("^X", "", df_long$Month)
    df_long$Month <- gsub("\\.", "-", df_long$Month)
    df_long$Month <- as.Date(paste0(df_long$Month, "-01"), format = "%Y-%m-%d")
    
    df_long
  })
  
  ##-----------------------Time Series Plot for Selected Station-----------------------
  output$timeseries_plot <- renderPlot({
    req(input$station)
    df <- monthly_values()[monthly_values()$StationID == input$station,]
    
    plot(df$Month, df$Rain, type = "o", pch = 16, col = "blue",
         xlab = "Month", ylab = "Rainfall (mm)",
         main = paste("Monthly Precipitation - Station:", input$station))
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
      ym <- names(month_labels())[month_labels() == input$selected_month]
      r_show <- r_monthly()[[ym]]
      title_txt <- paste("Monthly Rainfall:", input$selected_month)
    }
    
    tm_shape(r_show) +
      tm_raster(
        col.scale = tm_scale(values = "-Spectral"),
        col.legend = tm_legend(title = title_txt)
      ) +
      tm_layout(legend.outside = TRUE)
  })
  
  ###------------------------------------Climate Indices------------------------
  output$index_parameters <- renderUI({
    req(input$climate_index)
    
    if (input$climate_index == "RxDday") {
      numericInput("rolling_window", "Rolling Window (days):", value = 5, min = 1, max = 10)
    } else if (input$climate_index %in% c("PRCPTOT", "CDD")) {
      numericInput("threshold", "Threshold (mm):", value = 1, min = 0, max = 200)
    }
  })
  
  ####------------------------------------Calculate Climate Indices-------------------------------
  indices_calculate <- eventReactive(input$compute_index, {
    req(r_daily(), input$climate_index, pts(), daily_dates())
    
    r <- r_daily() ##daily rasters
    points <- pts() ##coordinates for each location
    dates <- daily_dates() ##dates Jan 01 to Dec 31
    
    month_group <- format(dates, "%Y-%m")
    unique_months <- unique(month_group)
    result_list <- list()
    
    ##----------------------PRCPTOT--------------------------
    
    if (input$climate_index == "PRCPTOT") {
      req(input$threshold)
      threshold <- as.numeric(input$threshold)

      
      for (m in unique_months) {
        month_idx <- which(month_group == m)
        month_r <- r[[month_idx]]
        vals <- extract(month_r, points)
        vals <- vals[, -1, drop = FALSE]
        PRCPTOT_vals <- apply(vals, 1, function(x) sum(x[x >= threshold], na.rm = TRUE))
        points$PRCPTOT <- PRCPTOT_vals
        template <- month_r[[1]]
        PRCPTOT_r <- rasterize(points, template, field = "PRCPTOT")
        names(PRCPTOT_r) <- m
        result_list[[m]] <- PRCPTOT_r
      }
      PRCPTOT_stack <- rast(result_list)
      names(PRCPTOT_stack) <- unique_months
      return(PRCPTOT_stack)
    }
    
    ##----------------------CDD------------------------------
    else if(input$climate_index=="CDD"){
      req(input$threshold)
      threshold <- as.numeric(input$threshold)
      
      ##Extract daily rainfall at point
      rain_values<-terra::extract(r,points)
      ##Remove ID column
      rain_values<-rain_values[,-1]
      
      ##define the customize function
      CDD<-function(daily_precip,threshold=1){
        dry<-as.numeric(daily_precip)<threshold
        dry[is.na(dry)]<-FALSE
        
        if(all(!dry)) return(0) ##no dry days
        rle_dry<-rle(dry)
        max_cdd<-max(rle_dry$lengths[rle_dry$values])
        
        return(max_cdd)
      }
      
      ##Apply above function for each station
      cdd_values<-apply(rain_values,1,CDD,threshold=threshold)
      
      points$CDD<-cdd_values
      ##rasterize output
      CDD_raster<-rasterize(points,r[[1]],field="CDD")
      names(CDD_raster)<-"CDD"
      return(CDD_raster)
    }
    
    ##---------------------------------------Rxdday----------------------------------
    else if(input$climate_index=="RxDday"){
         req(input$rolling_window)
         roll_window<-as.numeric(input$rolling_window)
      
         
         ##loop through all the months January to December
         for (month in unique_months) {
           ##define end date and start date for each month
           monthly_index<-which(month_group==month)
           
           
           ##Daily precipitation data for each month
           monthly_precipitation<-r[[monthly_index]]
           
           ##Daily precipitation data in each month each location
           vals<-extract(monthly_precipitation,points)
           vals_mat<-as.matrix(vals[,-1,drop=FALSE]) ##remove ID column
           
           
           ## Compute the rolling window days
           if(ncol(vals_mat) >=  roll_window){
             roll_sum <- apply(vals_mat, 1, function(x) rollapply(x, width =  roll_window, FUN = sum, align = "left", na.rm = TRUE))
             roll_sum <- as.matrix(roll_sum)
             monthly_Rxdday <- apply(roll_sum, 2, max, na.rm = TRUE)
           }else{
             monthly_Rxdday <- rep(NA, nrow(vals))
           }
           
           
           ##Attach the result as points
           points$Rxdday<-monthly_Rxdday
           
           ##create the raster layers
           templete<-monthly_precipitation[[1]]
           Rxdday_raster<-rasterize(points,templete,field="Rxdday")
           names(Rxdday_raster)<-month
           result_list[[month]]<-Rxdday_raster
         }
         
         Rxdday_stack <- rast(result_list)
         names(Rxdday_stack) <- unique_months
         return(Rxdday_stack)
      
    }
    
  })
  
  ###---------------------------------------Month Selector for Indices-------------------------------
  output$index_month_selector <- renderUI({
    req(indices_calculate())
    selectInput("selected_index_month", "Select Month to View:",
                choices = names(indices_calculate()))
  })
  
  ###---------------------------------------Output of Climate Indices-------------------------------
  output$index_plot <- renderPlot({
    req(indices_calculate(), input$selected_index_month)
    r_stack <- indices_calculate()
    month_name <- input$selected_index_month
    rolling_window<-input$rolling_window
    
    if(input$climate_index=="PRCPTOT"){
      plot(r_stack[[month_name]], main = paste("PRCPTOT -", month_name))
    }
    else if(input$climate_index=="CDD"){
      plot(r_stack[[month_name]], main = paste("CDD -", month_name))
    }
    else if(input$climate_index=="RxDday"){
      plot(r_stack[[month_name]], main = paste0("Rx",rolling_window,"day -", month_name))
    }
    
  })
  
  ###-------------------------------------Download Button------------------------------------
  output$download_index <- downloadHandler(
    filename = function() {
      paste0(input$climate_index, "_", input$selected_index_month, ".tif")
    },
    content = function(file) {
      req(indices_calculate(), input$selected_index_month)
      writeRaster(indices_calculate()[[input$selected_index_month]], file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)

