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
      
      #---- TAB 3: Climate Indices ----
      tabItem(
        tabName = "climate_indice",
        fluidRow(
          column(
            width =4,
            box(width = 12, title = "Select Climate Index", status = "primary", solidHeader = TRUE,
                selectInput("climate_index", "Climate Index:",
                            choices = c("PRCPTOT", "CDD", "RxDday","Rnnmm","CWD","R95p","R99p","R95pTOT","R99pTOT")),
                uiOutput("index_parameters"),
                uiOutput("index_month_selector"),
                actionButton("compute_index", "Compute Index", icon = icon("cogs")),
                downloadButton("download_index", "Download Result")
            ),
            box(width = 12, title = "Description", status = "info", solidHeader = TRUE,
                htmlOutput("index_description"))
          ),
          column(
            width = 8,
            box(width = 12, title = "Climate Index Result", status = "success", solidHeader = TRUE,
                tmapOutput("index_map", height = "600px"))
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
  
  ##------------------------Load CSV----------------------------
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
  daily_dates <- reactive({
    req(r_daily())
    n <- nlyr(r_daily())         # number of layers
    start_date <- as.Date("1951-01-01")
    seq(start_date, by = "day", length.out = n)
  })
  
  output$meta <- renderPrint({
    req(r_daily())
    dates <- daily_dates()
    names(r_daily()) <- as.character(dates)
    r_daily() ## display raster meta
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
    if (nrow(vals) == 0) return(data.frame(StationID = character(0), Month = as.Date(character(0)), Rain = numeric(0)))
    
    vals <- vals[,-1,drop=FALSE] ##remove ID col
    df <- cbind(StationID = stations()$station_id, vals)
    df_long <- reshape::melt(df, id.vars = "StationID", variable.names = "Month", value.name = "Rain")
    
    # clean month names
    df_long$Month <- gsub("^X", "", df_long$Month)
    df_long$Month <- gsub("\\.", "-", df_long$Month)
    df_long$Month <- as.Date(paste0(df_long$Month, "-01"), format = "%Y-%m-%d")
    df_long
  })
  
  ##-----------------------Time Series Plot for Selected Station-----------------------
  output$timeseries_plot <- renderPlot({
    req(input$station)
    df <- monthly_values()[monthly_values()$StationID == input$station,]
    validate(need(nrow(df) > 0, "No data available for the selected station."))
    
    plot(df$Month, df$Rain, type = "o", pch = 16, col = "blue",
         xlab = "Month", ylab = "Rainfall (mm)",
         main = paste("Monthly Precipitation - Station:", input$station))
  })
  
  ##---------------------------------------Dynamic UI---------------------------------------------
  output$date_or_month_selector <- renderUI({
    req(r_daily())
    if (input$viewType == "daily") {
      selectInput("selected_day", "Select Date:", choices = format(daily_dates(), "%Y-%m-%d"))
    } else {
      selectInput("selected_month", "Select Month:", choices = month_labels())
    }
  })
  
  ##-----------------------------------------Render Map-------------------------------
  output$map <- renderTmap({
    req(r_daily())
    
    if (input$viewType == "daily") {
      req(input$selected_day)
      idx <- which(format(daily_dates(), "%Y-%m-%d") == input$selected_day)
      r_show <- r_daily()[[idx]]
      title_txt <- paste("Daily Rainfall:", input$selected_day)
    } else {
      req(input$selected_month)
      ym <- names(month_labels())[month_labels() == input$selected_month]
      r_show <- r_monthly()[[ym]]
      title_txt <- paste("Monthly Rainfall:", input$selected_month)
    }
    
    tm_shape(r_show) +
      tm_raster(col.scale = tm_scale(values = "-Spectral"),
                col.legend = tm_legend(title = title_txt)) +
      tm_layout(legend.outside = TRUE)
  })
  
  ###-----------------------------------Description of each climate index------------------------
  output$index_description <- renderUI({
    req(input$climate_index)
    
    desc <- switch(input$climate_index,
                   
                   "PRCPTOT" = "<b>PRCPTOT – Total Wet-Day Precipitation</b><br>
                 Total precipitation accumulated over all wet days (≥ 1 mm) within each month.
                 This index reflects the overall monthly rainfall input.",
                   
                   "CDD" = "<b>CDD – Consecutive Dry Days</b><br>
             Maximum number of consecutive days with rainfall < 1 mm.
             CDD indicates the duration of dry spells and is commonly used to study drought risk.",
                   
                   "CWD" = "<b>CWD – Consecutive Wet Days</b><br>
             Maximum number of consecutive days with rainfall ≥ 1 mm.
             CWD captures persistent wet spells and prolonged rainy conditions.",
                   
                   "RxDday" = "<b>RxDday – Maximum X-Day Precipitation</b><br>
                Highest accumulated rainfall over any X-day rolling window (e.g., 1-day, 5-day).
                This index measures short-duration extreme rainfall events.",
                   
                   "Rnnmm" = "<b>Rnnmm – Heavy Rainfall Days</b><br>
               Number of days where precipitation exceeds a specified threshold (e.g., ≥ 10 mm).
               It represents the frequency of heavy rainfall events.",
                   
                   "R95p" = "<b>R95p – Very Wet Days</b><br>
              Total rainfall from days exceeding the <b>95th percentile</b> of daily precipitation
              during the baseline period (1951–1980).<br><br>
              <u>How the percentile is computed:</u><br>
              • Only wet days (≥ 1 mm) from 1951–1980 are used.<br>
              • The 95th percentile (p95) is computed from this baseline distribution.<br>
              • For each year, all days with precipitation > p95 are summed.<br>
              This index represents moderate to strong rainfall extremes.",
                   
                   "R99p" = "<b>R99p – Extremely Wet Days</b><br>
              Total rainfall from days exceeding the <b>99th percentile</b> of daily precipitation
              during the baseline period (1951–1980).<br><br>
              <u>How the percentile is computed:</u><br>
              • Only wet days (≥ 1 mm) from the 1951–1980 baseline are used.<br>
              • The 99th percentile (p99) is derived from these values.<br>
              • All days above p99 are summed for each year.<br>
              This index reflects very rare and extreme rainfall events.",
                   
                   "R95pTOT" = "<b>R95pTOT – Contribution of Very Wet Days</b><br>
                 Percentage of annual precipitation contributed by days above the 95th percentile (R95p).
                 Indicates how dominant moderate extreme events are in total rainfall.",
                   
                   "R99pTOT" = "<b>R99pTOT – Contribution of Extremely Wet Days</b><br>
                 Percentage of annual precipitation contributed by days above the 99th percentile (R99p).
                 Highlights how rare extreme rainfall events influence total precipitation."
    )
    
    HTML(desc)
  })
  
  ###------------------------------------Climate Indices Parameters------------------------
  output$index_parameters <- renderUI({
    req(input$climate_index)
    if (input$climate_index == "RxDday") {
      numericInput("rolling_window", "Rolling Window (days):", value = 5, min = 1, max = 10)
    } else if (input$climate_index %in% c("PRCPTOT", "CDD","Rnnmm","CWD")) {
      numericInput("threshold", "Threshold (mm):", value = 1, min = 0, max = 200)
    }
  })
  
  ####------------------------------------Calculate Climate Indices-------------------------------
  indices_calculate <- eventReactive(input$compute_index, {
    req(r_daily(), input$climate_index, pts(), daily_dates())
    r <- r_daily()
    points <- pts()
    dates <- daily_dates()
    month_group <- format(dates, "%Y-%m")
    unique_months <- unique(month_group)
    result_list <- list()
    
    ## Example: PRCPTOT calculation
    if (input$climate_index == "PRCPTOT") {
      req(input$threshold)
      threshold <- as.numeric(input$threshold)
      for (m in unique_months) {
        month_idx <- which(month_group == m)
        month_r <- r[[month_idx]]
        vals <- extract(month_r, points)[,-1, drop = FALSE]
        PRCPTOT_vals <- apply(vals, 1, function(x) sum(x[x >= threshold], na.rm = TRUE))
        points$PRCPTOT <- PRCPTOT_vals
        PRCPTOT_r <- rasterize(points, month_r[[1]], field = "PRCPTOT")
        names(PRCPTOT_r) <- m
        result_list[[m]] <- PRCPTOT_r
      }
      PRCPTOT_stack <- rast(result_list)
      names(PRCPTOT_stack) <- unique_months
      return(PRCPTOT_stack)
    }
    
    ##----------------------CDD------------------------------
    else if (input$climate_index == "CDD") {
      req(input$threshold)
      threshold <- as.numeric(input$threshold)
      vals <- extract(r, points)[,-1, drop=FALSE]
      
      CDD_fun <- function(daily_precip, threshold){
        dry <- as.numeric(daily_precip) < threshold
        dry[is.na(dry)] <- FALSE
        if(all(!dry)) return(0)
        rle_dry <- rle(dry)
        max(rle_dry$lengths[rle_dry$values])
      }
      
      cdd_values <- apply(vals, 1, CDD_fun, threshold = threshold)
      points$CDD <- cdd_values
      CDD_raster <- rasterize(points, r[[1]], field = "CDD")
      names(CDD_raster) <- "CDD"
      return(CDD_raster)
    }
    
    ##----------------------RxDday----------------------------
    else if (input$climate_index == "RxDday") {
      req(input$rolling_window)
      roll_window <- as.numeric(input$rolling_window)
      
      for (m in unique_months) {
        month_idx <- which(month_group == m)
        month_r <- r[[month_idx]]
        vals <- extract(month_r, points)[,-1, drop=FALSE]
        
        roll_sum <- apply(vals, 1, function(x){
          if(length(x) >= roll_window){
            max(zoo::rollapply(x, width=roll_window, FUN=sum, align="left", na.rm=TRUE))
          } else NA
        })
        
        points$RxDday <- roll_sum
        RxDday_raster <- rasterize(points, month_r[[1]], field = "RxDday")
        names(RxDday_raster) <- m
        result_list[[m]] <- RxDday_raster
      }
      RxDday_stack <- rast(result_list)
      names(RxDday_stack) <- unique_months
      return(RxDday_stack)
    }
    
    ##----------------------Rnnmm----------------------------
    else if (input$climate_index == "Rnnmm") {
      req(input$threshold)
      threshold <- as.numeric(input$threshold)
      
      for (m in unique_months) {
        month_idx <- which(month_group == m)
        month_r <- r[[month_idx]]
        vals <- extract(month_r, points)[,-1, drop=FALSE]
        Rnnmm_vals <- apply(vals, 1, function(x) sum(x > threshold, na.rm=TRUE))
        points$Rnnmm <- Rnnmm_vals
        Rnnmm_raster <- rasterize(points, month_r[[1]], field = "Rnnmm")
        names(Rnnmm_raster) <- m
        result_list[[m]] <- Rnnmm_raster
      }
      Rnnmm_stack <- rast(result_list)
      names(Rnnmm_stack) <- unique_months
      return(Rnnmm_stack)
    }
    
    ##-------------------------CWD-----------------------------------
    else if(input$climate_index=="CWD"){
       req(input$threshold)
       threshold <- as.numeric(input$threshold)
       vals <- extract(r, points)[,-1, drop=FALSE]
       
       ##define the customize function
       CWD<-function(daily_precip,threshold){
         wet<-as.numeric(daily_precip)>=threshold
         if(all(!wet)) return(0) ##no wet days all are dry days
         rle_wet<-rle(wet)
         max_cwd<-max(rle_wet$lengths[rle_wet$values])
         
         return(max_cwd)
       }
       cwd_values <- apply(vals, 1,CWD, threshold = threshold)
       points$CWD <- cwd_values
       CWD_raster <- rasterize(points, r[[1]], field = "CWD")
       names(CWD_raster) <- "CWD"
       return(CWD_raster)
      
    }
    
    ##---------------------------------R95p-----------------------------------------
    else if(input$climate_index=="R95p"){
      ##read the 95th percentile value in the file
      p95 <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/p95_threshold.rds")
      vals <- extract(r, points)[,-1, drop=FALSE]
      
      ##define the customize function
      R95p<-function(daily_precip,threshold){
        wet<-sum(daily_precip[as.numeric(daily_precip)>threshold])
        return(wet)
      }
      
      ##Apply above function for each station
      very_wet_days<-apply(vals,1,R95,threshold=p95)
      
      points$R95p<-very_wet_days
      ##rasterize output
      R95p_raster<-rasterize(points,r[[1]],field="R95p")
      names(R95p_raster) <- "R95p"
      return(R95p_raster)
    }
    
    ##---------------------------------R99p-----------------------------------------
    else if(input$climate_index=="R99p"){
      ##read the 99th percentile value in the file
      p99 <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/p99_threshold.rds")
      vals <- extract(r, points)[,-1, drop=FALSE]
      
      ##define the customize function
      R99p<-function(daily_precip,threshold){
        wet<-sum(daily_precip[as.numeric(daily_precip)>threshold])
        return(wet)
      }
      
      ##Apply above function for each station
      extreme_wet_days<-apply(vals,1,R99p,threshold=p99)
      
      points$R99p<-extreme_wet_days
      ##rasterize output
      R99p_raster<-rasterize(points,r[[1]],field="R99p")
      names(R99p_raster) <- "R99p"
      return(R99p_raster)
    }
    ##---------------------------------R95pTOT-----------------------------------------
    else if(input$climate_index=="R95pTOT"){
      ##read the annual PRCPTOT percentile value in the file
      annual_PRCPTOT <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/annual_PRCPTOT.rds")
      ##read the R95p value in the file
      R95p <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/R95_threshold.rds")
      
      ##calculate the indices
      R95pTOT<-(100*R95p)/annual_PRCPTOT
      
      points$R95pTOT<-R95pTOT
      ##rasterize output
      R95pTOT_raster<-rasterize(points,r[[1]],field="R95pTOT")
      names(R95pTOT_raster) <- "R95pTOT"
      return(R95pTOT_raster)
      
    }
    ##------------------------------R99pTOT----------------------------------------------
    else if(input$climate_index=="R99pTOT"){
      ##read the annual PRCPTOT percentile value in the file
      annual_PRCPTOT <- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/annual_PRCPTOT.rds")
      ##read the R99p value in the file
      R99<- readRDS("C:/Hydrology-Project/Rainfall Trend/scripts/R99_threshold.rds")
      
      ##calculate the indices
      R99pTOT<-(100*R99)/annual_PRCPTOT
      
      points$R99pTOT<-R99pTOT
      ##rasterize output
      R99pTOT_raster<-rasterize(points,r[[1]],field="R99pTOT")
      names(R99pTOT_raster) <- "R99pTOT"
      return(R99pTOT_raster)
      
    }
  })
  
  ###---------------------------------------Month Selector for Indices-------------------------------
  output$index_month_selector <- renderUI({
    req(indices_calculate())
    selectInput("selected_index_month", "Select Month to View:",
                choices = names(indices_calculate()))
  })
  
  ###---------------------------------------Output of Climate Indices-------------------------------
  output$index_map <- renderTmap({
    req(indices_calculate(), input$selected_index_month)
    
    r_stack <- indices_calculate()
    m <- input$selected_index_month
    r_show <- r_stack[[m]]
    
    tm_shape(r_show) +
      tm_raster(
        palette = "-Spectral",
        title = paste(input$climate_index, ":", m)
      ) +
      tm_layout(legend.outside = TRUE)
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

 
