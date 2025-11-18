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
          
          ####--------------------------LEFT COLUMN---------------------------------------------
          column(
            width =4,
            box(width = 12, title = "Select Climate Index", status = "primary", solidHeader = TRUE,
                selectInput("climate_index", "Climate Index:",
                            choices = c("PRCPTOT", "CDD", "RxDday","Rnnmm")),
                
                uiOutput("index_parameters"),
                uiOutput("index_month_selector"),
                actionButton("compute_index", "Compute Index", icon = icon("cogs")),
                downloadButton("download_index", "Download Result")
            ),
            # NEW: Separate box for description (outside and below)
            box(width = 12, title = "Description", status = "info", solidHeader = TRUE,
                htmlOutput("index_description"))
          ),
          ####-------------------------RIGHT COLUMN----------------------------------------
          column(
            width = 8,
            # NEW: Separate box for description (outside and below)
            box(width = 12, title = "Climate Index Result", status = "success", solidHeader = TRUE,
                plotOutput("index_plot", height = "600px"))
            
          )
        )
        
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  ## ----------------------Load Raster--------------------------
  r_daily <- reactive({
    req(input$ncfile)
    rast(input$ncfile$datapath)
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
  
  ##-----------------------------------------Extract Monthly Data for Stations------------------
  monthly_values <- reactive({
    req(r_monthly(), pts(), stations())
    
    vals <- extract(r_monthly(), pts()) ##return location ID and monthly columns
    
    if (nrow(vals) == 0) {
      return(data.frame(StationID = character(0), Month = as.Date(character(0)), Rain = numeric(0)))
    }
    
    vals <- vals[,-1,drop=FALSE] ##remove ID col
    
    # check row counts match
    if (nrow(vals) != nrow(stations())) {
      stop("Mismatch between extracted raster points and station coordinates.")
    }
    
    df <- cbind(StationID = stations()$station_id, vals)
    
    df_long <- reshape::melt(df, id.vars = "StationID", variable.names = "Month", value.name = "Rain")
    
    # clean month names
    df_long$Month <- gsub("^X", "", df_long$Month)
    df_long$Month <- gsub("\\.", "-", df_long$Month)
    df_long$Month <- as.Date(paste0(df_long$Month, "-01"), format = "%Y-%m-%d")
    
    
    df_long
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
