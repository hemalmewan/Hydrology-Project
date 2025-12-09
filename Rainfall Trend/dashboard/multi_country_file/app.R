library(shiny)
library(shinydashboard)
library(terra)
library(tmap)
library(DT)

tmap_mode("view") 

ui <- dashboardPage(
  dashboardHeader(title = "Multi-Country Rainfall Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster Viewer", tabName = "raster", icon = icon("globe")),
      menuItem("Data Quality", tabName = "quality", icon = icon("chart-line")),
      menuItem("Climate Indices", tabName = "climate_indice", icon = icon("cloud-rain")),
      menuItem("Multi-Year Analysis", tabName ="multi_year", icon = icon("history"))
    ),
    
    hr(),
    
    # ---------------------------------------------------------
    # 1. NEW: Country Selector (The Master Control)
    # ---------------------------------------------------------
    selectInput("country", "Select Country:",
                choices = c("India", "Ghana", "Ethiopia"),
                selected = "India"),
    
    hr(),
    
    numericInput("year","Enter Year (e.g., 1951):", value = 1951, min=1900, max =2024),
    
    radioButtons("viewType", "Select Raster Type:",
                 choices = c("Daily" = "daily", 
                             "Monthly" = "monthly",
                             "Seasonal"="seasonal",
                             "Annual"="annual"),
                 selected = "annual"),
    
    # ---------------------------------------------------------
    # 2. UPDATED: Dynamic Selector
    # This will display Date, Month, OR Country-Specific Season
    # ---------------------------------------------------------
    uiOutput("date_or_month_selector")
  ),
  
  dashboardBody(
    tabItems(
      #---- TAB 1: Raster Viewer ----
      tabItem(
        tabName = "raster",
        fluidRow(
          box(width = 12, title = "Rainfall Map", status = "primary", solidHeader = TRUE,
              tmapOutput("map", height = "800px"),
              hr(), 
              downloadButton("download_raster_map", "Download Current Map (.tif)", class = "btn-primary")
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
              # Note: Min/Max will need to be updated by server based on country CSV
              numericInput("station", "Station ID:", value =1, min=1) 
          ),
          box(width = 8, title ="Monthly Time Series Plot",
              plotOutput("timeseries_plot", height ="350px"))
        ),
        fluidRow(
          box(width =6, title ="Monthly Precipitation Distribution",
              plotOutput("monthly_boxplot", height ="350px")),
          box(width =6, title ="Monthly Precipitation Amounts",
              plotOutput("monthly_barplot", height ="350px"))
        ),
        fluidRow(
          box(
            width = 12, title = "Seasonal Statistics for Selected Station",
            status = "warning", solidHeader = TRUE,
            
            actionButton("compute_seasonal_stats", "Compute Seasonal Stats", icon = icon("calculator")),
            downloadButton("download_seasonal_stats", "Download CSV"),
            br(), br(),
            
            # ---------------------------------------------------------
            # 3. UPDATED: Dynamic Legend
            # Replaced hardcoded HTML legend with a UI output so it 
            # can explain "Kiremt" vs "Monsoon" automatically
            # ---------------------------------------------------------
            uiOutput("season_description_text"),
            
            div(style = "overflow-x:auto; width:100%;",
                tableOutput("seasonal_stats_table")
            )
          )
        )
      ),
      
      #---- TAB 3: Multi-Year Analysis ----
      tabItem(tabName = "multi_year",
              fluidRow(
                box(width = 3, title = "Settings", status = "primary", solidHeader = TRUE,
                    
                    radioButtons("multi_analysis_type", "Analysis Type:",
                                 choices = c("Annual Total" = "annual", 
                                             "Specific Month" = "monthly",
                                             "Specific Season" = "seasonal"),
                                 selected = "annual"),
                    
                    # 2a. Month Selector (Standard for all countries)
                    conditionalPanel(
                      condition = "input.multi_analysis_type == 'monthly'",
                      selectInput("multi_month", "Select Month:",
                                  choices = setNames(sprintf("%02d", 1:12), month.name), 
                                  selected = "07") 
                    ),
                    
                    # ---------------------------------------------------------
                    # 4. UPDATED: Dynamic Season Selector for Multi-Year
                    # Replaced hardcoded selectInput with uiOutput
                    # ---------------------------------------------------------
                    conditionalPanel(
                      condition = "input.multi_analysis_type == 'seasonal'",
                      uiOutput("multi_year_season_selector") 
                    ),
                    
                    hr(),
                    
                    sliderInput("year_range", "Select Year Range:", 
                                min = 1951, max = 2007, value = c(1951, 1956), step = 1),
                    
                    helpText("Displays comparison maps for the selected period."),
                    hr(),
                    
                    h4("Location Analysis (Optional)"),
                    numericInput("multi_lon", "Longitude:", value = NA, step = 0.1),
                    numericInput("multi_lat", "Latitude:", value = NA, step = 0.1),
                    
                    br(),
                    actionButton("run_multi_year", "Generate Maps", icon = icon("globe"), 
                                 class = "btn-success", style = "width:100%;")
                ),
                
                box(width = 9, title = "Multi-Year Rainfall Grid", status = "primary", solidHeader = TRUE,
                    plotOutput("multi_year_map", height = "900px") 
                )
              )
      ),
      
      #---- TAB 4: Climate Indices ----
      tabItem(
        tabName = "climate_indice",
        fluidRow(
          column(
            width =4,
            box(width = 12, title = "Select Climate Index", status = "primary", solidHeader = TRUE,
                selectInput("climate_index", "Climate Index:",
                            choices = c("PRCPTOT", "CDD", "RxDday","Rnnmm","CWD","R95p","R99p","R95pTOT","R99pTOT")),
                
                uiOutput("time_scale_ui"),
                
                numericInput("sel_lon", "Longitude (optional):", value = NA),
                numericInput("sel_lat", "Latitude (optional):", value = NA),
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

# Dummy server to make it runnable for testing the UI
server <- function(input, output, session) {}

shinyApp(ui, server)
