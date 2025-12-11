library(shiny)
library(shinydashboard)
library(terra)
library(tmap)
library(DT)

##------------------------------------Seasonal Variation of each Country--------------------------------
country_config <- list(
  "India" = list(
    folder_name = "india",  # Folder name inside 'C:/Hydrology-Project/Rainfall Trend/NCDF/'
    year_range  = c(1951, 2007),
    seasons     = list(
      "Winter"       = c(1, 2),        # Jan, Feb
      "Pre-monsoon"  = c(3, 4, 5),     # Mar, Apr, May
      "Monsoon"      = c(6, 7, 8, 9),  # Jun, Jul, Aug, Sep
      "Post-monsoon" = c(10, 11, 12)   # Oct, Nov, Dec
    )
  ),
  "Ghana" = list(
    folder_name = "ghana",
    year_range  = c(1981, 2024),
    seasons     = list(
      "Dry Season"= c(1, 2, 12,11), ##Jan,Feb,Dec and Nov
      "Wet Season"=  c(3,4,5,6,7,9,10) ##March,Apr,May,June,July,Sept,Oct
    )
  ),
  "Ethiopia" = list(
    folder_name = "ethiopia",
    year_range  = c(1981, 2024),
    seasons     = list(
      "Bega (Dry)"          = c(12, 1), # Dec-Jan
      "Belg (Short Rains)"  = c(3, 4, 5),    # March,May,Apr
      "Kiremt (Long Rains)" = c(6, 7, 8)     # Jun,July,Aug
    )
  )
)

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
    
    # 1. Country Selector
    selectInput("country", "Select Country:",
                choices = names(country_config), # Dynamically get names from config
                selected = "India"),
    
    hr(),
    
    # 2. Year Input (Ranges updated by server)
    numericInput("year","Enter Year:", value = 1951, min=1951, max=2007),
    
    radioButtons("viewType", "Select Raster Type:",
                 choices = c("Daily" = "daily", 
                             "Monthly" = "monthly",
                             "Seasonal"="seasonal",
                             "Annual"="annual"),
                 selected = "annual"), # Default to Annual as requested
    
    # 3. Dynamic Selector (Date / Month / Season)
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
      
      #---- TAB 2: Data Quality (Placeholder for now) ----
      tabItem(
        tabName = "quality",
        h3("Data Quality Section (Connect your station CSV logic here)")
      ),
      
      #---- TAB 3: Multi-Year Analysis (Simplified for this example) ----
      tabItem(tabName = "multi_year",
              h3("Multi-Year Analysis Section")
      ),
      
      #---- TAB 4: Climate Indices (Simplified for this example) ----
      tabItem(
        tabName = "climate_indice",
        h3("Climate Indices Section")
      )
    )
  )
)


server <- function(input, output, session) {
  ##-----------------------------------Country Configuration----------------------------
  current_config <- reactive({
    req(input$country)
    country_config[[input$country]]
  })
  
  observeEvent(input$country,{
    req(current_config())
    cfg <- current_config()
    
    # Update single year input
    updateNumericInput(session, "year",
                       value = cfg$year_range[1], # Reset to start year
                       min = cfg$year_range[1],
                       max = cfg$year_range[2])
    
    # Update slider (if used in other tabs)
    updateSliderInput(session, "year_range",
                      min = cfg$year_range[1],
                      max = cfg$year_range[2],
                      value = c(cfg$year_range[1], cfg$year_range[1] + 5))
  })
  
  ##----------------------------------Load Daily Rasters--------------------------------
  r_daily <- reactive({
    req(input$country, input$year)
    
    cfg <- current_config()
    
    # Construct Path: e.g., C:/.../India/Daily_nc_1951.nc
    # NOTE: Adjust base path if needed
    nc_path <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/", 
                      cfg$folder_name, "/Daily_nc_", input$year, ".nc")
    
    validate(need(file.exists(nc_path), paste("NetCDF file not found:", nc_path)))
    
    rast(nc_path)
  })
  
  ##-----------------------------calculate the number of dates--------------
  daily_dates <- reactive({
    req(r_daily(), input$year)
    n <- nlyr(r_daily()) # Get number of layers (e.g., 365 or 366)
    
    # Generate sequence starting from Jan 1st of selected year
    start_date <- as.Date(paste0(input$year, "-01-01"))
    seq(start_date, by = "day", length.out = n)
  })
  
  ##--------------------------------Select the Type of the raster----------------
  output$date_or_month_selector <- renderUI({
    req(daily_dates(), input$viewType)
    
    if (input$viewType == "daily") {
      dates <- daily_dates()
      dateInput("selected_day", "Select Date:", 
                value = dates[1], min = min(dates), max = max(dates))
      
    } else if (input$viewType == "monthly") {
      selectInput("selected_month", "Select Month:", choices = month.name)
      
    } else if (input$viewType == "seasonal") {
      # Get country-specific season names
      cfg <- current_config()
      selectInput("selected_season", "Select Season:", 
                  choices = names(cfg$seasons))
      
    } else {
      return(NULL) # Nothing needed for Annual
    }
  })
  ##---------------------------------Compute the Raster based view-------------------
  current_raster_data <- reactive({
    req(r_daily(), daily_dates(),input$viewType)
    
    r <- r_daily()
    dates <- daily_dates()
    cfg <- current_config()
    
    r_out <- NULL
    title_txt <- ""
    file_name <- ""
    
    # --- A. ANNUAL (Default) ---
    if (input$viewType == "annual") {
      r_out <- sum(r, na.rm = TRUE)
      title_txt <- paste("Annual Rainfall -", input$country, input$year)
      file_name <- paste0("Annual_", input$country, "_", input$year)
      
      # --- B. DAILY ---
    } else if (input$viewType == "daily") {
      req(input$selected_day)
      # Find index of selected day
      idx <- which(dates == as.Date(input$selected_day))
      validate(need(length(idx) > 0, "Selected date not found in file."))
      
      r_out <- r[[idx]]
      title_txt <- paste("Daily Rainfall -", input$selected_day)
      file_name <- paste0("Daily_", input$selected_day)
      
      # --- C. MONTHLY ---
    } else if (input$viewType == "monthly") {
      req(input$selected_month)
      # Identify month index (1-12)
      target_m_idx <- match(input$selected_month, month.name)
      
      # Extract layers for that month
      date_months <- as.numeric(format(dates, "%m"))
      
      layer_indices <- which(date_months == target_m_idx)
      validate(need(length(layer_indices) > 0, "No data for this month."))
      
      # Subset and Sum
      r_sub <- r[[layer_indices]]
      r_out <- sum(r_sub, na.rm = TRUE)
      
      title_txt <- paste("Monthly Rainfall -", input$selected_month, input$year)
      file_name <- paste0("Monthly_", input$selected_month, "_", input$year)
      
      # --- D. SEASONAL (Country Specific) ---
    } else if (input$viewType == "seasonal") {
      req(input$selected_season)
      
      # Get the list of month numbers for the selected season
      target_months <- cfg$seasons[[input$selected_season]]
      
      # Identify layers matching these months
      date_months <- as.numeric(format(dates, "%m"))
      layer_indices <- which(date_months %in% target_months)
      
      validate(need(length(layer_indices) > 0, paste("No data found for season:", input$selected_season)))
      
      # Subset and Sum
      r_sub <- r[[layer_indices]]
      r_out <- sum(r_sub, na.rm = TRUE)
      
      title_txt <- paste("Seasonal Rainfall -", input$selected_season, input$year)
      file_name <- paste0("Seasonal_", input$selected_season, "_", input$year)
    }
    
    list(r = r_out, title = title_txt, name = file_name)
  })
  
  ##--------------------------------------------Output Map---------------------
  output$map <- renderTmap({
    data <- current_raster_data()
    req(data$r)
    
    # Optional: Disaggregate for smoother look (visual only)
    r_show <- terra::disagg(data$r, fact = 5, method = "bilinear")

    
    #Create a dynamic legend title based on the user's selection
    legend_label <- switch(input$viewType,
      "daily"    = paste0("Daily Precip:", input$selected_day),
      "monthly"  = paste0("Monthly Precip:", input$selected_month),
      "seasonal" = paste0("Seasonal Precip:", input$selected_season),
      "annual"   = paste0("Annual Precip:", input$year)
    )
    
    names(r_show) <- "Precipitation (mm)"
    
    ##render map view
    tm_shape(r_show) +
      tm_raster(
        title = legend_label,    # Sets the Legend Title
        palette = "Blues",       # Standard tmap color palette
        style = "cont",          # Continuous scale
        alpha = 0.8
      ) +
      tm_layout(
        main.title = data$title, 
        main.title.position = "center"
      )
  })
  
  ##---------------------------------------Donwload the Map---------------------------
  output$download_raster_map <- downloadHandler(
    filename = function() { paste0(current_raster_data()$name, ".tif") },
    content = function(file) {
      req(current_raster_data()$r)
      writeRaster(current_raster_data()$r, file, overwrite = TRUE)
    }
  )
  
}
shinyApp(ui, server)
