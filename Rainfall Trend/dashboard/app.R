# app.R
library(shiny)
library(shinydashboard)
library(terra)
library(tmap)
library(reshape)
library(zoo)
library(ggplot2)
library(DT)

tmap_mode("view")  # Enable interactive maps

ui <- dashboardPage(
  dashboardHeader(title = "Rainfall Viewer Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster Viewer", tabName = "raster", icon = icon("globe")),
      menuItem("Data Quality", tabName = "quality", icon = icon("chart-line")),
      menuItem("Climate Indice", tabName = "climate_indice", icon = icon("cloud-rain"))
    ),
    
    numericInput("year","Enter Year (e.g., 1951):",value = 1951,min=1900,max =2010),
    
    radioButtons("viewType", "Select Raster Type:",
                 choices = c("Daily" = "daily", 
                             "Monthly" = "monthly",
                             "Seasonal"="seasonal",
                             "Annual"="annual"),
                 selected = "daily"),
    
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
              
              hr(), # Horizontal line for separation
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
              numericInput("station", "Station ID:", value =1,min=1,max =357)
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
            
            # --- Season descriptions ---
            tags$div(
              style = "background:#f7f7f7; padding:12px; border-radius:6px; margin-bottom:15px;",
              HTML("
      <b>Season Definitions:</b><br>
      <ul style='margin:0; padding-left:20px;'>
        <li><b>Winter:</b> December, January, February</li>
        <li><b>Pre-monsoon:</b> March, April, May</li>
        <li><b>Monsoon:</b> June, July, August, September</li>
        <li><b>Post-monsoon:</b> October, November</li>
        <li><b>Annual:</b> Entire year (January–December)</li>
      </ul>
    ")
            ),
            
            # Expanded table
            div(style = "overflow-x:auto; width:100%;",
                tableOutput("seasonal_stats_table")
            )
          )
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
                # NEW: lon/lat inputs (optional)
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


server <- function(input, output, session) {
  
  ## ----------------------Load Raster--------------------------
  r_daily <- reactive({
    req(input$year)
    nc_path<-paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/Daily_nc_",input$year,".nc")
    validate(need(file.exists(nc_path), paste("NetCDF file not found:", nc_path)))
    rast(nc_path)
  })
  
  ##------------------------Load CSV----------------------------
  stations <- reactive({
    req(input$year)
    csv_path<-paste0("C:/Hydrology-Project/Rainfall Trend/CSV files/drf_",input$year,"_new2.csv")
    validate(need(file.exists(csv_path), paste("Station file not found:", csv_path)))
    read.csv(csv_path)
  })
  
  ##--------------------------Extract the coordinates-----------------------------
  pts <- reactive({
    req(stations(), r_daily())
    vect(stations(), geom = c("lon", "lat"), crs = crs(r_daily()))
  })
  
  # update station numeric input limits (min/max) so user can type within range
  observeEvent(stations(), {
    st_ids <- stations()$station_id
    if(!is.null(st_ids) && length(st_ids)>0 && is.numeric(st_ids)){
      updateNumericInput(session, "station", value = st_ids[1], min = min(st_ids, na.rm = TRUE), max = max(st_ids, na.rm = TRUE))
    }
  })
  
  ##-----------------------------Meta Data of the raster files-----------------------
  daily_dates <- reactive({
    req(r_daily())
    n <- nlyr(r_daily())         # number of layers
    start_date <- as.Date(paste0(input$year,"-01-01"))
    seq(start_date, by = "day", length.out = n)
  })
  
  output$meta <- renderPrint({
    req(r_daily())
    dates <- daily_dates()
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
  
  ##------------------------------------Aggregate to Seasonal (Sum) [NEW]--------------------------------
  r_seasonal <- reactive({
    req(r_daily())
    r <- r_daily()
    dates <- daily_dates()
    months <- as.numeric(format(dates, "%m"))
    
    season_list <- list(
      "Winter"       = c(1, 2, 12),
      "Pre-monsoon"  = c(3, 4, 5),
      "Monsoon"      = c(6, 7, 8, 9),
      "Post-monsoon" = c(10, 11)
    )
    
    # Calculate sum for each season and store in a list
    seasonal_stack <- list()
    for(season in names(season_list)){
      indices <- which(months %in% season_list[[season]])
      if(length(indices) > 0){
        # Subset layers for this season and sum them
        seasonal_stack[[season]] <-sum(r[[indices]], na.rm = TRUE)
      }
    }
    
    # Convert list to raster stack
    stk <- rast(seasonal_stack)
    names(stk) <- names(seasonal_stack)
    stk
  })
  
  ##------------------------------------Aggregate to Annual (Sum) [NEW]--------------------------------
  r_annual <- reactive({
    req(r_daily())
    sum(r_daily(), na.rm = TRUE)
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
    req(input$station, r_daily(), pts(), stations(), daily_dates())
    
    # find the row index of the selected station in the stations() data.frame
    st_idx <- which(stations()$station_id == input$station)
    validate(need(length(st_idx) == 1, "Selected station not found in station list."))
    
    # get raster and dates
    r <- r_daily()
    dates <- daily_dates()
    
    # ensure raster layer names match dates (optional but safe)
    if (length(names(r)) != length(dates) || any(names(r) != as.character(dates))) {
      names(r) <- as.character(dates)
    }
    
    # extract daily values for all stations (first column is ID)
    vals_all <- terra::extract(r, pts())   # data.frame: ID, layer1, layer2, ...
    
    # get the numeric precipitation vector for the selected station
    # remove the first column (ID)
    precip_vec <- as.numeric(vals_all[st_idx, -1])
    
    # build dataframe for plotting
    plot_df <- data.frame(
      date = dates,
      precipitation = precip_vec
    )
    
    # simple validation: need some non-NA values to plot
    validate(need(any(!is.na(plot_df$precipitation)), "No precipitation data available for this station/year."))
    
    # plot
    ggplot(plot_df, aes(x = date, y = precipitation)) +
      geom_line(na.rm = TRUE,col="blue") +
      geom_point(size = 0.8, na.rm = TRUE) +
      theme_minimal() +
      labs(
        title = paste0("Daily Precipitation - Station: ", input$station, " (", input$year, ")"),
        x = "Date",
        y = "Daily Precipitation (mm)"
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")
      )
  })
  
  ##----------------------------------------Monthly Distribution of each location------------------------------------------------
  output$monthly_boxplot <- renderPlot({
    req(input$station, r_daily(), pts(), stations(), daily_dates())
    
    # -------------------------------
    # 1. Identify selected station
    # -------------------------------
    st_idx <- which(stations()$station_id == input$station)
    validate(need(length(st_idx) == 1, "Selected station not found."))
    
    # -------------------------------
    # 2. Extract daily values for all stations
    # -------------------------------
    vals_all <- terra::extract(r_daily(), pts())   # ID + daily layers
    
    # remove first column (ID)
    precip_vec <- as.numeric(vals_all[st_idx, -1])
    
    # -------------------------------
    # 3. Build dataframe
    # -------------------------------
    df <- data.frame(
      date  = daily_dates(),
      prec  = precip_vec
    )
    
    # Add month factor (Jan–Dec)
    df$month <- factor(format(df$date, "%b"), 
                       levels = month.abb)  # ordered
    
    # -------------------------------
    # 4. Need some valid data
    # -------------------------------
    validate(need(any(!is.na(df$prec)), 
                  "No precipitation data for this station/year."))
    
    # -------------------------------
    # 5. Plot monthly distributions
    # -------------------------------
    ggplot(df, aes(x = month, y = prec)) +
      geom_boxplot(fill = "skyblue", outlier.color = "red", na.rm = TRUE) +
      theme_minimal() +
      labs(
        title = paste("Monthly Precipitation Distribution - Station", input$station,"(",input$year,")"),
        x = "Month",
        y = "Daily Precipitation (mm)"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(size = 12)
      )
  })
  
  ##----------------------------------------Monthly Precipitation Amount of each location------------------------------------------------
  output$monthly_barplot <- renderPlot({
    req(input$station, r_daily(), pts(), stations(), daily_dates())
    
    # 1. Identify station
    st_idx <- which(stations()$station_id == input$station)
    validate(need(length(st_idx) == 1, "Station not found."))
    
    # 2. Extract daily rainfall
    vals_all <- terra::extract(r_daily(), pts())
    vals_all<-vals_all[,-1]# remove ID column
    precip_vec <- as.numeric(vals_all[st_idx,])
    
    # 3. Create dataframe
    df <- data.frame(
      date  = daily_dates(),
      pr    = precip_vec
    )
    
    # 4. Add month abbreviation
    df$month <- factor(format(df$date, "%b"), levels = month.abb)
    
    # 5. Categorize rainfall intensity groups
    df$category <- cut(
      df$pr,
      breaks = c(-Inf, 1, 2, 5, 10, 20, 50, 100, Inf),
      labels = c(
        "<1 mm (Dry)",
        "1–2 mm",
        "2–5 mm",
        "5–10 mm",
        "10–20 mm",
        "20–50 mm",
        "50–100 mm",
        ">100 mm"
      ),
      right = FALSE
    )
    
    # 6. Count number of days in each category per month
    df_count <- aggregate(
      list(count = rep(1, nrow(df))),
      by = list(month = df$month, category = df$category),
      FUN = sum
    )
    
    # 7. Plot stacked bar chart
    ggplot(df_count, aes(x = month, y = count, fill = category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(
        title = paste("Rainfall Category Distribution - Station", input$station),
        x = "Month",
        y = "Number of Days",
        fill = "Rainfall Category"
      ) +
      theme(
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold")
      )
  })
  
  ##---------------------------------------Dynamic UI---------------------------------------------
  output$date_or_month_selector <- renderUI({
    req(r_daily())
    
    if (input$viewType == "daily") {
      selectInput("selected_day", "Select Date:", choices = format(daily_dates(), "%Y-%m-%d"))
    } 
    else if (input$viewType == "monthly") {
      selectInput("selected_month", "Select Month:", choices = month_labels())
    }
    else if (input$viewType == "seasonal") {
      selectInput("selected_season", "Select Season:", 
                  choices = c("Winter", "Pre-monsoon", "Monsoon", "Post-monsoon"))
    } 
    else {
      # For Annual, we don't need a secondary selector, but we return NULL to keep UI clean
      return(NULL)
    }
  })
  
  ##---------------------------------------Seasonal Statistics of each location---------------------------------------
  seasonal_stats_result <- eventReactive(input$compute_seasonal_stats, {
    req(r_daily(), pts(), stations(), input$station)
    
    st_idx <- which(stations()$station_id == input$station)
    validate(need(length(st_idx) == 1, "Selected station not found."))
    
    # Extract and ensure numeric vector
    vals_all <- terra::extract(r_daily(), pts())
    rain_values <- vals_all[,-1]           # remove ID
    station_rain <- as.numeric(rain_values[st_idx, ])
    
    n <- nlyr(r_daily())
    dates <- seq(as.Date(paste0(input$year, "-01-01")), by = "day", length.out = n)
    months <- as.numeric(format(dates, "%m"))
    
    # Seasonal columns
    winter_cols       <- which(months %in% c(12, 1, 2))
    pre_monsoon_cols  <- which(months %in% c(3, 4, 5))
    monsoon_cols      <- which(months %in% c(6, 7, 8, 9))
    post_monsoon_cols <- which(months %in% c(10, 11))
    
    # Compute stats
    season_stats <- function(x){
      c(
        Mean = mean(x, na.rm = TRUE),
        Min  = min(x, na.rm = TRUE),
        Max  = max(x, na.rm = TRUE),
        SD   = sd(x, na.rm = TRUE),
        Skewness = moments::skewness(x, na.rm = TRUE),
        Kurtosis = moments::kurtosis(x, na.rm = TRUE)
      )
    }
    
    df <- data.frame(
      Season = c("Winter","Pre-monsoon","Monsoon","Post-monsoon","Annual"),
      rbind(
        season_stats(station_rain[winter_cols]),
        season_stats(station_rain[pre_monsoon_cols]),
        season_stats(station_rain[monsoon_cols]),
        season_stats(station_rain[post_monsoon_cols]),
        season_stats(station_rain)
      )
    )
    
    df
  })
  
  
  output$seasonal_stats_table <- renderTable({
    req(seasonal_stats_result())
    seasonal_stats_result()
  }, striped = TRUE, bordered = TRUE, hover = TRUE,width ="100%")
  
  ##----------------------------------------------------Download the result------------------------------
  output$download_seasonal_stats <- downloadHandler(
    filename = function() {
      paste0("Seasonal_Stats_Station_", input$station, "_", input$year, ".csv")
    },
    content = function(file) {
      req(seasonal_stats_result())
      write.csv(seasonal_stats_result(), file, row.names = FALSE)
    }
  )
  ##--------------------------------------------Calculate Rasters for each season-----------------------
  current_raster_data <- reactive({
    req(r_daily(), input$viewType)
    
    # Initialize variables to return
    r_out <- NULL
    file_name <- ""
    title_txt <- ""
    
    if (input$viewType == "daily") {
      req(input$selected_day)
      idx <- which(format(daily_dates(), "%Y-%m-%d") == input$selected_day)
      r_out <- r_daily()[[idx]]
      file_name <- paste0("Daily_Rainfall_", input$selected_day)
      title_txt <- paste("Daily Rainfall:", input$selected_day)
      
    } else if (input$viewType == "monthly") {
      req(input$selected_month)
      ym <- names(month_labels())[month_labels() == input$selected_month]
      r_out <- r_monthly()[[ym]]
      file_name <- paste0("Monthly_Rainfall_", ym)
      title_txt <- paste("Monthly Rainfall:", input$selected_month)
      
    } else if (input$viewType == "seasonal") {
      req(input$selected_season)
      r_out <- r_seasonal()[[input$selected_season]]
      file_name <- paste0("Seasonal_Rainfall_", input$selected_season, "_", input$year)
      title_txt <- paste("Seasonal Rainfall:", input$selected_season, input$year)
      
    } else if (input$viewType == "annual") {
      r_out <- r_annual()
      file_name <- paste0("Annual_Rainfall_", input$year)
      title_txt <- paste("Annual Rainfall:", input$year)
    }
    
    # Return a list containing everything we need
    list(r = r_out, name = file_name, title = title_txt)
  })
  ##-----------------------------------------Render Map-------------------------------
  output$map <- renderTmap({
    # Get data from the reactive above
    data <- current_raster_data()
    req(data$r)
    
    r_show <- data$r
    
    # Apply Smoothing for Display (Visual only)
    r_show <- terra::disagg(r_show, fact = 5, method = "bilinear")
    
    tm_shape(r_show) +
      tm_raster(
        col.scale = tm_scale_continuous(values = "Blues"),
        col.legend = tm_legend(title = "Rainfall (mm)"),
        alpha = 0.8
      ) +
      tm_layout(
        main.title = data$title,
        main.title.position = "center",
        legend.outside = TRUE,
        legend.outside.position = "right",
        legend.frame = TRUE,
        legend.bg.color = "white"
      ) +
      tm_compass(type = "4star", position = c("left", "top"), size = 2) +
      tm_scale_bar(position = c("left", "bottom"))
  })
  
  ##---------------------------Download the Raster map-----------------------------------
  output$download_raster_map <- downloadHandler(
    filename = function() {
      paste0(current_raster_data()$name, ".tif")
    },
    content = function(file) {
      req(current_raster_data()$r)
      
      # NOTE: We download the RAW data (scientifically accurate), 
      # not the smoothed/disaggregated version used for display.
      writeRaster(current_raster_data()$r, file, overwrite = TRUE)
    }
  )
  
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
      very_wet_days<-apply(vals,1,R95p,threshold=p95)
      
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
    
    # Apply Smoothing (Optional, looks nicer)
    r_show <- terra::disagg(r_show, fact = 5, method = "bilinear")
    
    title_txt <- paste(input$climate_index, "-", m)
    
    # Check if user entered coordinates
    show_point <- !is.na(input$sel_lon) && !is.na(input$sel_lat)
    
    if(show_point){
      # 1. Create the point vector
      # We create a dataframe first to handle attributes easily
      pt_df <- data.frame(lon = input$sel_lon, lat = input$sel_lat)
      pt_vect <- vect(pt_df, geom = c("lon", "lat"), crs = crs(r_show))
      
      # 2. EXTRACT THE VALUE
      # We drill down into the raster at this point to get the value
      # terra::extract returns a dataframe. 
      extracted_data <- terra::extract(r_show, pt_vect)
      
      # The value is usually in the second column (first is ID), or first if ID=FALSE.
      # Let's safely grab the numeric value.
      # Since r_show is one layer, we grab the column corresponding to the layer name or index 2.
      val <- extracted_data[, 2] 
      
      # Handle cases where point is in the ocean (NA)
      val_display <- ifelse(is.na(val), "No Data", round(val, 2))
      
      # 3. Create the Label Text
      # Example: "PRCPTOT: 150.42"
      label_txt <- paste0(input$climate_index, ": ", val_display, "\n",
                          "(", round(input$sel_lon, 2), ", ", round(input$sel_lat, 2), ")")
      
      # Add this label back to the vector so tmap can read it
      pt_vect$map_label <- label_txt
      
      # 4. Render Map with Point and Label
      tm_shape(r_show) +
        tm_raster(
          col.scale = tm_scale_continuous(values = "Blues"),
          col.legend = tm_legend(title = input$climate_index),
          alpha = 0.8
        ) +
        tm_shape(pt_vect) +
        tm_symbols(size = 1.0, shape = 21, col = "red", border.col = "black") +
        # This adds the text label next to the dot
        tm_text("map_label", xmod = 1, ymod = 1, size = 1.0, bg.color="white", bg.alpha=0.7) + 
        tm_layout(
          main.title = title_txt,
          main.title.position = "center",
          legend.outside = TRUE,
          legend.outside.position = "right"
        )
      
    } else {
      # Default: Show raster only (No point selected)
      tm_shape(r_show) +
        tm_raster(
          col.scale = tm_scale_continuous(values = "Blues"),
          col.legend = tm_legend(title = input$climate_index),
          alpha = 0.8
        ) +
        tm_layout(
          main.title = title_txt,
          main.title.position = "center",
          legend.outside = TRUE,
          legend.outside.position = "right"
        )
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

 
