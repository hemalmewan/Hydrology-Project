library(shiny)
library(shinydashboard)
library(terra)
library(tmap)
library(DT)
library(zoo)
library(DBI)
library(RPostgres)
library(pool)

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================
db_pool <- dbPool(
  RPostgres::Postgres(),
  dbname = "shinydb",
  host = "postgres", 
  port = 5432,
  user = "shinyuser",
  password = "shiny_password"
)

onStop(function() {
  poolClose(db_pool)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================
country_config <- list(
  "India" = list(
    folder_name = "india",
    year_range  = c(1951, 2007),
    default_loc = c(lat = 20.5, lon = 78.9),
    seasons     = list(
      "Winter"       = c(1, 2),
      "Pre-monsoon"  = c(3, 4, 5),
      "Monsoon"      = c(6, 7, 8, 9),
      "Post-monsoon" = c(10, 11, 12)
    )
  ),
  "Ghana" = list(
    folder_name = "ghana",
    year_range  = c(1981, 2024),
    default_loc = c(lat = 7.9, lon = -1.0),
    seasons     = list(
      "Dry Season" = c(1, 2, 12, 11),
      "Wet Season" = c(3, 4, 5, 6, 7, 9, 10)
    )
  ),
  "Ethiopia" = list(
    folder_name = "ethiopia",
    year_range  = c(1981, 2024),
    default_loc = c(lat = 9.1, lon = 40.4),
    seasons     = list(
      "Bega (Dry)"           = c(12, 1),
      "Belg (Short Rains)"  = c(3, 4, 5),
      "Kiremt (Long Rains)" = c(6, 7, 8)
    )
  )
)

tmap_mode("view") 

# ==============================================================================
# UI
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = "Multi-Country Rainfall"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs", 
                menuItem("Raster Viewer", tabName = "raster", icon = icon("globe")),
                menuItem("Climate Indices", tabName = "climate_indice", icon = icon("cloud-rain")),
                menuItem("Multi-Year Analysis", tabName ="multi_year", icon = icon("history"))
    ),
    
    hr(),
    
    # Global Country Selector
    selectInput("country", "Select Country:",
                choices = names(country_config), 
                selected = "India"),
    
    hr(),
    
    # --- SINGLE YEAR / VIEWER CONTROLS ---
    # Kept in sidebar only for the "Viewer" tabs, hidden for Multi-Year
    conditionalPanel(
      condition = "input.tabs !== 'multi_year'",
      numericInput("year", "Enter Year:", value = 1951, min = 1951, max = 2007)
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'raster'",
      radioButtons("viewType", "Select Raster Type:",
                   choices = c("Daily" = "daily", 
                               "Monthly" = "monthly",
                               "Seasonal" = "seasonal",
                               "Annual" = "annual"),
                   selected = "annual"),
      uiOutput("date_or_month_selector"),
      hr()
    ),
    
    # Climate Indices (Single Year) Specifics
    conditionalPanel(
      condition = "input.tabs == 'climate_indice'",
      selectInput("climate_index", "Climate Index:",
                  choices = c("PRCPTOT", "CDD", "RxDday", "Rnnmm", "CWD", 
                              "R95p", "R99p", "R95pTOT", "R99pTOT")),
      uiOutput("index_parameters"),
      hr(),
      uiOutput("index_scale_ui"),
      conditionalPanel(
        condition = "input.index_scale == 'monthly'",
        uiOutput("index_month_selector")
      )
    ),
    
    # --- COMMON POINT EXTRACTION ---
    h4("Point Value Extraction", style = "margin-left: 15px; color: #b8c7ce;"),
    numericInput("sidebar_lat", "Latitude:", value = 20.5, step = 0.1),
    numericInput("sidebar_lon", "Longitude:", value = 78.9, step = 0.1),
    div(style = "text-align:center; margin-bottom:10px;",
        actionButton("btn_get_value", "Get Value", icon = icon("crosshairs"), 
                     class = "btn-success", style = "width:90%;")
    ),
    uiOutput("point_value_box")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .box { box-shadow: 0 1px 3px rgba(0,0,0,0.1); border-top: 3px solid #3c8dbc; }
      .well { background-color: #f7f7f7; border: 1px solid #e3e3e3; box-shadow: none; }
      .nav-tabs-custom { box-shadow: none; }
    "))),
    
    tabItems(
      #---- TAB 1: Raster Viewer ----
      tabItem(
        tabName = "raster",
        fluidRow(
          box(width = 12, title = "Rainfall Map", solidHeader = FALSE,
              tmapOutput("map", height = "800px"),
              hr(), 
              downloadButton("download_raster_map", "Download Current Map (.tif)", class = "btn-primary")
          )
        )
      ),
      
      #---- TAB 2: Climate Indices (Single Year) ----
      tabItem(
        tabName = "climate_indice",
        fluidRow(
          column(
            width = 4,
            box(width = 12, title = "Controls", solidHeader = FALSE,
                div(style="display:flex; gap:10px;",
                    actionButton("compute_index", "Compute Index", icon = icon("cogs"), class = "btn-success"),
                    downloadButton("download_index", "Download Result", class = "btn-default")
                ),
                hr(),
                htmlOutput("index_description")
            )
          ),
          column(
            width = 8,
            box(width = 12, title = "Climate Index Result", solidHeader = FALSE,
                tmapOutput("index_map", height = "600px")
            )
          )
        )
      ),
      
      #---- TAB 3: Multi-Year Analysis (REDESIGNED) ----
      tabItem(tabName = "multi_year",
              fluidRow(
                box(
                  width = 12, 
                  title = "Multi-Year Analysis Workflow", 
                  status = "primary", 
                  solidHeader = TRUE,
                  
                  # The Tabset Panel mimicking the "Climpact" tabs
                  tabsetPanel(id = "multi_year_subtabs", type = "tabs",
                              
                              # --- SUBTAB 1: Raster Visualization ---
                              tabPanel("Raster Visualization", icon = icon("globe"),
                                       br(),
                                       fluidRow(
                                         # LEFT COLUMN: INPUTS (The "Sidebar" inside the tab)
                                         column(width = 3,
                                                wellPanel(
                                                  h4("Configuration", style = "margin-top:0; border-bottom:1px solid #ddd; padding-bottom:5px;"),
                                                  numericInput("vis_start_year", "Start Year:", 1951, 1951, 2007),
                                                  numericInput("vis_end_year", "End Year:", 1956, 1951, 2007),
                                                  hr(),
                                                  selectInput("multi_time_scale", "Time Scale:", 
                                                              choices = c("Annual", "Seasonal", "Monthly"), 
                                                              selected = "Annual"),
                                                  
                                                  # Dynamic inputs based on scale
                                                  conditionalPanel(
                                                    condition = "input.multi_time_scale == 'Seasonal'",
                                                    uiOutput("ui_multi_season")
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.multi_time_scale == 'Monthly'",
                                                    selectInput("multi_month", "Select Month:", choices = month.name)
                                                  ),
                                                  br(),
                                                  actionButton("run_multi_raster", "Generate Maps", icon = icon("play"), 
                                                               class = "btn-primary", width = "100%")
                                                )
                                         ),
                                         
                                         # RIGHT COLUMN: VISUALIZATION
                                         column(width = 9,
                                                plotOutput("multi_year_raster_plot", height = "750px")
                                         )
                                       )
                              ),
                              
                              # --- SUBTAB 2: Climate Indices ---
                              tabPanel("Climate Indices", icon = icon("chart-line"),
                                       br(),
                                       fluidRow(
                                         # LEFT COLUMN: INPUTS
                                         column(width = 3,
                                                wellPanel(
                                                  h4("Settings", style = "margin-top:0; border-bottom:1px solid #ddd; padding-bottom:5px;"),
                                                  numericInput("idx_start_year", "Start Year:", 1951, 1951, 2007),
                                                  numericInput("idx_end_year", "End Year:", 1956, 1951, 2007),
                                                  hr(),
                                                  selectInput("multi_index_type", "Climate Index:",
                                                              choices = c("PRCPTOT", "CDD", "RxDday", "Rnnmm", "CWD", 
                                                                          "R95p", "R99p", "R95pTOT", "R99pTOT")),
                                                  
                                                  # Dynamic Parameters for Index
                                                  uiOutput("multi_index_params"),
                                                  br(),
                                                  actionButton("run_multi_index", "Compute Indices", icon = icon("cogs"), 
                                                               class = "btn-success", width = "100%")
                                                )
                                         ),
                                         
                                         # RIGHT COLUMN: VISUALIZATION
                                         column(width = 9,
                                                plotOutput("multi_year_index_plot", height = "750px")
                                         )
                                       )
                              )
                  )
                )
              )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # --- Helper Function: Index Calculation Logic ---
  calculate_index_logic <- function(stack_in, idx_type, thresh, win, pct_val, country, year_range) {
    if (idx_type == "PRCPTOT") { return(app(stack_in, function(x) sum(x[x >= thresh], na.rm=TRUE))) }
    else if (idx_type == "Rnnmm") { return(app(stack_in, function(x) sum(x > thresh, na.rm=TRUE))) }
    else if (idx_type == "CDD") { 
      return(app(stack_in, function(x) { 
        d <- x < thresh; d[is.na(d)] <- FALSE; if(!any(d)) return(0); 
        max(rle(d)$lengths[rle(d)$values]) 
      })) 
    }
    else if (idx_type == "CWD") { 
      return(app(stack_in, function(x) { 
        w <- x >= thresh; w[is.na(w)] <- FALSE; if(!any(w)) return(0); 
        max(rle(w)$lengths[rle(w)$values]) 
      })) 
    }
    else if (idx_type == "RxDday") { 
      return(app(stack_in, function(x) { 
        if(length(x) >= win) max(rollapply(x, win, sum, align="left", fill=NA, na.rm=TRUE), na.rm=TRUE) 
        else NA 
      })) 
    }
    else if (idx_type %in% c("R95p", "R99p", "R95pTOT", "R99pTOT")) {
      # Baseline Fetch Logic (Simplified for example)
      base_start <- year_range[1]
      base_end <- min(base_start + 29, year_range[2])
      baseline_years <- base_start:base_end
      
      baseline_stack_list <- list()
      for(b_year in baseline_years) {
        query <- "SELECT file_data FROM raster_storage WHERE country = $1 AND year = $2"
        res <- dbGetQuery(db_pool, query, params = list(tolower(country), b_year))
        if(nrow(res) > 0) {
          tmp <- tempfile(fileext = ".nc")
          writeBin(res$file_data[[1]], tmp)
          r_base_yr <- rast(tmp)
          if(crs(r_base_yr) == "") crs(r_base_yr) <- "EPSG:4326"
          baseline_stack_list[[length(baseline_stack_list)+1]] <- r_base_yr
        }
      }
      
      if(length(baseline_stack_list) == 0) return(NULL)
      full_baseline <- rast(baseline_stack_list)
      
      wet_base <- ifel(full_baseline >= thresh, full_baseline, NA)
      p_rast <- app(wet_base, fun=function(x) quantile(x, probs=pct_val, na.rm=TRUE))
      
      wet_mask <- stack_in > p_rast
      extreme_sum <- sum(stack_in * wet_mask, na.rm=TRUE)
      
      if (idx_type %in% c("R95p", "R99p")) {
        return(extreme_sum)
      } else {
        wet_days_curr <- ifel(stack_in >= thresh, stack_in, NA)
        total_wet_sum <- sum(wet_days_curr, na.rm=TRUE)
        return((extreme_sum / total_wet_sum) * 100)
      }
    }
    return(NULL)
  }
  
  # --- Config & Defaults ---
  current_config <- reactive({
    req(input$country)
    country_config[[input$country]]
  })
  
  observeEvent(input$country, {
    req(current_config())
    cfg <- current_config()
    
    # Update Standard Inputs (Viewer)
    updateNumericInput(session, "year", value = cfg$year_range[1], min = cfg$year_range[1], max = cfg$year_range[2])
    updateNumericInput(session, "sidebar_lat", value = cfg$default_loc["lat"])
    updateNumericInput(session, "sidebar_lon", value = cfg$default_loc["lon"])
    
    # Update Multi-Year Inputs (Tab 1: Visualization)
    updateNumericInput(session, "vis_start_year", value = cfg$year_range[1], min = cfg$year_range[1], max = cfg$year_range[2])
    updateNumericInput(session, "vis_end_year", value = min(cfg$year_range[1]+5, cfg$year_range[2]), min = cfg$year_range[1], max = cfg$year_range[2])
    
    # Update Multi-Year Inputs (Tab 2: Indices)
    updateNumericInput(session, "idx_start_year", value = cfg$year_range[1], min = cfg$year_range[1], max = cfg$year_range[2])
    updateNumericInput(session, "idx_end_year", value = min(cfg$year_range[1]+5, cfg$year_range[2]), min = cfg$year_range[1], max = cfg$year_range[2])
  })
  
  output$ui_multi_season <- renderUI({
    req(current_config())
    cfg <- current_config()
    selectInput("multi_season_select", "Select Season:", choices = names(cfg$seasons))
  })
  
  # --- Data Loading (Standard Viewer) ---
  r_daily <- reactive({
    req(input$country, input$year)
    country_key <- tolower(input$country)
    year_val <- input$year
    query <- "SELECT file_data FROM raster_storage WHERE country = $1 AND year = $2"
    result <- dbGetQuery(db_pool, query, params = list(country_key, year_val))
    validate(need(nrow(result) > 0, paste("No data found for", input$country, "in", year_val)))
    raw_data <- result$file_data[[1]]
    tmp_nc <- tempfile(fileext = ".nc")
    writeBin(raw_data, tmp_nc)
    r <- rast(tmp_nc)
    if(crs(r) == "") crs(r) <- "EPSG:4326"
    return(r)
  })
  
  daily_dates <- reactive({
    req(r_daily(), input$year)
    n <- nlyr(r_daily()) 
    seq(as.Date(paste0(input$year, "-01-01")), by = "day", length.out = n)
  })
  
  output$date_or_month_selector <- renderUI({
    req(daily_dates(), input$viewType)
    if (input$viewType == "daily") {
      dates <- daily_dates()
      dateInput("selected_day", "Select Date:", value = dates[1], min = min(dates), max = max(dates))
    } else if (input$viewType == "monthly") {
      selectInput("selected_month", "Select Month:", choices = month.name)
    } else if (input$viewType == "seasonal") {
      cfg <- current_config()
      selectInput("selected_season", "Select Season:", choices = names(cfg$seasons))
    } else return(NULL)
  })
  
  # --- Raster Viewer Logic (Tab 1) ---
  current_raster_data <- reactive({
    req(r_daily(), daily_dates(), input$viewType)
    r <- r_daily()
    dates <- daily_dates()
    cfg <- current_config()
    r_out <- NULL; title_txt <- ""; file_name <- ""
    
    if (input$viewType == "annual") {
      r_out <- sum(r, na.rm = TRUE)
      title_txt <- paste("Annual -", input$country, input$year)
      file_name <- paste0("Annual_", input$country, "_", input$year)
    } else if (input$viewType == "daily") {
      req(input$selected_day)
      idx <- which(dates == as.Date(input$selected_day))
      validate(need(length(idx) > 0, "Date not found"))
      r_out <- r[[idx]]
      title_txt <- paste("Daily -", input$selected_day)
      file_name <- paste0("Daily_", input$selected_day)
    } else if (input$viewType == "monthly") {
      req(input$selected_month)
      idx <- which(format(dates, "%B") == input$selected_month)
      validate(need(length(idx) > 0, "Month not found"))
      r_out <- sum(r[[idx]], na.rm=TRUE)
      title_txt <- paste("Monthly -", input$selected_month, input$year)
      file_name <- paste0("Monthly_", input$selected_month, "_", input$year)
    } else if (input$viewType == "seasonal") {
      req(input$selected_season)
      months <- as.numeric(format(dates, "%m"))
      target <- cfg$seasons[[input$selected_season]]
      idx <- which(months %in% target)
      validate(need(length(idx) > 0, "Season not found"))
      r_out <- sum(r[[idx]], na.rm=TRUE)
      title_txt <- paste(input$selected_season, "-", input$year)
      file_name <- paste0("Season_", input$selected_season, "_", input$year)
    }
    list(r = r_out, title = title_txt, name = file_name)
  })
  
  # --- Point Extraction ---
  selected_point_coords <- eventReactive(input$btn_get_value, {
    req(input$sidebar_lat, input$sidebar_lon)
    data.frame(lon = input$sidebar_lon, lat = input$sidebar_lat)
  })
  
  extracted_value_smart <- reactive({
    req(selected_point_coords(), input$tabs)
    coords <- selected_point_coords()
    target_raster <- NULL
    
    if (input$tabs == "raster") {
      req(current_raster_data()$r)
      target_raster <- current_raster_data()$r
    } else if (input$tabs == "climate_indice") {
      req(indices_calculate())
      data <- indices_calculate()
      if (input$index_scale == "annual") target_raster <- data
      else {
        req(input$selected_index_month)
        if(input$selected_index_month %in% names(data)) target_raster <- data[[input$selected_index_month]]
      }
    } else if(input$tabs == "multi_year"){
      if(input$multi_year_subtabs == "Raster Visualization") {
        req(multi_year_stack())
        target_raster <- multi_year_stack()[[nlyr(multi_year_stack())]]
      } else {
        req(multi_year_index_stack())
        target_raster <- multi_year_index_stack()[[nlyr(multi_year_index_stack())]]
      }
    }
    if(is.null(target_raster)) return(NA)
    pt <- vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")
    val_df <- terra::extract(target_raster, pt)
    val <- if(ncol(val_df) >= 2) as.numeric(val_df[1, 2]) else as.numeric(val_df[1, 1])
    return(val)
  })
  
  output$point_value_box <- renderUI({
    req(input$btn_get_value)
    val <- extracted_value_smart()
    label <- if(input$tabs == "climate_indice") "Index Value:" else "Precipitation:"
    txt <- if(is.na(val)) "No Data" else paste(round(val, 2))
    div(style = "background-color: #3c8dbc; color: white; padding: 10px; border-radius: 5px; margin-top: 10px; text-align: center;",
        h5(label, style="margin:0 0 5px; font-weight:bold;"), h4(txt, style="margin:0; font-weight:bold;"))
  })
  
  output$map <- renderTmap({
    data <- current_raster_data()
    req(data$r)
    r_show <- terra::disagg(data$r, fact = 5, method = "bilinear")
    tm <- tm_shape(r_show) + tm_raster(title = "Precip (mm)", palette = "Blues", style = "cont", alpha = 0.8) +
      tm_layout(main.title = data$title, main.title.position = "center", frame = FALSE)
    
    if (input$btn_get_value > 0 && input$tabs == "raster") {
      coords <- selected_point_coords()
      val <- extracted_value_smart()
      coords$map_label <- if(!is.na(val)) paste(round(val, 1), "mm") else "No Data"
      
      # --- UPDATED SINGLE YEAR OVERLAY ---
      tm <- tm + tm_shape(vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")) +
        tm_symbols(col = "red", size = 1.0, shape = 21, border.col = "white", border.lwd = 2) +
        tm_text("map_label", 
                size = 1.4,             # Larger Text
                col = "black", 
                bg.color = "white", 
                bg.alpha = 1.0,         # Fully Opaque Background
                ymod = 1.1, 
                fontface = "bold")
    }
    tm
  })
  
  output$download_raster_map <- downloadHandler(
    filename = function() { paste0(current_raster_data()$name, ".tif") },
    content = function(file) { writeRaster(current_raster_data()$r, file, overwrite = TRUE) }
  )
  
  # ============================================================================
  # MULTI-YEAR ANALYSIS LOGIC
  # ============================================================================
  
  # --- Subtab 1: Raster Visualization Data ---
  multi_year_stack <- eventReactive(input$run_multi_raster, {
    # Using NEW input IDs from the "Visualization" subtab
    req(input$vis_start_year, input$vis_end_year, input$multi_time_scale)
    
    cfg <- current_config()
    year_seq <- input$vis_start_year:input$vis_end_year
    scale <- input$multi_time_scale
    
    target_months <- NULL
    label_suffix <- ""
    
    if (scale == "Annual") {
      target_months <- 1:12
      label_suffix <- "Annual"
    } else if (scale == "Seasonal") {
      req(input$multi_season_select)
      target_months <- cfg$seasons[[input$multi_season_select]]
      label_suffix <- input$multi_season_select
    } else if (scale == "Monthly") {
      req(input$multi_month)
      target_months <- which(month.name == input$multi_month)
      label_suffix <- input$multi_month
    }
    
    stack_list <- list()
    withProgress(message = paste("Analyzing", scale, "Rainfall..."), value = 0, {
      for(i in seq_along(year_seq)) {
        yr <- year_seq[i]
        incProgress(1/length(year_seq), detail = paste("Processing:", yr))
        query <- "SELECT file_data FROM raster_storage WHERE country = $1 AND year = $2"
        res <- dbGetQuery(db_pool, query, params = list(tolower(input$country), yr))
        
        if(nrow(res) > 0) {
          tmp <- tempfile(fileext = ".nc")
          writeBin(res$file_data[[1]], tmp)
          r_daily_yr <- rast(tmp)
          if(crs(r_daily_yr) == "") crs(r_daily_yr) <- "EPSG:4326"
          
          dates <- seq(as.Date(paste0(yr, "-01-01")), by = "day", length.out = nlyr(r_daily_yr))
          months_idx <- as.numeric(format(dates, "%m"))
          relevant_layers_idx <- which(months_idx %in% target_months)
          
          if(length(relevant_layers_idx) > 0) {
            r_subset <- r_daily_yr[[relevant_layers_idx]]
            r_sum <- sum(r_subset, na.rm = TRUE)
            names(r_sum) <- paste(yr)
            stack_list[[as.character(yr)]] <- r_sum
          }
        }
      }
    })
    validate(need(length(stack_list) > 0, "No data found."))
    rast(stack_list)
  })
  
  # --- Subtab 2: Climate Indices Data ---
  output$multi_index_params <- renderUI({
    req(input$multi_index_type)
    idx <- input$multi_index_type
    if (idx == "RxDday") {
      numericInput("multi_rolling_window", "Rolling Window (days):", value = 5, min = 1, max = 10)
    } else if (idx %in% c("PRCPTOT", "CDD","Rnnmm","CWD")) {
      numericInput("multi_threshold", "Threshold (mm):", value = 1, min = 0, max = 200)
    } else if (idx %in% c("R95p", "R99p", "R95pTOT", "R99pTOT")) {
      tagList(
        numericInput("multi_threshold", "Wet Day Threshold (mm):", value = 1, min = 0),
        numericInput("multi_percentile_val", "Percentile (0-1):", 
                     value = if(grepl("99", idx)) 0.99 else 0.95, min = 0, max = 1, step = 0.01)
      )
    }
  })
  
  multi_year_index_stack <- eventReactive(input$run_multi_index, {
    # Using NEW input IDs from the "Indices" subtab
    req(input$idx_start_year, input$idx_end_year, input$multi_index_type)
    
    year_seq <- input$idx_start_year:input$idx_end_year
    idx_type <- input$multi_index_type
    cfg <- current_config()
    
    thresh <- if(!is.null(input$multi_threshold)) as.numeric(input$multi_threshold) else 1
    win <- if(!is.null(input$multi_rolling_window)) as.numeric(input$multi_rolling_window) else 5
    pct_val <- if(!is.null(input$multi_percentile_val)) as.numeric(input$multi_percentile_val) else 0.95
    
    stack_list <- list()
    withProgress(message = paste("Calculating Multi-Year", idx_type), value = 0, {
      for(i in seq_along(year_seq)) {
        yr <- year_seq[i]
        incProgress(1/length(year_seq), detail = paste("Year:", yr))
        
        query <- "SELECT file_data FROM raster_storage WHERE country = $1 AND year = $2"
        res <- dbGetQuery(db_pool, query, params = list(tolower(input$country), yr))
        if(nrow(res) > 0) {
          tmp <- tempfile(fileext = ".nc")
          writeBin(res$file_data[[1]], tmp)
          r_daily_yr <- rast(tmp)
          if(crs(r_daily_yr) == "") crs(r_daily_yr) <- "EPSG:4326"
          
          r_idx <- calculate_index_logic(r_daily_yr, idx_type, thresh, win, pct_val, input$country, cfg$year_range)
          if(!is.null(r_idx)) {
            names(r_idx) <- as.character(yr)
            stack_list[[as.character(yr)]] <- r_idx
          }
        }
      }
    })
    validate(need(length(stack_list) > 0, "No data could be calculated."))
    rast(stack_list)
  })
  
  # --- Render Plot: Raster Visualization ---
  output$multi_year_raster_plot <- renderPlot({
    req(multi_year_stack())
    data_stack <- multi_year_stack()
    tmap_mode("plot")
    scale <- input$multi_time_scale
    suffix <- if(scale=="Seasonal") input$multi_season_select else if(scale=="Monthly") input$multi_month else "Annual"
    
    tm <- tm_shape(data_stack) +
      tm_raster(style = "cont", palette = "YlGnBu", title = paste(suffix, "(mm)")) +
      tm_facets(ncol = 3, free.scales = FALSE) + 
      tm_layout(main.title = paste(input$country, "-", suffix, "Rainfall"), 
                main.title.position = "center", legend.outside = TRUE, frame = FALSE)
    
    if (input$btn_get_value > 0 && input$multi_year_subtabs == "Raster Visualization") {
      tm <- tm + generate_point_overlay(data_stack)
    }
    tm
  })
  
  # --- Render Plot: Climate Indices ---
  output$multi_year_index_plot <- renderPlot({
    req(multi_year_index_stack())
    data_stack <- multi_year_index_stack()
    tmap_mode("plot")
    pal <- if(input$multi_index_type == "CDD") "YlOrRd" else "YlGnBu"
    
    tm <- tm_shape(data_stack) +
      tm_raster(style = "cont", palette = pal, title = input$multi_index_type) +
      tm_facets(ncol = 3, free.scales = FALSE) + 
      tm_layout(main.title = paste("Multi-Year Trend:", input$multi_index_type), 
                main.title.position = "center", legend.outside = TRUE, frame = FALSE)
    
    if (input$btn_get_value > 0 && input$multi_year_subtabs == "Climate Indices") {
      tm <- tm + generate_point_overlay(data_stack)
    }
    tm
  })
  
  # --- Helper: Generate Point Overlay (UPDATED FOR CLARITY) ---
  generate_point_overlay <- function(data_stack) {
    coords <- selected_point_coords() 
    pt_vect <- vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")
    extracted_vals <- terra::extract(data_stack, pt_vect)
    
    pt_list <- list()
    for(yr in names(data_stack)) {
      val <- extracted_vals[[yr]]
      label_txt <- if(is.na(val)) "NA" else paste(round(val, 1))
      pt_list[[yr]] <- data.frame(lon = coords$lon, lat = coords$lat, year_match = yr, display_val = label_txt)
    }
    pt_data <- do.call(rbind, pt_list)
    pt_sf <- vect(pt_data, geom=c("lon", "lat"), crs="EPSG:4326")
    
    tm_shape(pt_sf) +
      # Clear Red Dot
      tm_symbols(col = "red", size = 0.8, shape = 21, border.col = "white", border.lwd = 1.5) + 
      # High Contrast Text Label
      tm_text("display_val", 
              size = 1.3,           # Larger
              col = "black", 
              bg.color = "white", 
              bg.alpha = 1.0,       # Opaque Background
              ymod = 1.1, 
              fontface = "bold") + 
      tm_facets(by = "year_match")
  }
  
  # ============================================================================
  # SINGLE YEAR INDICES LOGIC (Existing - Now using Helper)
  # ============================================================================
  output$index_parameters <- renderUI({
    req(input$climate_index)
    idx <- input$climate_index
    if (idx == "RxDday") numericInput("rolling_window", "Rolling Window (days):", value = 5, min = 1, max = 10)
    else if (idx %in% c("PRCPTOT", "CDD","Rnnmm","CWD")) numericInput("threshold", "Threshold (mm):", value = 1, min = 0, max = 200)
    else if (idx %in% c("R95p", "R99p", "R95pTOT", "R99pTOT")) {
      tagList(
        numericInput("threshold", "Wet Day Threshold (mm):", value = 1, min = 0),
        numericInput("percentile_val", "Percentile (0-1):", value = if(grepl("99", idx)) 0.99 else 0.95, min = 0, max = 1, step = 0.01)
      )
    }
  })
  
  output$index_scale_ui <- renderUI({
    req(input$climate_index)
    if (input$climate_index %in% c("CDD", "CWD", "R95p", "R99p", "R95pTOT", "R99pTOT")) {
      radioButtons("index_scale", "Calculation Time Scale:", choices = c("Annual" = "annual"), selected = "annual", inline = TRUE)
    } else {
      radioButtons("index_scale", "Calculation Time Scale:", choices = c("Monthly" = "monthly", "Annual" = "annual"), selected = "monthly", inline = TRUE)
    }
  })
  
  output$index_month_selector <- renderUI({
    req(input$index_scale == "monthly")
    selectInput("selected_index_month", "Select Month to View:", choices = month.name)
  })
  
  output$index_description <- renderUI({
    req(input$climate_index)
    desc <- switch(input$climate_index,
                   "PRCPTOT" = "<b>PRCPTOT</b>: Total precipitation on wet days (>= threshold).",
                   "CDD" = "<b>CDD</b>: Maximum consecutive dry days (< threshold).",
                   "CWD" = "<b>CWD</b>: Maximum consecutive wet days (>= threshold).",
                   "Rnnmm" = "<b>Rnnmm</b>: Count of days where precipitation > threshold.",
                   "RxDday" = "<b>RxDday</b>: Maximum precipitation over a rolling window.",
                   "R95p" = "<b>R95p</b>: Sum of precip on days exceeding 95th percentile of 30-year baseline.",
                   "R99p" = "<b>R99p</b>: Sum of precip on days exceeding 99th percentile of 30-year baseline.",
                   "R95pTOT" = "<b>R95pTOT</b>: % of total annual precip from R95p events.",
                   "R99pTOT" = "<b>R99pTOT</b>: % of total annual precip from R99p events."
    )
    HTML(desc)
  })
  
  indices_calculate <- eventReactive(input$compute_index, {
    req(r_daily(), input$climate_index)
    r <- r_daily()
    idx_type <- as.character(input$climate_index)
    scale <- input$index_scale
    cfg <- current_config()
    thresh <- if(!is.null(input$threshold)) as.numeric(input$threshold) else 1
    win <- if(!is.null(input$rolling_window)) as.numeric(input$rolling_window) else 5
    pct_val <- if(!is.null(input$percentile_val)) as.numeric(input$percentile_val) else 0.95
    
    withProgress(message = "Calculating Indices...", value = 0, {
      if (scale == "annual") {
        res <- calculate_index_logic(r, idx_type, thresh, win, pct_val, input$country, cfg$year_range)
        names(res) <- paste(idx_type, "Annual", input$year)
        return(res)
      } else {
        dates <- daily_dates()
        months <- as.numeric(format(dates, "%m"))
        monthly_stack <- list()
        for(m in 1:12) {
          incProgress(1/12, detail = month.name[m])
          idx_m <- which(months == m)
          if(length(idx_m) > 0) {
            r_sub <- r[[idx_m]]
            res_m <- calculate_index_logic(r_sub, idx_type, thresh, win, pct_val, input$country, cfg$year_range)
            names(res_m) <- month.name[m]
            monthly_stack[[month.name[m]]] <- res_m
          }
        }
        return(rast(monthly_stack))
      }
    })
  })
  
  output$index_map <- renderTmap({
    req(indices_calculate())
    data <- indices_calculate()
    scale <- input$index_scale
    if (scale == "annual") {
      r_show <- data
      title_txt <- paste(input$climate_index, "- Annual", input$year)
    } else {
      req(input$selected_index_month)
      if(input$selected_index_month %in% names(data)){
        r_show <- data[[input$selected_index_month]]
        title_txt <- paste(input$climate_index, "-", input$selected_index_month, input$year)
      } else return(NULL)
    }
    r_show <- terra::disagg(r_show, fact = 5, method = "bilinear")
    pal <- if(input$climate_index %in% c("CDD")) "YlOrRd" else "YlGnBu"
    tm <- tm_shape(r_show) +
      tm_raster(title = "Index Value", palette = pal, style = "cont", alpha = 0.8) +
      tm_layout(main.title = title_txt, main.title.position = "center", frame = FALSE)
    
    if (input$btn_get_value > 0 && input$tabs == "climate_indice") {
      coords <- selected_point_coords()
      val <- extracted_value_smart()
      label_txt <- if(!is.na(val)) paste(round(val, 2)) else "No Data"
      coords$map_label <- label_txt
      
      # --- UPDATED SINGLE YEAR INDEX OVERLAY ---
      tm <- tm + tm_shape(vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")) +
        tm_symbols(col = "red", size = 1.0, shape = 21, border.col = "white", border.lwd = 2) +
        tm_text("map_label", 
                size = 1.4,             # Larger Text
                col = "black", 
                bg.color = "white", 
                bg.alpha = 1.0,         # Opaque Background
                ymod = 1.1, 
                fontface = "bold")
    }
    tm
  })
  
  output$download_index <- downloadHandler(
    filename = function() { 
      suf <- if(input$index_scale == "monthly") input$selected_index_month else "Annual"
      paste0(input$climate_index, "_", suf, "_", input$year, ".tif") 
    },
    content = function(file) {
      req(indices_calculate())
      data <- indices_calculate()
      to_save <- if(input$index_scale == "monthly") data[[input$selected_index_month]] else data
      writeRaster(to_save, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)