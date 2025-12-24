library(shiny)
library(shinydashboard)
library(terra)
library(tmap)
library(DT)
library(zoo)

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
      "Bega (Dry)"          = c(12, 1),
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
    
    selectInput("country", "Select Country:",
                choices = names(country_config), 
                selected = "India"),
    
    hr(),
    
    # --- SINGLE YEAR INPUT (Hidden on Multi-Year Tab) ---
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
    conditionalPanel(
      condition = "input.tabs == 'multi_year'",
      h4("Analysis Settings", style = "color: #b8c7ce;"),
      numericInput("start_year", "Start Year:", value = 1951, min = 1951, max = 2007),
      numericInput("end_year", "End Year:", value = 2007, min = 1951, max = 2007),
      selectInput("multi_season", "Select Season:", 
                  choices = c("Annual", "Winter", "Monsoon", "Post-monsoon"), # Default, updates server-side
                  selected = "Annual"),
      hr()
    ),
    
    
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
    tags$head(tags$style(HTML(".box { box-shadow: none; border-top: none; }"))),
    
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
      
      #---- TAB 3: Multi-Year Analysis ----
      tabItem(tabName = "multi_year", 
             fluidRow(
               box(width=12,title ="Multi-Year Analysis Result",solidHeader =FALSE,
                   h4("Analysis Output will appear here."))
             )
              
         ),
      
      #---- TAB 4: Climate Indices (UPDATED) ----
      tabItem(
        tabName = "climate_indice",
        fluidRow(
          column(
            width = 4,
            box(width = 12, title = "Select Climate Index", solidHeader = FALSE,
                
                selectInput("climate_index", "Climate Index:",
                            choices = c("PRCPTOT", "CDD", "RxDday", "Rnnmm", "CWD", 
                                        "R95p", "R99p", "R95pTOT", "R99pTOT")),
                
                # Dynamic Parameters
                uiOutput("index_parameters"),
                
                hr(),
                
                # Dynamic Time Scale Selector (Changed from static radioButtons)
                uiOutput("index_scale_ui"),
                
                # Month Selector (Visible only if Monthly selected)
                conditionalPanel(
                  condition = "input.index_scale == 'monthly'",
                  uiOutput("index_month_selector")
                ),
                
                br(),
                
                div(style="display:flex; gap:10px;",
                    actionButton("compute_index", "Compute Index", icon = icon("cogs"), class = "btn-success"),
                    downloadButton("download_index", "Download Result", class = "btn-default")
                )
            ),
            
            box(width = 12, title = "Description", solidHeader = FALSE,
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
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # --- Config & Defaults ---
  current_config <- reactive({
    req(input$country)
    country_config[[input$country]]
  })
  
  observeEvent(input$country, {
    req(current_config())
    cfg <- current_config()
    ##Update Single Year Input(Raster/Indices)
    updateNumericInput(session, "year", value = cfg$year_range[1], min = cfg$year_range[1], max = cfg$year_range[2])
    
    ##Update Lat/Lon Defaults
    updateNumericInput(session, "sidebar_lat", value = cfg$default_loc["lat"])
    updateNumericInput(session, "sidebar_lon", value = cfg$default_loc["lon"])
    
    ##Update Multi Year Inputs (Start/End)
    updateNumericInput(session,"start_year",value =cfg$year_range[1],min=cfg$year_range[1],max=cfg$year_range[2])
    updateNumericInput(session,"end_year",value =cfg$year_range[2],min=cfg$year_range[1],max =cfg$year_range[2])
    
    # 4. Update Multi-Year Season Choices
    # Add 'Annual' to the specific country seasons
    season_choices <- c("Annual", names(cfg$seasons))
    updateSelectInput(session, "multi_season", choices = season_choices, selected = "Annual")
    
  })
  
  # --- Load Daily Data ---
  r_daily <- reactive({
    req(input$country, input$year)
    cfg <- current_config()
    nc_path <- paste0("C:/Hydrology-Project/Rainfall Trend/NCDF/", 
                      cfg$folder_name, "/Daily_nc_", input$year, ".nc")
    validate(need(file.exists(nc_path), paste("NetCDF file not found:", nc_path)))
    rast(nc_path)
  })
  
  daily_dates <- reactive({
    req(r_daily(), input$year)
    n <- nlyr(r_daily()) 
    seq(as.Date(paste0(input$year, "-01-01")), by = "day", length.out = n)
  })
  
  # --- UI Outputs for Raster Viewer ---
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
  
  # --- Raster Viewer Logic ---
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
      if (input$index_scale == "annual") {
        target_raster <- data
      } else {
        req(input$selected_index_month)
        if(input$selected_index_month %in% names(data)){
          target_raster <- data[[input$selected_index_month]]
        }
      }
    }
    
    if(is.null(target_raster)) return(NA)
    pt <- vect(coords, geom = c("lon", "lat"), crs = crs(target_raster))
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
  
  # --- Map Render ---
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
      tm <- tm + tm_shape(vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")) +
        tm_symbols(col = "red", size = 1.0, shape = 21, border.col = "white", border.lwd = 2) +
        tm_text("map_label", ymod = 1, bg.color="white", bg.alpha=0.7, fontface="bold")
    }
    tm
  })
  
  output$download_raster_map <- downloadHandler(
    filename = function() { paste0(current_raster_data()$name, ".tif") },
    content = function(file) { writeRaster(current_raster_data()$r, file, overwrite = TRUE) }
  )
  
  # ============================================================================
  # 4. CLIMATE INDICES LOGIC (Updated UI)
  # ============================================================================
  
  # 1. Dynamic UI for Parameters
  output$index_parameters <- renderUI({
    req(input$climate_index)
    idx <- input$climate_index
    
    if (idx == "RxDday") {
      numericInput("rolling_window", "Rolling Window (days):", value = 5, min = 1, max = 10)
    } else if (idx %in% c("PRCPTOT", "CDD","Rnnmm","CWD")) {
      numericInput("threshold", "Threshold (mm):", value = 1, min = 0, max = 200)
    } else if (idx %in% c("R95p", "R99p", "R95pTOT", "R99pTOT")) {
      tagList(
        numericInput("threshold", "Wet Day Threshold (mm):", value = 1, min = 0),
        numericInput("percentile_val", "Percentile (0-1):", 
                     value = if(grepl("99", idx)) 0.99 else 0.95, 
                     min = 0, max = 1, step = 0.01)
      )
    }
  })
  
  # 2. Dynamic UI for Time Scale (Logic Change Here)
  output$index_scale_ui <- renderUI({
    req(input$climate_index)
    
    # Indices that are Annual ONLY
    annual_only_indices <- c("CDD", "CWD", "R95p", "R99p", "R95pTOT", "R99pTOT")
    
    if (input$climate_index %in% annual_only_indices) {
      radioButtons("index_scale", "Calculation Time Scale:",
                   choices = c("Annual" = "annual"),
                   selected = "annual", inline = TRUE)
    } else {
      # Allow both for PRCPTOT, Rnnmm, RxDday
      radioButtons("index_scale", "Calculation Time Scale:",
                   choices = c("Monthly" = "monthly", "Annual" = "annual"),
                   selected = "monthly", inline = TRUE)
    }
  })
  
  # 3. Dynamic UI for Month Selector
  output$index_month_selector <- renderUI({
    # Only show if Monthly scale is selected
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
                   "R95p" = "<b>R95p</b>: Sum of precip on days exceeding user-defined percentile (default 95th).",
                   "R99p" = "<b>R99p</b>: Sum of precip on days exceeding user-defined percentile (default 99th).",
                   "R95pTOT" = "<b>R95pTOT</b>: % of total annual precip from R95p events.",
                   "R99pTOT" = "<b>R99pTOT</b>: % of total annual precip from R99p events."
    )
    HTML(desc)
  })
  
  # 4. Compute Indices
  indices_calculate <- eventReactive(input$compute_index, {
    req(r_daily(), input$climate_index)
    
    r <- r_daily()
    idx_type <- as.character(input$climate_index)
    scale <- input$index_scale
    
    # Safe Inputs
    thresh <- if(!is.null(input$threshold)) as.numeric(input$threshold) else 1
    win <- if(!is.null(input$rolling_window)) as.numeric(input$rolling_window) else 5
    pct_val <- if(!is.null(input$percentile_val)) as.numeric(input$percentile_val) else 0.95
    
    calc_idx_func <- function(stack_in) {
      if (idx_type == "PRCPTOT") return(app(stack_in, function(x) sum(x[x >= thresh], na.rm=TRUE)))
      else if (idx_type == "Rnnmm") return(app(stack_in, function(x) sum(x > thresh, na.rm=TRUE)))
      else if (idx_type == "CDD") return(app(stack_in, function(x) { d <- x < thresh; d[is.na(d)] <- FALSE; if(!any(d)) return(0); max(rle(d)$lengths[rle(d)$values]) }))
      else if (idx_type == "CWD") return(app(stack_in, function(x) { w <- x >= thresh; w[is.na(w)] <- FALSE; if(!any(w)) return(0); max(rle(w)$lengths[rle(w)$values]) }))
      else if (idx_type == "RxDday") return(app(stack_in, function(x) { if(length(x) >= win) max(rollapply(x, win, sum, align="left", fill=NA, na.rm=TRUE), na.rm=TRUE) else NA }))
      
      # --- PERCENTILE INDICES (Dynamic) ---
      else if (idx_type %in% c("R95p", "R99p", "R95pTOT", "R99pTOT")) {
        wet_days <- ifel(stack_in >= thresh, stack_in, NA)
        p_rast <- app(wet_days, fun=function(x) quantile(x, probs=pct_val, na.rm=TRUE))
        wet_mask <- stack_in > p_rast
        extreme_sum <- sum(stack_in * wet_mask, na.rm=TRUE)
        
        if (idx_type %in% c("R95p", "R99p")) {
          return(extreme_sum)
        } else {
          total_wet_sum <- sum(wet_days, na.rm=TRUE)
          return((extreme_sum / total_wet_sum) * 100)
        }
      }
      return(NULL)
    }
    
    withProgress(message = "Calculating Indices...", value = 0, {
      if (scale == "annual") {
        res <- calc_idx_func(r)
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
            res_m <- calc_idx_func(r_sub)
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
      
      tm <- tm + tm_shape(vect(coords, geom = c("lon", "lat"), crs = "EPSG:4326")) +
        tm_symbols(col = "red", size = 1.0, shape = 21, border.col = "white", border.lwd = 2) +
        tm_text("map_label", ymod = 1, bg.color="white", bg.alpha=0.7, fontface="bold")
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