library(shiny)
library(shinydashboard)
library(tmap)
library(terra)

ui <- dashboardPage(
  dashboardHeader(title = "Climate Indices Dashboard"),
  dashboardSidebar(
    numericInput("year", "Select Year:", value = 1951, min = 1951, max = 2100),
    selectInput("month", "Select Month:", choices = 1:12),
    selectInput("index", "Select Index:", choices = c("PRCPTOT", "Rxdday", "CDD")),
    actionButton("run_pipeline", "Compute Indices")
  ),
  dashboardBody(
    tmapOutput("map")
  )
)


server <- function(input, output, session) {
  
  # Reactive to run pipeline when button clicked
  observeEvent(input$run_pipeline, {
    year <- input$year
    
    # Call your pipeline function
    # Make sure pipeline function accepts 'year' and writes outputs to outputs folder
    compute_climate_indices(year = year, output_dir = "../outputs/")
    
    showNotification(paste("Pipeline run for year", year), type = "message")
  })
  
  output$map <- renderTmap({
    req(input$year, input$month, input$index)
    
    # Raster path dynamically
    raster_path <- paste0("../outputs/", input$index, "/", input$index, "_", input$year, "_", sprintf("%02d", input$month), ".tif")
    
    if(!file.exists(raster_path)) return(NULL)
    
    r <- rast(raster_path)
    
    tm_shape(r) +
      tm_raster(palette = "-Spectral", title = paste(input$index, input$month, input$year)) +
      tm_layout(legend.outside = TRUE)
  })
}

shinyApp(ui, server)