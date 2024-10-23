#' In this analysis we will be viewing the elevation data
#' from NASA and plotting it with leaflet.
#' We intend on creating some slope visualisations.

# install('leaflet')
# install('raster')
# install('sf')
# install('shiny')
# install('shinyMobile')
# renv::install("leafem")
# renv::install("mapview")
library(sf)
library(raster)
library(leaflet)
library(terra)
library(shiny)
library(shinyMobile)
library(leafem)
library(mapview)
library(leaflet.extras)

ELEVATION_FILE_PATH <- 'data'

elevation_files <- file.path(ELEVATION_FILE_PATH, list.files(ELEVATION_FILE_PATH))

elevation_rasters <- lapply(elevation_files, raster)

# Calculate slope for each raster separately and store in a list
slope_list <- lapply(elevation_rasters, function(r) {
  terrain(r, v = 'slope', unit = 'degrees')
})

ui <- f7Page(
  title = "Snowboarding Slope Map", 
  f7SingleLayout(
    navbar = f7Navbar(title = "Slope Visualization"), 
    f7Slider(
      inputId = "minSlope",
      label = "Minimum Slope (°)",
      min = 20,
      max = 90, 
      value = 20
    ),
    f7Slider(
      inputId = "maxSlope",
      label = "Maximum Slope (°)",
      min = 0, 
      max = 90, 
      value = 90
    ),
    leafletOutput("slopeMap", height = "70vh") # Set map height for mobile screen 
  )
)

# Server definition 

server <- function(input, output, session) {
  
  # Render the leaflet map
  output$slopeMap <- renderLeaflet({
    
    # Initialize leaflet map
    map <- leaflet() %>% 
      addProviderTiles("Thunderforest.Outdoors", options = providerTileOptions(apikey = Sys.getenv("THUNDERBIRD_API")))
    
    # Loop through each slope raster and add it to the map
    for (slope in slope_list) {
      slope_filtered <- calc(slope, function(x) {
        x[x < input$minSlope | x > input$maxSlope | x < 20] <- NA
        # filter out all slopes less than 20 degrees.
        return(x)
      })
      
      # Create a color palette for the slope values
      plot_palette <- colorRampPalette(c("green", "yellow", "orange", "red"))
      breaks <- c(20, 30, 40, 50, 60, Inf)  # Values from 20 to 60, and anything above 60 as "Inf"
      
      # Create a color palette for values between 20 and 60, and set black for values above 60
      custom_palette <- c(plot_palette(length(breaks) - 2), "black")
      
      map <- map %>%
        addGeoRaster(
          slope_filtered, 
          colorOptions = colorOptions(palette = custom_palette,
                                      breaks = breaks),
          opacity = 0.3,
          smooth = TRUE
        )
    }
    
    # Add a legend
    map <- map %>%
      addLegend(
        pal = colorNumeric(
          palette = custom_palette,
          domain = c(20, 80),
          na.color = "transparent"
        ),
        values = values(slope_filtered), 
        title = "Slope (Degrees)"
      ) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        circleOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      leaflet.extras::addSearchOSM(
        
      )
    
    # Return the final map
    map
  })
}

shinyApp(ui = ui, server = server)
