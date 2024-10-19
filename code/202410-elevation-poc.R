#' In this analysis we will be viewing the elevation data
#' from NASA and plotting it with leaflet.
#' We intend on creating some slope visualisations.


install('leaflet')
install('raster')
install('sf')
install('shiny')
install('shinyMobile')
renv::install("leafem")
renv::install("mapview")
library(raster); library(sf)
library(leaflet);library(terra)
library(shiny); library(shinyMobile)
library(leafem); library(mapview)


ELEVATION_FILE_PATH <- 'data/N49W123.hgt'

elevation_raster <- raster(ELEVATION_FILE_PATH)

slope <- terrain(elevation_raster, v = 'slope', unit = 'degrees')

slope <- aggregate(slope, fact = 4)

ui <- f7Page(
  title = "Snowboarding Slope Map", 
  f7SingleLayout(
    navbar = f7Navbar(title = "Slope Visualization"), 
    f7Slider(
      inputId = "minSlope",
      label = "Minimum Slope (°)",
      min = 0,
      max = 90, 
      value = 25),
    f7Slider(
      inputId = "maxSlope",
      label = "Maximum Slope (°)",
      min = 0, 
      max = 90, 
      value = 40),
    leafletOutput("slopeMap",
                  height = "70vh") # Set map height for mobile screen 
  )
)

# Server definition 

server <- function(input, output, session) {
  
  # Render the leaflet map
  output$slopeMap <- renderLeaflet({
    
    # Filter the slope raster based on input from the sliders
    slope_filtered <- calc(slope, function(x) {
      x[x < input$minSlope | x > input$maxSlope] <- NA
      return(x)
    })
    
    # Create a color palette for the slope values
    pal <- colorNumeric(
      palette = "RdYlBu", 
      domain = values(slope_filtered), 
      na.color = "transparent"
    )
    
    # Render the map with the filtered slope raster
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      addRasterImage(
        slope_filtered, 
        colors = pal, 
        opacity = 0.7
      ) %>%
      addLegend(
        pal = pal, 
        values = values(slope_filtered), 
        title = "Slope (Degrees)"
      )
  })
}


shinyApp(ui = ui, server = server)


# TODO: Use leafem and mapview to tile the map and figure out how to get colour working
