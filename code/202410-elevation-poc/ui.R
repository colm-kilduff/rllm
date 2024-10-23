library(sf)
library(raster)
library(leaflet)
library(terra)
library(shiny)
library(shinyMobile)
library(leafem)
library(mapview)
library(leaflet.extras)

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
