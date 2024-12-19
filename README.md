# Colm Kilduff's Personal Projects

Welcome to my GitHub repository where I store my personal data science and visualization projects. This repository features projects across a range of topics, including Shiny applications, geospatial visualizations, and other data-driven explorations. 

## Projects

### 1. Slope Visualization of the Lower Mainland (Shiny App)
A local Shiny app that visualizes slope gradients in the Lower Mainland of British Columbia, Canada. This project uses geospatial data to create an interactive map where users can explore slopes for outdoor activities like snowboarding and skiing.

#### Key Features
- **Slope Filtering**: Set minimum and maximum slope angles for targeted visualization.
- **Color Mapping**: Visualize slope steepness with a color-coded gradient.
- **Map Legend and Search**: Navigate the map with a detailed legend and location-based search options.

#### Technologies Used
- `R` and `Shiny` for app development
- `leaflet` for interactive mapping
- `raster` and `terra` for geospatial processing
- `shinyMobile` for mobile-friendly UI design

### 2. Chanterelle habitat mapping

#### Key Features

*   **iNaturalist Data Download:** Downloads observation and taxa data directly from the iNaturalist Open Data S3 bucket using the `aws.s3` package.
*   **Observation Filtering:** Filters iNaturalist observations to include only confirmed sightings of *Cantharellus formosus*.
*   **Environmental Data Integration:** Integrates various environmental datasets, including:
    *   BC provincial boundary shapefile.
    *   Douglas fir distribution raster.
    *   Elevation raster (DEM).
    *   Land cover raster.
    *   Soil parent materials raster.
*   **Geospatial Processing:** Performs geospatial operations using `sf`, `raster`, `terra`, and `stars`, including:
    *   Creating buffers around chanterelle observations.
    *   Clipping and masking rasters to the BC boundary.
    *   Extracting environmental data within observation buffers.
    *   Reprojecting data to a consistent coordinate system.
*   **Data Chunking:** Implements chunking strategies for efficient processing of large raster datasets.
*   **Feature Engineering:** Creates new variables such as slope, aspect, and categorical groupings of land cover and soil types.
*   **Train/Test Split:** Splits the data into training and testing sets for model development and evaluation.
*   **Logistic Regression:** Uses generalized linear models (GLMs) with a binomial family to model the probability of chanterelle presence.
*   **Model Evaluation:** Assesses model performance using metrics like Area Under the Curve (AUC) calculated with the `pROC` package.
*   **Iterative Model Refinement:** Iteratively improves models by adding and refining predictor variables.
*   **Spatial Modeling (Future):** Plans to incorporate spatial autocorrelation and other spatial modeling techniques (e.g., GAMs with spatial smoothers).

#### Technologies Used

*   `aws.s3`
*   `data.table`
*   `dplyr`
*   `sf`
*   `raster`
*   `stars`
*   `terra`
*   `units`
*   `pROC`
*   `mgcv` (for potential future GAMs)

## Getting Started

To explore this project locally, clone the repository and install the necessary R packages.


