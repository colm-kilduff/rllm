library(aws.s3); library(R.utils); library(data.table); library(dplyr); library(sf); library(ggplot2); library(raster); library(stars); library(terra)
PROJ_CRS <- 4326
WRITE_INAT_FROM_S3 <- FALSE
CREATE_CHANTERELLE_OBSERVATIONS <- FALSE
READ_AND_WRITE_DOUGLAS_FIRS <- FALSE
READ_AND_WRITE_BC_SHP <- FALSE
READ_AND_WRITE_ELEVATION <- FALSE
READ_AND_WRITE_LANDCOVER <- FALSE
# In this analysis, we will be downloading the inaturalist observation and taxa datasets.
# We will then filter for Pacific golden chanterelle (Cantherellus formosus).
filter <- dplyr::filter

bc_shp <-
  st_read(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_shapefile.shp'))

### Creation of datasets

if (WRITE_INAT_FROM_S3){
  
  s3_bucket <- aws.s3::get_bucket('inaturalist-open-data')
  
  taxa_object <- aws.s3::get_object('taxa.csv.gz', s3_bucket)
  
  obs_file_path <- aws.s3::save_object('observations.csv.gz', s3_bucket, file.path('./code/202411-chanterelle-map/data', 'observations.csv.gz'))
  
  obs_file_path |> 
    gunzip()
  
  taxa_file_path <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "taxa.csv.gz")
  
  writeBin(taxa_object, taxa_file_path)
  
}

if (CREATE_CHANTERELLE_OBSERVATIONS){
  
  observations_csv <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "observations.csv")
  taxa_csv <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "taxa.csv")
  
  cantherellus_formosus_taxon <-
    fread(taxa_csv) |>
    dplyr::filter(name == 'Cantharellus formosus') |>
    as_tibble()
  
  chunk_size <- 10000000
  
  filtered_chunks <- list()
  
  row_start <- 1
  
  header <- fread(observations_csv, nrows = 0)
  
  # initialise chunk so that the while loop starts
  chunk <- list()
  
  while(!is.null(chunk)) {
    
  chunk <- tryCatch({
    fread(observations_csv, skip = row_start, nrows = chunk_size)
  },
  error = function(e) {
    message("An error occurred: ", e)
    NULL  # Return NULL value if an error occurs
  })
  
  if (!is.null(chunk)){
    
  if (nrow(chunk) == 0) break
  
  setnames(chunk, names(header))
  
  filtered_chunk <- 
    chunk |>
    dplyr::filter(taxon_id %in% cantherellus_formosus_taxon$taxon_id)
  
  if (nrow(filtered_chunk) > 0) {
    filtered_chunks <- append(filtered_chunks, list(filtered_chunk))
  }
  
  print(glue('{row_start}_to_{row_start + chunk_size}'))
  
  # Move to the next chunk
  row_start <- row_start + chunk_size
  
  }
  
  }
  
  chanterelle_observations <- bind_rows(filtered_chunks)
  
  chanterelle_observations |>
    write.csv('./code/202411-chanterelle-map/data/chanterelle_observations.csv')
  
}

if (READ_AND_WRITE_BC_SHP){

boundaries <- 
  sf::st_read(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/canvec_15M_CA_Admin/geo_political_region_2.shp'))

bc_shp <-
  boundaries |>
  filter(juri_en == 'British Columbia') |>
  summarise(geometry = st_union(geometry))

bc_shp |>
  st_write(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_shapefile.shp'))

}

if (READ_AND_WRITE_DOUGLAS_FIRS){
  
douglas_firs <- 
  raster(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/SCANFI_sps_douglasFir_SW_2020_v1.1.tif'))

bc_douglas_firs <-
  douglas_firs |> 
  crop(bc_shp |> st_transform(st_crs(douglas_firs)))

bc_douglas_firs |>
  raster::writeRaster(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_douglas_fir_raster.tif'))

}

if (READ_AND_WRITE_ELEVATION){
  
  elevation_raster <- 
    raster(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/mrdem-30-dtm-hillshade.tif'))
  
  bc_shp_transformed <- 
    bc_shp |>
    st_transform(st_crs(elevation_raster))
  
  bc_elevation_raster <-
    elevation_raster |>
    crop(st_bbox(bc_shp_transformed)) |>
    mask(bc_shp_transformed)
  
  bc_elevation_raster |>
    raster::writeRaster(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_elevation_raster.tif'), overwrite = TRUE)
  
}

if (READ_AND_WRITE_LANDCOVER){
  
  landcover_raster <-
    raster(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/SCANFI_att_nfiLandCover_SW_2020_v1.1.tif'))
  
  bc_shp_transformed <- 
    bc_shp |>
    st_transform(st_crs(landcover_raster))
  
  bc_landcover_raster_cropped <-
    landcover_raster |>
    crop(st_bbox(bc_shp_transformed))
  
  bc_landcover_masked <- 
    bc_landcover_raster_cropped |>
    mask(bc_shp_transformed)
  
  bc_landcover_masked |>
    raster::writeRaster(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_landcover_raster.tif'))
  
}

### Data Prep

chanterelle_observations <- 
  read.csv('./code/202411-chanterelle-map/data/chanterelle_observations.csv') |>
  filter(!is.na(latitude), !is.na(longitude), quality_grade != 'needs_id') |>
  as_tibble() |>
  st_as_sf(coords = c('longitude', 'latitude')) |>
  st_set_crs(4326)

bc_landcover <- stars::read_stars(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_landcover_raster.tif'))

bc_elevation <- stars::read_stars(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_elevation_raster.tif'))

### Chunking Landcover
#
# We need to chunk the landcover object into manageable pieces. We can deal with a maximum of maybe
# 20 million cells at a time.

dims <- dim(bc_landcover)

# Calculate chunk size
chunk_size <- 20000000 # 50 million cells
total_cells <- prod(dims[1:2])
num_chunks <- ceiling(total_cells / chunk_size)

# Determine grid size
rows_per_chunk <- ceiling(dims[1] / sqrt(num_chunks))
cols_per_chunk <- ceiling(dims[2] / sqrt(num_chunks))

start_x <- 1
while (start_x <= dims[1]) {
  start_y <- 1
  while (start_y <= dims[2]) {
    
    # Determine chunk boundaries
    end_x <- min(start_x + cols_per_chunk - 1, dims[1])
    end_y <- min(start_y + rows_per_chunk - 1, dims[2])
    
    # Extract landcover chunk
    landcover_chunk <- 
      bc_landcover[start_x:end_x, start_y:end_y] |>
      st_as_stars() |>
      rename(landcover_id = bc_landcover_raster.tif)
    
    # Add elevation data to the chunk
    landcover_chunk$elevation <- 
      bc_elevation |>
      st_warp(landcover_chunk) |>
      rename(elevation = bc_elevation_raster.tif) |>
      pull(elevation)
    
    # Advance start_y to next chunk
    start_y <- start_y + rows_per_chunk
  }
  
  # Advance start_x to next chunk
  start_x <- start_x + cols_per_chunk
}



# Now we can read in the observations and taxa datasets.
# We will use fread to first identify the row in taxa with Cantherellus formosus

#TODO: Get and read in elevation data
#TOOD: Download and read in NRCAN dataset
#TODO: Download and read in BC shapefile.
#TODO: Intersect all datasets and model using a gam with a thin-plate spline on lat long
#TODO: Use leaflet to visualise the model.
#TODO: Download and read in the BC crown land map

#This is to do work on a map of chanterelle locations in BC - the outcome of this will be a
#Model of chanterelle locations in BC
#Optional - Create a javascript app to host the model map

#App features could include user account for people
# to save the locations of their finds.
#Starting with chanterelle locations because it is the most sought after 
# mushroom for foragers.
# key variables in the model would be elevation and tree species if available (nrcan will do for
# a proxy for species)
# Final version of the application will have:
# - Probability of occurence of mushrooms
# - A computer vision model for mushroom identification
# - The ability to save routes and mushroom locations
# - application to be built using react native most likely with inspiration
# from gaia gps
# - Alerts to users based on weather patterns to go look for their chosen
# mushrooms based on time of year and weather.
# - Goal of application - Engage the general population in the study of mushrooms.
# Initially need early adopters for feedback etc. Doesn't need to be many people just
# an active group of feedback users.
# Add yes or no option for people did they find their mushroom or not.


# Some easy models to add would be morels (burn areas), 
# pine mushrooms - similar difficulty to Chanterelles

# Launch the app with the initial product of frequency of occurence
# Next feature can be add in particular mushrooms,
# Feature
# 89 for the year vs 20 a month.


