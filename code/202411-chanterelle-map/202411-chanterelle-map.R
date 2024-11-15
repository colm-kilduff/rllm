library(aws.s3); library(R.utils); library(data.table); library(dplyr); library(sf); library(ggplot2)
WRITE_FROM_S3 <- FALSE
PROJ_CRS <- 4326
CREATE_CHANTERELLE_OBSERVATIONS <- FALSE
# In this analysis, we will be downloading the inaturalist observation and taxa datasets.
# We will then filter for Pacific golden chanterelle (Cantherellus formosus).
filter <- dplyr::filter

if (WRITE_FROM_S3){
  
  s3_bucket <- aws.s3::get_bucket('inaturalist-open-data')
  
  taxa_object <- aws.s3::get_object('taxa.csv.gz', s3_bucket)
  
  obs_file_path <- aws.s3::save_object('observations.csv.gz', s3_bucket, file.path('./code/202411-chanterelle-map/data', 'observations.csv.gz'))
  
  obs_file_path |> 
    gunzip()
  
  taxa_file_path <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "taxa.csv.gz")
  
  writeBin(taxa_object, taxa_file_path)
  
}

observations_csv <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "observations.csv")
taxa_csv <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "taxa.csv")

cantherellus_formosus_taxon <-
  fread(taxa_csv) |>
  dplyr::filter(name == 'Cantharellus formosus') |>
  as_tibble()

if (CREATE_CHANTERELLE_OBSERVATIONS){
  
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

chanterelle_observations <- 
  read.csv('./code/202411-chanterelle-map/data/chanterelle_observations.csv') |>
  filter(!is.na(latitude), !is.na(longitude), quality_grade != 'needs_id') |>
  as_tibble() |>
  st_as_sf(coords = c('longitude', 'latitude')) |>
  st_set_crs(4326)

# plotting the observations

ggplot() +
  # add a background map for this
  geom_sf(data = chanterelle_observations)

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
# - application to be built using react most likely with inspiration
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


