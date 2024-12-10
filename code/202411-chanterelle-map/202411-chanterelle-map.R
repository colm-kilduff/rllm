library(aws.s3); library(R.utils); library(data.table); library(dplyr); library(sf)
library(ggplot2); library(raster); library(stars); library(terra); library(units)
library(pROC)
PROJ_CRS <- 4326
OBS_BUFFER <- set_units(1000, 'm')
WRITE_INAT_FROM_S3 <- FALSE
CREATE_CHANTERELLE_OBSERVATIONS <- FALSE
READ_AND_WRITE_DOUGLAS_FIRS <- FALSE
READ_AND_WRITE_BC_SHP <- FALSE
READ_AND_WRITE_ELEVATION <- FALSE
READ_AND_WRITE_LANDCOVER <- FALSE
WRITE_COMBINED_BUFFER <- FALSE
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

bc_douglas_firs <- rast(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_douglas_fir_raster.tif'))

soil_parent_materials <- 
  rast(file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "soil_parent_materials", "HaBC_PM.tif"))

### Chunking Landcover
#
# We need to chunk the landcover object into manageable pieces. We can deal with a maximum of maybe
# 20 million cells at a time.


chanterelle_observations_buffer <-
  chanterelle_observations |>
  st_join(bc_shp |>
            st_transform(PROJ_CRS),
          join = st_intersects,
          left = FALSE) |>
  st_buffer(OBS_BUFFER)

ITERATION <- 1
list_of_combined_buffers <- list()

while(ITERATION <= nrow(chanterelle_observations_buffer)){
  
  chanterelle_buffer_iteration <-
    chanterelle_observations_buffer |>
    filter(row_number() == ITERATION) |>
    st_transform(st_crs(bc_elevation))

  cropped_elevation <- 
    rast(bc_elevation) |>
    crop(st_bbox(chanterelle_buffer_iteration)) |>
    mask(chanterelle_buffer_iteration) |>
    st_as_stars() |>
    st_transform(PROJ_CRS)
  
  cropped_slope <-
    rast(bc_elevation) |>
    crop(st_bbox(chanterelle_buffer_iteration)) |>
    mask(chanterelle_buffer_iteration) |>
    terrain(v = "slope", unit = 'degrees') |>
    st_as_stars() |>
    st_transform(PROJ_CRS)
  
  cropped_aspect <-
    rast(bc_elevation) |>
    crop(st_bbox(chanterelle_buffer_iteration)) |>
    mask(chanterelle_buffer_iteration) |>
    terrain(v = "aspect", unit = 'degrees') |>
    st_as_stars() |>
    st_transform(PROJ_CRS)

  cropped_landcover <-
    rast(bc_landcover) |>
    crop(st_bbox(chanterelle_buffer_iteration |> 
                   st_transform(
                     st_crs(bc_landcover)
                     )
                 )
         ) |>
    mask(chanterelle_buffer_iteration |>
           st_transform(
             st_crs(bc_landcover)
           )) |>
    st_as_stars() |>
    st_transform(PROJ_CRS) |>
    st_warp(cropped_elevation)
  
  cropped_bc_douglas_firs <-
    bc_douglas_firs |>
    crop(st_bbox(chanterelle_buffer_iteration |>
                   st_transform(
                     st_crs(bc_douglas_firs)
                   )
                 )
         ) |>
    mask(chanterelle_buffer_iteration |>
           st_transform(
             st_crs(bc_douglas_firs)
           )
    ) |>
    st_as_stars() |>
    st_transform(PROJ_CRS) |>
    st_warp(cropped_elevation)
  
  cropped_soil <-
    soil_parent_materials |>
    crop(st_bbox(chanterelle_buffer_iteration |>
                   st_transform(
                     st_crs(soil_parent_materials)
                   )
    )
    ) |>
    mask(chanterelle_buffer_iteration |>
           st_transform(
             st_crs(soil_parent_materials)
           )
    ) |>
    st_as_stars() |>
    st_transform(PROJ_CRS) |>
    st_warp(cropped_elevation)
  
  combined_raster <- c(cropped_elevation, cropped_slope, cropped_aspect, cropped_landcover, cropped_bc_douglas_firs, cropped_soil)
  
  names(combined_raster) <- c("elevation", "slope", "aspect", "land_cover", "douglas_firs", "soil_class")
  
  combined_raster <- st_as_sf(combined_raster)
  
  list_of_combined_buffers[[length(list_of_combined_buffers) + 1]] <- combined_raster

  ITERATION <- ITERATION + 1
  
}

combined_buffer <- 
  do.call(rbind, list_of_combined_buffers) |>
  distinct()

if (WRITE_COMBINED_BUFFER){
  
  combined_buffer |>
    write_rds(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/combined_buffer.rds'))
  
  
}

chanterelle_df <-
  combined_buffer |> 
  st_join(chanterelle_observations |> 
            st_transform(PROJ_CRS),
          join = st_intersects, 
          left = TRUE) |> 
  st_drop_geometry()  |>
  mutate(is_chanterelle = !is.na(observation_uuid))

set.seed(123)

chanterelle_df <- 
  chanterelle_df |>
  # There are too few observations of soil class 14
  mutate(soil_class = ifelse(soil_class == 14, 11, soil_class),
         slope_cut = cut(slope, breaks = c(10, 20, 30, 40, 50, 60))
         ) |>
  group_by(soil_class) |>
  mutate(sample = sample(c("TRAIN", "TEST"), size = n(), replace = TRUE, prob = c(0.7, 0.3)),
         ) |>
  # removing NAs for simplicity
  filter(!is.na(elevation), !is.na(slope), !is.na(aspect), !is.na(land_cover), !is.na(douglas_firs), !is.na(soil_class))

chanterelle_df |>
  group_by(is_chanterelle) |>
  summarise(n(), mean(elevation, na.rm = T), mean(land_cover, na.rm = T), mean(douglas_firs, na.rm =T))

# Now we can read in the observations and taxa datasets.
# We will use fread to first identify the row in taxa with Cantherellus formosus
#TODO: Join in variable chanterelle observations to the dataset. 
#TODO: Figure out I will model it with such a low amount of observations relative to data points.
#TODO: Intersect all datasets and model using a gam with a thin-plate spline on lat long
#TODO: Use leaflet to visualise the model.
#TODO: Download and read in the BC crown land map


### Modelling

chanterelle_model_1 <- glm(is_chanterelle ~ elevation + as.factor(land_cover) + douglas_firs, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_1 |>
  summary()

chanterelle_df$prediction_1 <- predict(chanterelle_model_1, newdata = chanterelle_df, type = 'response')

roc(chanterelle_df$is_chanterelle, chanterelle_df$prediction_1) |>
  auc()


# Next we will add in the soil type.

chanterelle_model_2 <- glm(is_chanterelle ~ elevation + as.factor(land_cover) + douglas_firs + as.factor(soil_class), data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_2 |>
  summary()

chanterelle_df$prediction_2 <- predict(chanterelle_model_2, newdata = chanterelle_df, type = 'response')

roc(chanterelle_df$is_chanterelle, chanterelle_df$prediction_2) |>
  auc()

# Now we will add slope.

chanterelle_model_3 <- glm(is_chanterelle ~ elevation + as.factor(land_cover) + douglas_firs + as.factor(soil_class) + slope, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_3 |>
  summary()

chanterelle_df$prediction_3 <- predict(chanterelle_model_3, newdata = chanterelle_df, type = 'response')

roc(chanterelle_df$is_chanterelle, chanterelle_df$prediction_3) |>
  auc()

# Next we try a cut variable on slope.

chanterelle_model_4 <- glm(is_chanterelle ~ elevation + as.factor(land_cover) + douglas_firs + as.factor(soil_class) + slope_cut, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_4 |>
  summary()

chanterelle_df$prediction_4 <- predict(chanterelle_model_4, newdata = chanterelle_df, type = 'response')

roc(chanterelle_df$is_chanterelle, chanterelle_df$prediction_4) |>
  auc()

#TODO: try out slope, crown cover, 

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


# Below we plot the chanterrelle observations in BC
ggplot() + 
  geom_sf(data = bc_shp) + 
  geom_sf(data = chanterelle_df
          )
