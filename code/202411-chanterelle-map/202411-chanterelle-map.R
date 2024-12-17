# source(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/202411-chanterelle-map.R'))
library(aws.s3); library(R.utils); library(data.table); library(dplyr); library(sf)
library(ggplot2); library(raster); library(stars); library(terra); library(units)
library(pROC); library(mgcv)
PROJ_CRS <- 4326
OBS_BUFFER <- set_units(1000, 'm')
WRITE_INAT_FROM_S3 <- FALSE
CREATE_CHANTERELLE_OBSERVATIONS <- FALSE
READ_AND_WRITE_DOUGLAS_FIRS <- FALSE
READ_AND_WRITE_BC_SHP <- FALSE
READ_AND_WRITE_ELEVATION <- FALSE
READ_AND_WRITE_LANDCOVER <- FALSE
WRITE_COMBINED_BUFFER <- FALSE
READ_AND_WRITE_VFI <- FALSE
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

chanterelle_observations <- 
  read.csv('./code/202411-chanterelle-map/data/chanterelle_observations.csv') |>
  filter(!is.na(latitude), !is.na(longitude), quality_grade != 'needs_id') |>
  as_tibble() |>
  st_as_sf(coords = c('longitude', 'latitude')) |>
  st_set_crs(4326)

chanterelle_observations_buffer <-
  chanterelle_observations |>
  st_join(bc_shp |>
            st_transform(PROJ_CRS),
          join = st_intersects,
          left = FALSE) |>
  st_buffer(OBS_BUFFER)

# Making the VFI dataset useable.

if (READ_AND_WRITE_VFI){
  
  gdb_path <- file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/vfi_2023/VEG_COMP_LYR_R1_POLY_2023.gdb')
  
  query <- 
    "SELECT 
    PROJ_AGE_1, 
    PROJ_AGE_2, 
    PROJ_HEIGHT_1, 
    PROJ_HEIGHT_2, 
    WHOLE_STEM_BIOMASS_PER_HA, 
    BRANCH_BIOMASS_PER_HA, 
    SOIL_MOISTURE_REGIME_1, 
    SOIL_MOISTURE_REGIME_2, 
    SOIL_NUTRIENT_REGIME, 
    HERB_COVER_PCT, 
    BRYOID_COVER_PCT, 
    SPECIES_CD_1, 
    SPECIES_CD_2, 
    SPECIES_CD_3, 
    CROWN_CLOSURE, 
    VERTICAL_COMPLEXITY, 
    BEC_ZONE_CODE, 
    BEC_SUBZONE, 
    Shape
  FROM VEG_COMP_LYR_R1_POLY
  LIMIT 1000000 OFFSET 0"
  
  vfi <- st_read(gdb_path, query = query)
  
}


### Data Prep

bc_landcover <- stars::read_stars(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_landcover_raster.tif'))

bc_elevation <- stars::read_stars(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_elevation_raster.tif'))

bc_douglas_firs <- rast(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/bc_douglas_fir_raster.tif'))

soil_parent_materials <- 
  rast(file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "soil_parent_materials", "HaBC_PM.tif"))

### Chunking Landcover
#
# We need to chunk the landcover object into manageable pieces. We can deal with a maximum of maybe
# 20 million cells at a time.


if (WRITE_COMBINED_BUFFER){

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
  
  combined_buffer |>
    write_rds(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/combined_buffer.rds'))
  
  
} else {
  
  combined_buffer <- 
    read_rds(file.path(Sys.getenv('PROJ_DIR'), 'code/202411-chanterelle-map/data/combined_buffer.rds'))
  
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
         soil_class_grouping = case_when(
           soil_class == 2 ~ "Human-Induced",
           soil_class == 3 ~ "Gravity-Induced",
           soil_class == 4 ~ "Weathering-Induced",
           soil_class == 5 ~ "Wind-Induced",
           soil_class %in% c(6, 7, 9, 10, 16, 17, 18) ~ "Water-Induced",
           soil_class %in% c(8, 11) ~ "Ice-Induced",
           soil_class == 12 ~ "Organic",
           soil_class == 13 ~ "Rock",
           soil_class == 14 ~ "Undifferentiated",
           soil_class == 15 ~ "Volcanic"
         ),
         soil_class_grouping_b = case_when(soil_class %in% c(16, 17) ~ "16&17",
                                     soil_class %in% c(10, 7) ~ "10&7",
                                     T ~ 'Rest of soils'),
         slope_cut = cut(slope, breaks = c(-0.00000001, 10, 20, 30, 40, 50, 60, 90)),
         land_cover = case_when(
           land_cover == 1 ~ "Bryoid",
           land_cover == 2 ~ "Herbs",
           land_cover == 3 ~ "Rock",
           land_cover == 4 ~ "Shrub",
           land_cover == 5 ~ "Treed broadleaf",
           land_cover == 6 ~ "Treed conifer",
           land_cover == 7 ~ "Treed mixed",
           land_cover == 8 ~ "a_Water"
         ),
         land_cover_grouping = case_when(land_cover == "Treed conifer" ~ "Treed conifer",
                                         land_cover == "Treed mixed" ~ "Treed mixed",
                                         land_cover == "Treed broadleaf" ~ "Treed broadleaf",
                                         T ~ 'Rest of land covers'),
         ) |>
  group_by(soil_class) |>
  mutate(sample = sample(c("TRAIN", "TEST"), size = n(), replace = TRUE, prob = c(0.7, 0.3)),
         ) |>
  # removing NAs for simplicity
  filter(!is.na(elevation), !is.na(slope), !is.na(aspect), !is.na(land_cover), !is.na(douglas_firs), !is.na(soil_class)) |>
  ungroup()

# Now we can read in the observations and taxa datasets.
# We will use fread to first identify the row in taxa with Cantherellus formosus
#TODO: Join in variable chanterelle observations to the dataset. 
#TODO: Figure out I will model it with such a low amount of observations relative to data points.
#TODO: Intersect all datasets and model using a gam with a thin-plate spline on lat long
#TODO: Use leaflet to visualise the model.
#TODO: Download and read in the BC crown land map

### Analysis
#
# In this section we will be doing some data exploration for variables in the model.
# 
# We check out landcover.

chanterelle_df |>
  filter(sample == 'TRAIN') |>
  group_by(land_cover) |>
  summarise(n = n(), chanterelle_rate = mean(is_chanterelle)) |>
  arrange(desc(chanterelle_rate))

# We checkout soil.

chanterelle_df |>
  filter(sample == 'TRAIN') |>
  group_by(soil_class) |>
  summarise(n = n(), chanterelle_rate = mean(is_chanterelle)) |>
  arrange(desc(chanterelle_rate))

# Below is the soil type legend 
#
# 2	Anthroprogenic
# 3	Colluvium
# 4	Weathered Bedrock
# 5	Eolian
# 6	Fluvial
# 7	Glaciofluvial
# 8	Ice
# 9	Lacustrine
# 10	Glaciolacustrine
# 11	Till
# 12	Organic
# 13	Rock
# 14	Undifferentiated
# 15	Volcanic
# 16	Marine
# 17	Glaciomarine
# 18	Water


### Modelling

auc_by_group <- function(data, response_var, prediction_var, ...){
  
  group_vars <- quos(...)
  response <- enquo(response_var)
  prediction <- enquo(prediction_var)
  
  # Group data if grouping variables are provided
  data %>%
    group_by(!!!group_vars) %>%
    summarise(
      auc_value = as.numeric(auc(roc(!!response, !!prediction, quiet = TRUE))), 
      .groups = "drop"
    )
}

chanterelle_model_1 <- glm(is_chanterelle ~ elevation, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_1 |>
  summary()

chanterelle_df$prediction_1 <- predict(chanterelle_model_1, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_1, sample)


# Just slightly better than random.
#Next we will add land cover class

chanterelle_model_2 <- glm(is_chanterelle ~ elevation + as.factor(land_cover), data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_2 |>
  summary()

chanterelle_df$prediction_2 <- predict(chanterelle_model_2, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_2, sample)


# Most of the levels of the land cover are insignificant so we will try out land cover groupings.

chanterelle_model_3 <- glm(is_chanterelle ~ elevation + land_cover_grouping, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_3 |>
  summary()

chanterelle_df$prediction_3 <- predict(chanterelle_model_3, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_3, sample)

# Although 2 of the levels are insignificant they rank well so we will keep them.

# Next we will add % of crown cover that is douglas fir.


chanterelle_model_4 <- glm(is_chanterelle ~ elevation + land_cover_grouping + douglas_firs, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_4 |>
  summary()

chanterelle_df$prediction_4 <- predict(chanterelle_model_4, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_4, sample)

# Again, the variable is not significant but because it improves fit and makes logical sense, 
# we will keep it in the model. The test auc jumped up significantly from the douglas fir variable.
#
# Next we will add in the soil type.

chanterelle_model_5 <- glm(is_chanterelle ~ elevation + land_cover_grouping + douglas_firs + as.factor(soil_class), data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_5 |>
  summary()

chanterelle_df$prediction_5 <- predict(chanterelle_model_5, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_5, sample)

# This results in some overfitting so we will try the soil grouping instead.

chanterelle_model_6 <- glm(is_chanterelle ~  elevation + land_cover_grouping + douglas_firs + soil_class_grouping, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_6 |>
  summary()

chanterelle_df$prediction_6 <- predict(chanterelle_model_6, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_6, sample)

# The regular soil class by itself is thus far the best.
# We will try a simplified version of the soil grouping.

chanterelle_model_7 <- glm(is_chanterelle ~  elevation + land_cover_grouping + douglas_firs + soil_class_grouping_b, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_7 |>
  summary()

chanterelle_df$prediction_7 <- predict(chanterelle_model_7, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_7, sample)

# This is worse again, we will keep the soil class
# Now we will add slope.

chanterelle_model_8 <- glm(is_chanterelle ~ elevation + as.factor(land_cover) + douglas_firs + as.factor(soil_class) + slope, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_8 |>
  summary()

chanterelle_df$prediction_8 <- predict(chanterelle_model_8, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_8, sample)

# There is a big jump in slope for the train AUC and a minor jump in test AUC.

stop('TO HERE')

#TODO: Do something with slope to reduce the overfit it introduces
# Next we try a cut variable on slope.

chanterelle_model_4 <- glm(is_chanterelle ~ elevation + as.factor(land_cover) + douglas_firs + as.factor(soil_class) + slope_cut, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_4 |>
  summary()

chanterelle_df$prediction_4 <- predict(chanterelle_model_4, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_4, sample)

# Trying out aspect

chanterelle_model_5 <- glm(is_chanterelle ~ elevation + as.factor(land_cover) + douglas_firs + as.factor(soil_class) + slope + aspect, data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_5 |>
  summary()

chanterelle_df$prediction_5 <- predict(chanterelle_model_5, newdata = chanterelle_df, type = 'response')
  
chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_5, sample)

# We do not believe that a GLM is the best type of model for this. We would ideally
# like a model that can have non-linear effects. We will try out a gam model first.
# In particular, slope seems to be non-linear, aspect we would expect to be non-linear.

chanterelle_model_6 <- gam(is_chanterelle ~ s(elevation) + as.factor(land_cover) + douglas_firs + as.factor(soil_class) + slope + s(aspect), data = chanterelle_df |> filter(sample == 'TRAIN'), family = 'binomial')

chanterelle_model_6 |>
  summary()

chanterelle_df$prediction_6 <- predict(chanterelle_model_6, newdata = chanterelle_df, type = 'response')

chanterelle_df |>
  auc_by_group(is_chanterelle, prediction_6, sample)


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
