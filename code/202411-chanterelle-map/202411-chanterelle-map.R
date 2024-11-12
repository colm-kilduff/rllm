library(aws.s3)
WRITE_FROM_S3 <- TRUE
PROJ_CRS <- 4326

# In this analysis, we will be downloading the inaturalist observation and taxa datasets.
# We will then filter for Pacific golden chanterelle (Cantherellus formosus).


if (WRITE_FROM_S3){
  
  s3_bucket <- aws.s3::get_bucket('s3://inaturalist-open-data')
  
  aws.s3::get_bucket_df(s3_bucket, 'taxa')
  
  taxa_object <- aws.s3::get_object('taxa.csv.gz', s3_bucket)
  
  observation_object <- aws.s3::get_object('observations.csv.gz', s3_bucket)
  
  taxa_file_path <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "taxa.csv.gz")
  
  observations_file_path <- file.path(Sys.getenv('PROJ_DIR'), "code", "202411-chanterelle-map", "data", "observations.csv.gz")
  
  writeBin(taxa_object, taxa_file_path)
  
  writeBin(observation_object, observations_file_path)
  
}


# Now we can read in the observations and taxa datasets.
# We will use fread to first identify the row in taxa with Cantherellus formosus

#TODO: Get and read in elevation data
#TODO: Iterate through observations to filter only Cantherellus formosis
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


