# Script to calculate area weights for census tract to leveed area conversion
# This script:
# 1. Loads census tract geometries and leveed area data
# 2. Calculates intersection areas and weights for each leveed area
# 3. Distinguishes between weights for averages vs totals
# 4. Saves the weights for later use in variable conversion

# Clear environment and set up basic logging
rm(list = ls())

# Create logs directory and set up basic logging
dir.create("logs", showWarnings = FALSE)

# Load required packages
cat("Loading required packages...\n")
library(dplyr)
library(sf)
library(parallel)
library(doSNOW)
library(geojsonsf)

#------------------------------------------------------------------------------
# 1. Load Data
#------------------------------------------------------------------------------

# Load census tract geometries (downloaded and saved from Tigris)
tracts <- st_read("Data/Raw/Census/CA_tracts_2021.gpkg") %>%
  mutate(tract_area = as.numeric(st_area(.)))
cat("Census tracts loaded. Rows:", nrow(tracts), "\n")

# Load leveed area data
leveed_area <- geojson_sf("Data/Cleaned/NLDscrape2024_LeveeArea2023_joined.geojson") %>% 
  st_make_valid()
cat("Leveed area data loaded. Rows:", nrow(leveed_area), "\n")

# Remove test limitation if present
#leveed_area <- leveed_area[1:50,]

#------------------------------------------------------------------------------
# 2. Calculate Area Weights
#------------------------------------------------------------------------------

# Set up parallel processing
cores <- detectCores() - 1
cat("Using", cores, "cores for parallel processing\n")

cl <- makeCluster(cores)
registerDoSNOW(cl)

# Ensure consistent CRS
tracts <- tracts %>% 
  st_transform(st_crs(leveed_area)) %>% 
  st_make_valid()
cat("CRS transformed and geometries made valid\n")

# Set up progress bar
pb <- txtProgressBar(min = 0, max = nrow(leveed_area), style = 3)
opts <- list(progress = function(x) setTxtProgressBar(pb, x))

cat("Starting calculation of area weights for", nrow(leveed_area), "leveed areas...\n")

# Process each leveed area to calculate weights
weight_results <- foreach(
  i = 1:nrow(leveed_area),
  .combine = 'rbind',
  .packages = c('sf', 'dplyr'),
  .options.snow = opts,
  .errorhandling = 'pass'
) %dopar% {
  tryCatch({
    # Find intersections for this leveed area
    overlapping_tracts <- st_intersection(leveed_area[i,], tracts)
    
    if (nrow(overlapping_tracts) == 0) {
      return(data.frame(
        levee_id = leveed_area$id[i],
        tract_geoid = NA_character_,
        intersection_area = NA_real_,
        tract_area = NA_real_,
        weight_for_averages = NA_real_,  # intersection_area / sum(intersection_areas)
        weight_for_totals = NA_real_,    # intersection_area / tract_area
        has_intersection = FALSE
      ))
    }
    
    # Calculate intersection areas
    intersection_areas <- as.numeric(st_area(overlapping_tracts))
    tract_areas <- overlapping_tracts$tract_area
    
    # Calculate weights for AVERAGES (e.g., median income, median home value)
    # Weight = intersection_area / sum(all_intersection_areas_for_this_levee)
    weights_for_averages <- intersection_areas / sum(intersection_areas)
    
    # Calculate weights for TOTALS (e.g., population, number of households)
    # Weight = intersection_area / tract_area (proportional allocation)
    weights_for_totals <- intersection_areas / tract_areas
    
    # Return results for this leveed area
    data.frame(
      levee_id = leveed_area$id[i],
      tract_geoid = overlapping_tracts$GEOID,
      intersection_area = intersection_areas,
      tract_area = tract_areas,
      weight_for_averages = weights_for_averages,
      weight_for_totals = weights_for_totals,
      has_intersection = TRUE
    )
  }, error = function(e) {
    return(data.frame(
      levee_id = leveed_area$id[i],
      tract_geoid = NA_character_,
      intersection_area = NA_real_,
      tract_area = NA_real_,
      weight_for_averages = NA_real_,
      weight_for_totals = NA_real_,
      has_intersection = FALSE
    ))
  })
}

# Close progress bar
close(pb)

# Stop parallel processing
stopCluster(cl)

cat("Parallel processing completed\n")

#------------------------------------------------------------------------------
# 3. Process and Save Results
#------------------------------------------------------------------------------

cat("Processing weight results...\n")

# Convert to data frame and clean up
weights_df <- as.data.frame(weight_results)

# Save the area weights
write.csv(weights_df, "Data/Cleaned/CensusTractToLeveeArea_Weights.csv", row.names = FALSE)

