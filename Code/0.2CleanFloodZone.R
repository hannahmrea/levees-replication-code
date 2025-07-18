# Load required packages
library(sf)
library(dplyr)
library(tidyr)

# Read FEMA flood hazard data
message("Reading FEMA flood hazard data...")
fld_haz <- st_read("Data/Raw/FEMA/NFHL_06_20230323.gdb", 
                   layer = "S_FLD_HAZ_AR")

# Function to classify flood zones into more detailed categories
classify_floodzone <- function(zone, subtype) {
  if (zone %in% c('A', 'AE', 'AH', 'AO', 'V', 'VE', 'A99')) {
    return('SFHA')  # Special Flood Hazard Area
  } else if (zone == 'X' && subtype == "AREA WITH REDUCED FLOOD RISK DUE TO LEVEE") {
    return('LEVEE_PROTECTED')  # X zone with levee protection
  } else if (zone == 'X') {
    return('X_OTHER')  # Other X zones
  } else {
    return(NA)  # Areas not relevant for flood zone analysis (e.g., ocean)
  }
}

# Process and unify flood zones
message("Processing and unifying flood zones...")

# First, ensure the geometry is valid and clean
fld_haz <- fld_haz %>%
  st_make_valid() %>%
  filter(!st_is_empty(.)) %>%
  st_buffer(0)

# Then add zone classification
fld_haz <- fld_haz %>%
  mutate(
    ZONE_CLASS = factor(mapply(classify_floodzone, FLD_ZONE, ZONE_SUBTY))
  ) %>%
  filter(!is.na(ZONE_CLASS))

# Group and summarize by zone class
unified_zones <- fld_haz %>%
  group_by(ZONE_CLASS) %>%
  summarise(
    n_features = n(),
    .groups = "drop"
  ) %>%
  st_make_valid()

# Save unified zones as R object
message("Saving unified zones...")
saveRDS(unified_zones, "Data/Cleaned/unified_flood_zones.rds")

# Save individual zone types as geojson for sharing/viewing
message("Saving individual zone types as GeoJSON...")
unified_zones %>%
  filter(ZONE_CLASS == 'SFHA') %>%
  st_write("Data/Cleaned/SFHA_zones.geojson", 
           delete_dsn = TRUE)

unified_zones %>%
  filter(ZONE_CLASS == 'LEVEE_PROTECTED') %>%
  st_write("Data/Cleaned/zoneX_from_levee.geojson", 
           delete_dsn = TRUE)

message("Done! Cleaned data saved to Data/Cleaned")

# Print summary statistics
message("\nSummary of unified flood zones:")
print(unified_zones %>% st_drop_geometry() %>% select(ZONE_CLASS, n_features)) 