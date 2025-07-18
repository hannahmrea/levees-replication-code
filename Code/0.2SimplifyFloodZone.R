
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
  } else {
    return(NA)  # Areas not relevant for flood zone analysis
  }
}

# Process and simplify flood zones
message("Processing and simplifying flood zones...")
flood_zones_simple <- fld_haz %>%
  # Clean and validate geometries
  st_make_valid() %>%
  filter(!st_is_empty(.)) %>%
  # Add zone classification
  mutate(
    ZONE_CLASS = factor(mapply(classify_floodzone, FLD_ZONE, ZONE_SUBTY))
  ) %>%
  # Filter to only keep SFHA and LEVEE_PROTECTED
  filter(!is.na(ZONE_CLASS)) %>%
  # Simplify geometries (increased tolerance for better performance)
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
  # Ensure consistent geometry type
  st_cast("MULTIPOLYGON") %>%
  # Remove any empty geometries
  filter(!st_is_empty(.))

# Print summary
message("\nSummary of processed flood zones:")
print(table(flood_zones_simple$ZONE_CLASS))
message(sprintf("\nTotal number of features: %d", nrow(flood_zones_simple)))

# Save the simplified data
message("Saving simplified flood zones...")
st_write(flood_zones_simple, "Data/Cleaned/flood_zones_simple.gpkg", delete_dsn = TRUE)
