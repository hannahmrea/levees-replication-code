# clean_levee_data.R
#
# Purpose: Clean and combine National Levee Database (NLD) data, including both
# spatial data and API-scraped detailed information
# Author: Hannah Melville-Rea
# Last modified: May 2025

# Load required packages
library(dplyr)
library(sf)
library(jsonlite)
library(geojsonsf)

# Clear environment
rm(list = ls())

#------------------------------------------------------------------------------
# 1. Load Spatial Data (Downloaded May 4, 2023)
#------------------------------------------------------------------------------

# Load levee system geometries
system_2023 <- geojson_sf("Data/Raw/NationalLeveeDatabase/LeveeSystem.geojson")
system_2023 <- st_zm(system_2023, drop = TRUE, what = "ZM")  # Drop ZM features
system_2023 <- system_2023 %>%
  rename(femaAccr = femaAccreditationRatingDescription)

# Load leveed areas
area_2023 <- geojson_sf("Data/Raw/NationalLeveeDatabase/LeveeArea.geojson")
area_2023 <- st_make_valid(area_2023)  # Clean and fix geometry

#------------------------------------------------------------------------------
# 2. Load API-Scraped Data (Scraped February 23, 2024)
#------------------------------------------------------------------------------

# Define variables for extraction
variables <- c(
  "id", "name", "location", "counties", "femaAccreditationRatingDescription", 
  "leveeLengthInMiles", "isFederal",
  "authorizations", "sponsors", "sponsorRoleIds", "ripStatusName",
  "lsacRatingName", "riskAssessmentDate", "peopleAtRisk",
  "structuresAtRisk", "financialRisk", "lastEditedDate",
  "peopleBreakdownAm", "peopleBreakdownPm",
  "structureBreakdown", "buildingBreakdown", "speciesBreakdown"
)

# Path to API scraped data
json_folder <- "Data/Raw/NationalLeveeDatabase/APIscrape_20240223"
json_files <- list.files(json_folder, pattern = "\\.json$", full.names = TRUE)

# Process JSON files
extracted_data <- list()
for (file_path in json_files) {
  data <- fromJSON(file_path)
  values <- lapply(variables, function(var) {
    if (is.null(data[[var]])) { 
      NA
    } else if (is.list(data[[var]])) {
      toString(data[[var]])
    } else if (length(data[[var]]) > 1) {
      toString(data[[var]])
    } else {
      data[[var]]
    }
  })
  extracted_data[[file_path]] <- data.frame(setNames(as.list(values), variables))
}

# Combine all API data
system_details_2024 <- bind_rows(extracted_data)

#------------------------------------------------------------------------------
# 3. Process 2024 API Data for Summary Table
#------------------------------------------------------------------------------

# Clean and process 2024 API data
system_summary_2024 <- system_details_2024 %>%
  mutate(
    accredited = case_when(
      femaAccreditationRatingDescription == "See FIRM Panel for more information" ~ "Accredited Levee System",
      femaAccreditationRatingDescription == "No Regulatory Flood Hazard Information Published by FEMA" | 
        is.na(femaAccreditationRatingDescription) ~ "Non-Accredited Levee System",
      TRUE ~ femaAccreditationRatingDescription
    )
  )

#------------------------------------------------------------------------------
# 4. Process 2023 Spatial Data for Maps
#------------------------------------------------------------------------------

# Clean system spatial data
system_spatial_2023 <- system_2023 %>%
  select(
    id, name, location, counties, femaAccr,
    geometry, segmentCount, districtNames,
    divisionNames, huc4Names, floodSource
  ) %>%
  mutate(
    accredited = case_when(
      femaAccr == "See FIRM Panel for more information" ~ "Accredited Levee System",
      femaAccr == "No Regulatory Flood Hazard Information Published by FEMA" | is.na(femaAccr) ~ "Non-Accredited Levee System",
      TRUE ~ femaAccr
    )
  ) %>%
  select(-femaAccr)

# Create final area dataset
area_spatial_2023 <- area_2023 %>%
  left_join(
    system_spatial_2023 %>%
    st_drop_geometry() %>%
    select(id, accredited),
    by = c("systemId" = "id")
  ) %>%
  select(systemId, systemName, accredited, 
         leveedAreaSource, areaSquareMiles, geometry)

#------------------------------------------------------------------------------
# 5. Create Combined Dataset with API and Spatial Information
#------------------------------------------------------------------------------

# Create a combined dataset with both API and spatial information
combined_levee_data <- system_summary_2024 %>%
  left_join(
    area_spatial_2023 %>%
      select(-accredited),  # Drop the accredited column from spatial data
    by = c("id" = "systemId")
  ) %>%
  # Keep all columns from both datasets, handling duplicates
  select(
    # API data columns
    id, name, location, counties, accredited,
    femaAccreditationRatingDescription,
    leveeLengthInMiles, isFederal, authorizations, sponsors, sponsorRoleIds,
    ripStatusName, lsacRatingName, riskAssessmentDate,
    peopleAtRisk, structuresAtRisk, financialRisk, lastEditedDate,
    peopleBreakdownAm, peopleBreakdownPm, structureBreakdown,
    buildingBreakdown, speciesBreakdown,
    # Spatial data columns
    systemName, leveedAreaSource, areaSquareMiles,
    # Keep geometry last
    geometry
  )

#------------------------------------------------------------------------------
# 6. Save Outputs
#------------------------------------------------------------------------------

# Save 2024 API summary (non-spatial)
write.csv(system_summary_2024, 
          "Data/Cleaned/LeveeSystem_2024_API_summary.csv", 
          row.names = FALSE)

# Save 2023 spatial data
st_write(system_spatial_2023, 
         "Data/Cleaned/LeveeSystem_2023_spatial.geojson", 
         driver = "GeoJSON", 
         delete_dsn = TRUE)

st_write(area_spatial_2023, 
         "Data/Cleaned/LeveeArea_2023_spatial.geojson", 
         driver = "GeoJSON", 
         delete_dsn = TRUE)

# Save combined dataset (2024 API data with 2023 spatial data)
st_write(combined_levee_data,
         "Data/Cleaned/NLDscrape2024_LeveeArea2023_joined.geojson",
         driver = "GeoJSON",
         delete_dsn = TRUE)

