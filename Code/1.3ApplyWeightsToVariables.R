# Script to convert census tract variables to leveed areas using the conversion function
# This script demonstrates how to use the convert_census_to_levee function

# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(sf)
library(geojsonsf)

# Load the conversion function
source("Code/Functions/convert_census_to_levee.R")

#------------------------------------------------------------------------------
# 1. Load Data
#------------------------------------------------------------------------------

# Load pre-calculated area weights
weights_df <- read.csv("Data/Cleaned/CensusTractToLeveeArea_Weights.csv")
cat("Area weights loaded. Rows:", nrow(weights_df), "\n")

# Load cleaned census data
census_data <- read.csv("Data/Raw/Census/ACS5_2021_bytract_2025-05-29.csv") %>%
  select(-X) # Remove row number column
cat("Census data loaded. Rows:", nrow(census_data), "\n")

# Load leveed area data for metadata
leveed_area <- geojson_sf("Data/Cleaned/NLDscrape2024_LeveeArea2023_joined.geojson") %>% 
  st_make_valid()
cat("Leveed area data loaded. Rows:", nrow(leveed_area), "\n")

#------------------------------------------------------------------------------
# 2. Define Variables to Convert
#------------------------------------------------------------------------------

# Define variables and their types
variables <- list(
  # Average variables (use weight_for_averages)
  "MedianIncome" = list(var = "MedianIncome", moe = "MedianIncome_moe", type = "average"),
  
  # Total variables (use weight_for_totals)
  "TotalPopulation" = list(var = "Pop", moe = "Pop_moe", type = "total"),
  "TotalHouses" = list(var = "TotHouses", moe = "TotHouses_moe", type = "total")
)

#------------------------------------------------------------------------------
# 3. Convert Variables
#------------------------------------------------------------------------------

# Use the conversion function
results <- convert_census_to_levee(
  census_data = census_data,
  weights_data = weights_df,
  leveed_area_data = leveed_area,
  variables_to_convert = variables#,
  #output_file = "Data/Cleaned/LeveedArea_CensusVariables.rds"
)

#Save results 
write.csv(results, "Data/Cleaned/LeveedArea_ACSVariables.csv")


