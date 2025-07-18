# clean_census_demographics.R
#
# Purpose: Clean and process ACS data focusing on income, poverty, and housing metrics
# at the census tract level
# Author: Hannah Melville-Rea
# Last modified: May 2025
#
# Notes on ACS 5-year estimates:
# - Data represents period estimates (2019-2023 for 2023 data)
# - Margins of error (MOE) are at 90% confidence level
# - Some areas may have high MOE; check CV = (MOE/1.645)/estimate
# - Coefficients of Variation (CV) > 0.4 indicate unreliable estimates


# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)      # For data manipulation
library(stringr)    # For string operations
library(tidycensus) # For census data access
library(censusapi)  # For census API
library(tigris)


#------------------------------------------------------------------------------
# 1. Set up Census API
#------------------------------------------------------------------------------

# Get API key from environment variable
api_key <- Sys.getenv("CENSUS_API_KEY")
if (api_key == "") {
  stop("Census API key not found. Please set the CENSUS_API_KEY environment variable in .Renviron")
}
Sys.setenv(CENSUS_KEY = api_key)

# Document data download date
download_date <- Sys.Date()

#------------------------------------------------------------------------------
# 2. Load and Process Census Data
#------------------------------------------------------------------------------

# Define year
yr <- 2021
state_fips <- "state:06" #California

# Get population and housing unit data
pop_data <- getCensus(
  name = "acs/acs5/profile",
  vintage = yr,
  vars = c("NAME",
           "DP05_0001E", # Total population
           "DP05_0001M", # MOE: Total population
           "DP05_0086E", # Total housing units
           "DP05_0086M"  # MOE: Total housing units
  ), 
  regionin = state_fips,
  region = "tract:*"
)

# Rename population and housing columns and calculate CV
pop_data <- pop_data %>%
  rename(
    Pop = DP05_0001E,
    Pop_moe = DP05_0001M,
    TotHouses = DP05_0086E,
    TotHouses_moe = DP05_0086M
  ) %>%
  mutate(
    Pop_cv = (Pop_moe/1.645)/Pop,
    TotHouses_cv = (TotHouses_moe/1.645)/TotHouses
  )

# Get household income, poverty, and housing tenure data
household <- getCensus(
  name = "acs/acs5",
  vintage = yr,
  vars = c(
    "NAME",
    # Estimates
    "B17001_002E",    # Population below poverty level
    "B17001_001E",    # Total population for poverty status
    "B25027_002E",    # Housing units with mortgage
    "B25027_010E",    # Housing units without mortgage
    "B25003_003E",    # Renter-occupied units
    "B19013_001E",    # Median household income
    # Margins of Error
    "B17001_002M",    # MOE: Below poverty
    "B17001_001M",    # MOE: Poverty status total
    "B25027_002M",    # MOE: With mortgage
    "B25027_010M",    # MOE: Without mortgage
    "B25003_003M",    # MOE: Renter-occupied
    "B19013_001M"     # MOE: Median income
  ), 
  region = "tract:*",
  regionin = state_fips
)
# Replace invalid data with NA and rename columns
household <- household %>%
  mutate(B19013_001E = na_if(B19013_001E, -666666666)) %>%
  rename(
    # Estimates
    PopBelowPoverty = B17001_002E,
    TotalPopWithPovertyStatus = B17001_001E,
    Mortgage = B25027_002E,
    NoMortgage = B25027_010E,
    Rent = B25003_003E,
    MedianIncome = B19013_001E,
    # Margins of Error
    PopBelowPoverty_moe = B17001_002M,
    TotalPopWithPovertyStatus_moe = B17001_001M,
    Mortgage_moe = B25027_002M,
    NoMortgage_moe = B25027_010M,
    Rent_moe = B25003_003M,
    MedianIncome_moe = B19013_001M
  ) %>%
  # Calculate CVs
  mutate(
    PopBelowPoverty_cv = (PopBelowPoverty_moe/1.645)/PopBelowPoverty,
    Mortgage_cv = (Mortgage_moe/1.645)/Mortgage,
    NoMortgage_cv = (NoMortgage_moe/1.645)/NoMortgage,
    Rent_cv = (Rent_moe/1.645)/Rent,
    MedianIncome_cv = (MedianIncome_moe/1.645)/MedianIncome) %>%
  # Add reliability flags for very high CVs
  mutate(
    across(ends_with("_cv"), 
           ~if_else(is.infinite(.) | is.na(.) | . < 0 | . > 1000, NA_real_, .),  # Handle extreme values
           .names = "{.col}")
  )


# Merge demographic and household data
census_data <- pop_data %>%
  left_join(household, by = c("state", "county", "tract", "NAME"))

# Create GEOID and calculate proportions with reliability flags
census_data <- census_data %>%
  mutate(
    GEOID = str_c(state, county, tract),
    across(where(is.numeric), ~ na_if(., -666666666)),
    # Calculate proportions
    PctBelowPoverty = PopBelowPoverty / TotalPopWithPovertyStatus,
    Mortgage_prop = Mortgage / TotHouses,
    NoMortgage_prop = NoMortgage / TotHouses,
    Rent_prop = Rent / TotHouses,
    # Flag unreliable estimates (CV > 0.4)
    unreliable_pop = Pop_cv > 0.4,
    unreliable_houses = TotHouses_cv > 0.4,
    unreliable_poverty = PopBelowPoverty_cv > 0.4,
    unreliable_income = MedianIncome_cv > 0.4,
    unreliable_mortgage = Mortgage_cv > 0.4,
    unreliable_rent = Rent_cv > 0.4
  )

# Save data with ACS year in filename
write.csv(census_data, 
        file = paste0("Data/Raw/Census/ACS5_", yr, "_bytract_", download_date, ".csv"))


##______________________
## Download 2010 CA block group geometries
##______________________

# Download 2010 CA block group geometries
bgs2010 <- block_groups(state = '06', year = 2010)
bgs2010 <- st_transform(bgs2010, 4326) # Ensure consistent CRS

# Save for future use
st_write(bgs2010, "Data/Raw/Census/CA_blockgroups_2010.gpkg", delete_dsn = TRUE)