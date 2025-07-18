# Script to clean NFIP policies data for California
# This script:
# 1. Loads and cleans dates
# 2. Converts census block group IDs to 2010 format
# 3. Adds necessary columns for analysis
# 4. Saves the cleaned data

# Load required packages
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(scales)
library(fst)

# Function to simplify dates and add year column
simplify_dates <- function(df, year_col = NULL) {
  date_columns <- colnames(df)[grep("Date|date", colnames(df))]
  
  df <- df %>%
    mutate(across(all_of(date_columns), ~as.Date(sub("T.*", "", .))))
  
  # Add a year column if specified
  if (!is.null(year_col)) {
    df <- df %>%
      mutate(!!gsub("Date", "Year", year_col) := year(.data[[year_col]]))
  }
  
  return(df)
}

# Function to convert block group IDs to 2010 format
convert_to_2010_blkgrp <- function(dt, conversion_tab) {
  dt <- as.data.table(dt)
  
  # Make sure censusBlockGroupFips is numeric
  dt[, censusBlockGroupFips := as.numeric(censusBlockGroupFips)]
  
  # Create a new GEOID10 column set to the original FIPS by default
  dt[, GEOID10 := censusBlockGroupFips]
  
  # Use an update join to overwrite it where there's a 2020->2010 mapping
  dt[conversion_tab, 
     on = .(censusBlockGroupFips = GEOID_BLKGRP_20),
     GEOID10 := i.GEOID_BLKGRP_10]
  
  return(dt)
}

# Main cleaning function
clean_nfip_policies <- function(input_file, output_file) {
  # Read the data
  pols <- data.table::fread(input_file)
  
  # Remove row number column if it exists
  if ("V1" %in% colnames(pols)) {
    pols <- pols[, !"V1", with = FALSE]
  }
  
  # Clean dates and add year column
  pols <- simplify_dates(pols, "policyEffectiveDate")
  
  # Add occupancy category
  pols[, occupancyCategory := ifelse(occupancyType %in% c(4, 6, 17, 18, 19), 
                                   "Non-residential", "Residential")]
  
  # Load and process conversion table
  conversion <- data.table::fread("Data/Raw/blkgrp20_blkgrp10.txt", 
                                sep = "|", 
                                header = TRUE)
  
  # Select relevant columns for mapping
  conversion <- conversion[, .(GEOID_BLKGRP_20, GEOID_BLKGRP_10, AREALAND_PART, AREAWATER_PART)]
  
  # Ensure all specified columns are numeric
  conversion[, (names(conversion)) := lapply(.SD, as.numeric)]
  
  # Group by GEOID_BLKGRP_20 and find the GEOID_BLKGRP_10 with the largest area share
  conversion_tab <- conversion[, .(
    Total_Area_Land = sum(AREALAND_PART),
    AREALAND_PART = AREALAND_PART[which.max(AREALAND_PART)],
    GEOID_BLKGRP_10 = GEOID_BLKGRP_10[which.max(AREALAND_PART)]
  ), by = GEOID_BLKGRP_20]
  
  # Calculate the max area share
  conversion_tab[, Max_Area_Share := fifelse(Total_Area_Land > 0, 
                                           AREALAND_PART / Total_Area_Land, 
                                           NA_real_)]
  
  # Convert block group IDs to 2010 format
  pols <- convert_to_2010_blkgrp(pols, conversion_tab)
  
  # Save the cleaned data
  write_fst(pols, output_file)
  
  # Print summary statistics
  cat("Cleaning complete. Summary statistics:\n")
  cat("Total policies:", sum(pols$policyCount, na.rm = TRUE), "\n")
  cat("Years covered:", min(pols$policyEffectiveYear), "to", max(pols$policyEffectiveYear), "\n")
  cat("Residential policies:", sum(pols[occupancyCategory == "Residential"]$policyCount, na.rm = TRUE), "\n")
  cat("Primary residence policies:", sum(pols[primaryResidenceIndicator == 1]$policyCount, na.rm = TRUE), "\n")
}

# Run the cleaning
input_file <- "Data/Raw/NFIP/NfipPolicies_CA.csv"
output_file <- "Data/Cleaned/NfipPolicies_CA_clean.fst"

# Run the cleaning function
clean_nfip_policies(input_file, output_file) 