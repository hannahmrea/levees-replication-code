# Levees and Flood Insurance Analysis

This repository contains code and data for analyzing the relationship between levee systems, flood insurance, and socioeconomic characteristics in California. The analysis examines how levee protection status relates to flood insurance participation and demographic patterns.

## Overview

This project analyzes:
- National Levee Database (NLD) data for California
- American Community Survey (ACS) demographic data
- National Flood Insurance Program (NFIP) policy data
- Spatial relationships between leveed areas and census geographies

## Data Sources

### 1. Spatial Data (GeoJSON files)
- **Download Date**: May 4, 2023
- **Source**: National Levee Database (NLD)
- **URL**: https://levees.sec.usace.army.mil/#/levees/search/&viewType=map&resultsType=systems&advanced=true&hideList=false&eventSystem=false
- **Download Process**:
  1. Filter state to California
  2. Click "Download Data > GeoJSON"
  3. Save files as `System.geojson` and `LeveedArea.geojson`

### 2. API-Scraped Data
- **Scrape Date**: February 23, 2024
- **Source**: National Levee Database API
- **Data Location**: `Data/NationalLeveeDatabase/APIscrape_20240223/`
- **Content**: Detailed information including:
  - Population at risk
  - Buildings and structures
  - Risk assessments
  - LSAC (Levee Safety Action Classification)
  - Financial risk estimates

### 3. Census Data (ACS)
The analysis uses American Community Survey (ACS) 5-year estimates for demographic and socioeconomic characteristics at the census tract level in California. 

**Data Vintage**: 2021 ACS 5-year estimates (covering 2017-2021)


### 4. NFIP Policy Data
- **Source**: FEMA National Flood Insurance Program
- **Content**: Policy-level flood insurance data including coverage amounts, premiums, and policyholder information

## Setup and Installation

### Requirements
- R version 4.0.0 or higher
- Required R packages (see `Code/install_packages.R` for complete list):
  - tidyverse
  - ggplot2
  - sf
  - jsonlite
  - geojsonsf
  - parallel
  - doSNOW
  - scales

### API Setup
1. Request a Census API key from: https://api.census.gov/data/key_signup.html
2. Create a `.Renviron` file in the project root
3. Add your Census API key: `CENSUS_API_KEY=your_key_here`

### Installation
1. Clone this repository
2. Run `Code/install_packages.R` to install required packages
3. Set up your Census API key as described above

#### Weight Calculation 
**`1.2CensusToLeveeAreaConversion.R`** - Calculates area weights for converting census tract data to leveed areas

**Important**: This script calculates two types of weights:
- **`weight_for_averages`**: For median income, median home value, percentages
  - Formula: `intersection_area / sum(all_intersection_areas_for_levee)`
  - Weights sum to 1.0 for each leveed area
  - Used for area-weighted averages
- **`weight_for_totals`**: For population counts, household counts
  - Formula: `intersection_area / tract_area`
  - Represents proportional allocation
  - Used for estimating counts within leveed areas

#### Variable Conversion (Flexible)
**`1.3ApplyWeightsToVariables.R`** - Applies weights to convert census variables to leveed areas

**Key Features**:
- Flexible variable selection (modify `average_vars` and `total_vars` sections)
- Automatically uses appropriate weights for each variable type
- Handles both point estimates and margins of error (MOEs)
- Calculates quality metrics (coefficient of variation)

**Variable Classification**:
- **AVERAGE variables**: median income, median home value, median rent, percentages
- **TOTAL variables**: population counts, household counts, housing unit counts


## Directory Structure
```
├── Code/                    # R scripts for analysis
│   ├── 0_*.R              # Data cleaning scripts
│   ├── 1_*.R              # Spatial analysis scripts
│   ├── Fig*.R             # Figure generation scripts
│   ├── Tab*.R             # Table generation scripts
│   └── Functions/         # Helper functions
├── Data/
│   ├── Raw/               # Raw data downloads (not in git)
│   └── Cleaned/           # Processed data files (some in git)
└── Output/                # Generated figures and tables
```

## Key Output Files

### Data Files
- `Data/Cleaned/CensusToLeveeArea_Weights.csv` - Area weights for variable conversion
- `Data/Cleaned/LeveedArea_ACSVariables.csv` - Converted census variables for leveed areas
- `Data/Cleaned/NLDscrape2024_LeveeArea2023_joined.geojson` - Enhanced leveed area data

### Figures and Tables
- `Output/` directory contains all generated figures and tables

## Notes on Spatial Analysis

### Weight Calculation Nuances
The analysis distinguishes between two types of census variables:

1. **Average Variables** (median income, median home value):
   - Use area-weighted averages
   - Weights sum to 1.0 for each leveed area
   - Represents "typical" values in the leveed area

2. **Total Variables** (population, household counts):
   - Use proportional allocation
   - Weights represent fraction of tract in leveed area
   - Represents estimated counts within leveed area

### Spatial Projections
- All spatial operations use California Albers projection (EPSG:3310) for accurate area calculations
- Census tract data is transformed to match leveed area projection

### Data Quality
- Missing values in Census data are coded as -666666666 and converted to NA
- Coefficient of variation (CV) is calculated for variables with margins of error
- High uncertainty areas (CV > 0.4) are flagged

### Calculation of Median Income for Leveed Areas

Median household income for each leveed area is calculated using an area-weighted average approach. Because census tracts and leveed areas do not align perfectly, we estimate the median income for each leveed area by overlaying census tracts with leveed area polygons and weighting each tract's median income by the proportion of its area that falls within the leveed area.

**Steps:**
1. For each leveed area, identify all intersecting census tracts.
2. Calculate the area of intersection between each tract and the leveed area.
3. Compute the weight for each tract as:  
   `weight = intersection_area / sum(all_intersection_areas_for_levee)`
4. The leveed area's median income is the sum of each tract's median income multiplied by its weight:
   `leveed_area_median_income = sum(weight * tract_median_income)`

This method ensures that the resulting median income reflects the spatial distribution of census tracts within each leveed area. The calculation is performed in the scripts `1.2CensusToLeveeAreaConversion.R` (for weight calculation) and `1.3ApplyWeightsToVariables.R` (for applying weights to variables such as median income).


## Notes on Data Fields

### Population at Risk
- Estimated population within the leveed area
- NOT a life loss projection
- Source: Higher Level Risk Assessment (HLRA) or National Structures Inventory (NSI) v2 (2019)

### Buildings at Risk
- Estimated number of structures in leveed area
- Includes most significant structures
- May exclude minor structures (sheds, etc.)


## Troubleshooting

### Common Issues
1. **API Key Errors**: Ensure `.Renviron` file is in project root with correct API key
2. **Package Installation**: Run `Code/install_packages.R` to install all dependencies
3. **Memory Issues**: For large datasets, consider running scripts with increased memory allocation
4. **Spatial Errors**: Ensure all spatial data uses consistent coordinate reference systems

### Performance Notes
- Weight calculation uses parallel processing for efficiency
- Large spatial operations may take significant time

## Contact
For questions about this analysis, please contact hannahmr@stanford.edu.
