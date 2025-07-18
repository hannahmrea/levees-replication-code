# Script to create a map of California counties showing percent and count of population below poverty (SAIPE)
rm(list = ls())

# Load required packages
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(scales)
library(censusapi)
library(stringr)

# Load tract-level ACS data (already cleaned, with reliability flags)
census_data <- read.csv("Data/Raw/Census/ACS5_2021_bytract_2025-05-29.csv")

# Drop unreliable tracts (CV > 0.4 or NA for poverty)
census_data <- census_data %>%
  filter(unreliable_poverty == FALSE & !is.na(unreliable_poverty)) %>% 
  # Add leading zeros to GEOID
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))

# Get tract geometries
tracts <- st_read("Data/Raw/Census/CA_tracts_2021.gpkg")

# Join ACS data to geometries (GEOID is the join key)
tracts_data <- left_join(tracts, census_data, by = "GEOID")

# Filter to tracts that intersect with leveed areas
leveedarea_tracts <- read.csv("Data/Cleaned/tracts_leveedarea_overlap.csv")
leveedtracts <- leveedarea_tracts %>% filter(intersects_leveedarea == 1) %>% pull(GEOID)
leveedtracts <- str_pad(leveedtracts, width = 11, pad = "0")
tracts_data <- tracts_data %>% filter(GEOID %in% leveedtracts)

# Remove tracts with missing poverty data
tracts_data <- tracts_data %>%
  filter(!is.na(PopBelowPoverty), !is.na(TotalPopWithPovertyStatus), TotalPopWithPovertyStatus > 0)

# Calculate percent below poverty (should already be in PctBelowPoverty, but recalc for safety)
tracts_data <- tracts_data %>%
  mutate(PctBelowPoverty = PopBelowPoverty / TotalPopWithPovertyStatus * 100)

# Calculate centroids for dot placement
tract_centroids <- st_centroid(tracts_data)

summary(tract_centroids$PopBelowPoverty)

# Custom color scale: navy (0%) -> white (12%) -> maroon (25%+)
# Values above 25% are capped at maroon
tract_centroids$PctBelowPoverty_capped <- pmin(tract_centroids$PctBelowPoverty, 25)

# Exaggerated size scale: smaller min, larger max
# Comment: Dot color is navy (0%) to white - national average (12%) to maroon (25%+).
p_tract <- ggplot() +
  geom_sf(data = ca_state, fill = "#e7e7e7", color = NA) +  
  geom_sf(data = tract_centroids, 
          aes(color = PctBelowPoverty_capped, size = PopBelowPoverty),
          alpha = 0.7, shape = 16) +
  coord_sf(xlim = c(-124.5, -114.5), ylim = c(32.5, 42.5)) +
  scale_color_gradient2(
    low = "navy", mid = "white", high = "darkred",
    midpoint = 12,
    limits = c(0, 25),
    name = "% Below Poverty",
    oob = scales::squish
  ) +
  scale_size_continuous(
    name = "Population Below Poverty",
    range = c(0.01, 4),  # Exaggerate dot sizes
    breaks = c(100, 1000)
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Poverty in California by Census Tract (ACS 2021, Reliable Tracts Only)",
    subtitle = "Dot color: % below poverty (navy = low, maroon = 25%+, white = 12% national rate)\nDot size: population below poverty",
    x = NULL,
    y = NULL
  )

# Save the tract-level plot
ggsave("Output/Fig4_PovertyByTract.jpg", p_tract, width = 10, height = 8, dpi = 600)
