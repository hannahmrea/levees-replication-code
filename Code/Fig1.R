#Plot flood zone and levee system data: 
rm(list = ls())

# Load required packages
library(sf)
library(leaflet)
library(tidyverse)
library(tmap)  # For static maps
library(mapview)  # For interactive viewing

# Read and clean data
levees <- st_read("Data/Cleaned/LeveeSystem_2023_spatial.geojson") %>%
  st_transform(4326) %>%  # Convert to WGS84 for leaflet
  st_make_valid() 

# Read and simplify flood zones
flood_zones <- st_read("Data/Cleaned/flood_zones_simple.gpkg") %>%
  st_transform(4326) %>%  # Convert to WGS84 for leaflet
  st_make_valid() 

# Create color palettes and styles for levees
levee_colors <- c(
  "Accredited Levee System" = "#006400",          # Dark green
  "Provisionally Accredited Levee (PAL) System" = "#33bf33",  # Light green
  "A99" = "#f852a5",                              # Pink
  "Non-Accredited Levee System" = "#FFA500"       # Orange
)

floodzone_colors <- c(
  "SFHA" = "#98c5c8",
  "LEVEE_PROTECTED" = "#f3efc0"
)


### CREATE STATIC MAPS FOR REGIONS (full interactive map below)
# Define regions of interest with wider extents
regions <- list(
  Sacramento = c(-122.2, 37, -121.2, 40), 
  SanJose = c(-122.3, 37.2, -121.8, 37.8),
  LosAngeles = c(-118.5, 33.3, -116.1, 34.3),
  Fresno = c(-121, 36.1, -119.7, 37.4)
)

# Function to create static map for a region
create_static_map <- function(region_name, bbox) {
  message(sprintf("Creating map for %s...", region_name))
  
  # Filter data to region
  region_levees <- levees %>%
    st_crop(st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                      xmax = bbox[3], ymax = bbox[4])))
  
  region_flood_zones <- flood_zones %>%
    st_crop(st_bbox(c(xmin = bbox[1], ymin = bbox[2], 
                      xmax = bbox[3], ymax = bbox[4])))
  
  message(sprintf("Found %d levees and %d flood zones in region", 
                 nrow(region_levees), nrow(region_flood_zones)))
  
  # Create static map
  tm <- tm_shape(region_flood_zones) +
    # Add flood zones
    tm_fill("ZONE_CLASS", 
            fill.scale = tm_scale(values = c("SFHA" = floodzone_colors[["SFHA"]], 
                                           "LEVEE_PROTECTED" = floodzone_colors[["LEVEE_PROTECTED"]])),
            fill_alpha = 0.9) +
    # Add levees
    tm_shape(region_levees) +
    tm_lines("accredited",
             col.scale = tm_scale(values = levee_colors),
             lwd = 3) +  # Increased line width from 2 to 3
    # Add basemap - using CartoDB.Positron for a cleaner look
    tm_basemap("CartoDB.Positron") +
    # Add title
    tm_title(paste(region_name, "Flood Zones and Levees")) +
    tm_layout(legend.position = c("right", "top"))
  
  # Save the map
  output_file <- paste0("Output/", region_name, "_Fig1.png")
  message(sprintf("Saving map to: %s", output_file))
  
  # Use png device directly instead of tmap_save
  png(output_file, width = 10, height = 8, units = "in", res = 300)
  print(tm)
  dev.off()
  
  # Verify file was created
  if (file.exists(output_file)) {
    message(sprintf("Successfully created %s", output_file))
  } else {
    warning(sprintf("Failed to create %s", output_file))
  }
}

# Create static maps for each region
for (region in names(regions)) {
  create_static_map(region, regions[[region]])
}

#____________
#### Create interactive map of Califrnia
m <- leaflet() %>%
  # Add basemap
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Add flood zones
  addPolygons(
    data = flood_zones %>% filter(ZONE_CLASS == 'SFHA'),
    fillColor = floodzone_colors[["SFHA"]],
    fillOpacity = 0.9,
    weight = 0,
    group = "SFHA Zones"
  ) %>%
  addPolygons(
    data = flood_zones %>% filter(ZONE_CLASS == 'LEVEE_PROTECTED'),
    fillColor = floodzone_colors[["LEVEE_PROTECTED"]],
    fillOpacity = 0.9,
    weight = 0,
    group = "Levee Protected Areas"
  ) %>%
  
  # Add levees with different styles based on accreditation
  # Accredited levees (solid dark green)
  addPolylines(
    data = levees %>% filter(accredited == "Accredited Levee System"),
    color = levee_colors[["Accredited Levee System"]],
    weight = 2,
    opacity = 1,
    group = "Accredited Levees"
  ) %>%
  # Provisionally accredited levees (dashed green)
  addPolylines(
    data = levees %>% filter(accredited == "Provisionally Accredited Levee (PAL) System"),
    color = levee_colors[["Provisionally Accredited Levee (PAL) System"]],
    weight = 2,
    opacity = 1,
    dashArray = "5,10",
    group = "Provisional Levees"
  ) %>%
  # A99 levees (pink)
  addPolylines(
    data = levees %>% filter(accredited == "A99"),
    color = levee_colors[["A99"]],
    weight = 2,
    opacity = 1,
    group = "A99 Levees"
  ) %>%
  # Non-accredited levees (orange)
  addPolylines(
    data = levees %>% filter(accredited == "Non-Accredited Levee System"),
    color = levee_colors[["Non-Accredited Levee System"]],
    weight = 2,
    opacity = 1,
    group = "Non-Accredited Levees"
  ) %>%
  
  # Add layer control
  addLayersControl(
    overlayGroups = c(
      "SFHA Zones",
      "Levee Protected Areas",
      "Accredited Levees",
      "Provisional Levees",
      "A99 Levees",
      "Non-Accredited Levees"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Add legend
  addLegend(
    position = "bottomright",
    colors = c(unname(floodzone_colors), unname(levee_colors)),
    labels = c(
      "Special Flood Hazard Area (SFHA)",
      "Area with Reduced Flood Risk due to Levee",
      "Accredited Levee System",
      "Provisionally Accredited Levee",
      "A99 (Under Construction)",
      "Non-Accredited Levee System"
    ),
    opacity = 1,
    title = "Flood Zones and Levee Systems"
  )

# Display the interactive map
m
