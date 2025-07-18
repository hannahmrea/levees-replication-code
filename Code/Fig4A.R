# Script to create a map of California showing the average CRS class rating
# by census block group for policies from 2009-2023
rm(list = ls())

# Load required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(scales)
library(fst)

# Read cleaned NFIP data
policies <- read_fst("Data/Cleaned/NfipPolicies_CA_clean.fst", as.data.table = TRUE)

# Filter to policies from 2009-2023 (excluding 2024)
policies <- policies[policyEffectiveYear >= 2009 & policyEffectiveYear <= 2023]

# Filter to blockgroups that intersect with leveed areas
leveedarea_bgs <- read.csv("Data/Cleaned/bgs_leveedarea_overlap.csv")
leveedarea_bgs$GEOID10 <- sub("^0", "", leveedarea_bgs$GEOID10) # Remove leading zeros if present
leveedbgs <- leveedarea_bgs %>% filter(intersects_leveedarea == 1) %>% pull(GEOID10)
policies <- policies[GEOID10 %in% leveedbgs]

# Calculate average CRS class by block group
bg_crs <- policies[, .(
  total_policies = sum(policyCount, na.rm = TRUE),
  avg_crs_class = mean(crsClassCode, na.rm = TRUE)
), by = GEOID10]

# Assign categories based on average CRS class
bg_crs[, crsClassCategory := fcase(
  avg_crs_class <= 4.5, "Class 1-4",
  avg_crs_class <= 7.5, "Class 5-7",
  avg_crs_class <= 10, "Class 8-10",
  is.na(avg_crs_class), "Non-participating"
)]

# Diagnostic: Print summary of policy counts and average CRS classes
cat("\nSummary of policies per block group:\n")
print(summary(bg_crs$total_policies))
cat("\nSummary of average CRS classes:\n")
print(summary(bg_crs$avg_crs_class))

# Filter to block groups with at least 10 policies
bg_crs <- bg_crs[total_policies >= 10]

# Get California block groups using tigris
ca_bgs <- block_groups(state = "CA", year = 2010)
# For join, add leading 0 and make character
bg_crs$GEOID10 <- paste0("0", bg_crs$GEOID10)

# Join the CRS data to the spatial data
ca_bgs <- left_join(ca_bgs, bg_crs, by = c("GEOID10" = "GEOID10"))

# Get California state boundary for cropping
ca_state <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
  filter(STUSPS == "CA")

# Subset spatial data to only include block groups with policy data
ca_bgs_with_data <- ca_bgs[!is.na(ca_bgs$total_policies),]

# Create the dot plot
p <- ggplot() +
  geom_sf(data = ca_state, fill = "#dfdede", color = "gray", linewidth = 0.2) +
  geom_sf(data = ca_bgs_with_data, 
          aes(fill = crsClassCategory),
          alpha = 0.7, color = NA) +
  coord_sf(xlim = c(-124.5, -114.5), ylim = c(32.5, 42.5)) +
  scale_fill_manual(
    values = c(
      "Class 1-4" = "#1396bf",  
      "Class 5-7" = "#9bd5c7",  
      "Class 8-10" = "#ecf5bf", 
      "Non-participating" = "#f7f8e1"
    ),
    name = "CRS Class"
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
    title = "CRS Class Rating by Census Block Group",
    subtitle = "Average Rating 2009-2023 (Block Groups with ≥10 Policies)",
    x = NULL,
    y = NULL
  )

# Save the plot
ggsave("Output/Fig4_CRS.jpeg", p, width = 10, height = 8, dpi = 300)
