# Script to create a map of California showing the proportion of Zone X vs SFHA policies
# by census block group for primary residence policies in 2023
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

# Filter to 2023 primary residence policies (including A99)
policies <- policies[primaryResidenceIndicator == 1 & policyEffectiveYear == 2023]

# Filter to blockgroups that intersect with leveed areas
leveedarea_bgs <- read.csv("Data/Cleaned/bgs_leveedarea_overlap.csv")
leveedarea_bgs$GEOID10 <- sub("^0", "", leveedarea_bgs$GEOID10) # Remove leading zeros if present
leveedbgs <- leveedarea_bgs %>% filter(intersects_leveedarea == 1) %>% pull(GEOID10)
policies <- policies[GEOID10 %in% leveedbgs]

# Create flood zone category
policies[, floodZoneCategory := fifelse(
  ratedFloodZone == "X", "Zone X",
  fifelse(grepl("^[AV]", ratedFloodZone), "SFHA", NA_character_)
)]
# Remove NA values (less than 1%)
policies <- policies[!is.na(floodZoneCategory)]

# Calculate proportion of SFHA policies by block group
bg_props <- policies[, .(
  total_policies = sum(policyCount, na.rm = TRUE),
  sfha_policies = sum(policyCount[floodZoneCategory == "SFHA"], na.rm = TRUE)
), by = GEOID10]

bg_props[, prop_sfha := sfha_policies / total_policies]

# Diagnostic: Print summary of policy counts
cat("\nSummary of policies per block group:\n")
print(summary(bg_props$total_policies))

# Diagnostic: Count block groups by policy count ranges
cat("\nNumber of block groups by policy count ranges:\n")
print(table(cut(bg_props$total_policies, 
               breaks = c(0, 1, 5, 10, 50, 100, 500, 1000, Inf),
               labels = c("1", "2-5", "6-10", "11-50", "51-100", "101-500", "501-1000", "1000+"))))

# Filter to block groups with at least 10 policies
bg_props <- bg_props[total_policies >= 10]

# Get California block groups using tigris
ca_bgs <- block_groups(state = "CA", year = 2010)
#For join, add leading 0 and make character
bg_props$GEOID10 <- paste0("0", bg_props$GEOID10)

# Join the proportions to the spatial data
ca_bgs <- left_join(ca_bgs, bg_props, by = c("GEOID10" = "GEOID10"))

# Get California state boundary for cropping - using counties to get tighter boundary
ca_state <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
  filter(STUSPS == "CA")

# Subset spatial data to only include block groups with policy data
ca_bgs_with_data <- ca_bgs[!is.na(ca_bgs$total_policies),]

# Calculate centroids of block groups
bg_centroids <- st_centroid(ca_bgs_with_data)

# Create the dot plot
p <- ggplot() +
  geom_sf(data = ca_state, fill = "#e7e7e7", color = "black", linewidth = 0.5) +
  geom_sf(data = bg_centroids, 
          aes(color = prop_sfha, size = total_policies),
          alpha = 0.7) +
  coord_sf(xlim = c(-124.5, -114.5), ylim = c(32.5, 42.5)) +
  scale_color_gradient2(
    low = "#D7191C",  # Red for Zone X
    mid = "white",    # White for 50-50
    high = "#7B3294", # Purple for SFHA
    midpoint = 0.5,
    limits = c(0, 1),
    labels = percent_format(),
    name = "Proportion of\nSFHA Policies"
  ) +
  scale_size_continuous(
    name = "Number of\nPolicies",
    range = c(0.5, 5),
    breaks = c(10, 50, 100, 500, 1000),
    trans = "sqrt"
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
    title = "Proportion of SFHA vs Zone X Policies by Census Block Group",
    subtitle = "Primary Residence Policies in 2023 (Block Groups with ≥10 Policies)",
    x = NULL,
    y = NULL
  )

# Save the plots
ggsave("Output/Fig3_CA.jpeg", p, width = 10, height = 8, dpi = 300)
