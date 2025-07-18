# Script to create ribbon plot comparing flood insurance policy costs
# between SFHA and Zone X areas for primary residence policies
# Excludes A99 policies
rm(list = ls())

# Load required packages
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)
library(fst)
library(boot)
library(readr)

# Set random number generator for reproducibility
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)

# Read cleaned NFIP data
policies <- read_fst("Data/Cleaned/NfipPolicies_CA_clean.fst", as.data.table = TRUE)

# Filter to primary residence policies and exclude A99
policies <- policies[primaryResidenceIndicator == 1 & ratedFloodZone != "A99" & policyEffectiveYear < 2024]

# Filter to blockgroups that intersect with leveed areas
leveedarea_bgs <- read_csv("Data/Cleaned/bgs_leveedarea_overlap.csv")
leveedarea_bgs$GEOID10 <- sub("^0", "", leveedarea_bgs$GEOID10) # Remove leading zeros if present
leveedbgs <- leveedarea_bgs %>% filter(intersects_leveedarea == 1) %>% pull(GEOID10)
policies <- policies[GEOID10 %in% leveedbgs]

# Create flood zone category
policies[, floodZoneCategory := fifelse(
  ratedFloodZone == "X", "Zone X",
  fifelse(grepl("^[AV]", ratedFloodZone), "SFHA", NA_character_)
)]
#Remove NA values
policies <- policies[!is.na(floodZoneCategory)]

# Calculate summary statistics by year and flood zone category (for plotting)
summary_stats <- policies[, .(
  median_cost = median(policyCost/policyCount, na.rm = TRUE),
  q25 = quantile(policyCost/policyCount, 0.25, na.rm = TRUE),
  q75 = quantile(policyCost/policyCount, 0.75, na.rm = TRUE),
  n_policies = sum(policyCount, na.rm = TRUE)
), by = .(policyEffectiveYear, floodZoneCategory)]

# Define custom colors
zone_colors <- c("SFHA" = "#7B3294",   # purple
                 "Zone X" = "#D7191C") # red

# Create the ribbon plot with custom colors
p <- ggplot(summary_stats, aes(x = policyEffectiveYear, y = median_cost, 
                              fill = floodZoneCategory, color = floodZoneCategory)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_continuous(breaks = seq(min(summary_stats$policyEffectiveYear), 
                                max(summary_stats$policyEffectiveYear), by = 1)) +
  scale_color_manual(values = zone_colors) +
  scale_fill_manual(values = zone_colors) +
  labs(
    title = "Median Flood Insurance Policy Cost by Flood Zone",
    subtitle = "Primary Residence Policies Only (Excluding A99)",
    x = "Year",
    y = "Policy Cost ($)",
    fill = "Flood Zone",
    color = "Flood Zone"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

# Save the plot
ggsave("Output/Fig2_FloodInsuranceCosts.jpeg", p, width = 10, height = 6, dpi = 300)

#Save data for plot: 
fwrite(summary_stats, "Output/Fig2_data.csv")

#______________________
# Print descriptive statistics for Fig 2 
#______________________

# Prepare data for bootstrapping
dt_boot <- copy(policies)
dt_boot[, cost := policyCost/policyCount]
dt_boot <- dt_boot[floodZoneCategory %in% c("SFHA", "Zone X")]

# 1. Annual bootstrapped medians and CIs (for cumulative difference)
year_group_boot <- dt_boot[, {
  x <- cost
  bs <- replicate(1000, median(sample(x, replace = TRUE), na.rm = TRUE))
  list(
    median = median(x, na.rm = TRUE),
    ci_lower = quantile(bs, 0.025, na.rm = TRUE),
    ci_upper = quantile(bs, 0.975, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE)
  )
}, by = .(policyEffectiveYear, floodZoneCategory)]

# Pivot wide for difference calculation
year_group_wide <- dcast(year_group_boot, policyEffectiveYear ~ floodZoneCategory, value.var = c("median", "ci_lower", "ci_upper"))

# Calculate difference (SFHA - Zone X) and its CI for each year
year_group_wide[, median_diff := median_SFHA - `median_Zone X`]
year_group_wide[, diff_lower := ci_lower_SFHA - `ci_upper_Zone X`]
year_group_wide[, diff_upper := ci_upper_SFHA - `ci_lower_Zone X`]


# 2. Cumulative difference in policy cost (2009-2023)
annual_diff <- year_group_wide[policyEffectiveYear >= 2009 & policyEffectiveYear <= 2023]
total_median_diff <- sum(annual_diff$median_diff, na.rm = TRUE)
total_lower_diff  <- sum(annual_diff$diff_lower, na.rm = TRUE)
total_upper_diff  <- sum(annual_diff$diff_upper, na.rm = TRUE)

cat(sprintf(
  "Overall, we estimate that for the period 2009 to 2023, households behind non-accredited levees paid $%d to $%d more for flood insurance in SFHA compared to Zone X.\n",
  round(total_lower_diff), round(total_upper_diff)
))


# 3. Period bootstrapped medians and CIs (for narrative)
dt_boot[, period := ifelse(policyEffectiveYear <= 2020, "2009–2020", "2021–2023")]
boot_median_ci <- function(x, nboot = 1000) {
  bs <- replicate(nboot, median(sample(x, replace = TRUE), na.rm = TRUE))
  c(median = median(x, na.rm = TRUE),
    ci_lower = quantile(bs, 0.025, na.rm = TRUE),
    ci_upper = quantile(bs, 0.975, na.rm = TRUE))
}
period_boot <- dt_boot[, as.list(boot_median_ci(cost)), by = .(period, floodZoneCategory)]
period_wide <- dcast(period_boot, period ~ floodZoneCategory, value.var = c("median", "ci_lower.2.5%", "ci_upper.97.5%"))
setnames(period_wide,
         old = c("median_SFHA", "ci_lower.2.5%_SFHA", "ci_upper.97.5%_SFHA",
                 "median_Zone X", "ci_lower.2.5%_Zone X", "ci_upper.97.5%_Zone X"),
         new = c("median_SFHA", "ci_lower_SFHA", "ci_upper_SFHA",
                 "median_ZoneX", "ci_lower_ZoneX", "ci_upper_ZoneX"))
period_wide[, diff_median := median_SFHA - median_ZoneX]
period_wide[, diff_lower := ci_lower_SFHA - ci_upper_ZoneX]
period_wide[, diff_upper := ci_upper_SFHA - ci_lower_ZoneX]

# Print narrative
for (i in 1:nrow(period_wide)) {
  p <- period_wide$period[i]
  cat(sprintf(
    "%s: Households in SFHA paid a median of $%d–$%d (95%% CI) compared to $%d–$%d for Zone X. The difference was $%d–$%d.\n",
    p,
    round(period_wide$ci_lower_SFHA[i]), round(period_wide$ci_upper_SFHA[i]),
    round(period_wide$ci_lower_ZoneX[i]), round(period_wide$ci_upper_ZoneX[i]),
    round(period_wide$diff_lower[i]), round(period_wide$diff_upper[i])
  ))
}

# Save annual bootstrap data for reference
n_stats <- summary_stats %>% select(policyEffectiveYear, floodZoneCategory, n_policies)
year_group_boot <- left_join(year_group_boot, n_stats, by = c("policyEffectiveYear", "floodZoneCategory"))
fwrite(year_group_boot, "Output/Fig2_data.csv")
