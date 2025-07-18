# Script to create violin plots comparing median income across levee accreditation types
# by different geographic regions
rm(list = ls())

# Load required packages
library(dplyr)
library(ggplot2)
library(sf)
library(scales)

# Load the leveed area data with demographics (from the new conversion script)
data <- read.csv("Data/Cleaned/LeveedArea_ACSVariables.csv")

# Rename columns to match what the plot expects
data <- data %>%
  rename(
    MedIncome = MedianIncome_weighted_value,
    Pop = peopleAtRisk
  )

# Filter data to only include accredited, PAL, and non-accredited levees
filtered_data <- data %>% filter(accredited %in% c("Accredited Levee System", 
                                                  "Provisionally Accredited Levee (PAL) System",
                                                  "Non-Accredited Levee System"), 
                                  !is.na(MedIncome))


# Create the violin plot
income_violin_plot <- ggplot(filtered_data, aes(x = MedIncome, y = accredited, color = accredited)) + 
  geom_jitter(aes(size = Pop), alpha = 0.3, width = 0, height = 0.08, shape = 16) + 
  geom_violin(width = 0.4, fill = NA, position = position_nudge(y = 0.4)) +  
  stat_summary(fun = median, geom = "point", size = 3, shape = 18, 
               position = position_nudge(y = 0.4), aes(color = accredited)) +
  scale_x_continuous(labels = scales::dollar, limits = c(0, 260000)) +  
  scale_size_continuous(
    name = "Population behind levee",
    breaks = c(10, 1000, 100000),
    labels = scales::comma_format()
  ) +  
  scale_color_manual(values = c(
    "Accredited Levee System" = "darkgreen",
    "Provisionally Accredited Levee (PAL) System" = "green4",
    "Non-Accredited Levee System" = "orange2"
  )) +
  labs(x = "Annual Household Median Income", y = "") + 
  scale_y_discrete(labels = c(
    "Accredited Levee System" = "Accredited",
    "Provisionally Accredited Levee (PAL) System" = "Provisionally Accredited",
    "Non-Accredited Levee System" = "Non-Accredited"
  ), limits = rev(c("Accredited Levee System", 
                    "Provisionally Accredited Levee (PAL) System", 
                    "Non-Accredited Levee System"))) +  
  theme_minimal(base_size = 16) +  
  theme(
    legend.position = "top",  
    axis.title.x = element_text(size = 22),  
    axis.text.y = element_text(face = "bold"),  
    panel.grid.major.y = element_blank()
  ) +
  guides(color = "none")

# Save the plot
ggsave("Output/Fig4_MedIncome.jpg", income_violin_plot, width = 10, height = 5)

#Summary statistics: 
filtered_data %>%
    group_by(accredited) %>%
    summarise(
      n_levees = n(),
      median_income = median(MedIncome, na.rm = TRUE),
      mean_income = mean(MedIncome, na.rm = TRUE),
      total_population = sum(Pop, na.rm = TRUE),
      .groups = 'drop'
    ) %>% print()
