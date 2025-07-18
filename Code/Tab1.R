# Tab1.R
#
# Purpose: Generate summary table of levee systems by accreditation status
# Author: Hannah Melville-Rea
# Last modified: June 2025

# Load required packages
library(dplyr)
library(kableExtra)

# Load cleaned 2024 API summary data
levee_systems <- read.csv("Data/Cleaned/LeveeSystem_2024_API_summary.csv")

# Create summary table
summary_table <- levee_systems %>%
  group_by(accredited) %>%
  summarise(
    systems = n(),
    miles = sum(leveeLengthInMiles, na.rm = TRUE),  # Use reported length
    population = sum(peopleAtRisk, na.rm = TRUE),
    buildings = sum(structuresAtRisk, na.rm = TRUE)
  ) %>%
  # Format numbers with commas
  mutate(
    population = format(population, big.mark = ","),
    buildings = format(buildings, big.mark = ","),
    miles = round(miles, 0)
  ) %>%
  # Rename accreditation statuses to be shorter
  mutate(accredited = case_when(
    accredited == "Accredited Levee System" ~ "Accredited",
    accredited == "Provisionally Accredited Levee (PAL) System" ~ "Provisionally Accredited",
    accredited == "Under construction (A99)" ~ "Under construction (A99)",
    accredited == "Non-Accredited Levee System" ~ "Non-Accredited",
    TRUE ~ accredited
  )) %>%
  # Order rows as desired
  arrange(factor(accredited, 
                levels = c("Accredited", 
                          "Provisionally Accredited", 
                          "Under construction (A99)", 
                          "Non-Accredited")))

#Print table 
summary_table

# Generate LaTeX table
latex_table <- kable(summary_table, 
                    format = "latex",
                    booktabs = FALSE,
                    col.names = c("", "Systems", "Miles", "Population", "Buildings"),
                    align = c('l', 'r', 'r', 'r', 'r')) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  add_header_above(c(" " = 1, "Levees" = 2, "Assets" = 2)) %>%
  row_spec(0, bold = TRUE) %>%  # Bold header row
  column_spec(1, bold = TRUE) %>%  # Bold only the first column
  footnote(
    general = "Summarized from the National Levee Database (Febuary, 2024).",
    footnote_as_chunk = TRUE
  )

# Save LaTeX table to file
writeLines(latex_table, "Output/Tab1_levee_summary.tex")
