# Script to install required R packages on Sherlock
# This should be run once to set up the environment

# List of required packages
required_packages <- c(
  "dplyr",
  "sf", 
  "parallel",
  "doSNOW",
  "geojsonsf",
  "scales"
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      cat("Installing package:", package, "\n")
      install.packages(package, repos = "https://cran.rstudio.com/")
    } else {
      cat("Package", package, "is already installed\n")
    }
  }
}

# Install packages
cat("Installing required packages...\n")
install_if_missing(required_packages)

# Verify installations
cat("\nVerifying package installations...\n")
for (package in required_packages) {
  if (require(package, character.only = TRUE, quietly = TRUE)) {
    cat("✓", package, "installed successfully\n")
  } else {
    cat("✗", package, "installation failed\n")
  }
}

cat("\nPackage installation complete!\n") 