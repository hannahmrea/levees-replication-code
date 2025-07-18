# Helper functions for NLD data preprocessing

#' Set consistent factor levels for NLD categorical variables
#' @param data A data frame containing NLD data
#' @return The same data frame with standardized factor levels
set_nld_factors <- function(data) {
  # Set accreditation status levels if the column exists
  if ("accredited" %in% names(data)) {
    data$accredited <- factor(
      data$accredited,
      levels = c(
        "Accredited Levee System",
        "Provisionally Accredited Levee (PAL) System",
        "A99",
        "Non-Accredited Levee System"
      )
    )
  }
  
  # Set LSAC rating levels if the column exists
  if ("lsacRatingName" %in% names(data)) {
    data$lsacRatingName <- factor(
      data$lsacRatingName,
      levels = c("Not Screened", "Very Low", "Low", "Moderate", "High", "Very High")
    )
  }
  
  # Set LSAC screening status if the column exists
  if ("lsacScreened" %in% names(data)) {
    data$lsacScreened <- factor(
      data$lsacScreened,
      levels = c("Not Screened", "Screened")
    )
  }
  
  return(data)
} 