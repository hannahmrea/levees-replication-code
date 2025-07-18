# Function to convert census tract variables to leveed areas using area weights
# This function:
# 1. Takes census data and pre-calculated weights
# 2. Converts specified variables from tract to leveed area level
# 3. Handles both average and total variables with appropriate weights
# 4. Returns leveed area data with converted variables

convert_census_to_levee <- function(
  census_data,           # Census tract data (data.frame)
  weights_data,          # Pre-calculated weights (data.frame)
  leveed_area_data,      # Leveed area metadata (sf object)
  variables_to_convert,  # List of variables to convert
  output_file = NULL,    # Optional: save results to file
  verbose = TRUE         # Whether to print diagnostic information
) {
  
  # Load required packages
  library(dplyr)
  library(sf)
  
  if (verbose) {
    cat("=== CONVERSION DIAGNOSTICS ===\n")
    cat("Census data rows:", nrow(census_data), "\n")
    cat("Weights data rows:", nrow(weights_data), "\n")
    cat("Leveed areas:", nrow(leveed_area_data), "\n")
    cat("Variables to convert:", length(variables_to_convert), "\n\n")
  }
  
  # Function to calculate weighted estimate and MOE
  calculate_weighted_estimate <- function(data, var_name, moe_name, var_type) {
    weight_col <- if (var_type == "average") "weight_for_averages" else "weight_for_totals"
    
    if (var_type == "average") {
      # For average variables (like median income), only include tracts with valid data
      result <- data %>%
        group_by(levee_id) %>%
        summarise(
          # Only sum values for tracts with valid data
          weighted_value = sum(!!sym(var_name) * !!sym(weight_col), na.rm = TRUE),
          weighted_moe = sqrt(sum((!!sym(moe_name) * !!sym(weight_col))^2, na.rm = TRUE)),
          n_tracts_with_data = sum(!is.na(!!sym(var_name))),
          total_weight = sum(!!sym(weight_col), na.rm = TRUE),
          # For average variables, normalize by weight of tracts WITH data
          weight_with_data = sum(!!sym(weight_col) * (!is.na(!!sym(var_name)))),
          .groups = 'drop'
        ) %>%
        mutate(
          # Calculate the weighted average properly
          weighted_value = ifelse(weight_with_data > 0, 
                                 weighted_value / weight_with_data, 
                                 NA_real_),
          weighted_moe = ifelse(weight_with_data > 0, 
                               weighted_moe / weight_with_data, 
                               NA_real_)
        ) %>%
        select(-weight_with_data)  # Remove the intermediate column
    } else {
      # For total variables, use the original calculation
      result <- data %>%
        group_by(levee_id) %>%
        summarise(
          weighted_value = sum(!!sym(var_name) * !!sym(weight_col), na.rm = TRUE),
          weighted_moe = sqrt(sum((!!sym(moe_name) * !!sym(weight_col))^2, na.rm = TRUE)),
          n_tracts_with_data = sum(!is.na(!!sym(var_name))),
          total_weight = sum(!!sym(weight_col), na.rm = TRUE),
          .groups = 'drop'
        )
    }
    
    return(result)
  }
  
  # Join weights with census data
  weights_with_data <- weights_data %>%
    filter(has_intersection) %>%
    left_join(census_data, by = c("tract_geoid" = "GEOID"))
  
  if (verbose) {
    cat("After join:\n")
    cat("  Rows in joined data:", nrow(weights_with_data), "\n")
    cat("  Unique leveed areas:", n_distinct(weights_with_data$levee_id), "\n")
    cat("  Unique census tracts:", n_distinct(weights_with_data$tract_geoid), "\n\n")
  }
  
  # Process all variables
  results_list <- list()
  for (var_name in names(variables_to_convert)) {
    var_info <- variables_to_convert[[var_name]]
    if (verbose) cat("Processing", var_name, "(", var_info$type, "variable)...\n")
    
    # Check if variable exists in census data
    if (!var_info$var %in% names(census_data)) {
      cat("  Warning: Variable", var_info$var, "not found in census data\n")
      next
    }
    
    # Check for missing data
    missing_count <- sum(is.na(weights_with_data[[var_info$var]]))
    if (verbose) {
      cat("  Missing values in", var_info$var, ":", missing_count, "\n")
    }
    
    result <- calculate_weighted_estimate(
      weights_with_data, 
      var_info$var, 
      var_info$moe, 
      var_info$type
    )
    
    # Rename columns to include variable name
    result <- result %>%
      rename_with(~paste0(var_name, "_", .), -levee_id)
    
    results_list[[var_name]] <- result
    
    if (verbose) {
      cat("  Leveed areas with data:", sum(!is.na(result[[paste0(var_name, "_weighted_value")]])), "\n\n")
    }
  }
  
  # Start with leveed area metadata
  leveed_area_metadata <- leveed_area_data %>%
    st_drop_geometry() %>%
    mutate(id = as.character(id)) %>%
    select(id, name, accredited, peopleAtRisk, counties, areaSquareMiles, leveeLengthInMiles)
  
  # Combine all results
  if (length(results_list) > 0) {
    combined_results <- results_list[[1]]
    for (i in 2:length(results_list)) {
      combined_results <- combined_results %>%
        left_join(results_list[[i]], by = "levee_id")
    }
    
    # Ensure levee_id is character for proper joining
    combined_results <- combined_results %>% 
      mutate(levee_id = as.character(levee_id))
    
    final_results <- leveed_area_metadata %>%
      left_join(combined_results, by = c("id" = "levee_id"))
  } else {
    final_results <- leveed_area_metadata
  }
  
  # Calculate coefficient of variation for variables with MOEs
  for (var_name in names(variables_to_convert)) {
    estimate_col <- paste0(var_name, "_weighted_value")
    moe_col <- paste0(var_name, "_weighted_moe")
    
    if (estimate_col %in% names(final_results) && moe_col %in% names(final_results)) {
      cv_col <- paste0(var_name, "_CV")
      final_results <- final_results %>%
        mutate(
          # Calculate CV, handling edge cases
          !!cv_col := case_when(
            # If estimate is 0 or very small, set CV to NA
            !!sym(estimate_col) <= 0 ~ NA_real_,
            # If MOE is 0, set CV to 0 (perfect precision)
            !!sym(moe_col) <= 0 ~ 0,
            # Otherwise calculate CV = (MOE/1.645) / estimate
            TRUE ~ (!!sym(moe_col) / 1.645) / !!sym(estimate_col)
          )
        )
    }
  }
  
  # Print summary statistics
  if (verbose) {
    cat("=== FINAL SUMMARY ===\n")
    for (var_name in names(variables_to_convert)) {
      estimate_col <- paste0(var_name, "_weighted_value")
      if (estimate_col %in% names(final_results)) {
        non_na_count <- sum(!is.na(final_results[[estimate_col]]))
        cat(var_name, ": ", non_na_count, "/", nrow(final_results), " leveed areas have data\n")
      }
    }
    cat("\n")
  }
  
  # Save results if output file specified
  if (!is.null(output_file)) {
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(final_results, output_file)
    if (verbose) cat("Results saved to:", output_file, "\n")
  }
  
  # Return results
  return(final_results)
} 
