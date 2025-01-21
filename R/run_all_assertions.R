#' Run All Data Validation Assertions
#'
#' This function performs multiple validation checks on a dataset using various assertion functions.
#' It loads metadata from specified CSV files, validates the dataset against expected field properties,
#' and stops execution if any warnings are encountered.
#'
#' @param new_data A data frame. The dataset to validate.
#' @param output_dir A character string. The directory containing metadata CSV files (`field_info.csv`, `numeric_details.csv`, `data_types.csv`).
#'
#' @return No return value. The function stops execution and displays warnings if any validation checks fail.
#' @export

run_all_assertions <- function(new_data, output_dir) {
  # Create a list to store warnings
  warnings_list <- list()

  # Custom warning handler
  warning_handler <- function(w) {
    warnings_list <<- c(warnings_list, list(w$message))
    invokeRestart("muffleWarning")
  }
  # Load all metadata files
  field_info <- read_csv2(file.path(output_dir, "field_info.csv"), show_col_types = FALSE)
  numeric_details <- read_csv2(file.path(output_dir, "numeric_details.csv"), show_col_types = FALSE)
  data_types <- read_csv2(file.path(output_dir, "data_types.csv"), show_col_types = FALSE)
  # Join metadata tables
  metadata <- field_info %>%
    left_join(numeric_details, by = "preferred_field_name") %>%
    left_join(data_types, by = "preferred_field_name")

  # Run all assertions with warning handling
  withCallingHandlers({
    assert_range_validation(new_data, metadata)
    assert_type_consistency(new_data, metadata)
    assert_missing_values(new_data, metadata)
    assert_field_consistency(new_data, field_info)
    assert_field_distinctness(new_data, metadata)
  }, warning = warning_handler)

  # Check if any warnings were produced
  if (length(warnings_list) > 0) {
    # Create a numbered list of warnings
    numbered_warnings <- paste(seq_along(warnings_list), warnings_list, sep = ". ")

    # Combine all warnings into a single message with a numbered list
    warning_message <- paste("The following warnings were produced:",
                             paste(numbered_warnings, collapse = "\n"),
                             sep = "\n\n")

    # Stop execution with the combined warning message
    stop(warning_message, call. = FALSE)
  }
}



