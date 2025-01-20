#' Assert Consistency of Missing Values in Data
#'
#' This function checks whether the percentage of missing values in a dataset matches the documented percentage
#' in a metadata reference. It warns if there are significant discrepancies.
#'
#' @param data A data frame. The dataset to check for missing values.
#' @param metadata A data frame. Metadata containing expected missing value percentages and valid value counts.
#'   It must include the columns `raw_field_name`, `percentage_of_missing_values`, `count_of_valid_values`,
#'   and `preferred_field_name`.
#'
#' @return No return value. The function issues warnings if the actual missing value percentages
#'   deviate significantly from the documented values.
#' @export

assert_missing_values <- function(data, metadata) {
  for (i in 1:nrow(metadata)) {
    field <- metadata$raw_field_name[i]
    if (field %in% names(data)) {
      documented_missing_percentage <- metadata$percentage_of_missing_values[i]
      documented_count_of_valid_values <- metadata$count_of_valid_values[i]
      current_missing_percentage <- mean(is.na(data[[field]])) * 100
      current_count_of_valid_values <- sum(!is.na(data[field]))

      if (abs(current_missing_percentage - documented_missing_percentage) > 0.01) {  # Allow for small floating-point differences
        warning(sprintf("Mismatch in percentage of missing values for field '%s' (preferred name: '%s'):
                        Documented: %.2f%%, Current: %.2f%%. %s values should be valid, but only %s were found.",
                        field, metadata$preferred_field_name[i],
                        documented_missing_percentage, current_missing_percentage,
                        documented_count_of_valid_values,
                        current_count_of_valid_values))
      }
    }
  }
}
