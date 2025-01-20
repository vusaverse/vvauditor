#' Assert Type Consistency Between Data and Metadata
#'
#' This function checks whether the data types of fields in a dataset match the expected types specified in the metadata.
#' It warns if any fields have a different type than expected.
#'
#' @param data A data frame. The dataset containing the fields to validate.
#' @param metadata A data frame. Metadata specifying the expected data types for each field.
#'   It must include the columns `raw_field_name`, `type_of_variable`, and `preferred_field_name`.
#'
#' @return No return value. The function issues warnings if any fields have an unexpected type.
#' @export

assert_type_consistency <- function(data, metadata) {
  for (i in 1:nrow(metadata)) {
    field <- metadata$raw_field_name[i]
    if (field %in% names(data)) {
      expected_type <- metadata$type_of_variable[i]
      actual_type <- class(data[[field]])[1]

      if (expected_type != actual_type) {
        warning(sprintf("Type inconsistency in field '%s' (preferred name: '%s'): expected %s, got %s",
                        field, metadata$preferred_field_name[i], expected_type, actual_type))
      }
    }
  }
}
