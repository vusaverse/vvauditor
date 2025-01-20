#' Check if the fieldnames of the dataset are the same
#'
#' Assert Field Consistency Between Data and Metadata
#'
#' This function checks for consistency between the field names in new data and the field names specified in a metadata reference.
#' It warns if there are missing fields in the new data or if new unexpected fields appear in the data that are not defined in the metadata.
#'
#' @param new_data A data frame. The new dataset whose field names need to be checked.
#' @param field_info A data frame. Metadata containing a column named `raw_field_name` that lists the expected field names.
#'
#' @return No return value. The function issues warnings if there are inconsistencies in field names.
#' @export

assert_field_consistency <- function(new_data, field_info) {
  # Read the field_info metadata

  # Get the raw field names from the metadata
  metadata_fields <- field_info$raw_field_name

  # Get the field names from the new data
  data_fields <- names(new_data)

  # Check for fields in metadata that are missing in the new data
  missing_fields <- setdiff(metadata_fields, data_fields)
  if (length(missing_fields) > 0) {
    warning(sprintf("The following fields from the metadata are missing in the new data: %s",
                    paste(missing_fields, collapse = ", ")))
  }

  # Check for new fields in the data that are not in the metadata
  new_fields <- setdiff(data_fields, metadata_fields)
  if (length(new_fields) > 0) {
    warning(sprintf("The following new fields in the data are not accounted for in the metadata: %s",
                    paste(new_fields, collapse = ", ")))
  }
}
