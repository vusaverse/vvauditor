#' Assert Field Existence in New Data
#'
#' This function checks whether all fields that existed in a previous dataset are still present in a new dataset, based on a metadata reference.
#' It warns if any fields from the previous dataset are missing in the new dataset.
#'
#' @param new_data A data frame. The new dataset whose field names need to be checked.
#' @param previous_data A data frame. The previous dataset used as a reference for expected fields.
#' @param metadata A data frame. Metadata containing a column named `raw_field_name`, which defines the expected field names.
#'
#' @return No return value. The function issues warnings if any expected fields are missing in the new dataset.
#' @export

assert_field_existence <- function(new_data, previous_data, metadata) {
  new_fields <- metadata$raw_field_name[metadata$raw_field_name %in% names(new_data)]
  previous_fields <- metadata$raw_field_name[metadata$raw_field_name %in% names(previous_data)]
  missing_fields <- setdiff(previous_fields, new_fields)

  if (length(missing_fields) > 0) {
    warning(sprintf("Fields missing in new dataset: %s", paste(missing_fields, collapse = ", ")))
  }
}
