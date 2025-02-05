#' Assert Range Validation for Data Fields
#'
#' This function checks whether the values in a dataset fall within the expected minimum and maximum range
#' as specified in the metadata. It warns if any values violate the expected range.
#'
#' @param data A data frame. The dataset containing the fields to validate.
#' @param metadata A data frame. Metadata containing expected minimum and maximum values for each field.
#'   It must include the columns `raw_field_name`, `min`, `max`, and `preferred_field_name`.
#'
#' @return No return value. The function issues warnings if any values fall outside the expected range.
#' @export

assert_range_validation <- function(data, metadata) {
  for (i in seq_len(nrow(metadata))) {
    field <- metadata$raw_field_name[i]
    if (field %in% names(data) && !is.na(metadata$min[i]) && !is.na(metadata$max[i])) {
      lower <- metadata$min[i]
      upper <- metadata$max[i]

      if (!all(data[[field]] >= lower & data[[field]] <= upper, na.rm = TRUE)) {
        warning(sprintf(
          "Range violation in field '%s' (preferred name: '%s'): values outside [%s, %s]",
          field, metadata$preferred_field_name[i], lower, upper
        ))
      }
    }
  }
}
