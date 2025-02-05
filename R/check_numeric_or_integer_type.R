#' Check for Numeric or Integer Type
#'
#' This function checks if the specified column in the provided dataframe has a numeric or integer type.
#' It uses the checkmate::assert_numeric or checkmate::assert_integer function to perform the assertion,
#' depending on the value of the `field_type` parameter.
#'
#' @param column_name A character vector or string with the column name to be tested.
#' @param dataframe The dataframe that contains the column.
#' @param column_prefix Default is NULL. If provided, this text is prepended to the variable name in the assertion message.
#' @param field_type Default is "numeric". Specify "integer" to check if the column has an integer type. This parameter must be either "integer" or "numeric".
#' @param ... The remaining parameters are passed to the function assert_numeric or assert_integer.
#' @family assertions
#' @family tests
#' @examples
#' # Create a dataframe with a numeric column
#' dataframe <- data.frame(a = c(1, 2, 3))
#' # Check the numeric type of the 'a' column
#' check_numeric_or_integer_type("a", dataframe)
#' @export
check_numeric_or_integer_type <- function(column_name, dataframe, column_prefix = NULL, field_type = "numeric", ...) {
  # Check if field_type is either "integer" or "numeric"
  if (!(field_type %in% c("integer", "numeric"))) {
    stop("field_type must be either 'integer' or 'numeric'")
  }

  if (field_type == "integer") {
    checkmate::assert_integer(dataframe[[column_name]],
      .var.name = trimws(paste(column_prefix, column_name)),
      ...
    )
  } else {
    checkmate::assert_numeric(dataframe[[column_name]],
      .var.name = trimws(paste(column_prefix, column_name)),
      ...
    )
  }
}
