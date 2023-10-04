#' Check for POSIXct Type
#'
#' This function checks if the specified column in the provided dataframe has a POSIXct type.
#' It uses the checkmate::assert_posixct function to perform the assertion.
#'
#' @param column_name A character vector or string with the column name to be tested.
#' @param dataframe The dataframe that contains the column.
#' @param column_prefix Default is NULL. If provided, this text is prepended to the variable name in the assertion message.
#' @param ... The remaining parameters are passed to the function assert_posixct.
#' @family assertions
#' @family tests
#' @examples
#' # Create a dataframe with a POSIXct column
#' dataframe <- data.frame(date = as.POSIXct("2023-10-04"))
#' # Check the POSIXct type of the 'date' column
#' check_posixct_type("date", dataframe)
#' @export
check_posixct_type <- function(column_name, dataframe, column_prefix = NULL, ...) {
  checkmate::assert_posixct(dataframe[[column_name]],
                            .var.name = trimws(paste(column_prefix, column_name)),
                            ...)
}
