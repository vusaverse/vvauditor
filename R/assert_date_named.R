#' Assert Date Value in Column
#'
#' This function asserts that the values in a specified column of a data frame are of Date type.
#' It uses the `checkmate::assert_date` function to perform the assertion.
#'
#' @param column A character vector or string with the column name to be tested.
#' @param df The data frame that contains the column.
#' @param prefix_column A character string that will be prepended to the column name in the assertion message. Default is NULL.
#' @param ... Additional parameters are passed to the `checkmate::assert_date` function.
#' @return None
#'
#' @export
assert_date_named <- function(column, df, prefix_column = NULL, ...) {
  checkmate::assert_date(df[[column]],
    .var.name = trimws(paste(prefix_column, column)),
    ...
  )
}
