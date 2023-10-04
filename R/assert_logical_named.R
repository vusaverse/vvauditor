#' Assert Logical Value in Column
#'
#' This function asserts that the values in a specified column of a data frame are logical.
#' It uses the `checkmate::assert_logical` function to perform the assertion.
#'
#' @param column A character vector or string with the column name to be tested.
#' @param df The data frame that contains the column.
#' @param prefix_column A character string that will be prepended to the column name in the assertion message. Default is NULL.
#' @param ... Additional parameters are passed to the `checkmate::assert_logical` function.
#' @return None
#' @examples
#' # Create a data frame
#' df <- data.frame(a = c(TRUE, FALSE, TRUE, FALSE), b = c(1, 2, 3, 4))
#' # Assert that the values in column "a" are logical
#' assert_logical_named("a", df)
#'
#' @export
assert_logical_named <- function(column, df, prefix_column = NULL, ...) {
  checkmate::assert_logical(df[[column]],
    .var.name = trimws(paste(prefix_column, column)),
    ...
  )
}
