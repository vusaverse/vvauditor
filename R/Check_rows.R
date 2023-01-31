#'Check rows
#'
#'This function prints the number of rows of a data frame. This function is used
#' to check that rows are not deleted or doubled unless expected.
#'
#' @param df The data frame whose rows are to be counted
#' @param name The name of the data file (this will be printed)
#' @examples
#' check_rows(mtcars)
#'
#'
#' @return A message is printed to the console with the number of rows of the data
#' @export
check_rows <- function(df, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(df))
  }
  print(paste("Number of rows of the ", name , ": ", nrow(df), sep=""))
}
