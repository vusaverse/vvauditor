#' Check for No Duplicate Rows
#'
#' This function checks if there are any duplicate rows in the provided dataframe.
#' If there are duplicate rows, a message is added to the provided collection.
#'
#' @param dataframe A dataframe.
#' @param collection A list to store the message if there are duplicate rows.
#' @param unique_columns Default is NULL. If provided, these are the columns to check for uniqueness.
#' @return The updated collection.
#' @examples
#' # Create a dataframe with some duplicate rows
#' dataframe <- data.frame(a = c(1, 1, 2), b = c(2, 2, 3))
#' collection <- checkmate::makeAssertCollection()
#' check_no_duplicate_rows(dataframe, collection, c("a", "b"))
#' @export
check_no_duplicate_rows <- function(dataframe, collection, unique_columns = NULL) {
  if (!is.null(unique_columns) && !checkmate::testNames(unique_columns, subset.of = names(dataframe))) {
    collection$push(paste("Insufficient columns to determine duplicate rows",
                          "The following columns are missing:",
                          paste(setdiff(unique_columns, names(dataframe)), collapse = "\n"),
                          sep = "\n"))
    return(collection)
  }

  assert_no_duplicates_in_group(dataframe, group_vars = unique_columns, collection)

  return(collection)
}
