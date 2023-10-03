#' Check for Duplicate Rows in Selected Columns
#'
#' This function checks if there are any duplicate rows in the specified columns of a data frame.
#' It prints the unique rows and returns a boolean indicating whether the number of rows in the original data frame is the same as the number of rows in the data frame with duplicate rows removed.
#'
#' @param data A data frame.
#' @param columns A character vector of column names.
#' @return A logical value indicating whether the number of rows in the original data frame is the same as the number of rows in the data frame with duplicate rows removed.
#' @examples
#' # Create a data frame
#' df <- data.frame(a = c(1, 2, 3, 1), b = c(4, 5, 6, 4), c = c(7, 8, 9, 7))
#' # Check for duplicate rows in the first two columns
#' check_duplicates(df, c("a", "b"))
#'
#' @export
check_duplicates <- function(data, columns) {
  # Select the specified columns
  selected_columns <- data %>% dplyr::select(dplyr::all_of(columns))

  # Remove duplicate rows
  unique_rows <- selected_columns %>% dplyr::distinct()

  # Print the unique rows
  message(unique_rows)

  # Check if the number of rows in the original data frame is the same as the number of rows in the data frame with duplicate rows removed
  if (nrow(selected_columns) == nrow(unique_rows)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
