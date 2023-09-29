#' Find Common Columns Between Data Frames
#'
#' This function identifies common column names between multiple data frames.
#' It takes a variable number of data frames as input and returns a character
#' vector containing the common column names.
#'
#' @param ... A variable length list of data frames.
#'
#' @return A character vector of column names found in common between all data frames.
#'
#' @examples
#' df1 <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
#' df2 <- data.frame(a = c(7, 8, 9), b = c(10, 11, 12), c = c(13, 14, 15))
#' common_columns <- find_common_columns(df1, df2)
#' print(common_columns)
#'
#' @export
find_common_columns <- function(...) {
  list_of_data_frames <- list(...)
  column_names_list <- lapply(list_of_data_frames, names)
  common_columns <- Reduce(intersect, column_names_list)

  return(common_columns)
}
