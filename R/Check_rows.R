#'Check rows
#'
#'This function prints the number of rows of a data frame. This function is used
#' to check that rows are not deleted or doubled unless expected.
#'
#'@param data The data file whose rows are to be counted
#'@param name The name of the data file (this will be printed)
#'
#'@return A message is printed to the console with the number of rows of the data
#'@export
check_rows <- function(data = Analysis_set, name = "analysis set") {
  Analysis_set <- NULL
  print(paste("Number of rows of the ", name , ": ", nrow(data),sep=""))
}