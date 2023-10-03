#' Remove Duplicates and NA Values from Input
#'
#' This function removes duplicate values and NA values from the input.
#' It first removes NA values from the input using the `na.omit` function from the `stats` package.
#' Then it removes duplicate values from the result using the `unique` function.
#'
#' @param input A vector or data frame.
#' @return A vector or data frame with duplicate values and NA values removed.
#' @examples
#' # Create a vector with duplicate values and NA values
#' input <- c(1, 2, NA, 2, NA, 3, 4, 4, NA, 5)
#' # Remove duplicate values and NA values
#' output <- remove_duplicates_and_na(input)
#' print(output)
#'
#' @export
remove_duplicates_and_na <- function(input) {
  # Remove NA values
  input <- stats::na.omit(input)

  # Remove duplicate values
  input <- unique(input)

  return(input)
}
