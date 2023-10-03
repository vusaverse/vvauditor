#' Construct Regex for Matching Function Parameter Content
#'
#' This function constructs a regex pattern for matching the content of a parameter in a function.
#' It uses the `base::paste0` function to construct the regex pattern.
#'
#' @param parameter The parameter whose value is to be searched in a function.
#' @return A regex pattern as a character string.
#' @examples
#' # Create a parameter name
#' parameter <- "my_parameter"
#' # Construct a regex pattern for matching the content of the parameter
#' pattern <- regex_content_parameter(parameter)
#'
#' @export
regex_content_parameter <- function(parameter) {
  base::paste0("(?<=",
               parameter,
               "\\s{0,20}\\=\\s{0,20}\").*")
}
