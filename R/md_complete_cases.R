#' MD complete cases
#'
#' Print the complete cases of the data.
#'
#' @param data The data frame.
#' @param digits Default: 1. number of digits for rounding.
#' @return Message with the number of rows, number of rows with missing values and the percentage of complete rows.
#' @examples
#' # example code
#' md_complete_cases(iris)
#'
#' iris$Sepal.Length[5] <- NA_character_
#' md_complete_cases(iris)
#'
#' @export
md_complete_cases <- function(data, digits = 1) {
    Number_of_rows <- nrow(data)
    Number_of_rows_complete <- sum(stats::complete.cases(data))
    message("Number of rows in the data: ", Number_of_rows, "\n",
        "Number of rows without missing values: ", Number_of_rows_complete, "\n",
        "Percentage complete rows: ", round((Number_of_rows_complete / Number_of_rows) * 100, digits = digits), "%",
        sep = "")

}
