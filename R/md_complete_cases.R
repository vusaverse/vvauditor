#' MD complete cases
#'
#' Print the complete cases the de data.
#'
#' @param data The data frame.
#' @param digits Default: 1. number of digits for rounding.
#'
#' @export
md_complete_cases <- function(data, digits = 1){
    Number_of_rows <- nrow(data)
    Number_of_rows_complete <- sum(stats::complete.cases(data))
    cat("Number of rows in the data: ", Number_of_rows, "\n",
        "Number of rows without missing values: ", Number_of_rows_complete, "\n",
        "Percentage complete rows: ", round((Number_of_rows_complete / Number_of_rows) * 100, digits = digits), "%",
        sep = "")

}
