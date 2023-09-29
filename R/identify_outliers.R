#' Identify Outliers in a Data Frame Column
#'
#' This function identifies outliers in a specified column of a data frame.
#' It returns a tibble containing the unique values, tally, and whether it is an outlier or not.
#'
#' @param df The data frame.
#' @param var The column to check for outliers.
#'
#' @return A tibble containing the unique values, tally, and whether each value is an outlier or not.
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3, 100, 101), b = c(4, 5, 6, 7, 8), c = c(7, 8, 9, 100, 101))
#' outliers <- identify_outliers(df, 'a')
#' print(outliers)
#'
#' @export
identify_outliers <- function(df, var) {
  n <- NULL
  check <- dplyr::select(.data = df, {{ var }}) %>%
    dplyr::group_by({{ var }}) %>%
    dplyr::tally() %>%
    dplyr::mutate(outlier = (n < stats::quantile(n, 0.05) | n > stats::quantile(n, 0.95)))

  return(check)
}
