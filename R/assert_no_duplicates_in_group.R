#' Assert No Duplicates in Group
#'
#' This function asserts that there are no duplicate rows in the specified columns of a data frame.
#' It groups the data frame by the specified columns, counts the number of unique values for each group, and checks if there are any groups with more than one row.
#' If there are, it prints an error message and stops the execution (unless `assertion_fail` is set to "warn").
#'
#' @param df A data frame.
#' @param group_vars A character vector of column names.
#' @param assertion_fail A character string indicating the action to take if the assertion fails. Can be "stop" (default) or "warn".
#' @return The input data frame.
#' @export
assert_no_duplicates_in_group <- function(df, group_vars, assertion_fail = "stop") {
  # Check whether the group variables exist in the data frame
  if (!all(group_vars %in% names(df))) {
    assertion_message(paste("Not enough columns to determine duplicate rows",
                            "The following columns are missing:",
                            paste(setdiff(group_vars, names(df)), collapse = "\n"),
                            sep = "\n"),
                      assertion_fail = assertion_fail)
  }

  # Check whether the group variables exist in the data frame
  if (any(group_vars %in% names(df))) {
    # Create a Tibble
    result <- df %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(dplyr::vars(dplyr::one_of(group_vars))) %>%
      dplyr::summarise(Number_rows = dplyr::n()) %>%
      dplyr::mutate(Number_double_per_field = Number_rows - 1)

    # Count the number of duplicates
    Count_doubles <- count_more_than_1(result$Number_rows)
    Total_double <- sum(result$Number_double_per_field)

    # If assertion succeeds, result must be TRUE
    result <- TRUE
    variables <- paste(group_vars, collapse = ", ")
    # If number of duplicates is more than 0, it will be displayed in the assert message.
    if (Count_doubles > 0) {
      result <- paste("In the combination of the columns", variables, "there are",
                      Count_doubles,
                      "rows that appear more often. The total number of duplicated rows is",
                      Total_double)
      assertion_message(result, assertion_fail)
    }
  }
  return(invisible(df))
}
