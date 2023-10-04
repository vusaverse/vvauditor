#' Check for No Duplicates in Group
#'
#' This function checks if there is exactly one row per group in the provided dataframe.
#' If there are multiple rows per group, the assertion fails.
#'
#' @param dataframe The dataframe to be checked.
#' @param group_variables The group variables as a character vector. The default is NULL.
#' @param assertion_fail How the function reacts to a failure. This can be a "warning", where only a warning is given on the failure,
#' or a "stop", where the function execution is stopped and the message is displayed, or an "AssertCollection", where the failure message is added to an assertion collection.
#' @family assertions
#' @family tests
#' @examples
#' # Create a dataframe with some groups having more than one row
#' dataframe <- data.frame(a = c(1, 1, 2), b = c(2, 2, 3), c = c("x", "x", "y"))
#' # Check the uniqueness of rows per group
#' check_no_duplicates_in_group(dataframe)
#' @export
check_no_duplicates_in_group <- function(dataframe,
                                         group_variables = NULL,
                                         assertion_fail = "stop") {
  row_count <- NULL

  # Check if the variables to test exist in the dataframe, and give a message otherwise
  if (!is.null(group_variables) && !all(group_variables %in% names(dataframe))) {
    assertion_message(paste("Insufficient columns to determine duplicate rows",
                            "The following columns are missing:",
                            paste(setdiff(group_variables, names(dataframe)), collapse = "\n"),
                            sep = "\n"),
                      assertion_fail = assertion_fail)
  }

  # Check if the given columns exist in the dataframe before grouping
  if (any(group_variables %in% names(dataframe))) {
    result <- dataframe %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(dplyr::vars(dplyr::one_of(group_variables))) %>%
      dplyr::summarise(row_count = dplyr::n()) %>%
      dplyr::mutate(duplicates_per_field = row_count - 1)

    duplicate_count <- count_more_than_1(result$row_count)
    total_duplicates <- sum(result$duplicates_per_field)

    result <- TRUE
    variables <- paste(group_variables, collapse = ", ")
    if (duplicate_count > 0) {
      result <- paste("In the combination of the columns", variables, "there are",
                      duplicate_count, "rows that occur more than once. The total number of duplicate rows is",
                      total_duplicates)
      assertion_message(result, assertion_fail)
    }
  }
  return(invisible(dataframe))
}
