#' Return Assertion Messages
#'
#' This function returns a message indicating whether an assertion test has passed or failed.
#' An "assertion collection" from the checkmate package must be provided. The message can be returned as an error or a warning.
#' For some assertions, only warnings are allowed, as an error would stop the script from running.
#' This is done for the following assertions: percentage missing values, duplicates, subset, and set_equal.
#'
#' @param collection An object with the class "AssertCollection".
#' @param collection_name The name of the collection. This name is mentioned in the messages.
#' @param fail "stop" or "warning". If the assertions fail, an error is returned and the script output is stopped. If "warning", only a warning is returned.
#' @param silent If FALSE (default), the success message is printed in the console. If TRUE, it is not shown.
#' @param output_map A map, like 1. Read data, where the file is stored.
#'
#' @return The message indicating whether the assertion test has passed or failed.
#' @export
return_assertions_message <- function(collection, collection_name, fail = "stop", silent = FALSE, output_map = NULL) {
  stopifnot(class(collection) == "AssertCollection")

  if (collection$isEmpty()) {
    if (silent) {
      return(invisible(paste(collection_name, "Assertions PASSED", sep = ": ")))
    } else {
      return(paste(collection_name, "Assertions PASSED", sep = ": "))
    }
  } else {
    message_count <- length(collection$getMessages())

    assertion_title <- paste(collection_name, ": ", message_count, " Assertion",
      if (message_count == 1) {
        ""
      } else {
        "s"
      }, " FAILED",
      sep = ""
    )

    warning_messages <- c()
    error_messages <- c()
    for (message in collection$getMessages()) {
      if (grepl("Must be a subset of", message) | grepl("Must be equal to set", message) |
        grepl("Allowed % NAs is", message) | grepl("Number of duplicate rows", message)) {
        warning_messages <- c(warning_messages, message)
      } else {
        error_messages <- c(error_messages, message)
      }
    }

    assertion_messages <- paste(
      cli::style_bold(paste0(1:message_count, ".")),
      collection$getMessages(),
      collapse = "\n"
    )

    return_message <- paste(cli::style_bold(cli::col_red(assertion_title)),
      cli::col_cyan(assertion_messages),
      sep = "\n"
    )

    message(return_message)

    if (fail == "stop") {
      if (length(error_messages) > 0) {
        stop(return_message)
      } else {
        return(warning(return_message))
      }
    } else if (fail == "warning") {
      return(warning(return_message))
    }
  }
}


#' Check for columns with only NA values
#'
#' This function checks if there are any columns in the provided dataframe that contain only NA values.
#' If such columns exist, their names are added to the provided collection.
#'
#' @param df A dataframe.
#' @param collection A list to store the names of the columns with only NA values.
#' @return The updated collection.
#' @examples
#' # Create a dataframe with some columns containing only NA values
#' df <- data.frame(a = c(1, NA, 3), b = c(NA, NA, NA), c = c(4, 5, 6))
#' collection <- checkmate::makeAssertCollection()
#' check_na_columns(df, collection)
#' @export
check_na_columns <- function(df, collection) {
  p_na <- variable <- NULL
  na_percentage <- data.frame(p_na = round(100 * sapply(df, function(x) sum(is.na(x))) / nrow(df), 2))
  na_percentage$variable <- rownames(na_percentage)
  rownames(na_percentage) <- NULL

  if (any(na_percentage$p_na == 100)) {
    columns <- na_percentage %>%
      dplyr::filter(p_na == 100) %>%
      dplyr::select(variable) %>%
      unlist() %>%
      toString()
    collection$push(paste("The following columns contain 100% NA's:", columns))
  }
  return(collection)
}


#' Check for Columns with Only 0s
#'
#' This function checks if there are any columns in the provided dataframe that contain only 0 values.
#' If such columns exist, their names are added to the provided collection.
#'
#' @param dataframe A dataframe.
#' @param collection A list to store the names of the columns with only 0 values.
#' @return The updated collection.
#' @examples
#' # Create a dataframe with some columns containing only 0 values
#' dataframe <- data.frame(a = c(0, 0, 0), b = c(1, 2, 3), c = c(0, 0, 0))
#' collection <- checkmate::makeAssertCollection()
#' check_zero_columns(dataframe, collection)
#' @export
check_zero_columns <- function(dataframe, collection) {
  percentage_zeros <- variable <- p_zeros <- NULL

  ## Create dataframe with percentage 0 per column
  df_zeros <- data.frame(p_zeros = round(100 * sapply(
    dataframe,
    function(x) sum(x == 0, na.rm = T)
  ) / nrow(dataframe), 2))
  df_zeros$variable <- rownames(df_zeros)
  rownames(df_zeros) <- NULL

  if (any(df_zeros$p_zeros == 100)) {
    columns <- df_zeros %>%
      dplyr::filter(p_zeros == 100) %>%
      dplyr::select(variable) %>%
      unlist() %>%
      toString()
    collection$push(paste("The following columns contain 100% 0s:", columns))
  }
  return(collection)
}


#' Check for Non-Zero Rows
#'
#' This function checks if there are more than 0 rows in the provided dataframe.
#' If there are 0 rows, a message is added to the provided collection.
#'
#' @param dataframe A dataframe.
#' @param collection A list to store the message if there are 0 rows.
#' @return The updated collection.
#' @examples
#' # Create an empty dataframe
#' dataframe <- data.frame()
#' collection <- checkmate::makeAssertCollection()
#' check_non_zero_rows(dataframe, collection)
#' @export
check_non_zero_rows <- function(dataframe, collection) {
  if (nrow(dataframe) <= 0) {
    collection$push("The dataset contains 0 rows")
  }
  return(collection)
}
