#' Create dataset summary statistics table
#'
#' This function creates a summary statistics table for a dataframe, providing insights into
#' the nature of the data contained within. It includes detailed statistics for each column,
#' such as column types, missing value percentages, minimum and maximum values for numeric
#' columns, patterns for character columns, uniqueness of identifiers, and distributions.
#'
#' @param df_input A dataframe for which to create a summary statistics table.
#' @return A tibble with comprehensive summary statistics for each column in the input dataframe.
#' @importFrom purrr map_chr map_dbl map_lgl possibly
#' @export
create_dataset_summary_table <- function(df_input) {
  # Store column names
  column_names <- names(df_input)

  # Store column types
  column_types <- purrr::map_chr(df_input, get_first_element_class)

  # Calculate missing value percentages
  missing_value_percentages <- colMeans(is.na(df_input)) * 100

  # Find minimum and maximum values for numeric columns
  min_values <- purrr::map_dbl(df_input, find_minimum_value)
  max_values <- purrr::map_dbl(df_input, find_maximum_value)

  # Check for uniqueness of identifiers
  unique_identifiers <- purrr::map_lgl(column_names, is_unique_column, data_frame = df_input)

  # Calculate distribution statistics
  distribution_stats <- purrr::map_chr(df_input, get_distribution_statistics)

  # Calculate distribution percentages
  distribution_percentages <- purrr::map_chr(df_input, calculate_category_percentages)

  # Create a tibble with all summary statistics
  summary_table <- tibble::tibble(
    column_name = column_names,
    column_type = column_types,
    missing_value_percentage = missing_value_percentages,
    min_value = min_values,
    max_value = max_values,
    unique_identifier = unique_identifiers,
    distribution_statistics = distribution_stats,
    distribution_percentages = distribution_percentages
  )

  return(summary_table)
}


#' Check if a column in a dataframe has unique values
#'
#' @param column_name The name of the column to check for uniqueness.
#' @param data_frame A dataframe containing the column to check.
#' @return \code{TRUE} if the column has unique values, \code{FALSE} otherwise.
#' @export
#' @examples
#' # Create a dataframe with a unique ID column
#' data_frame <- tibble::tibble(
#'   id = c(1, 2, 3, 4, 5),
#'   value = c("a", "b", "c", "d", "e")
#' )
#' is_unique_column("id", data_frame) # Returns TRUE
#'
#' # Create a dataframe with duplicate values in the ID column
#' data_frame <- tibble::tibble(
#'   id = c(1, 2, 3, 4, 5, 1),
#'   value = c("a", "b", "c", "d", "e", "a")
#' )
#' is_unique_column("id", data_frame) # Returns FALSE
is_unique_column <- function(column_name, data_frame) {
  column_values <- data_frame %>% dplyr::pull(column_name)
  return(nrow(data_frame) == length(unique(column_values)))
}

#' Retrieve the class of the first element of a vector
#'
#' @param input_vector A vector whose first element's class is to be retrieved.
#' @return The class of the first element of the input vector.
#' @export
#' @examples
#' # Get the class of the first element in a numeric vector
#' get_first_element_class(c(1, 2, 3)) # Returns "numeric"
#'
#' # Get the class of the first element in a character vector
#' get_first_element_class(c("apple", "banana", "cherry")) # Returns "character"
get_first_element_class <- function(input_vector) {
  return(class(input_vector)[1])
}

#' Find the minimum numeric value in a vector, ignoring non-numeric values
#'
#' @param numeric_vector A vector from which to find the minimum numeric value.
#' @return The minimum numeric value in the input vector, or NA if none exist.
#' @export
#' @examples
#' # Find the minimum of a numeric vector
#' find_minimum_value(c(3, 1, 4, 1, 5, 9)) # Returns   1
#'
#' # Find the minimum of a mixed vector with non-numeric values
#' find_minimum_value(c(3, 1, 4, "two", 5, 9)) # Returns   1
#'
#' # Attempt to find the minimum of a vector with only non-numeric values
#' find_minimum_value(c("one", "two", "three")) # Returns NA
find_minimum_value <- function(numeric_vector) {
  if (is.numeric(numeric_vector)) {
    return(min(numeric_vector, na.rm = TRUE))
  } else {
    return(NA)
  }
}

#' Find the maximum numeric value in a vector, ignoring non-numeric values
#'
#' @param numeric_vector A vector from which to find the maximum numeric value.
#' @return The maximum numeric value in the input vector, or NA if none exist.
#' @export
#' @examples
#' # Find the maximum of a numeric vector
#' find_maximum_value(c(3, 1, 4, 1, 5, 9)) # Returns   9
#'
#' # Find the maximum of a mixed vector with non-numeric values
#' find_maximum_value(c(3, 1, 4, "two", 5, 9)) # Returns   9
#'
#' # Attempt to find the maximum of a vector with only non-numeric values
#' find_maximum_value(c("one", "two", "three")) # Returns NA
find_maximum_value <- function(numeric_vector) {
  if (is.numeric(numeric_vector)) {
    return(max(numeric_vector, na.rm = TRUE))
  } else {
    return(NA)
  }
}

#' Compute distribution statistics for a numeric vector
#'
#' This function computes summary statistics such as quartiles, mean, and standard deviation for a numeric vector.
#'
#' @param data_vector A numeric vector for which to compute summary statistics.
#' @return A character string describing the summary statistics of the input vector.
#' @export
#' @examples
#' # Compute summary statistics for a numeric vector
#' data_vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' get_distribution_statistics(data_vector)
get_distribution_statistics <- function(data_vector) {
  if (is.numeric(data_vector)) {
    calculated_quantiles <- stats::quantile(data_vector, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    calculated_summary_stats <- c(
      Mean = mean(data_vector, na.rm = TRUE),
      Std_Dev = stats::sd(data_vector, na.rm = TRUE)
    )
    formatted_output <- paste(
      "Q1 =", calculated_quantiles[1], " | median =", calculated_quantiles[2], " | Q3 = ", calculated_quantiles[3],
      " | mean =", format(calculated_summary_stats["Mean"], digits = 3), " | stdev =", format(calculated_summary_stats["Std_Dev"], digits = 3)
    )
    return(formatted_output)
  } else if (is.character(data_vector)) {
    # Filter out non-numeric characters
    numeric_elements <- grepl("^\\d*\\.?\\d*$", data_vector)
    if (any(numeric_elements)) {
      numeric_values <- as.numeric(data_vector[numeric_elements])
      calculated_quantiles <- stats::quantile(numeric_values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      calculated_summary_stats <- c(
        Mean = mean(numeric_values, na.rm = TRUE),
        Std_Dev = stats::sd(numeric_values, na.rm = TRUE)
      )
      formatted_output <- paste(
        "Q1 =", calculated_quantiles[1], " | median =", calculated_quantiles[2], " | Q3 = ", calculated_quantiles[3],
        " | mean =", format(calculated_summary_stats["Mean"], digits = 3), " | stdev =", format(calculated_summary_stats["Std_Dev"], digits = 3)
      )
      return(formatted_output)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

#' Calculate the percentage of categories in a data vector
#'
#' This function calculates the percentage of each category in a given data vector and returns the top   10 categories along with their percentages.
#' If the data vector is of Date class, it is converted to POSIXct. If the sum of the percentages is not   100%, an "Other" category is added to make up the difference, but only if the number of unique values exceeds  10.
#' If the data vector is of POSIXct class and the smallest percentage is less than   1%, the function returns "Not enough occurrences."
#'
#' @param data_vector A vector of categorical data.
#' @return A character string detailing the top   10 categories and their percentages, or a special message indicating not enough occurrences or unsupported data type.
#' @export
#' @examples
#' # Example with a character vector
#' data_vector <- c("cat", "dog", "bird", "cat", "dog", "cat", "other")
#' calculate_category_percentages(data_vector)
#'
#' # Example with a Date vector
#' data_vector <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
#' calculate_category_percentages(data_vector)
calculate_category_percentages <- function(data_vector) {
  # Initialize variables
  percentages <- n <- percent <- categories <- .data <- NULL

  # Convert Date objects to POSIXct
  if (inherits(data_vector, "Date")) {
    data_vector <- as.POSIXct(data_vector)
  }

  # Check if data_vector is a POSIXct object
  if (!lubridate::is.POSIXct(data_vector)) {
    # Calculate frequencies and percentages for non-Date vectors
    frequency_table <- janitor::tabyl(data_vector) %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::mutate(percent = n / sum(n) * 100) %>%
      dplyr::filter(!is.na(.data)) %>%
      dplyr::slice_max(order_by = percent, n = 10)

    categories <- frequency_table$data_vector
    percentages <- format(round(frequency_table$percent, 2), nsmall = 2)

    # Adjust percentages if total is not   100% and there are more than  10 unique values
    if (sum(as.numeric(percentages)) < 100 && length(unique(data_vector)) > 10) {
      percentages <- c(percentages, sprintf("%.2f", 100 - sum(as.numeric(percentages))))
      categories <- c(categories, "Other")
    } else {
      percentages[length(percentages)] <- sprintf("%.2f", 100 - sum(as.numeric(percentages[-length(percentages)])))
    }

    result <- paste0(categories, ": (", percentages, "%)", collapse = " | ")
    return(result)
  } else {
    # Calculate frequencies and percentages for POSIXct vectors
    frequency_table <- janitor::tabyl(data_vector) %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::mutate(percent = n / sum(n) * 100) %>%
      dplyr::filter(!is.na(.data)) %>%
      dplyr::slice_max(order_by = percent, n = 10)

    categories <- as.character(frequency_table$data_vector)
    percentages <- format(round(frequency_table$percent, 2), nsmall = 2)

    # Adjust percentages if total is not   100% and there are more than  10 unique values
    if (sum(as.numeric(percentages)) < 100 && length(unique(data_vector)) > 10) {
      percentages <- c(percentages, sprintf("%.2f", 100 - sum(as.numeric(percentages))))
      categories <- c(categories, "Other")
    } else {
      percentages[length(percentages)] <- sprintf("%.2f", 100 - sum(as.numeric(percentages[-length(percentages)])))
    }

    # Check if the smallest percentage is less than   1%
    if (as.numeric(percentages[1]) < 1) {
      return("Not enough occurrences")
    } else {
      result <- paste0(categories, ": (", percentages, "%)", collapse = " | ")
      return(result)
    }
  }
  return("Data type not supported")
}
