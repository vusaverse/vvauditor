#' Create numeric details csv
#'
#' This function returns a numeric details csv. Containing numeric information about the dataset
#'
#' @param data A dataframe for which to create a numeric details csv.
#' @param mapping A dataframe containing a mapping named vector, containing preferred fieldnames
#' Example:
#'   column_names <- c(
#'     mpg = "mpg", cyl = "cyl", disp = "disp", hp = "hp",
#'     drat = "drat", wt = "wt", qsec = "qsec", vs = "vs",
#'     am = "am", gear = "gear", carb = "carb", spare_tire = "spare_tire"
#'   )
#' @return Dataframe containing numeric details.
#' @export

create_numeric_details <- function(data, mapping) {
  # Apply get_dist to all columns
  all_stats <- data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), get_dist))

  # Create a dataframe with separate columns for each statistic
  numeric_details <- dplyr::tibble(
    raw_field_name = names(all_stats),
    distribution = unlist(all_stats)
  ) %>%
      tidyr::separate(distribution,
      into = c("q1", "median", "q3", "mean", "sd"),
      sep = " \\| ",
      remove = FALSE
    ) %>%
    dplyr::mutate(dplyr::across(q1:sd, ~ gsub(".*=\\s*", "", .))) %>%
    dplyr::mutate(dplyr::across(q1:sd, as.numeric)) %>%
    dplyr::mutate(
      min = sapply(data[raw_field_name], min, na.rm = TRUE),
      max = sapply(data[raw_field_name], max, na.rm = TRUE)
    ) %>%
    dplyr::mutate(preferred_field_name = mapping[raw_field_name]) %>%
    dplyr::select(preferred_field_name, min, max, q1, median, q3, mean, sd)

  return(numeric_details)
}
