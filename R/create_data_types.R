#' Create data types tibble
#'
#' This function returns a data types tibble. Containing type information about the dataset.
#'
#' @param data A dataframe for which to create a data types csv.
#' @param mapping A dataframe containing a mapping named vector, containing preferred fieldnames
#' Example:
#'   column_names <- c(
#'     mpg = "mpg", cyl = "cyl", disp = "disp", hp = "hp",
#'     drat = "drat", wt = "wt", qsec = "qsec", vs = "vs",
#'     am = "am", gear = "gear", carb = "carb", spare_tire = "spare_tire"
#'   )
#' @return Tibble containing data_types
#' @export

# Function to create data_types.csv
create_data_types <- function(data, mapping) {
  dplyr::tibble(
    preferred_field_name = mapping,
    type_of_variable = map_chr(data, ~ class(.x)[1]),
    percentage_of_missing_values = map_dbl(data, ~ mean(is.na(.)) * 100) %>% round(2),
    count_of_valid_values = purrr::map_int(data, ~ sum(!is.na(.))),
    count_of_invalid_values = purrr::map_int(data, ~ sum(is.na(.))),
    distinct_count = sapply(data, function(x) length(unique(x)))
  )
}
