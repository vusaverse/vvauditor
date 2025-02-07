#' Create categorical details csv
#'
#' This function returns a categorical details csv. Containing categorical information about the dataset
#'
#' @param data A dataframe for which to create a categorical details csv.
#' @param mapping A dataframe containing a mapping named vector, containing preferred fieldnames
#' Example:
#'   column_names <- c(
#'     mpg = "mpg", cyl = "cyl", disp = "disp", hp = "hp",
#'     drat = "drat", wt = "wt", qsec = "qsec", vs = "vs",
#'     am = "am", gear = "gear", carb = "carb", spare_tire = "spare_tire"
#' @return Dataframe containing categorical details
#' @export

create_categorical_details <- function(data, mapping) {
  categorical_cols <- data %>% dplyr::select(dplyr::where(~ is.factor(.) | is.character(.)))
  if (ncol(categorical_cols) == 0) {
    return(dplyr::tibble(
      preferred_field_name = character()
    ))
  }

  result <- categorical_cols %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), get_ratio_separate)) %>%
    tidyr::pivot_longer(dplyr::everything(),
      names_to = "raw_field_name",
      values_to = "values"
    ) %>%
    dplyr::group_by(raw_field_name) %>%
    dplyr::mutate(col_num = rep(1:10, length.out = dplyr::n())) %>% # Changed from 1:20 to 1:10
    tidyr::pivot_wider(
      names_from = col_num, values_from = values,
      names_prefix = "value_"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(preferred_field_name = mapping[raw_field_name]) %>%
    dplyr::select(preferred_field_name, starts_with("value_"))

  colnames(result)[2:11] <- c( # Changed from 2:21 to 2:11
    "category_1", "percent_1", "category_2", "percent_2",
    "category_3", "percent_3", "category_4", "percent_4",
    "category_5", "percent_5"
  )

  return(result)
}
