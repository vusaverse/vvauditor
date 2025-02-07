#' Create subset fields
#'
#' This function returns a subsetfields info df.  Containing subsetfields information about the dataset
#'
#' @param data A dataframe for which to create a subsetfields csv.
#' @param mapping A dataframe containing a mapping named vector, containing preferred fieldnames
#' Example:
#'   column_names <- c(
#'     mpg = "mpg", cyl = "cyl", disp = "disp", hp = "hp",
#'     drat = "drat", wt = "wt", qsec = "qsec", vs = "vs",
#'     am = "am", gear = "gear", carb = "carb", spare_tire = "spare_tire"
#'   )
#' @return Dataframe containing subset info
#' @export

create_subset_fields <- function(data, mapping) {
  is_field_subset <- function(field_name, df) {
    if (is.character(df[[field_name]])) {
      return(length(unique(df[[field_name]])) < 20)
    }
    return(FALSE)
  }

  subset_fields <- names(data)[sapply(names(data), function(x) is_field_subset(x, data))]

  if (length(subset_fields) > 0) {
    subset_info <- data %>%
      dplyr::select(dplyr::all_of(subset_fields)) %>%
      tidyr::gather(key = "column_name", value = "value") %>%
      dplyr::distinct() %>%
      dplyr::arrange(column_name, value) %>%
      dplyr::filter(!is.na(value))

    # Rename columns using the mapping
    subset_info <- subset_info %>%
      dplyr::mutate(column_name = mapping[column_name])

    return(subset_info)
  }

  return(NULL)
}
