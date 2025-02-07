#' Create field info
#'
#' This function returns a dataframe containing field info information about the dataset
#'
#' @param data A dataframe for which to create a field info csv.
#' @param raw_data_path A string containing the original location of the original raw file
#' @param broker The name of of the organisation or person that distributes the dataset
#' @param product The name of the product where this dataset is used in
#' @param public_dataset Boolean containing whether the dataset is publicly available
#' is_primary_key Is_primary_key is variable that can be manually set to TRUE if the dataset contains a primary key.
#' @return Dataframe containing subset info
#' @export

create_field_info <- function(data,
                              raw_data_path = NULL,
                              broker = NULL,
                              product = NULL,
                              public_dataset = NULL) {
  field_info <- dplyr::tibble(
    preferred_field_name = names(data),
    raw_field_name = names(data),
    in_use = TRUE,
    is_unique_column = purrr::imap_lgl(data, ~ {
      vvauditor::is_unique_column(.y, data)
    }),
    is_primary_key = FALSE,
    raw_data_path = raw_data_path,
    broker=broker,
    product=product,
    public_dataset=public_dataset
  )
  return(field_info)
}
