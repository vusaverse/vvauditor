#' Assert Field Uniqueness Consistency Between Data and Metadata
#'
#' This function checks whether the uniqueness of columns in a new dataset matches the expected uniqueness
#' defined in a metadata reference. It warns if any columns do not conform to the expected uniqueness.
#'
#' @param new_data A data frame. The dataset whose column uniqueness needs to be verified.
#' @param metadata A data frame. Metadata containing a column named `is_unique_column`, indicating whether each field is expected to be unique.
#'
#' @return No return value. The function issues warnings if any columns deviate from their expected uniqueness.
#' @export

assert_field_distinctness <- function(new_data, metadata){
  #Check if columns of new data are unique
  new_data_uniqueness <- purrr::imap_lgl(new_data, ~ is_unique_column(.y, new_data))
  is_unique_metadata <- metadata %>% dplyr::pull(is_unique_column)
  comparison <- new_data_uniqueness == is_unique_metadata
  if(!all(comparison)){
    wrong_columns <- names(comparison)[!comparison]
    #warning message weergeven op basis van unique/not unique
    warning(sprintf("The following fields are unique/not unique, going in against expectation: %s",
                    paste(wrong_columns, collapse = ", ")))
  }
}
