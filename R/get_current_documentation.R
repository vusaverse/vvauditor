get_current_documentation <- function() {
  # Load all metadata files
  field_info <- readr::read_delim("documentation/field_info.csv", show_col_types = FALSE)
  numeric_details <- readr::read_delim(file.path("documentation/numeric_details.csv"), show_col_types = FALSE)
  data_types <- readr::read_delim(file.path("documentation/data_types.csv"), show_col_types = FALSE)
  # Join metadata tables
  metadata <- field_info %>%
    dplyr::left_join(numeric_details, by = "preferred_field_name") %>%
    dplyr::left_join(data_types, by = "preferred_field_name")
}
