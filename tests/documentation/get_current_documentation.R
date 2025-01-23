get_current_documentation <- function() {
  # Load all metadata files
  field_info <- read_delim("tests/documentation/field_info.csv", show_col_types = FALSE)
  numeric_details <- read_delim(file.path("tests/documentation/numeric_details.csv"), show_col_types = FALSE)
  data_types <- read_delim(file.path("tests/documentation/data_types.csv"), show_col_types = FALSE)
  # Join metadata tables
  metadata <- field_info %>%
    left_join(numeric_details, by = "preferred_field_name") %>%
    left_join(data_types, by = "preferred_field_name")
}
