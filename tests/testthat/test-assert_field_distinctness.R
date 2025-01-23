test_that("checks whether the uniqueness of columns in a new dataset matches the expected uniqueness", {
  mtcars_test <- read_rds("tests/datasets/mtcars_test.rds")

  # column mpg is editted to be is_unique = True in field_info.csv
  metadata <- get_current_documentation() %>%
    mutate(is_unique_column = ifelse(preferred_field_name == "mpg", TRUE, FALSE))

  warnings_captured <- capture_warnings(assert_field_distinctness(mtcars_test, metadata))
  expect_true(length(warnings_captured) > 0)
})
