test_that("Tests if fields are consistent", {
  mtcars_test <- read_rds("tests/datasets/mtcars_test.rds")

  # Change a fieldname to test if the inconsistency drops an error.
  mtcars_test_changed <- mtcars_test %>%
    rename(geer = gear)

  metadata <- get_current_documentation()
  warnings_captured <- capture_warnings(assert_field_consistency(mtcars_test_changed, metadata))
  expect_true(length(warnings_captured) > 0)
})
