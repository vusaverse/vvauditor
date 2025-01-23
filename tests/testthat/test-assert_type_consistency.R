test_that("checks types match.", {
  mtcars_test <- read_rds("tests/datasets/mtcars_test.rds")

  mtcars_test_changed <- mtcars_test %>%
    mutate(mpg = as.character(mpg))

  metadata <- get_current_documentation()
  warnings_captured <- capture_warnings(assert_type_consistency(mtcars_test_changed, metadata))
  expect_true(length(warnings_captured) > 0)
})
