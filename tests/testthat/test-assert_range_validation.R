test_that("checks whether an unexpected outlier is found in numeric data.", {
  mtcars_test <- return_mtcars_testfile()

  mtcars_test_changed <- mtcars_test %>%
    mutate(mpg = 35.4)

  metadata <- get_current_documentation()
  warnings_captured <- capture_warnings(assert_range_validation(mtcars_test_changed, metadata))
  expect_true(length(warnings_captured) > 0)
})
