test_that("checks whether the number of missing values diverts too much from the expectation", {
  mtcars_test <- return_mtcars_testfile()

  mtcars_test_changed <- mtcars_test %>%
    mutate(mpg = NA)

  metadata <- get_current_documentation()
  warnings_captured <- capture_warnings(assert_missing_values(mtcars_test_changed, metadata))
  expect_true(length(warnings_captured) > 0)
})
