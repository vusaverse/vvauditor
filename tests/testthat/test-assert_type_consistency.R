test_that("checks if types match.", {
  mtcars_test <- return_mtcars_testfile()

  mtcars_test_changed <- mtcars_test %>%
    dplyr::mutate(mpg = as.character(mpg))

  metadata <- get_current_documentation()
  warnings_captured <- capture_warnings(assert_type_consistency(mtcars_test_changed, metadata))
  expect_true(length(warnings_captured) > 0)
})
