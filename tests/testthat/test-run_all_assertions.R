test_that("Tests if no warnings are found in the main assertion function.", {
  mtcars_test <- return_mtcars_testfile()
  warnings_captured <- capture_warnings(run_all_assertions(mtcars_test, "documentation/"))
  expect_true(length(warnings_captured) == 0)
})
