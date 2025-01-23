test_that("Tests if no warnings are found in the main assertion function.", {
  mtcars_test <- read_rds("tests/datasets/mtcars_test.rds")

  warnings_captured <- capture_warnings(run_all_assertions(mtcars_test, "tests/documentation/"))
  expect_true(length(warnings_captured) == 0)
})
