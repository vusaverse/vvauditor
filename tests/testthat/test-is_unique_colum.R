test_that("returns TRUE for a column with unique values", {
  data_frame <- data.frame(id = c(1, 2, 3, 4, 5))
  expect_true(is_unique_column("id", data_frame))
})

test_that("returns FALSE for a column with duplicate values", {
  data_frame <- data.frame(id = c(1, 2, 3, 4, 5, 1))
  expect_false(is_unique_column("id", data_frame))
})
