test_that("returns the maximum numeric value for a numeric vector", {
  numeric_vector <- c(3,  1,  4,  1,  5,  9)
  expect_equal(find_maximum_value(numeric_vector),  9)
})

test_that("returns NA for a vector with only non-numeric values", {
  non_numeric_vector <- c("one", "two", "three")
  expect_equal(find_maximum_value(non_numeric_vector), NA)
})
