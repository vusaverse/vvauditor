test_that("returns the class of the first element for a numeric vector", {
  input_vector <- c(1, 2, 3)
  expect_equal(get_first_element_class(input_vector), "numeric")
})

test_that("returns the class of the first element for a character vector", {
  input_vector <- c("apple", "banana", "cherry")
  expect_equal(get_first_element_class(input_vector), "character")
})

test_that("returns the class of the first element for a logical vector", {
  input_vector <- c(TRUE, FALSE, TRUE)
  expect_equal(get_first_element_class(input_vector), "logical")
})

test_that("returns 'list' for an empty list", {
  input_vector <- list()
  expect_equal(get_first_element_class(input_vector), "list")
})

test_that("returns 'NULL' for a NULL input", {
  input_vector <- NULL
  expect_equal(get_first_element_class(input_vector), "NULL")
})
