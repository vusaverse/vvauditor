test_that("regex_year_date function works correctly", {
  # Test the function with the "yyyy" format
  regex <- regex_year_date("yyyy")
  expect_match("2023", regex)
  expect_no_match("2023-09-29", regex)

  # Test the function with the "yyyy-MM-dd" format
  regex <- regex_year_date("yyyy-MM-dd")
  expect_match("2023-09-29", regex)
  expect_no_match("2023/09/29", regex)

  # Add more tests here for other formats
})
