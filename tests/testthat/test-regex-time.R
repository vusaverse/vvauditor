test_that("regex_time function works correctly", {
  # Test the function with the "hh:mm" format
  regex <- regex_time("hh:mm")
  expect_match("09:05", regex)
  expect_no_match("9:5", regex)

  # Test the function with the "h:m" format
  regex <- regex_time("h:m")
  expect_match("9:5", regex)
  expect_no_match("09:05", regex)

  # Add more tests here for other formats
})
