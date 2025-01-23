#' Read and return the mtcars testfile
#'
#' Gets the modified rds dataset for testing assertions.
#'
#' @return returns mtcars_test dataframe

return_mtcars_testfile <- function() {
  testfile_path <- paste0(getwd(), "/datasets/mtcars_test.rds")
  mtcars_test <- readr::read_rds(testfile_path)
  return(mtcars_test)
}
