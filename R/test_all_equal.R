#' Test all equal
#'
#' Test whether all values in a vector are equal.
#' @param x Vector to test.
#' @param na.rm default: FALSE. exlude NAs from the test.
#' @return Boolean result of the test
#' @family tests
#' @export
test_all_equal <- function(x, na.rm = FALSE) {
    if(!na.rm) {
        return(length(unique(x)) <= 1)
    } else {
        return(length(unique(x[!is.na(x)])) <= 1)
    }
}
