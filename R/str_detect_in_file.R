#' Detect string in file
#'
#' @param file Path to file.
#' @param pattern Pattern to match.
#' @param only_comments default FALSE. Whether to only search in commented lines.
#' @param collapse default: FALSE: search file line by line.
#' If true, then pattern is search in the entire file at once after collapsing.
#'  (only_comments does not work when collapse is set to TRUE)
#' @return Boolean whether pattern exists in file.
#' @export
str_detect_in_file <- function(file, pattern, only_comments = FALSE, collapse = FALSE) {

    Lines <- readLines(file, warn = FALSE)

    if (!collapse) {
        if (only_comments) {
            Search_line <- stringr::str_detect(Lines, "^\\s*#")
        } else {
            Search_line <- T
        }

        Result <- any(stringr::str_detect(Lines[Search_line], pattern = pattern))

    } else {
        Lines_collapsed <- paste(c(Lines, ""), collapse = "\n")


        Result <- stringr::str_detect(Lines_collapsed, pattern = pattern)
    }

    if (is.na(Result)) {
        Result <- FALSE
    }

    return(Result)
}
