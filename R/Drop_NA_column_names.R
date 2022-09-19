#' Drop NA column names
#'
#' Deletes columns whose name is NA or whose name is empty
#'
#' @param x dataframe
#'
#' @return dataframe without columns that are NA
#' @export
drop_na_column_names <- function(x) {
    return(x[!is.na(names(x)) & !(names(x) == "")])
}