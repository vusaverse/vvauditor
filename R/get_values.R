#' Get values of column
#'
#' A function to determine what kind of values are present in columns.
#' @param df The dataframe
#' @param column Column to get values from.
#' @return The class of the column values
#' @examples
#' get_values(mtcars, "mpg")
#'
#'
#' @export
get_values <- function(df, column) {
    if (inherits(df[[column]], "numeric")) {
        len <- length(unique(df[[column]]))
        ifelse(len > 15,
               return("Numeric values"),
               return(paste(unique(stats::na.omit(df[[column]])), sep = "", collapse = ", ")))
    } else if (inherits(df[[column]], "character")) {
        return(paste(range(df[[column]], na.rm = T), collapse = ", "))
    } else if (inherits(df[[column]], "logical")) {
        return("TRUE, FALSE")
    } else {
        return("NA")
    }
}
