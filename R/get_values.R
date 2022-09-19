#' Get values of column
#'
#' A function to determine what kind of values are present in columns.
#' @param analyseset The analysisset
#' @param column Column to get values from.
#'
#' @export
get_values <- function(analyseset, column) {
    if (inherits(analyseset[[column]], "numeric")) {
        len <- length(unique(analyseset[[column]]))
        ifelse(len > 15,
               return("Numerieke waarde"),
               return(paste(unique(stats::na.omit(analyseset[[column]])), sep = "", collapse = ", ")))
    } else if (inherits(analyseset[[column]], "character")) {
        return(paste(range(analyseset[[column]], na.rm = T), collapse = ", "))
    } else if (inherits(analyseset[[column]], "logical")) {
        return("TRUE, FALSE")
    } else {
        return("NA")
    }
}
