#' check double columns
#'
#' Check whether two dataframes have intersecting column names.
#' @param x Data frame x.
#' @param y Data frame y.
#' @param connector The connector columns as strings. Also possible as vector.
#' @return Message informing about overlap in columns between the dataframes.
#' @examples
#' check_double_columns(mtcars, iris)
#'
#' @family tests
#' @export
check_double_columns <- function(x, y, connector = NULL) {
    Differences <- intersect(names(x), names(y))

    Differences_without_connector <- setdiff(Differences, connector)

    Differences_count <- length(Differences_without_connector)

    if (Differences_count > 0) {
        message(paste(Differences_without_connector, collapse = "\n"))
        stop("Overlap in column names")
    } else {
      message("There are no overlapping columns between the dataframes")
    }
}
