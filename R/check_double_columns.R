#'check double columns
#'
#'Check whether two dataframes have intersecting column names.
#'@param x Data frame x.
#'@param y Data frame y.
#'@param connector The connector columns as strings. Also possible as vector.
#'@family tests
#'@export
check_double_columns <- function(x, y, connector = NULL) {
    Differences <- intersect(names(x), names(y))

    Differences_without_connector <- setdiff(Differences, connector)

    Differences_count <- length(Differences_without_connector)

    if (Differences_count > 0) {
        print(Differences_without_connector)
        stop("Overlap in column names")
    }
}
