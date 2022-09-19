#' unique id
#'
#' Check if parsed variable is a unique identifier.
#' This function was adapted from:
#' Source: https://edwinth.github.io/blog/unique_id/
#' @param x vector or dataframe.
#' @param ... optional variables, e.g. name of column or a vector of names.
#' @export
unique_id <- function(x, ...) {
    id_set <- x %>% dplyr::select(...)

    id_set_dist <- id_set %>% dplyr::distinct()

    print(id_set_dist)

    if (nrow(id_set) == nrow(id_set_dist)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
