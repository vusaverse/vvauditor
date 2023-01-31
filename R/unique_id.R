#' unique id
#'
#' Check if parsed variable is a unique identifier.
#' This function was adapted from:
#' Source: https://edwinth.github.io/blog/unique_id/
#' @param x vector or dataframe.
#' @importFrom magrittr %>%
#' @param ... optional variables, e.g. name of column or a vector of names.
#' @return Boolean whether variable is a unique identifier.
#' @examples
#' unique_id(iris, Species)
#'
#' mtcars$name <- rownames(mtcars)
#' unique_id(mtcars, name)
#'
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
