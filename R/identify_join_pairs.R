#' Identify Possible Join Pairs Between Data Frames
#'
#' This function identifies potential join pairs between two data frames based on the overlap
#' between the distinct values in their columns. It returns a data frame showing the possible join pairs.
#'
#' @param ... A list of two data frames.
#' @param cutoff The minimal percentage of overlap between the distinct values in the columns.
#'
#' @return A data frame showing candidate join pairs.
#'
#' @examples
#' df1 <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))
#' df2 <- data.frame(a = c(7, 8, 9), b = c(10, 11, 12), c = c(13, 14, 15))
#' candidate_pairs <- identify_join_pairs(df1, df2, cutoff = 0.20)
#' print(candidate_pairs)
#'
#' @export
identify_join_pairs <- function(..., cutoff = 0.20) {
  A <- o <- score <- NULL
  args <- list(...)

  df <- expand.grid(A = names(as.data.frame(args[1])), o = names(as.data.frame(args[2])))

  check_candidate_score_internal <- function(col1, col2) {
    score <- length(intersect(
      kit::funique(as.data.frame(args[1])[[col1]]),
      kit::funique(as.data.frame(args[2])[[col2]])
    )) /
      length(kit::funique(as.data.frame(args[1])[[col1]]))
    return(score)
  }

  df <- df %>%
    dplyr::mutate(score = purrr::map2(.x = A, .y = o, .f = check_candidate_score_internal)) %>%
    dplyr::filter(score > cutoff) %>%
    dplyr::mutate(score = as.numeric(score)) %>%
    dplyr::arrange(dplyr::desc(score))

  return(df)
}
