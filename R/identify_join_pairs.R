#' Identify Possible Join Pairs Between Data Frames
#'
#' This function identifies potential join pairs between two data frames based on the overlap
#' between the distinct values in their columns. It returns a data frame showing the possible join pairs.
#'
#' @param ... A list of two data frames.
#' @param similarity_cutoff The minimal percentage of overlap between the distinct values in the columns.
#'
#' @return A data frame showing candidate join pairs.
#'
#' @examples
#' identify_join_pairs(iris, iris3)
#'
#' @export
identify_join_pairs <- function(..., similarity_cutoff = 0.20) {
  data_frame1_column <- data_frame2_column <- score <- NULL
  data_frames <- list(...)

  df_pairs <- expand.grid(
    data_frame1_column = names(as.data.frame(data_frames[[1]])),
    data_frame2_column = names(as.data.frame(data_frames[[2]]))
  )

  calculate_similarity_score <- function(column1, column2) {
    unique_values_df1 <- stats::na.omit(kit::funique(as.data.frame(data_frames[[1]])[[column1]]))
    unique_values_df2 <- stats::na.omit(kit::funique(as.data.frame(data_frames[[2]])[[column2]]))
    similarity_score <- length(intersect(unique_values_df1, unique_values_df2)) / length(unique_values_df1)
    return(similarity_score)
  }

  df_pairs <- df_pairs %>%
    dplyr::mutate(score = purrr::map2_dbl(.x = data_frame1_column, .y = data_frame2_column, .f = calculate_similarity_score)) %>%
    dplyr::filter(score > similarity_cutoff) %>%
    dplyr::arrange(dplyr::desc(score))

  return(df_pairs)
}
