#' Find pattern in R scripts
#'
#' Function to search for a pattern in R scripts.
#'
#' @param pattern Pattern to search
#' @param path Directory to search in
#' @param case.sensitive Whether pattern is case sensitive or not
#' @param comments whether to search in commented lines
#' @return Dataframe containing R script paths
#' @export
find_pattern_r <- function(pattern, path = ".", case.sensitive = TRUE, comments = FALSE) {
  dt <- findR::findRscript(path, pattern = pattern, case.sensitive = case.sensitive, show.results = TRUE, comments = comments)
  dt <- unique(dt[1])
  return(dt)
}
