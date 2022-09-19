#' Find pattern in R scripts
#'
#' Function to search for a pattern in R scripts.
#'
#' @param pattern Pattern to search on
#' @param path de folder waar gezocht moet worden, default working directory
#' @param case.sensitive Whether pattern is case sensitve or ntot
#' @param comments whether to search in commented lines
#' @return dataframe met R script paths
#' @export
#' @examples
#' \dontrun{
#' find_pattern_r(pattern = "DUO_", path = "05. Rapporten/")
#' find_pattern_r("DUO_", c("05. Rapporten/", "02. Manipuleren/"))
#' }
find_pattern_r <- function(pattern, path = ".", case.sensitive = TRUE, comments = FALSE) {
  dt <- findR::findRscript(path, pattern = pattern, case.sensitive = case.sensitive, show.results = TRUE, comments = comments)
  dt <- unique(dt[1])
  return(dt)
}
