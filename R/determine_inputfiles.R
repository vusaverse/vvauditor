#' Determine input files
#'
#' Determine which files are read in.
#'
#' @param path Het complete path van het script
#'
#' @export
determine_inputfiles <- function(path) {
    testString <- "readrds_csv"

    if (TRUE %in% base::grepl(testString,
                              base::readLines(path, warn = FALSE))) {
        determine_data_files(path, testString)
    }
}
