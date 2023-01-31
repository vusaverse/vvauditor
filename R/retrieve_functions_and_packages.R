#' Retrieve functions and packages
#'
#' Retrieves functions and their corresponding packages used in a given script.
#'
#' @param path The complete path of the script.
#' @return Used_functions
#'
#' @export
retrieve_functions_and_packages <- function(path){

    tryCatch (tmp <- utils::getParseData(parse(path, keep.source=TRUE)),
              error = function(e) message(as.character(e)))

    ## Retrieve all functions in the script.
    nms <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")]

    if (length(nms) > 0) {
        df_tmp <- data.frame(nms = nms, filename = path)
        funs <- unique(sort(as.character(df_tmp$nms)))
        src <- paste(as.vector(sapply(funs, utils::find)))
        Used_functions <- data.frame(nms = funs, src = src, filename = path)
        return(Used_functions)
    }
}
