#' Duplicates in column
#'
#' Searches for duplicates in a data frame column.
#'
#' @param df Data frame.
#' @param col Column name.
#' @return Rows containing duplicated values.
#' @family tests
#' @examples
#' duplicates_in_column(mtcars, "mpg")
#'
#' @export
duplicates_in_column <- function(df, col){
    ## Check if object "df" is a data frame.
    if (!is.data.frame(df)) {
        return(warning("df is not a data frame"))
    }
    ## Check if column exists in df.
    if (!col %in% names(df)) {
        return(warning("The specified column does not exist in the data frame."))
    }


    values_col <- df[[col]]

    duplicates <- unique(values_col[duplicated(values_col)])

    duplicates_df <- df[values_col %in% duplicates,]

    return(duplicates_df)

}
