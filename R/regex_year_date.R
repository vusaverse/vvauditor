#' Generate regular expression of a year date.
#'
#' This function generates a regular expression for year date based on the input format.
#'
#' @param format The format of the year date. Possible values are:
#' \itemize{
#'   \item{"yyyy"}: to generate "2023".
#'   \item{"yyyy-MM-dd"}: to generate "2023-09-29".
#'   \item{"yyyy/MM/dd"}: to generate "2023/09/29".
#'   \item{"yyyy.MM.dd"}: to generate "2023.09.29".
#'   \item{"yyyy-M-d"}: to generate "2023-9-29".
#'   \item{"yyyy/M/d"}: to generate "2023/9/29".
#'   \item{"yyyy.M.d"}: to generate "2023.9.29".
#'   \item{"yyyy-MM-dd HH:mm:ss"}: to generate "2023-09-29 12:34:56".
#'   \item{"yyyy/MM/dd HH:mm:ss"}: to generate "2023/09/29 12:34:56".
#'   \item{"yyyy-MM-dd HH:mm"}: to generate "2023-09-29 12:34".
#'   \item{"yyyy/MM/dd HH:mm"}: to generate "2023/09/29 12:34".
#' }
#'
#' @return A regular expression.
#'
#' @examples
#' regex_year_date("yyyy")
#' regex_year_date("yyyy-MM-dd")
#' regex_year_date("yyyy/MM/dd")
#' regex_year_date("yyyy.MM.dd")
#' regex_year_date("yyyy-M-d")
#' regex_year_date("yyyy/M/d")
#' regex_year_date("yyyy.M.d")
#' regex_year_date("yyyy-MM-dd HH:mm:ss")
#' regex_year_date("yyyy/MM/dd HH:mm:ss")
#' regex_year_date("yyyy-MM-dd HH:mm")
#' regex_year_date("yyyy/MM/dd HH:mm")
#'
#' @export
regex_year_date <- function(format = "yyyy") {
  formats <- c("yyyy" = "^\\d{4}$",
               "yyyy-MM-dd" = "^\\d{4}-\\d{2}-\\d{2}$",
               "yyyy/MM/dd" = "^\\d{4}/\\d{2}/\\d{2}$",
               "yyyy.MM.dd" = "^\\d{4}\\.\\d{2}\\.\\d{2}$",
               "yyyy-M-d" = "^\\d{4}-\\d{1,2}-\\d{1,2}$",
               "yyyy/M/d" = "^\\d{4}/\\d{1,2}/\\d{1,2}$",
               "yyyy.M.d" = "^\\d{4}\\.\\d{1,2}\\.\\d{1,2}$",
               "yyyy-MM-dd HH:mm:ss" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$",
               "yyyy/MM/dd HH:mm:ss" = "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}$",
               "yyyy-MM-dd HH:mm" = "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}$",
               "yyyy/MM/dd HH:mm" = "^\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}$"
  )
  return(formats[format])
}


