#' Generate regular expression of a time.
#'
#' This function generates a regular expression for time based on the input format.
#'
#' @param format The format of the time. Possible values are:
#' \itemize{
#'   \item{"hh:mm"}: to generate "09:05".
#'   \item{"h:m"}: to generate "9:5".
#'   \item{"hh:mm:ss"}: to generate "09:05:00".
#'   \item{"h:m:s"}: to generate "9:5:0".
#'   \item{"hh:mm:ss AM/PM"}: to generate "09:05:00 AM".
#'   \item{"h:m:s AM/PM"}: to generate "9:5:0 AM".
#' }
#'
#' @return A regular expression.
#'
#' @examples
#' regex_time("hh:mm")
#' regex_time("h:m")
#' regex_time("hh:mm:ss")
#' regex_time("h:m:s")
#' regex_time("hh:mm:ss AM/PM")
#' regex_time("h:m:s AM/PM")
#'
#' @export
regex_time <- function(format = "hh:mm") {
  formats <- c("hh:mm" = "^[0-2][0-9]:[0-5][0-9]$",
               "h:m" = "^([1-2])?[0-9]:([1-5])?[0-9]$",
               "hh:mm:ss" = "^([0-1]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$",
               "h:m:s" = "^([0-9]|0[0-9]|1[0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])$",
               "hh:mm:ss AM/PM" = "^(1[0-2]|0?[1-9]):([0-5][0-9]) ?([AaPp][Mm])$",
               "h:m:s AM/PM" = "^([0-9]|0[0-9]|1[0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9]) ?([AaPp][Mm])$"
               # Add more formats here if needed
  )
  return(formats[format])
}
