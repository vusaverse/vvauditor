#' Assert Message Based on Type
#'
#' This function asserts a message based on the type specified.
#' It can either push the message to an AssertCollection, print a warning, or stop execution with an error message.
#'
#' @param message A character string representing the message to be asserted.
#' @param assertion_fail A character string indicating the action to take if the assertion fails. Can be an AssertCollection, "warning", or "stop" (default).
#' @return None
#' @export
assertion_message <- function(message, assertion_fail = "stop") {
  if (inherits(assertion_fail, "AssertCollection")) {
    assertion_fail$push(message)
  } else if (assertion_fail == "warning") {
    warning(message)
  } else if (assertion_fail == "stop") {
    stop(message)
  }
}
