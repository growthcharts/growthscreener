#' Find message string
#'
#' @param msgcode Integer with message code
#' @return A string with the message code
#' @examples
#' msg(31)
#' @export
msg <- function(msgcode) {
  mess <- growthscreener::messages_hgt
  mess[mess[, "msgcode"] == msgcode, "msg"]
}
