#' Find message string
#'
#' @param msgcode Integer vector with message codes
#' @return A vector of strings with the message code
#' @examples
#' msg(c(31, 41))
#' @export
msg <- function(msgcode) {
  mess <- growthscreener::messages_hgt
  result <- character(length(msgcode))
  if (length(result) == 0) return(return)
  for (i in 1L:length(msgcode)) {
    m <- mess[mess[, "msgcode"] == msgcode[i], "msg"]
    if (length(m) == 0) m <- ""
    result[i] <- m
  }
  result
}
