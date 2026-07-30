#' Find message string
#'
#' @param msgcode Integer vector with message codes
#' @param prefix Logical. If `TRUE` (default), prepend the standard
#'   "Het advies volgens de JGZ-richtlijn ... is als volgt: " lead-in
#'   to advice messages (msgcodes 1031-1082, 2031-2076 and 4031-4046).
#' @return A vector of strings with the message code
#' @examples
#' msg(c(31, 41))
#' msg(c(31, 41), prefix = FALSE)
#' @export
msg <- function(msgcode, prefix = TRUE) {
  mess <- growthscreener::messages
  result <- character(length(msgcode))
  if (length(result) == 0) return(result)
  for (i in 1L:length(msgcode)) {
    m <- mess[mess[, "msgcode"] == msgcode[i], "msg"]
    if (length(m) == 0) m <- ""
    if (prefix) {
      richtlijn <- advies_richtlijn(msgcode[i])
      if (!is.na(richtlijn)) {
        m <- paste0("Het advies volgens de JGZ-richtlijn ", richtlijn,
                     " is als volgt: ", m)
      }
    }
    result[i] <- m
  }
  result
}

#' Determine the JGZ-richtlijn name for an advice msgcode
#'
#' @param msgcode Integer message code
#' @return A single string with the richtlijn name, or `NA` if the
#'   msgcode does not correspond to an advice message with a prefix
#' @noRd
advies_richtlijn <- function(msgcode) {
  if (msgcode >= 1031L && msgcode <= 1082L) return("lengtegroei")
  if (msgcode %in% c(2031L, 2042L, 2044L, 2045L, 2046L, 2073L)) return("overgewicht")
  if (msgcode >= 2074L && msgcode <= 2076L) return("ondergewicht")
  if (msgcode >= 4031L && msgcode <= 4046L) return("taalontwikkeling")
  NA_character_
}
