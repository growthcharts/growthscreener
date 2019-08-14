#' Fold long string across multiple lines
#'
#' @param s string
#' @seealso https://stackoverflow.com/questions/2351744/insert-line-breaks-in-long-string-word-wrap
#' @export
fold <- function(s) {
  gsub('(.{1,80})(\\s|$)', '\\1\n', s)
}