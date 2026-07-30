#' @import nlreferences
#' @importFrom centile        y2z
#' @importFrom dplyr          %>% .data arrange bind_rows case_when filter
#'                            full_join if_else left_join mutate mutate_at recode
#'                            rename select transmute slice vars
#' @importFrom dplyr          %>% .data arrange bind_rows distinct filter full_join
#'                            left_join mutate mutate_at recode rename select
#'                            transmute slice summarise vars
#' @importFrom stats          approx
#' @importFrom tidyselect     all_of starts_with
#' @importFrom tidyr          fill tibble
#' @importFrom utils          packageVersion
NULL

#' Extract the person data element from a BDS target list
#' @param x A list with elements `psn` and `xyz`
#' @return The `psn` element of `x`
#' @noRd
persondata <- function(x) {
  stopifnot(is.list(x), "psn" %in% names(x))
  x[["psn"]]
}

#' Extract the time data element from a BDS target list
#' @param x A list with elements `psn` and `xyz`
#' @return The `xyz` element of `x`
#' @noRd
timedata <- function(x) {
  stopifnot(is.list(x), "xyz" %in% names(x))
  x[["xyz"]]
}
