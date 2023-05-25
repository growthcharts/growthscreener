#' Calculate decimal age from date of birth and date of measurement
#'
#' @param dob Date of birth
#' @param dom Date of measurement
#' @note Internal function. Not to be called directly.
#' @examples
#' growthscreener:::date2age(dob = "17022020", dom = "04062021")
date2age <- function(dob = NA_character_, dom = NA_character_) {

  # missing data
  if (is.na(dob) || all(is.na(dom))) return(rep(NA_real_, length(dom)))

  # gsub to remove dashes and slashes, convert string to Date
  dob <- as.Date(gsub("(-)|(/)", "", dob), "%Y%m%d")
  dom <- as.Date(gsub("(-)|(/)", "", dom), "%Y%m%d")

  return (as.numeric(round((dom - dob)/365.25, 4)))
}