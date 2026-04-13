#' Calculate decimal age from date of birth and date of measurement
#'
#' @param dob Date of birth
#' @param dom Date of measurement
#' @note Internal function. Not to be called directly.
#' @examples
#' growthscreener:::date2age(dob = "20200217", dom = "20210604")
date2age <- function(dob = NA_character_, dom = NA_character_) {

  # check if dom has been input as age
  is_date <- function(x) {
    x <- as.character(x)
    valid_format <- grepl("^\\d{8}$", x)

    parsed <- suppressWarnings(
      as.Date(x, format = "%Y%m%d")
    )

    valid_format & !is.na(parsed)
  }

  if (all(!is_date(dom) & grepl("^[0-9.]+$", dom))) return(as.numeric(dom))

  # missing data
  if (is.na(dob) || all(is.na(dom))) return(rep(NA_real_, length(dom)))

  # gsub to remove dashes and slashes, convert string to Date
  dob <- as.Date(gsub("(-)|(/)", "", dob), "%Y%m%d")
  dom <- as.Date(gsub("(-)|(/)", "", dom), "%Y%m%d")

  return (as.numeric(round((dom - dob)/365.25, 4)))
}
