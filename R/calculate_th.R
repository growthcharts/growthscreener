#' Calculate target height SDS
#'
#' @param hgtf  Length of biological father (cm)
#' @param hgtm  Length of biological mother (cm)
#' @param sex   Character, either \code{"M"} (male) or \code{"F"} (female)
#' @param etn   Etnicity, one of \code{"N"} (dutch), \code{"T"} (turkish),
#'              \code{"M"} (moroccan) or \code{"H"} (hindustani).
#' @param dec   Integer vector, length 2, indicating rounding for
#'              th and th_z, respectively
#' @return      Numeric, length 2: target height (cm) and target height
#'              standard deviation score (z), relative to populations
#'              living in The Netherlands
#' @author      Stef van Buuren, 2019
#' @examples
#' calculate_th(180, 170, "M", "N")
#' @export
calculate_th <- function(hgtf, hgtm,
                         sex = NULL, etn = NULL,
                         dec = c(1L, 3L)) {

  # don't calculate if we're missing proper sex or etn
  if (is.null(sex) | is.null(etn)) return(c(NA, NA))
  if (!sex %in% c("M", "F")) return(c(NA, NA))
  if (!etn %in% c("N", "T", "M", "H")) return(c(NA, NA))

  if (sex == "M") {
    th <- switch(EXPR = etn,
                 "N" = 44.5 + 0.376 * hgtf + 0.411 * hgtm,
                 "T" = 29.6 + 0.441 * hgtf + 0.465 * hgtm,
                 "M" = 22.4 + 0.439 * hgtf + 0.508 * hgtm,
                 "H" = 43.6 + 0.366 * hgtf + 0.431 * hgtm,
                 NA)
    th_z <- switch(EXPR = etn,
                   "N" = (th - 183.8) / 7.1,
                   "T" = (th - 176.8) / 6.8,
                   "M" = (th - 177.2) / 7.7,
                   "H" = (th - 174.3) / 7.0,
                   NA)
  }

  if (sex == "F") {
    th <- switch(EXPR = etn,
                 "N" = 47.1 + 0.334 * hgtf + 0.364 * hgtm,
                 "T" = 32.8 + 0.389 * hgtf + 0.410 * hgtm,
                 "M" = 32.1 + 0.370 * hgtf + 0.429 * hgtm,
                 "H" = 49.4 + 0.308 * hgtf + 0.364 * hgtm,
                 NA)
    th_z <- switch(EXPR = etn,
                   "N" = (th - 170.7) / 6.3,
                   "T" = (th - 162.6) / 6.0,
                   "M" = (th - 162.8) / 6.5,
                   "H" = (th - 159.6) / 5.9,
                   NA)
  }

  th <- round(th, dec[1])
  th_z <- round(th_z, dec[2])

  c(th, th_z)
}
