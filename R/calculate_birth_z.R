#' Calculate birth weight SDS relative to Dutch references
#'
#' @param y     Birth weight (grammes) or birth length (cm).
#'              May be a vector. Converted to numeric.
#' @param sex   Character, either \code{"M"} (male) or \code{"F"} (female)
#' @param ga    Gestational age, completed week (Integer or character)
#' @param yname Either \code{"wgt"} (for birth weight) or \code{"hgt"}
#'              (for birth length)
#' @param dec   Number of decimals for rounding
#' @return      Numeric vector of \code{length(bw)} elements with
#'              standard deviation scores relative to Dutch birth
#'              weight references
#' @author      Stef van Buuren, 2019
#' @examples
#' calculate_birth_z(c(2500, 3000), sex = "M", ga = 36)
#' @export
calculate_birth_z <- function(y, sex, ga, yname = "wgt",
                              dec = 3) {
  # convert inputs
  y <- suppressWarnings(as.numeric(y))
  sex <- as.character(sex[1L])
  ga <- suppressWarnings(as.integer(ga[1L]))
  if (is.na(ga)) return(rep(NA, length(y)))

  # find reference
  ref <- NULL
  if (yname == "wgt") {
    ref <- growthscreener::ref.nl5defSGAgewicht
    y <- y / 1000
  }
  if (yname == "hgt") ref <- growthscreener::ref.nl5defSGAlengte
  if (is.null(ref)) return(rep(NA, length(y)))

  # find proper row
  idx <- ref$sex == sex & ref$ga == ga
  if (any(is.na(idx))) return(rep(NA, length(y)))
  if (!any(idx)) return(rep(NA, length(y)))

  # calculate
  z <- round(((y / ref$M[idx])^ref$L[idx] - 1) / (ref$L[idx] * ref$S[idx]), dec)
  z[is.nan(z)] <- NA
  z
}
