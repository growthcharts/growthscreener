#' Screen growth curves according to JGZ guidelines
#'
#' @param ind A tibble with a person attribute. Support for
#' `minihealth::individual` object is deprecated.
#' @inheritParams screen_curves_tgt
#' @return A data frame with the following columns
#' \describe{
#' \item{`Categorie`}{The category of the screening guidelines:
#' `1000 = hgt`, `2000 = wgt`, `3000 = hdc`}
#' \item{`CategorieOmschrijving`}{A string indicating the measure}
#' \item{`Code`}{Integer message code}
#' \item{`CodeOmschrijving`}{A string with the message}
#' \item{`Versie`}{Version of `growthscreener` package}
#' \item{`Leeftijd0`}{First date of the evaluation pair}
#' \item{`Leeftijd1`}{Second date of the evaluation pair}
#' }
#' @examples
#' screen_curves_ind(target)
#' @export
screen_curves_ind <- function(ind,
                              ynames = c("hgt", "wgt", "hdc"),
                              na.omit = TRUE,
                              recalculate_z = FALSE,
                              ...) {

  if (inherits(ind, "individual")) {
    stop("Sorry: Objects of class individual are no longer supported.")
  }

  screen_curves_tgt(tgt = ind,
                    ynames = ynames,
                    na.omit = na.omit,
                    recalculate_z = recalculate_z,
                    ...)
}
