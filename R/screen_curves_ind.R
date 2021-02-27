#' Screen growth curves according to JGZ guidelines
#'
#' @param ind An object of class \code{minihealth::individual}
#' @param ynames Character vector identifying the measures to be
#' screened. By default, \code{ynames = c("hgt", "wgt", "hdc")}.
#' @param na.omit A logical indicating whether records with a missing
#' \code{x} (age) or \code{y} (yname) should be removed. Defaults to
#' \code{TRUE}.
#' @param recalculate_z A logical indicating whether Z-scores should be
#' recalculated. Currently not functional. Z-scores are always
#' recalculated.
#' @return A data frame with the following columns
#' \describe{
#' \item{\code{Categorie}}{The category of the screening guidelines:
#' \code{1000 = hgt}, \code{2000 = wgt}, \code{3000 = hdc}}
#' \item{\code{CategorieOmschrijving}}{A string indicating the measure}
#' \item{\code{Code}}{Integer message code}
#' \item{\code{CodeOmschrijving}}{A string with the message}
#' \item{\code{Versie}}{Version of \code{growthscreener} package}
#' \item{\code{Leeftijd0}}{First date of the evaluation pair}
#' \item{\code{Leeftijd1}}{Second date of the evaluation pair}
#' }
#' @examples
#' library(clopus)
#' ind <- minihealth::donordata_to_individual(dnr = "smocc", id = 10022)
#' screen_curves_ind(ind)
#' @export
screen_curves_ind <- function(ind,
                              ynames = c("hgt", "wgt", "hdc"),
                              na.omit = TRUE,
                              recalculate_z = FALSE) {

  if (is.null(ind) | !is.individual(ind) | !length(ynames))
    return(data.frame(
      Categorie = integer(0),
      CategorieOmschrijving = character(0),
      Code = integer(0),
      CodeOmschrijving = character(0),
      Versie = integer(0),
      Leeftijd0 = character(0),
      Leeftijd1 = character(0),
      stringsAsFactors = FALSE)
    )

  # determine screening doms and ages
  domlist <- calculate_screening_doms(ind, ynames, na.omit = na.omit)

  # prepare output
  result <- vector("list", length(ynames))
  names(result) <- ynames

  # for each screening protocol, calculate all msgcode's for all dom0's
  for (yname in ynames) {
    da <- domlist[[yname]]
    n <- length(da$dom0)
    if (n >= 1L) {
      msgcodes <- rep(NA, n)
      for (i in 1L:n) {
        msgcodes[i] <-
          switch(yname,
                 hgt = calculate_advice_hgt(
                   sex = slot(ind, "sex"),
                   dob = get_dob(ind),
                   bw = slot(ind, "bw"),
                   bl = NA,
                   ga = slot(ind, "ga"),
                   etn = substr(slot(ind, "etn"), 1L, 1L),
                   hgtf = slot(ind, "hgtf"),
                   hgtm = slot(ind, "hgtm"),
                   dom1 = da$dom1,
                   y1 = da$y1,
                   dom0 = da$dom0[i],
                   y0 = da$y0[i]),
                 wgt = calculate_advice_wgt(
                   sex = slot(ind, "sex"),
                   dob = get_dob(ind),
                   ga = slot(ind, "ga"),
                   dom1 = da$dom1,
                   y1 = da$y1,
                   hgt1 = da$h1,
                   dom0 = da$dom0[i],
                   y0 = da$y0[i],
                   hgt0 = da$h0[i]),
                 hdc = calculate_advice_hdc(
                   sex = slot(ind, "sex"),
                   dob = get_dob(ind),
                   ga = slot(ind, "ga"),
                   dom1 = da$dom1,
                   y1 = da$y1,
                   dom0 = da$dom0[i],
                   y0 = da$y0[i]),
                 default = NA_integer_)
      }
      cati <- switch(yname,
                     hgt = 1000L,
                     wgt = 2000L,
                     hdc = 3000L,
                     NA_integer_)
      cato <- switch(yname,
                     hgt = "Lengte",
                     wgt = "Gewicht",
                     hdc = "Hoofdomtrek",
                     "Onbekend")

      result[[yname]] <- data.frame(
        Categorie = rep(cati, n),
        CategorieOmschrijving = rep(cato, n),
        Code = as.integer(msgcodes),
        CodeOmschrijving = msg(msgcodes),
        Versie = rep(as.character(packageVersion("growthscreener")), n),
        Leeftijd0 = format(da$dom0, "%Y%m%d"),
        Leeftijd1 = rep(format(da$dom1, "%Y%m%d"), n),
        stringsAsFactors = FALSE)
    }
  }
  bind_rows(result)
}
