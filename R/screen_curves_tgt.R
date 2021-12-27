#' Screen growth curves according to JGZ guidelines
#'
#' @param tgt Object of class `bdsreader::target`.
#' @param ynames Character vector identifying the measures to be
#' screened. By default, `ynames = c("hgt", "wgt", "hdc")`.
#' @param na.omit A logical indicating whether records with a missing
#' `x` (age) or `y` (yname) should be removed. Defaults to
#' `TRUE`.
#' @param recalculate_z A logical indicating whether Z-scores should be
#' recalculated. Currently not functional. Z-scores are always
#' recalculated.
#' @param \dots Not used, but required for extendibility
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
#' screen_curves_tgt(target)
#' @export
screen_curves_tgt <- function(tgt,
                              ynames = c("hgt", "wgt", "hdc"),
                              na.omit = TRUE,
                              recalculate_z = FALSE,
                              ...) {

  if (is.null(tgt) || !inherits(tgt, "target") || !length(ynames))
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
  child <- persondata(tgt)
  time <- timedata(tgt)

  # determine screening doms and ages
  domlist <- calculate_screening_doms(time, ynames, na.omit = na.omit)

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
                   sex = child$sex,
                   bw = child$bw,
                   bl = NA,
                   ga = child$ga,
                   etn = substr(child$etn, 1L, 1L),
                   hgtf = child$hgtf,
                   hgtm = child$hgtm,
                   dom1 = da$dom1,
                   y1 = da$y1,
                   dom0 = da$dom0[i],
                   y0 = da$y0[i]),
                 wgt = calculate_advice_wgt(
                   sex = child$sex,
                   ga = child$ga,
                   dom1 = da$dom1,
                   y1 = da$y1,
                   hgt1 = da$h1,
                   dom0 = da$dom0[i],
                   y0 = da$y0[i],
                   hgt0 = da$h0[i]),
                 hdc = calculate_advice_hdc(
                   sex = child$sex,
                   ga = child$ga,
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
        Leeftijd0 = format(child[["dob"]] + da$dom0, "%Y%m%d"),
        Leeftijd1 = rep(format(child[["dob"]] + da$dom1, "%Y%m%d"), n),
        stringsAsFactors = FALSE)
    }
  }
  bind_rows(result)
}
