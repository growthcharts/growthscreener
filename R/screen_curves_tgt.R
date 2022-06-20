#' Screen growth curves according to JGZ guidelines
#'
#' @param tgt A list with elements `psn` and `xyz`
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

  # if supplied tgt is incorrect return an empty object
  if (is.null(tgt) || !is.list(tgt) || !length(ynames))
    return(data.frame(
      Categorie = integer(0),
      CategorieOmschrijving = character(0),
      Code = integer(0),
      CodeOmschrijving = character(0),
      Versie = integer(0),
      Leeftijd = character(0),
      stringsAsFactors = FALSE)
    )
  child <- persondata(tgt)
  time <- timedata(tgt)

  # prepare output
  result <- vector("list", length(ynames))
  names(result) <- ynames

  # for each screening protocol, calculate msgcode
  for (yname in ynames) {
    msgcode <- NA_integer_
    msgcode <-
      switch(yname,
             hgt = calculate_advice_hgt(
               sex = child$sex,
               bw = child$bw,
               bl = NA,
               ga = child$ga,
               etn = child$etn, #substr(child$etn, 1L, 1L) AHJ: waarom werd dit gedaan?
               hgtf = child$hgtf,
               hgtm = child$hgtm,
               dom = unlist(time[time$yname == "hgt", "age"]),
               y = unlist(time[time$yname == "hgt", "y"])
               ),
             wgt = calculate_advice_wgt(
               sex = child$sex,
               ga = child$ga,
               dom = unlist(time[time$yname == "wgt", "age"]),
               y = unlist(time[time$yname == "wgt", "y"]),
               dom_hgt = unlist(time[time$yname == "hgt", "age"]),
               hgt = unlist(time[time$yname == "hgt", "y"])
               ),
             hdc = calculate_advice_hdc(
               sex = child$sex,
               ga = child$ga,
               dom = unlist(time[time$yname == "hdc", "age"]),
               y = unlist(time[time$yname == "hdc", "y"])
               ),
             default = NA_integer_)

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

    age1 <- unlist(time[time$yname == yname, "age"])
    age1 <- ifelse(!all(is.na(age1)), max(age1, na.rm=T), NA)

    result[[yname]] <- data.frame(
      Categorie = cati,
      CategorieOmschrijving = cato,
      Code = as.integer(msgcode),
      CodeOmschrijving = msg(msgcode),
      Versie = as.character(packageVersion("growthscreener")),
      Leeftijd = age1,
      stringsAsFactors = FALSE)
  }
  bind_rows(result)
}
