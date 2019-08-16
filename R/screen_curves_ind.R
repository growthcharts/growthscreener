#' Screen growth curves according to JGZ guidelines
#'
#' @param ind An object of class \code{minihealth::individual}
#' @return A data frame with 7 columns.
#' @examples
#' p <- minihealth::donordata_to_individual(dnr = "smocc", id = 10022)
#' screen_curves_ind(p)
#'
#' @export
screen_curves_ind <- function(ind) {

  if (is.null(ind) | !is.individual(ind))
    return(data.frame(
      Categorie = integer(0),
      CategorieOmschrijving = character(0),
      Code = integer(0),
      CodeOmschrijving = character(0),
      Richtlijn = integer(0),
      Leeftijd0 = character(0),
      Leeftijd1 = character(0),
      stringsAsFactors = FALSE)
    )

  # determine screening doms and ages
  da <- calculate_screening_doms(ind)

  # calculate msgcode for each dom0s
  n <- length(da$dom0)
  if (n >= 1L) {
    # bl <- birth_length
    msgcodes <- rep(NA, n)
    for (i in 1L:n) {
      msgcodes[i] <- calculate_advice_hgt(
        sex = ifelse(slot(ind, "sex") == "female", "F", "M"),
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
        y0 = da$y0[i])
    }
  }
  Resultaten <- data.frame(
        Categorie = 1L,
        CategorieOmschrijving = "Lengte",
        Code = as.integer(msgcodes),
        CodeOmschrijving = msg(msgcodes),
        Richtlijn = 101L,
        Leeftijd0 = format(da$dom0, "%Y%m%d"),
        Leeftijd1 = format(da$dom1, "%Y%m%d"),
        stringsAsFactors = FALSE)
  Resultaten
}
