#' List the available screeners for JGZ guidelines
#'
#' @inheritParams screen_curves_tgt
#' @return A data frame with the following columns
#' \describe{
#' \item{`Versie`}{Version of `growthscreener` package}
#' \item{`yname`}{JAMES name of outcome measure}
#' \item{`Categorie`}{The category of the screening guidelines, 1000/2000/3000}
#' \item{`CategorieOmschrijving`}{A string indicating the measure}
#' \item{`JGZRichtlijn`}{Name of JGZ Richtlijn}
#' \item{`Code`}{Integer message code}
#' \item{`CodeOmschrijving`}{A string with the message}
#' }
#' @export
list_screeners <- function(ynames = c("hgt", "wgt", "hdc"), ...) {

  mess <- growthscreener::messages
  mess$Categorie <- 1000 * trunc(mess[["msgcode"]] / 1000)
  mess$yname <- recode(mess$Categorie, `1000` = "hgt", `2000` = "wgt", `3000` = "hdc")
  mess$CategorieOmschrijving <- recode(mess[["yname"]],
                                       `hgt` = "Lengte naar leeftijd",
                                       `wgt` = "Gewicht/BMI naar leeftijd",
                                       `hdc` = "Hoofdomtrek naar leeftijd")
  mess$Code <- mess$msgcode
  mess$CodeOmschrijving <- mess$msg
  mess$Versie <- as.character(packageVersion("growthscreener"))
  mess$JGZRichtlijn <- recode(mess[["yname"]],
                              `hgt` = "JGZ-Richtlijn Lengtegroei 2019",
                              `wgt` = "JGZ-Richtlijn overgewicht 2012 en JGZ-Richtlijn Ondergewicht 2019",
                              `hdc` = "Informele richtlijn")

  result <- mess %>%
    filter(.data$yname %in% ynames) %>%
    select(all_of(c("Versie", "yname",
                    "Categorie", "CategorieOmschrijving", "JGZRichtlijn",
                    "Code", "CodeOmschrijving")))
  return(result)
}
