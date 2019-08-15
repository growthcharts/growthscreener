#' Pack screener code as JSON string
#'
#' This functions convert the message code to an JSON-string that
#' can be returned to the client. The format of the JSON conforms
#' to file \code{file.path(path.package("growthscreener"), "extdata",
#' "Voorbeeld_antwoordbericht_(2019-08-12).json")}.
#' @param url     The JAMES site URL created at the request of the client
#' @param msgcode Message code, integer
#' @param dom1    Measurement date 1 (current), class \code{Date}
#' @param dom0    Measurement date 0 (previous), class \code{Date}
#' @param yname   Currently only \code{"hgt"}
#' @return A JSON-string
#' @author Stef van Buuren 2019
#' @export
convert_msgcode_json <- function(url, msgcode,
                                 dom1 = ymd(), dom0 = ymd(),
                                 yname = "hgt") {
  catnum <- ifelse(yname == "hgt", 1, NA_integer_)
  catoms <- ifelse(yname == "hgt", "Lengte", NA_character_)
  msgc <- as.integer(msgcode)
  mess <- msg(msgcode)
  richtlijn <- ifelse(yname == "hgt", 101, NA_integer_)
  d0 <- ifelse (is.Date(dom0), format(dom0, "%Y%m%d"), character(0))
  d1 <- ifelse (is.Date(dom1), format(dom1, "%Y%m%d"), character(0))

  answer <- list(
    UrlGroeicurven = url,
    Resultaten =
      data.frame(
        Categorie = catnum,
        CategorieOmschrijving = catoms,
        Code = msgc,
        CodeOmschrijving = mess,
        Richtlijn = richtlijn,
        Leeftijd0 = d0,
        Leeftijd1 = d1,
        stringsAsFactors = FALSE)
      )
  toJSON(answer)
}
