#' Referral advice for head circumference
#'
#' This function traverses a decision tree for head circumference for children
#' below the age of 1.
#'
#' The decision tree always assesses the paired measurements of \code{y1} and
#' \code{y0}. The last observations (\code{y1}) is taken as the last
#' measurement, whereas \code{y0} can be one of the previous measurements.
#' @param sex   Character, either \code{"male"} or \code{"female"}
#' @param dob   Date of birth (class Date)
#' @param ga    Gestational age, completed weeks (Integer or character)
#' @param etn   Etnicity, one of \code{"NL"} (dutch), \code{"TU"} (turkish) or
#'   \code{"MA"} (moroccan).
#' @param dom1  Date of last measurement (Date)
#' @param y1    Head circumference at last measurement (cm)
#' @param dom0  Date of previous measurement (Date)
#' @param y0    Head circumference at previous measurement (cm)
#' @param d     Optional, list of derived variables, obtained by
#'   \code{calculate_helpers()}
#' @return \code{calculate_advice_hgt} returns an integer, the \code{msgcode}
#' @author Paula van Dommelen, Stef van Buuren, 2019
#' @seealso calculate_helpers
#' @rdname advice_hdc
#' @examples
#' msg(calculate_advice_hdc())
#' msgcode <- calculate_advice_hdc(sex = "female", etn = "NL",
#'                                 dob = as.Date("2015-09-01"),
#'                                 dom1 = as.Date("2016-05-01"), y1 = 40,
#'                                 dom0 = as.Date("2015-12-01"), y0 = 36)
#' msg(msgcode)
#' @export
calculate_advice_hdc <- function(sex = NA_character_, dob = as.Date(NA),
                                 ga = NA, etn = NA_character_,
                                 dom1 = as.Date(NA), y1 = NA,
                                 dom0 = as.Date(NA), y0 = NA,
                                 d = NULL) {

  if (is.null(d))
    d <- calculate_helpers(y = "hdc", sex = sex, dob = dob, ga = ga, etn = etn,
                           dom1 = dom1, y1 = y1, dom0 = dom0, y0 = y0)

  age1 <- d$age1
  age0 <- d$age0
  z1   <- d$z1
  z0   <- d$z0

  # start the sieve

  # return early if data are insufficient
  # if (is.na(sex)) return(19)
  if (!sex %in% c("male", "female")) return(219)
  if (is.na(dob)) return(216)
  if (is.na(dom1) | is.na(dom0)) return(215)
  if (is.na(y1) | is.na(y0)) return(ifelse(age1 < 1.0, 218, 221))
  if (!etn %in% c("NL", "TU", "MA")) return(220)

  # outside age range
  if (age1 >= 1.0) return(221)

  # check for gain z1 - z0
    # rapid increase (within 0.5 yrs)
    if(age1 - age0 <= 0.5){
      if((z1 - z0) > 0.5) return(241)
    }

    # shift of less than -0.5
    if((z1 - z0) < -0.5) return(242)

  # signal everthing is OK
  return(231)
}
