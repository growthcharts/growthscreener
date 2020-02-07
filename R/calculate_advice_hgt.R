#' Referral advice for body height
#'
#' This function traverses the decision tree of the
#' "JGZ-Richtlijn Lengtegroei 2019".
#'
#' The decision tree assesses both single and paired measurements.
#' The last observations (\code{y1}) is generally taken as the
#' last measurement, whereas \code{y0} can be one of the previous
#' measurements. For more than two measurements, there are many
#' pairs possible, and these pairs need not be consecutive.
#' The \code{y0} measurement needs to be defined by the user,
#' and is informally taken as an earlier measurement that maximumizes
#' the referal probability. On the other hand, defining pairs that are
#' remote in ages (e.g. period between 1 month and 14 years) is probably
#' not that useful. In practice, we may be interested in setting the
#' maximum period to, say, five years.
#' @param sex   Character, either \code{"male"} or \code{"female"}
#' @param dob   Date of birth (class Date)
#' @param bw    Birth weight (grammes)
#' @param bl    Birth length (cm)
#' @param ga    Gestational age, completed weeks (Integer or character)
#' @param etn   Etnicity, one of \code{"NL"} (dutch), \code{"TU"} (turkish),
#'              \code{"MA"} (moroccan) or \code{"HS"} (hindustani).
#' @param hgtf  Height of father (cm)
#' @param hgtm  Height of mother (cm)
#' @param dom1  Date of last measurement (Date)
#' @param y1    Height at last measurement (cm)
#' @param dom0  Date of previous measurement (Date)
#' @param y0    Height at previous measurement (cm)
#' @param d     Optional, list of derived variables, obtained by
#'              \code{calculate_helpers()}
#' @return \code{calculate_advice_hgt} returns an integer, the \code{msgcode}
#' @author Paula van Dommelen, Stef van Buuren, 2019
#' @seealso calculate_helpers
#' @rdname advice_hgt
#' @examples
#' msg(calculate_advice_hgt())
#' msgcode <- calculate_advice_hgt(sex = "male", etn = "NL",
#'                                 dob = as.Date("2018-07-31"),
#'                                 dom1 = as.Date("2018-12-12"), y1 = 64)
#' msg(msgcode)
#' @export
calculate_advice_hgt <- function(sex = NA_character_, dob = as.Date(NA),
                                 bw = NA, bl = NA, ga = NA,
                                 etn = NA_character_, hgtf = NA, hgtm = NA,
                                 dom1 = as.Date(NA), y1 = NA,
                                 dom0 = as.Date(NA), y0 = NA,
                                 d = NULL) {

  if (is.null(d))
    d <- calculate_helpers(y = "hgt", sex, dob, bw, bl, ga, etn,
                               hgtf, hgtm, dom1, y1, dom0, y0)
  bw_z <- d$bw_z
  bl_z <- d$bl_z
  th_z <- d$th_z
  age1 <- d$age1
  age0 <- d$age0
  z1   <- d$z1
  z0   <- d$z0

  # start the sieve

  # return early if data are insufficient
  # if (is.na(sex)) return(19)
  if (!sex %in% c("male", "female")) return(19)
  if (is.na(dob)) return(16)
  if (is.na(dom1)) return(15)
  if (is.na(y1)) return(ifelse(age1 < 18.0, 18, 21))
  if (!etn %in% c("NL", "TU", "MA", "HS")) return(20)

  # outside age range
  if (age1 >= 18.0) return(21)
  if (age1 < 0.0833) return(22)

  # check single measurement
  if (age1 < 3.0) {
    # short
    if (z1 < -2.5 & is.na(bw)) return(13)
    if (z1 < -3.0 & bw >= 2500) return(45)

    # tall
    if (z1 > 3.0) return(48)
    if (z1 > 2.5 & is.na(z0)) return(11)
    if (z1 > 1.0) return(77)
  }

  if (age1 >= 3.0 & age1 < 10.0) {

    # short
    if (z1 < -2.5) return(44)
    if (z1 < -2.0 & !is.na(bw_z)) if (bw_z < -2.0) return(42)
    if (z1 < -2.0 & !is.na(bl_z)) if (bl_z < -2.0) return(41)
    if (!is.na(th_z)) {
      if (z1 <  -2.0 & (z1 - th_z) < -1.6) return(43)
      if (z1 >= -2.0 & z1 < -1.0 & (z1 - th_z) < -2.0) return(53)
    }
    if (z1 < -2.0 & is.na(bw)) return(13)
    if (z1 < -2.0 & is.na(bl)) return(12)
    if (z1 < -1.0 & is.na(th_z)) return(14)

    # tall
    if (z1 > 2.5) return(47)
    if (!is.na(th_z)) if (z1 > 2.0 & (z1 - th_z) > 2.0) return(46)
    if (z1 > 2.0 & is.na(th_z)) return(82)
    if (z1 > 1.0 &
        ((age1 < 8.0 & sex == "female") | (age1 < 9.0 & sex == "male"))) return(79)
    if (z1 > 1.0) return(81)

    if (is.na(z0)) return(11)
  }

  if (age1 >= 10.0 & age1 < 18.0) {
    # short
    if (z1 < -2.5) return(44)

    # tall
    if (z1 > 2.5) return(47)
    if (z1 > 2.0 & sex == "female" & y1 >= 170) return(71)
    if (z1 > 2.0 & sex == "male" & y1 >= 185) return(72)
    if (z1 > 2.0) return(73)
    if (sex == "female" & y1 >= 170.0) return(74)
    if (sex == "male" & y1 >= 185.0) return(75)
  }

  # check for gain z1 - z0
  if (!is.na(z0)) {
    if (age1 < 3.0) {
      # short
      if (z1 < -2.5 & z0 < -2.5 & is.na(bw)) return(13)
      if (z1 < -2.5 & z0 < -2.5 & bw >= 2500) return(49)

      # tall
      if (z1 > 2.5 & z0 > 2.5) return(50)
    }

    if (age1 >= 3.0 & age1 < 10.0) {
      # short
      if ((z1 - z0) < -2.0) return(55)
      if (!is.na(th_z))
        if (z1 >= -2.0 & (z1 - z0) < -1.0 & (z1 - th_z) < -1.0) return(76)

      # tall
      if ((z1 - z0) > 2.0) return(54)
    }
  }

  # signal everthing is OK
  return(31)
}
