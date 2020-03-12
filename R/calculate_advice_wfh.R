#' Referral advice for weight for height
#'
#' This function traverses the decision tree of the "JGZ-Richtlijn overgewicht
#' 2012" and "JGZ-Richtlijn Ondergewicht 2019"
#'
#' The decision tree assesses both single and paired measurements.
#' The last observations (\code{y1}) is generally taken as the
#' last measurement, whereas \code{y0} can be one of the previous
#' measurements. For more than two measurements, there are many
#' pairs possible, and these pairs need not be consecutive.
#' The \code{y0} measurement needs to be defined by the user,
#' and is informally taken as an earlier measurement that maximumizes
#' the referal probability.
#'
#' @param sex     Character, either \code{"male"} or \code{"female"}
#' @param dob     Date of birth (class Date)
#' @param ga      Gestational age, completed weeks (Integer or character)
#' @param dom1    Date of last measurement (Date)
#' @param y1      Weight at last measurement (kg)
#' @param dom0    Date of previous measurement (Date)
#' @param y0      Weight at previous measurement (kg)
#' @param d       Optional, list of derived variables, obtained by
#'   \code{calculate_helpers()}
#' @param hgt1    Height at last measurement (cm)
#' @param hgt0    Height at previous measurement (cm)
#' @return \code{calculate_advice_hgt} returns an integer, the \code{msgcode}
#' @author Arjan Huizing, Stef van Buuren 2020
#' @seealso calculate_helpers
#' @rdname advice_wfh
#' @examples
#' msg(calculate_advice_wfh())
#' msgcode <- calculate_advice_wfh(sex = "male",
#'                                 dob = as.Date("2015-01-01"),
#'                                 dom1 = as.Date("2018-12-12"),
#'                                 y1 = 25, hgt1 = 120)
#' msg(msgcode)
#' @export
calculate_advice_wfh <- function(sex = NA_character_, dob = as.Date(NA),
                                 ga = NA, dom0 = as.Date(NA),
                                 y0 = NA, dom1 = as.Date(NA), y1 = NA,
                                 hgt1 = NA, hgt0 = NA,
                                 d = NULL){

  if (is.null(d)) {
    d <- calculate_helpers(yname = "wfh", lib = "preterm", sex = sex, dob = dob,
                           ga = ga, dom1 = dom1, y1 = log1p(y1), hgt1 = hgt1,
                           dom0 = dom0, y0 = log1p(y0), hgt0 = hgt0)
  }

  age0 <- d$age0
  z0   <- d$z0
  age1 <- d$age1
  z1   <- d$z1

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(2019)
  if (is.na(dob)) return(2016)
  if (is.na(dom1)) return(2015)
  if (is.na(hgt1)) return(2014)
  if (is.na(y1)) return(ifelse(age1 < 19.0, 2018, 2021))

  # outside age/hgt range
  if (age1 >= 19.0) return(2021)
  if (age1 < 2.0 & hgt1 < 35) return(2022)
  if (age1 < 2.0 & hgt1 > 120) return(2023)

  if (age1 < 1.0) {
    # fast increase
    if (is.na(y0)) return(2012)
    if (is.na(hgt0)) return(2013)
    if ((z1 - z0) > 0.67) return(2071)

    # fast decrease
    if ((z1 - z0) < -0.67) return(2072)
  }

  if (age1 >= 1.0 & age1 < 2.0) {
    # high weight
    if (z1 > 2.0) return(2042)
    if (z1 > 1.0) return(2073)

    # low weight
    if (z1 < -2.0) return(2074)

    # decreasing
    if (is.na(y0)) return(2012)
    if (is.na(hgt0)) return(2013)
    if ((z1 - z0) < -1.0) return(2075)
  }

  if (age1 >= 2.0) {
    # high weight
    bmi <- y1/(hgt1/100)^2
    bmi_table <- growthscreener::bmi_table
    bmi_table <- bmi_table[bmi_table$sex == sex, ]
    cutoff_obesity <- approx(x = bmi_table$age,
                             y = bmi_table$obesity.I,
                             xout = age1)$y
    cutoff_overweight <- approx(x = bmi_table$age,
                                y = bmi_table$overweight,
                                xout = age1)$y
    if (bmi > cutoff_obesity) return(2044)
    if (bmi > cutoff_overweight) {
      if (age1 < 5.0) return(2045)
      return(2046)
    }

    # low weight
    if (z1 < -2.0) return(2074)
    # decreasing weight (less than -1 SD)
    if (is.na(y0)) return(2012)
    if (is.na(hgt0)) return(2013)
    if ((z1 - z0) < -1.0) return(2075)
  }

  # signal everthing is OK
  return(2031)
}
