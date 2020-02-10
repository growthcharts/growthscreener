#' Referral advice for body weight
#'
#' This function traverses the decision tree of the "JGZ-Richtlijn overgewicht
#' 2012".
#'
#' The decision tree assesses body weight \code{y1} against height \code{hgt1}.
#' This is done either directly for children below the age of 2, or by
#' automatically calculating BMI and comparing that against cut-off points
#' defined in Cole (2000).
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
#' @param useBmi  Always use bmi cut-off points at ages over 2 (boolean)
#' @return \code{calculate_advice_hgt} returns an integer, the \code{msgcode}
#' @author Paula van Dommelen, Stef van Buuren, 2019
#' @seealso calculate_helpers
#' @rdname advice_wgt
#' @examples
#' msg(calculate_advice_wgt())
#' msgcode <- calculate_advice_wgt(sex = "male",
#'                                 dob = as.Date("2015-01-01"),
#'                                 dom1 = as.Date("2018-12-12"),
#'                                 y1 = 25, hgt1 = 120)
#' msg(msgcode)
#' @export
calculate_advice_wgt <- function(sex = NA_character_, dob = as.Date(NA),
                                 ga = NA, dom0 = as.Date(NA), y0 = NA,
                                 dom1 = as.Date(NA), y1 = NA,
                                 hgt1 = NA, hgt0 = NA,
                                 d = NULL, useBmi = TRUE){

  if(is.null(d))
    d <- calculate_helpers(y = "wfh", sex = sex, dob = dob, ga = ga,
                           dom1 = dom1, y1 = y1, hgt1 = hgt1,
                           dom0 = dom0, y0 = y0, hgt0 = hgt0)
  age0 <- d$age0
  z0   <- d$z0
  age1 <- d$age1
  z1   <- d$z1

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(119)
  if (is.na(dob)) return(116)
  if (is.na(dom1)) return(115)
  if (is.na(hgt1)) return(114)
  if (is.na(y1)) return(ifelse(age1 < 19.0, 118, 121))

  # outside age/hgt range
  if (age1 >= 19.0) return(121)
  if ((age1 < 2.0 | !useBmi) & (hgt1 < 35)) return(122)
  if ((age1 < 2.0 | !useBmi) & (hgt1 > 120)) return(123)

  # if directly using bmi
  if(useBmi & age1 >= 2.0){
    bmi <- y1/(hgt1/100)^2
    bmi_table <- growthscreener::bmi_table
    if(bmi > bmi_table[bmi_table$sex == sex &
                       bmi_table$age == floor(age1),
                       "ob"]) return(144)
    if(bmi > bmi_table[bmi_table$sex == sex &
                       bmi_table$age == floor(age1),
                       "ow"]){
      if(age1 < 5.0) return(145)
      return(146)
    }

    # signal everything is OK
    return(131)
  }

  # if using wfh z-scores
  if (age1 < 1.0) {
    # fast increase
    if (is.na(y0)) return(112)
    if (is.na(hgt0)) return(113)
    if ((z1 - z0) > 0.67) return(141)
  }

  if (age1 >= 1.0 & age1 < 2.0) {
    if(z1 > 2.0) return(142)
    if(z1 > 1.0) return(143)
  }

  if (age1 >= 2.0 & age1 < 19.0) {
    # calc BMI if higher than 1.0
    if (z1 > 1){
      bmi <- y1/(hgt1/100)^2
      bmi_table <- growthscreener::bmi_table
      if(bmi > bmi_table[bmi_table$sex == sex &
                         bmi_table$age == floor(age1),
                         "ob"]) return(144)
      if(bmi > bmi_table[bmi_table$sex == sex &
                         bmi_table$age == floor(age1),
                         "ow"]){
        if(age1 < 5.0) return(145)
        return(146)
      }
    }
  }

  # signal everthing is OK
  return(131)
}
