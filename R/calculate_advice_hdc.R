#' Referral advice for head circumference
#'
#' This function traverses a decision tree for head circumference for children
#' below the age of 1.
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

  if (is.null(d)){
    lib <- ifelse(ga < 37 & !is.na(ga), "preterm", "nl1997")
    d <- calculate_helpers(yname = "hdc", lib = lib, sex = sex, dob = dob, ga = ga,
                           etn = etn, dom1 = dom1, y1 = y1, dom0 = dom0, y0 = y0)
  }


  age1 <- d$age1
  age0 <- d$age0
  z1   <- d$z1
  z0   <- d$z0

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(3019)
  if (is.na(dob)) return(3016)
  if (is.na(dom1)) return(3015)
  if (is.na(y1)) return(ifelse(age1 < 1.0, 3018, 3021))
  #if (is.na(etn)) return(3020) # redundant.

  # outside age range
  if (age1 >= 1.0) return(3021)

  # check single measurement
  if(z1 > 2.5) return(3041)
  if(z1 < -2) return(3043)

  # check for change z1 - z0
    # perhaps add a filter so that we dont always need z0 values for a '31'
  if(is.na(dom0)) return(3022)
  if(is.na(y0)) return(3023)

  if((z1 - z0) > 2.5) return(3042)

  # increase (within 0.5 yrs)
  if(age1 - age0 <= 0.5){
    if((z1 - z0) > 0.5) return(3043)
  }
  # decrease
  if((z1 - z0) < -0.5) return(3044)


  # signal everthing is OK
  return(3031)
}
