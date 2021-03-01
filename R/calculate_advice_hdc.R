#' Referral advice for head circumference
#'
#' This function traverses a decision tree for head circumference for children
#' below the age of 1.
#'
#' The decision tree assesses both single and paired measurements.
#' The last observations (`y1`) is generally taken as the
#' last measurement, whereas `y0` can be one of the previous
#' measurements. For more than two measurements, there are many
#' pairs possible, and these pairs need not be consecutive.
#' The `y0` measurement needs to be defined by the user,
#' and is informally taken as an earlier measurement that maximizes
#' the referal probability.
#'
#' @param y1    Head circumference at last measurement (cm)
#' @param y0    Head circumference at previous measurement (cm)
#' @inheritParams calculate_advice_hgt
#' @return `calculate_advice_hdc` returns an integer, the `msgcode`,
#' between 3000-3999.
#' @author Arjan Huizing, Stef van Buuren, 2020
#' @seealso calculate_helpers
#' @rdname advice_hdc
#' @examples
#' msg(calculate_advice_hdc())
#' msgcode <- calculate_advice_hdc(sex = "female",
#'                                 dob = as.Date("2015-09-01"),
#'                                 dom1 = as.Date("2016-05-01"), y1 = 40,
#'                                 dom0 = as.Date("2015-12-01"), y0 = 36)
#' msg(msgcode)
#' @export
calculate_advice_hdc <- function(sex = NA_character_, dob = as.Date(NA),
                                 ga = NA,
                                 dom1 = as.Date(NA), y1 = NA,
                                 dom0 = as.Date(NA), y0 = NA,
                                 test_gain = TRUE,
                                 verbose = FALSE) {

  age1 <- as.integer(dom1 - dob)/365.25
  age0 <- as.integer(dom0 - dob)/365.25

  # select reference
  pt <- !is.na(ga) && ga < 37 && !is.na(age1) && age1 < 4
  year <- ifelse(pt, "2012", "1997")
  sub <- ifelse(pt, ga, "nl")
  refcode <- centile::make_refcode(name = "nl", year = year, yname = "hdc",
                                   sex = sex, sub = sub)

  # calculate z1 and z0
  reftab <- centile::load_reference(refcode, pkg = "nlreferences", verbose = verbose)
  if (is.null(reftab)) {
    z1 <- z0 <- NA_real_
  } else {
    z <- centile::y2z(y = c(y1, y0), x = c(age1, age0), refcode = reftab)
    z1 <- z[1L]
    z0 <- z[2L]
  }

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(3019)
  if (is.na(dob)) return(3016)
  if (is.na(dom1)) return(3015)
  if (is.na(y1)) return(ifelse(age1 < 1.0, 3018, 3021))

  # outside age range
  if (age1 >= 1.0) return(3021)

  # check single measurement
  if (is.na(z1)) return(3024)
  if (z1 > 2.5) return(3041)
  if (z1 < -2) return(3043)

  # check for change z1 - z0
  if (test_gain) {
    if (is.na(dom0)) return(3022)
    if (is.na(y0)) return(3023)
    if (is.na(z0)) return(3025)

    if ((z1 - z0) > 2.5) return(3042)

    # increase (within 0.5 yrs)
    if (age1 - age0 <= 0.5){
      if ((z1 - z0) > 0.5) return(3044)
    }

    # decrease
    if ((z1 - z0) < -0.5) return(3045)
  }

  # signal everything is OK
  return(3031)
}
