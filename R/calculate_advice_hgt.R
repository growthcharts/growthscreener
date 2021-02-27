#' Referral advice for body height
#'
#' This function traverses the decision tree of the
#' "JGZ-Richtlijn Lengtegroei 2019".
#'
#' The decision tree assesses both single and paired measurements.
#' The last observations (`y1`) is generally taken as the
#' last measurement, whereas `y0` can be one of the previous
#' measurements. For more than two measurements, there are many
#' pairs possible, and these pairs need not be consecutive.
#' The `y0` measurement needs to be defined by the user,
#' and is informally taken as an earlier measurement that maximumizes
#' the referal probability. On the other hand, defining pairs that are
#' remote in ages (e.g. period between 1 month and 14 years) is probably
#' not that useful. In practice, we may be interested in setting the
#' maximum period to, say, five years.
#'
#' @param sex   Character, either `"male"` or `"female"`
#' @param dob   Date of birth (class Date)
#' @param bw    Birth weight (grammes)
#' @param bl    Birth length (cm)
#' @param ga    Gestational age, completed weeks (Integer or character)
#' @param etn   Etnicity, one of `"NL"` (dutch), `"TU"` (turkish),
#'              `"MA"` (moroccan) or `"HS"` (hindustani).
#'              The default is `"NL"`. Only used for target height.
#' @param hgtf  Height of father (cm)
#' @param hgtm  Height of mother (cm)
#' @param dom1  Date of last measurement (Date)
#' @param y1    Height at last measurement (cm)
#' @param dom0  Date of previous measurement (Date)
#' @param y0    Height at previous measurement (cm)
#' @param test_gain Logical. Should the increase or decrease in Z-scores be
#' tested? The default is `TRUE`.
#' @param verbose Set to `TRUE` to obtain warnings on reference finding.
#' @return `calculate_advice_hgt` returns an integer, the `msgcode`
#' @author Paula van Dommelen, Stef van Buuren, 2021
#' @rdname advice_hgt
#' @examples
#' msg(calculate_advice_hgt())
#' msgcode <- calculate_advice_hgt(sex = "male",
#'                                 dob = as.Date("2018-07-31"),
#'                                 dom1 = as.Date("2018-12-12"),
#'                                 y1 = 64, ga = 35,
#'                                 test_gain = FALSE)
#' msg(msgcode)
#' @export
calculate_advice_hgt <- function(sex = NA_character_, dob = as.Date(NA),
                                 bw = NA, bl = NA, ga = NA,
                                 etn = "NL", hgtf = NA, hgtm = NA,
                                 dom1 = as.Date(NA), y1 = NA,
                                 dom0 = as.Date(NA), y0 = NA,
                                 test_gain = TRUE,
                                 verbose = FALSE) {

  bw_z <- calculate_birth_z(bw, sex, ga, yname = "wgt")
  bl_z <- calculate_birth_z(bl, sex, ga, yname = "hgt")
  th_z <- calculate_th(hgtf, hgtm, sex = sex, etn = etn)[2L]
  age1 <- as.integer(dom1 - dob)/365.25
  age0 <- as.integer(dom0 - dob)/365.25

  # select reference
  pt <- !is.na(ga) && ga < 37 && !is.na(age1) && age1 < 4
  year <- ifelse(pt, "2012", "2009")
  sub <- ifelse(pt, ga, "nl")
  refcode <- centile::make_refcode(name = "nl", year = year, yname = "hgt",
                                   sex = sex, sub = sub)

  # calculate z1 and z0
  reftab <- centile::load_reference(refcode, pkg = "jamesyzy", verbose = verbose)
  if (is.null(reftab)) {
    z1 <- z0 <- NA_real_
  } else {
    z <- centile::y2z(y = c(y1, y0), x = c(age1, age0), refcode = reftab)
    z1 <- z[1L]
    z0 <- z[2L]
  }

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(1019)
  if (is.na(dob)) return(1016)
  if (is.na(dom1)) return(1015)
  if (is.na(y1)) return(ifelse(age1 < 18.0, 1018, 1021))

  # outside age range
  if (age1 >= 18.0) return(1021)
  if (age1 < 0.0833) return(1022)
  if (is.na(z1)) return(1024)

  # check single measurement
  if (age1 < 3.0) {

    # short
    if (z1 < -2.5 && is.na(bw)) return(1013)
    if (z1 < -2.5 && is.na(ga)) return(1010)
    if (z1 < -3.0 && ga >= 37 && bw >= 2500) return(1045)
    if (z1 < -3.0 && bw_z >= -2) return(1061)

    # tall
    if (z1 > 3.0) return(1048)
    if (z1 > 2.5 && is.na(z0) && test_gain) return(1011)
    if (z1 > 1.0) return(1077)
  }

  if (age1 >= 3.0 && age1 < 10.0) {

    # short
    if (z1 < -2.5) return(1044)
    if (z1 < -2.0 && !is.na(bw_z)) if (bw_z < -2.0) return(1042)
    if (z1 < -2.0 && !is.na(bl_z)) if (bl_z < -2.0) return(1041)
    if (!is.na(th_z)) {
      if (z1 <  -2.0 && (z1 - th_z) < -1.6) return(1043)
      if (z1 >= -2.0 && z1 < -1.0 && (z1 - th_z) < -2.0) return(1053)
    }
    if (z1 < -2.0 && is.na(bw)) return(1013)
    if (z1 < -2.0 && is.na(bl)) return(1012)
    if (z1 < -1.0 && is.na(th_z)) return(1014)

    # tall
    if (z1 > 2.5) return(1047)
    if (!is.na(th_z)) if (z1 > 2.0 && (z1 - th_z) > 2.0) return(1046)
    if (z1 > 2.0 && is.na(th_z)) return(1082)
    if (z1 > 1.0 &&
        ((age1 < 8.0 && sex == "female") || (age1 < 9.0 && sex == "male"))) return(1079)
    if (z1 > 1.0) return(1081)

    if (is.na(z0) && test_gain) return(1011)
  }

  if (age1 >= 10.0 && age1 < 18.0) {

    # short
    if (z1 < -2.5) return(1044)

    # tall
    if (z1 > 2.5) return(1047)
    if (z1 > 2.0 && sex == "female" && y1 >= 170) return(1071)
    if (z1 > 2.0 && sex == "male" && y1 >= 185) return(1072)
    if (z1 > 2.0) return(1073)
    if (sex == "female" && y1 >= 170.0) return(1074)
    if (sex == "male" && y1 >= 185.0) return(1075)
  }

  # check for gain z1 - z0
  if (age1 < 3.0 && test_gain) {
    if (is.na(z0)) return(1025)

    # short
    if (z1 < -2.5 && z0 < -2.5 && is.na(bw)) return(1013)
    if (z1 < -2.5 && z0 < -2.5 && ga < 37 && bw >= 2500) return(1049)
    if (z1 < -2.5 && z0 < -2.5 && bw_z >= -2) return(1062)

    # tall
    if (z1 > 2.5 && z0 > 2.5) return(1050)
  }

  if (age1 >= 3.0 && age1 < 10.0 && test_gain) {
    if (is.na(z0)) return(1025)
    # short
    if ((z1 - z0) < -2.0) return(1055)
    if (!is.na(th_z))
      if (z1 >= -2.0 && (z1 - z0) < -1.0 && (z1 - th_z) < -1.0) return(1076)

    # tall
    if ((z1 - z0) > 2.0) return(1054)
  }

  # signal everything is OK
  return(1031)
}
