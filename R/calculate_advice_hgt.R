#' Referral advice for body height
#'
#' This function traverses the decision tree of the "JGZ-Richtlijn Lengtegroei
#' 2019".
#'
#' The decision tree assesses both single and paired measurements. The
#' observation corresponding to the oldest age is taken is the current
#' measurement.
#'
#' @param sex   Character, either `"male"` or `"female"`
#' @param bw    Birth weight (grammes)
#' @param bl    Birth length (cm)
#' @param ga    Gestational age, completed weeks (Integer or character)
#' @param etn   Etnicity, one of `"NL"` (dutch), `"TU"` (turkish), `"MA"`
#'   (moroccan) or `"HS"` (hindustani). Currently not functional, always uses
#'   `"NL"`. Only used for target height.
#' @param hgtf  Height of father (cm)
#' @param hgtm  Height of mother (cm)
#' @param dob   Date of birth (`ddmmYYYY`). Required if `dom` is supplied as a
#'   date string.
#' @param dom  Vector with dates of measurements. Either supplied as age in
#'   decimal years or a date in the format `ddmmYYYY`.
#' @param y     Vector with height measurements (cm)
#' @param test_gain Logical. Should the increase or decrease in Z-scores be
#'   tested? The default is `TRUE`.
#' @param verbose Set to `TRUE` to obtain warnings on reference finding.
#' @return `calculate_advice_hgt` returns an integer, the `msgcode`
#' @author Paula van Dommelen, Stef van Buuren, 2021
#' @rdname advice_hgt
#' @examples
#' msg(calculate_advice_hgt())
#' msgcode <- calculate_advice_hgt(sex = "male", dob = "01012020",
#'                                 dom = c("01022020", "01062020"),
#'                                 y = c(54, 68),
#'                                 ga = 35,
#'                                 test_gain = FALSE)
#' msg(msgcode)
#' @export
calculate_advice_hgt <- function(sex = NA_character_,
                                 bw = NA, bl = NA, ga = NA,
                                 etn = "NL", hgtf = NA, hgtm = NA,
                                 dob = NA_character_, dom = NA_character_, y = NA,
                                 test_gain = TRUE,
                                 verbose = FALSE) {

  etn <- "NL" # nieuwe groeistudie nodig voordat etn gebruikt kan worden

  # birth Z-scores
  bw_z <- calculate_birth_z(bw, sex, ga, yname = "wgt")
  bl_z <- calculate_birth_z(bl, sex, ga, yname = "hgt")
  th_z <- calculate_th(hgtf, hgtm, sex = sex, etn = etn)[2L]

  # convert date to age
  if (any(nchar(dom) >= 8 & !is.na(dom))) age <- date2age(dob, dom) else age <- dom
  age1 <- ifelse(!all(is.na(age)), max(age, na.rm=T), NA)

  # select reference
  pt <- !is.na(ga) && ga < 37 && !is.na(age1) && age1 < 4
  year <- ifelse(pt, "2012", "2009")
  sub <- ifelse(pt, ga, "nl")
  refcode <- centile::make_refcode(name = "nl", year = year, yname = "hgt",
                                   sex = sex, sub = sub)

  # calculate z-scores
  reftab <- centile::load_reference(refcode, pkg = "nlreferences", verbose = verbose)
  if (is.null(reftab)) {
    z1 <- z <- NA_real_
  } else {
    z <- centile::y2z(y = y, x = age, refcode = reftab)
    z1 <- z[which.max(age)]
    z0 <- z[-which.max(age)]
  }

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(1019)
  if (all(is.na(dom))) return(1015)
  if (any(nchar(dom) >= 8) & is.na(dob)) return(1016)
  if (!is.numeric(age)) return(1015)
  if (all(is.na(y))) return(ifelse(age1 < 18.0, 1018, 1021))

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
    if (z1 > 2.5 && all(is.na(z0)) && test_gain) return(1011)
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

    if (all(is.na(z0)) && test_gain) return(1011)
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
    if (all(is.na(z0))) return(1025)

    # Repeated measure needs to be after half a year, but within one year.
    repeated <- age1 - age <= 1 & age1 - age > 0.5

    if(any(repeated)){
      # short
      zrep <- min(z[repeated], na.rm = TRUE) # repeated low Z
      if (z1 < -2.5 && zrep < -2.5 && is.na(bw)) return(1013)
      if (z1 < -2.5 && zrep < -2.5 && ga < 37 && bw >= 2500) return(1049)
      if (z1 < -2.5 && zrep < -2.5 && bw_z >= -2) return(1062)

      # tall
      zrep <- max(z[repeated], na.rm = TRUE) # repeated high Z
      if (z1 > 2.5 && z0 > 2.5) return(1050)
    }
  }

  if (age1 >= 3.0 && age1 < 10.0 && test_gain) {
    if (all(is.na(z0))) return(1025)

    # Repeated measure any age above 3 years
    repeated <- age >= 3.0 & age != age[which.max(age)]

    if(any(repeated)){
      # short
      zrep <- max(z[repeated], na.rm = TRUE) # largest difference
      if ((z1 - zrep) < -2.0) return(1055)
      if (!is.na(th_z))
        if (z1 >= -2.0 && (z1 - zrep) < -1.0 && (z1 - th_z) < -1.0) return(1076)

      # tall
      zrep <- min(z[repeated], na.rm = TRUE) # largest difference
      if ((z1 - z0) > 2.0) return(1054)
    }
  }

  # signal everything is OK
  return(1031)
}
