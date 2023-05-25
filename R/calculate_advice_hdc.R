#' Referral advice for head circumference
#'
#' This function traverses a decision tree for head circumference for children
#' below the age of 1.
#'
#' The decision tree assesses both single and paired measurements. The
#' observation corresponding to the oldest age is taken is the current
#' measurement.
#'
#' @param y    Head circumference (cm)
#' @inheritParams calculate_advice_hgt
#' @return `calculate_advice_hdc` returns an integer, the `msgcode`,
#' between 3000-3999.
#' @author Arjan Huizing, Stef van Buuren, 2020
#' @rdname advice_hdc
#' @examples
#' msg(calculate_advice_hdc())
#' msgcode <- calculate_advice_hdc(sex = "female",
#'                                 dom = c(0.2491, 0.6653),
#'                                 y = c(36, 40))
#' msg(msgcode)
#' @export
calculate_advice_hdc <- function(sex = NA_character_,
                                 ga = NA, dob = NA_character_,
                                 dom = NA_integer_, y = NA,
                                 test_gain = TRUE,
                                 verbose = FALSE) {

  # convert date to age
  if (any(as.numeric(dom) > 999 & !is.na(dom))) age <- date2age(dob, dom) else age <- dom
  age1 <- ifelse(!all(is.na(age)), max(age, na.rm=T), NA)

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
    z <- centile::y2z(y = y, x = age, refcode = reftab)
    z1 <- z[which.max(age)]
    z0 <- z[-which.max(age)]
  }

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(3019)
  if (all(is.na(dom))) return(3015)
  if (any(nchar(dom) >= 8) & is.na(dob)) return(3016)
  if (!is.numeric(age)) return(3015)
  if (all(is.na(y))) return(ifelse(age1 < 1.0, 3018, 3021))

  # outside age range
  if (age1 >= 1.0) return(3021)

  # check single measurement
  if (is.na(z1)) return(3024)
  if (z1 > 2.5) return(3041)
  if (z1 < -2) return(3043)

  # check for change z1 - z0
  if (test_gain) {
    if (length(!is.na(dom)) < 2) return(3022)
    if (length(!is.na(y)) < 2) return(3023)
    if (all(is.na(z0))) return(3025)

    if (any((z1 - z0) > 2.5)) return(3042)

    # increase (within 0.5 yrs)
    age0 <- age[-which.max(age)]
    if (any(age1 - age0 <= 0.5)){
      if (any((z1 - z0[age1 - age0 <= 0.5]) > 0.5)) return(3044)
    }

    # decrease
    if (any((z1 - z0) < -0.5)) return(3045)
  }

  # signal everything is OK
  return(3031)
}
