#' Referral advice for weight for height
#'
#' This function traverses the decision tree of the "JGZ-Richtlijn overgewicht
#' 2012" and "JGZ-Richtlijn Ondergewicht 2019"
#'
#' The decision tree assesses both single and paired measurements.
#' The last observations (`y1`) is generally taken as the
#' last measurement, whereas `y0` can be one of the previous
#' measurements. For more than two measurements, there are many
#' pairs possible, and these pairs need not be consecutive.
#' The `y0` measurement needs to be defined by the user,
#' and is informally taken as an earlier measurement that maximizes
#' the referral probability.
#'
#' @inheritParams calculate_advice_hgt
#' @param hgt1    Height at last measurement (cm)
#' @param hgt0    Height at previous measurement (cm)
#' @return `calculate_advice_wgt()` returns an integer, the `msgcode`
#' @author Arjan Huizing, Stef van Buuren 2020
#' @examples
#' msg(calculate_advice_wgt())
#' msgcode <- calculate_advice_wgt(sex = "male", dom1 = 1441, y1 = 25, hgt1 = 120)
#' msg(msgcode)
#' @export
calculate_advice_wgt  <- function(sex = NA_character_,
                                  ga = NA, etn = NA,
                                  dom0 = NA_integer_,
                                  y0 = NA, dom1 = NA_integer_, y1 = NA,
                                  hgt1 = NA, hgt0 = NA,
                                  test_gain = TRUE,
                                  verbose = FALSE) {

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(2019)
  if (is.na(dom1)) return(2015)
  if (is.na(y1)) return(2018)
  if (is.na(hgt1)) return(2014)

  # check applicability
  age1 <- round(dom1/365.25, 4)
  age0 <- round(dom0/365.25, 4)
  if (age1 >= 19.0) return(2021)
  if (age1 < 2.0 && hgt1 < 35) return(2022)
  if (age1 < 2.0 && hgt1 > 120) return(2023)

  # obtain Z-values
  pick <- pick_reference_wgt(age1, sex, ga, etn)
  if (is.null(pick)) {
    z0 <- rep(NA_real_, length(y0))
    z1 <- rep(NA_real_, length(y1))
  } else {
    reftable <- centile::load_reference(refcode = pick, pkg = "nlreferences", verbose = verbose)
    if (is.null(reftable)) {
      z0 <- rep(NA_real_, length(y0))
      z1 <- rep(NA_real_, length(y0))
    } else {
      z0 <- centile::y2z(y = y0, x = hgt0, refcode = reftable)
      z1 <- centile::y2z(y = y1, x = hgt1, refcode = reftable)
    }
  }

  # start sieve

  if (age1 < 1.0) {

    # fast increase
    if (test_gain && age1 > 0.0) {
      if (is.na(y0)) return(2012)
      if (is.na(hgt0)) return(2013)
      if (!is.na(age0) && age0 >= 0.5 && z0 > 1.0 && (z1 - z0) > 0.67) return(2071)
    }

    # low weight
    if (!is.na(z1) && z1 < -2.0) return(2074)

    # fast decrease 1SD
    if (test_gain) {
      if (!is.na(z0) && !is.na(z1) && (z1 - z0) < -1.0) return(2075)
    }
  }

  if (age1 >= 1.0 && age1 < 2.0) {

    # high weight
    if (z1 > 2.0) return(2042)
    if (z1 > 1.0) return(2073)

    # increasing
    if (test_gain) {
      if (is.na(dom0)) return(2011)
      if (is.na(y0)) return(2012)
      if (is.na(hgt0)) return(2013)
      if (age0 >= 0.5 && z0 > 1.0 && (z1 - z0) > 0.67) return(2071)
    }

    # low weight
    if (z1 < -3.0) return(2076)
    if (z1 < -2.0) return(2074)

    # decreasing
    if (test_gain) {
      if (!is.na(z0) && (z1 - z0) < -1.0) return(2075)
    }
  }

  if (age1 >= 2.0) {

    bmi <- y1/(hgt1/100)^2
    bmi_table <- growthscreener::bmi_table
    grp <- "IOTF"
    if (!is.na(etn) && etn == "HS") grp <- "HS"
    bmi_table <- bmi_table[bmi_table$sex == sex & bmi_table$etn == grp, ]
    cutoff_obesity <- approx(x = bmi_table$age,
                             y = bmi_table$obesity,
                             xout = age1)$y
    cutoff_overweight <- approx(x = bmi_table$age,
                                y = bmi_table$overweight,
                                xout = age1)$y
    cutoff_underweight <- approx(x = bmi_table$age,
                                 y = bmi_table$underweight,
                                 xout = age1)$y

    # high weight (bmi)
    if (bmi > cutoff_obesity) return(2044)
    if (bmi > cutoff_overweight) {
      if (age1 < 5.0) return(2045)
      return(2046)
    }

    # low weight (bmi)
    if (bmi <= cutoff_underweight) return(2074)

    # decreasing (wfh)
    if (test_gain) {
      if (is.na(y0)) return(2012)
      if (is.na(hgt0)) return(2013)
      if (!is.na(z0) && (z1 - z0) < -1.0) return(2075)
    }
  }

  # signal everything is OK
  return(2031)
}
