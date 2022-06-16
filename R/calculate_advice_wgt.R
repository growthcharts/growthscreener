#' Referral advice for weight for height
#'
#' This function traverses the decision tree of the "JGZ-Richtlijn overgewicht
#' 2012" and "JGZ-Richtlijn Ondergewicht 2019"
#'
#' Oldest age assumed to be current
#'
#' The decision tree assesses both single and paired measurements. The last
#' observations (`y1`) is generally taken as the last measurement, whereas `y0`
#' can be one of the previous measurements. For more than two measurements,
#' there are many pairs possible, and these pairs need not be consecutive. The
#' `y0` measurement needs to be defined by the user, and is informally taken as
#' an earlier measurement that maximizes the referral probability.
#'
#' @inheritParams calculate_advice_hgt
#' @param dom_hgt Vector with date of measurement relating to height. Either
#'   supplied as age in decimal years or a date in the format `ddmmYYYY`.
#' @param hgt Vector with height measurements (cm)
#' @return `calculate_advice_wgt()` returns an integer, the `msgcode`
#' @author Arjan Huizing, Stef van Buuren 2020
#' @examples
#' msg(calculate_advice_wgt())
#' msgcode <- calculate_advice_wgt(sex = "male", dob = "01012020",
#'                                 dom = c("01022020", "01062020"),
#'                                 dom_hgt = c("01022020", "01062020"),
#'                                 y = c(5.4, 6.8),
#'                                 hgt = c(54, 60),
#'                                 ga = 35,
#'                                 test_gain = FALSE)
#' msg(msgcode)
#' @export
calculate_advice_wgt  <- function(sex = NA_character_,
                                  ga = NA, etn = NA,
                                  dom = NA_integer_, y = NA,
                                  dom_hgt = NA_integer_, hgt = NA,
                                  dob = NA_character_,
                                  test_gain = TRUE,
                                  verbose = FALSE) {

  # convert date to age
  if (any(nchar(dom) >= 8 & !is.na(dom))) age <- date2age(dob, dom) else age <- dom
  if (any(nchar(dom_hgt) >= 8 & !is.na(dom_hgt))) age_hgt <- date2age(dob, dom_hgt) else age_hgt <- dom_hgt

  # sort wgt and hgt observations
  df <- data.frame(age = age, y = y) %>%
    left_join(data.frame(age = age_hgt, hgt = hgt), by = "age")
  df1 <- df[which.max(df$age), ] # subset today

  # start the sieve

  # return early if data are insufficient
  if (!sex %in% c("male", "female")) return(2019)
  if (all(is.na(df$age))) return(2015)
  if (is.na(df1$y)) return(2018)
  if (is.na(df1$hgt) && df1$age >= 1.0) return(2014)

  # outside age range
  if (df1$age >= 19.0) return(2021)
  if (df1$age < 2.0 && df1$hgt < 35) return(2022)
  if (df1$age < 2.0 && df1$hgt > 120) return(2023)

  # obtain Z-values
  pick <- pick_reference_wgt(df1$age, sex, ga, etn)
  if (is.null(pick)) {
    df$z <- rep(NA_real_, nrow(df))
    df1$z <- NA_real_
  } else {
    reftable <- centile::load_reference(refcode = pick, pkg = "nlreferences", verbose = verbose)
    if (is.null(reftable)) {
      df$z <- rep(NA_real_, nrow(df))
      df1$z <- NA_real_
    } else {
      df$z <- centile::y2z(y = df$y, x = df$hgt, refcode = reftable)
      df1$z <- centile::y2z(y = df1$y, x = df1$hgt, refcode = reftable)
    }
  }
  df0 <- df[-which.max(df$age), ] # subset out today

  # apply decision tree
  if (df1$age < 1.0) {

    # low weight
    if (!is.na(df1$z) && df1$z < -2.0) return(2074)

    # fast decrease 1SD - for any previous measurement
    if (test_gain) {
      if (nrow(df0) < 1 | all(is.na(df0$y))) return(2012)
      #if (all(is.na(df0$age))) return(2011) -> Old version doesnt check, should we add this?
      if (all(is.na(df0$hgt))) return(2013)
      if (any(!is.na(df0$z) & !is.na(df1$z) & (df1$z - df0$z) < -1.0)) return(2075)
    }
  }

  if (df1$age >= 1.0 && df1$age < 2.0) {

    # high weight
    if (df1$z > 2.0) return(2042)
    if (df1$z > 1.0) return(2073)

    # low weight
    if (df1$z < -3.0) return(2076)
    if (df1$z < -2.0) return(2074)

    # decreasing - any previous measurement
    if (test_gain) {
      if (nrow(df0) < 1 | all(is.na(df0$y))) return(2012)
      if (all(is.na(df0$age))) return(2011)
      if (all(is.na(df0$hgt))) return(2013)
      if (any(!is.na(df0$z) & (df1$z - df0$z) < -1.0)) return(2075)
    }
  }

  if (df1$age >= 2.0) {

    bmi <- df1$y/(df1$hgt/100)^2
    bmi_table <- growthscreener::bmi_table
    grp <- "IOTF"
    if (!is.na(etn) && etn == "HS") grp <- "HS"
    bmi_table <- bmi_table[bmi_table$sex == sex & bmi_table$etn == grp, ]
    cutoff_obesity <- approx(x = bmi_table$age,
                             y = bmi_table$obesity,
                             xout = df1$age)$y
    cutoff_overweight <- approx(x = bmi_table$age,
                                y = bmi_table$overweight,
                                xout = df1$age)$y
    cutoff_underweight <- approx(x = bmi_table$age,
                                 y = bmi_table$underweight,
                                 xout = df1$age)$y

    # high weight (bmi)
    if (bmi > cutoff_obesity) return(2044)
    if (bmi > cutoff_overweight) {
      if (df1$age < 5.0) return(2045)
      return(2046)
    }

    # low weight (bmi)
    if (bmi <= cutoff_underweight) return(2074)

    # decreasing (wfh)
    if (test_gain) {
      if (all(is.na(df0$y))) return(2012)
      if (all(is.na(df0$hgt))) return(2013)
      # both observed together return(newnum)
      if (!is.na(df0$z) && (df1$z - df0$z) < -1.0) return(2075)
    }
  }

  # signal everything is OK
  return(2031)
}
