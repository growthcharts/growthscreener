#' Calculate values required for referral
#'
#' \code{calculate_helpers()} provides an optional pre-calculation for
#' \code{calculate_advice_hgt()}, \code{calculate_advice_wgt()} and
#' \code{calculate_advice_hdc()}. The user may wish to divide up calculations
#' into two steps if intermediate results are needed.
#' @param yname     Character, variable to calculate Z-scores of. Can be one of
#'   \code{"hgt"}, \code{"wgt"}, \code{"hdc"} or \code{"bmi"}.
#' @param lib   library to search the reference in
#' @param sex   Character, either \code{"male"} or \code{"female"}
#' @param dob   Date of birth (class Date)
#' @param bw    Birth weight (grammes)
#' @param bl    Birth length (cm)
#' @param ga    Gestational age, completed weeks (Integer or character)
#' @param etn   Etnicity, one of \code{"NL"} (dutch), \code{"TU"} (turkish),
#'   \code{"MA"} (moroccan) or \code{"HS"} (hindustani).
#' @param hgtf  Height of father (cm)
#' @param hgtm  Height of mother (cm)
#' @param dom1  Date of last measurement (Date)
#' @param y1    \code{y} at last measurement
#' @param dom0  Date of previous measurement (Date)
#' @param y0    \code{y} at previous measurement
#' @param hgt0  \code{hgt} at last measurement (cm), used when y is \code{wgt}
#' @param hgt1  \code{hgt} at previous measurement (cm), used when y is
#'   \code{wgt}
#' @author Paula van Dommelen, Stef van Buuren, Arjan Huizing, 2020
#' @return \code{calculate_helpers()} returns a \code{list} with the following
#'   elements: \describe{ \item{\code{bw_z}}{Birth weight SDS}
#'   \item{\code{bl_z}}{Birth length SDS} \item{\code{th}}{Target height (cm)}
#'   \item{\code{th_z}}{Target height SDS} \item{\code{age1}}{Age at last
#'   measurement} \item{\code{age0}}{Age at previous measurement}
#'   \item{\code{z1}}{y-value SDS at \code{age1}} \item{\code{z0}}{y-value SDS
#'   at \code{age0}} }
#' @export
calculate_helpers <- function(yname = "hgt", lib = "nl2009", sex = NA_character_,
                              dob = as.Date(NA), bw = NA, bl = NA, ga = NA,
                              etn = NA_character_, hgtf = NA, hgtm = NA,
                              dom1 = as.Date(NA), y1 = NA, hgt1 = NA,
                              dom0 = as.Date(NA), y0 = NA, hgt0 = NA) {

  bw_z <- calculate_birth_z(bw, sex, ga, yname = "wgt")
  bl_z <- calculate_birth_z(bl, sex, ga, yname = "hgt")
  thl  <- calculate_th(hgtf, hgtm, sex = sex, etn = etn)
  th   <- thl[1L]
  th_z <- thl[2L]
  age1 <- as.integer(dom1 - dob)/365.25
  age0 <- as.integer(dom0 - dob)/365.25

  # Find correct reference.
  s <- sex
  y <- yname
  pop <- ifelse(ga < 37 & age1 < 4 & !is.na(ga) & !is.na(age1), ga, "NL")

  if(yname == "wgt"){ # wfh is by hgt rather than by age.
    x1 <- hgt1
    x0 <- hgt0
    pop <- ""
  } else {
    x1 <- age1
    x0 <- age0
  }

  ref <- try(find.reference(yname == y & sex == s & sub == pop,
                        libname = lib, select = TRUE, exact = TRUE),
             silent = TRUE)
  z1 <- y2z(y = y1, x = x1, sex = sex, sub = etn,
            ref = ref)
  z0 <- y2z(y = y0, x = x0, sex = sex, sub = etn,
            ref = ref)

  list(bw_z = bw_z, bl_z = bl_z, th = th, th_z = th_z,
       age1 = age1, age0 = age0, z1 = z1, z0 = z0)
}
