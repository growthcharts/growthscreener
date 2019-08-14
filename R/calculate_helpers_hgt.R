#' @details \code{calculate_helpers_hgt()} provides an optional
#' pre-calculation for \code{calculate_advice_hgt()}. The user may
#' wish to divide up calculations into two steps if intermediate
#' results are needed.
#' @rdname advice_hgt
#' @inheritParams calculate_advice_hgt
#' @return \code{calculate_helpers_hgt()} returns a \code{list} with
#' the following elements:
#' \describe{
#' \item{\code{bw_z}}{Birth weight SDS}
#' \item{\code{bl_z}}{Birth length SDS}
#' \item{\code{th}}{Target height (cm)}
#' \item{\code{th_z}}{Target height SDS}
#' \item{\code{age1}}{Age at last measurement}
#' \item{\code{age0}}{Age at previous measurement}
#' \item{\code{z1}}{Height SDS at \code{age1}}
#' \item{\code{z0}}{Height SDS at \code{age0}}
#' }
#' @export
calculate_helpers_hgt <- function(sex = NA_character_, dob = as.Date(NA),
                                  bw = NA, bl = NA, ga = NA,
                                  etn = NA_character_, hgtf = NA, hgtm = NA,
                                  dom1 = as.Date(NA), y1 = NA,
                                  dom0 = as.Date(NA), y0 = NA) {
  bw_z <- calculate_birth_z(bw, sex, ga, yname = "wgt")
  bl_z <- calculate_birth_z(bl, sex, ga, yname = "hgt")
  thl  <- calculate_th(hgtf, hgtm, sex = sex, etn = etn)
  th   <- thl[1L]
  th_z <- thl[2L]
  age1 <- as.integer(dom1 - dob)/365.25
  age0 <- as.integer(dom0 - dob)/365.25
  z1 <- y2z(y = y1, x = age1, sex = sex, sub = etn,
            ref = growthscreener::nl5.hgt)
  z0 <- y2z(y = y0, x = age0, sex = sex, sub = etn,
            ref = growthscreener::nl5.hgt)

  list(bw_z = bw_z, bl_z = bl_z, th = th, th_z = th_z,
       age1 = age1, age0 = age0, z1 = z1, z0 = z0)
}
