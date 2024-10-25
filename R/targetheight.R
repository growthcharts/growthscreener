# targetheight.r

#' Target Height Calculation Using the Hermanussen-Cole Method
#'
#' This function calculates the target height of a child using the method
#' proposed by Hermanussen & Cole (2003). The formula accounts for assortative
#' mating and parent-offspring height correlations, leading to predictions that
#' are more moderate than the traditional "follow-the-centile" method.
#'
#' @aliases hermanussencole
#' @param hgtf Numeric vector with the height of the biological father in cm.
#' @param hgtm Numeric vector with the height of the biological mother in cm.
#' The length of \code{hgtm} must be the same as \code{hgtf}.
#' @param sex Character vector indicating the sex of the child for whom the
#'   target height is being calculated. Must be either \code{"male"} or
#'   \code{"female"} and have the same length as \code{hgtf}.
#' @param rPP Numeric value for the correlation between the heights of parents.
#'   The default value (0.19) is calculated for the Dutch population.
#'   Hermanussen & Cole (2003) used a value of 0.27.
#' @param rPO Numeric value for the correlation between parental height and
#'   offspring height. The default value (0.58) is based on the Dutch
#'   population, while Hermanussen & Cole (2003) used 0.57.
#' @param mu Numeric vector of length 2, representing the mean height in cm of
#'   boys and girls in the reference population. The default values \code{mu =
#'   c(183.8, 170.7)} come from the Fifth Dutch Growth Study (2009).
#' @param sigma Numeric vector of length 2, representing the standard deviation
#'   of height in cm for boys and girls in the reference population. The default
#'   values \code{sigma = c(7.1, 6.3)} are based on the Fifth Dutch Growth Study
#'   (2009).
#' @param pmu Numeric vector of length 2, representing the mean height in cm of
#'   fathers and mothers in the reference population. The default values
#'   \code{pmu = c(184, 170.6)} are from the Fourth Dutch Growth Study (1997).
#' @param psigma Numeric vector of length 2, representing the standard deviation
#'   of height in cm for fathers and mothers in the reference population. The
#'   default values \code{psigma = c(7.1, 6.5)} are from the Fourth Dutch Growth
#'   Study (1997).
#' @author Stef van Buuren
#' @references
#' Hermanussen M & Cole TJ (2003). "The Calculation of Target Height
#' Reconsidered." \emph{Hormone Research}, \bold{59}, 180--183.
#' @return A \code{data.frame} with the three columns:
#' \describe{
#'   \item{\code{y}}{Target height in cm.}
#'   \item{\code{y.sd}}{Standard deviation of the error term in cm.}
#'   \item{\code{z}}{Target height in standard deviation score (SDS),
#'   relative to the child reference.}
#' }
#' @seealso \code{\link{targetheight}}
#' @examples
#' \dontrun{
#' # Example 1: Target height for Dutch population, calculated using
#' # the Hermanussen & Cole method.
#' hermanussencole(hgtf = 157, hgtm = 154, sex = "male")
#'
#' # Example 2: The same target height calculated using the simplified method.
#' targetheight(hgtf = 157, hgtm = 154, sex = "male")
#'
#' # Example 3: Target height for case discussed in the appendix of
#' # Hermanussen & Cole (2003).
#' # A small discrepancy, 166.5 cm (published) versus 166.6 cm (calculated),
#' # is due to rounding differences.
#' hermanussencole(hgtf = c(157, 157), hgtm = c(154, 154),
#'                 sex = c("male", "female"),
#'                 rPP = 0.27, rPO = 0.57,
#'                 mu = c(178.2, 163.8), sigma = c(6.8, 6.0),
#'                 pmu = c(178.2, 163.8), psigma = c(6.8, 6.0))
#' }
#' @export
hermanussencole <- function(hgtf, hgtm, sex,
                            rPP = 0.19, rPO = 0.58,
                            mu = c(183.8, 170.7), sigma = c(7.1, 6.3),
                            pmu = c(184, 170.6), psigma = c(7.1, 6.5))
{
  stopifnot(length(hgtf) == length(hgtm))

  fz <- (hgtf - pmu[1]) / psigma[1]
  mz <- (hgtm - pmu[2]) / psigma[2]

  mphz <- (fz + mz)/2
  mphz <- ifelse(is.na(fz), mz, mphz)
  mphz <- ifelse(is.na(mz), fz, mphz)

  idx <- match(sex, c("male", "female"))
  mui <- mu[idx]
  sigmai <- sigma[idx]

  w <- ifelse(xor(is.na(fz), is.na(mz)), rPO, rPO * sqrt(2/(1+rPP)))
  # w <- ifelse(is.na(fz) & is.na(mz), NA, w)

  z <- w * mphz
  z.sd <- sqrt(1 - rPO^2)
  y <- z * sigmai + mui
  y.sd <- sigmai * z.sd
  return(data.frame(y = y, y.sd = y.sd, z = z))
}

#' Target Height Calculation for Dutch Population
#'
#' This function calculates the target height of a child based on the height of
#' the biological parents, using the method proposed by Hermanussen & Cole
#' (2003) with a simplified formula for the Dutch population.
#'
#' For more details, see Van Dommelen et al. (2012). The formulas are as
#' follows:
#'
#' \describe{
#' \item{For boys:}{TH = \code{44.5 + 0.376 * hgtf + 0.411 * hgtm}}
#' \item{For girls:}{TH = \code{47.1 + 0.334 * hgtf + 0.364 * hgtm}}
#' }
#'
#' If the height of the biological father is unknown, we use:
#' \describe{
#' \item{For boys:}{TH = \code{99.9 + 0.492 * hgtm}}
#' \item{For girls:}{TH = \code{96.3 + 0.436 * hgtm}}
#' }
#' If the mother's height is unknown, the function returns \code{NA} as the
#' target height cannot be calculated.
#'
#' @param hgtf Numeric vector with the height of the biological father in cm.
#'   Can include \code{NA} for unknown heights.
#' @param hgtm Numeric vector with the height of the biological mother in cm.
#'   Must have the same length as \code{hgtf}.
#' @param sex Character vector indicating the sex of the child for which target
#'   height is calculated. Must be either \code{"male"} or \code{"female"}.
#' @param etn Ethnicity. Not used in this function, but included for consistency
#'   with other functions.
#' @return A \code{data.frame} with three columns:
#' \describe{
#' \item{\code{y}}{The calculated target height in cm.}
#' \item{\code{y.sd}}{The standard deviation of the error term in cm.}
#' \item{\code{z}}{The target height in SDS, relative to the child reference.}
#' }
#' @references Hermanussen M, Cole TJ (2003). "The Calculation of Target Height
#' Reconsidered." \emph{Hormone Research}, \bold{59}, 180--183.
#'
#' Van Dommelen P, Schonbeck Y, Van Buuren S (2012). "A simple calculation of
#' the target height." \emph{Archives of Disease in Childhood}, \bold{97}, 182.
#' @seealso \code{\link{hermanussencole}}
#' @examples
#' \dontrun{
#' # Example: Target height for Dutch children with father height 180 cm and
#' # mother height 170 cm
#' # For both a boy and a girl
#' targetheight(hgtf = c(180, 180), hgtm = c(170, 170), sex = c("male", "female"))
#'
#' # Handling missing father height
#' targetheight(hgtf = c(NA, 180), hgtm = c(171, NA), sex = c("male", "male"))
#' }
#' @export
targetheight <- function(hgtf, hgtm, sex, etn = NULL){
  stopifnot(length(hgtf) == length(hgtm))
  idx <- match(sex, c("male", "female"))
  alpha <- c(44.5, 47.1)[idx]
  beta1 <- c(0.376, 0.334)[idx]
  beta2 <- c(0.411, 0.364)[idx]
  y <- alpha + beta1 * hgtf + beta2 * hgtm

  # Handle missing father heights
  fmis <- is.na(hgtf)
  if (any(fmis)) {
    alpha <- c(99.9, 96.3)[idx]
    beta2 <- c(0.492, 0.436)[idx]
    thmother <- alpha + beta2 * hgtm
    y[fmis] <- thmother[fmis]
  }

  y.sd <- c(5.783777, 5.132084)[idx]  # c(7.1, 6.3) * sqrt(1 - 0.58^2)
  z <- (y - c(183.8, 170.7)[idx]) / y.sd
  return(data.frame(y = y, y.sd = y.sd, z = z))
}
