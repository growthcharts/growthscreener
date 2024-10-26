% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/targetheight.R
\name{targetheight}
\alias{targetheight}
\title{Target Height Calculation for Dutch Population}
\usage{
targetheight(hgtf, hgtm, sex, etn = NULL)
}
\arguments{
\item{hgtf}{Numeric vector with the height of the biological father in cm.
Can include \code{NA} for unknown heights.}

\item{hgtm}{Numeric vector with the height of the biological mother in cm.
Must have the same length as \code{hgtf}.}

\item{sex}{Character vector indicating the sex of the child for which target
height is calculated. Must be either \code{"male"} or \code{"female"}.}

\item{etn}{Ethnicity. Not used in this function, but included for consistency
with other functions.}
}
\value{
A \code{data.frame} with three columns:
\describe{
\item{\code{y}}{The calculated target height in cm.}
\item{\code{y.sd}}{The standard deviation of the error term in cm.}
\item{\code{z}}{The target height in SDS, relative to the child reference.}
}
}
\description{
This function calculates the target height of a child based on the height of
the biological parents, using the method proposed by Hermanussen & Cole
(2003) with a simplified formula for the Dutch population.
}
\details{
For more details, see Van Dommelen et al. (2012). The formulas are as
follows:

\describe{
\item{For boys:}{TH = \code{44.5 + 0.376 * hgtf + 0.411 * hgtm}}
\item{For girls:}{TH = \code{47.1 + 0.334 * hgtf + 0.364 * hgtm}}
}

If the height of the biological father is unknown, we use:
\describe{
\item{For boys:}{TH = \code{99.9 + 0.492 * hgtm}}
\item{For girls:}{TH = \code{96.3 + 0.436 * hgtm}}
}
If the mother's height is unknown, the function returns \code{NA} as the
target height cannot be calculated.
}
\examples{
\dontrun{
# Example: Target height for Dutch children with father height 180 cm and
# mother height 170 cm
# For both a boy and a girl
targetheight(hgtf = c(180, 180), hgtm = c(170, 170), sex = c("male", "female"))

# Handling missing father height
targetheight(hgtf = c(NA, 180), hgtm = c(171, NA), sex = c("male", "male"))
}
}
\references{
Hermanussen M, Cole TJ (2003). "The Calculation of Target Height
Reconsidered." \emph{Hormone Research}, \bold{59}, 180--183.

Van Dommelen P, Schonbeck Y, Van Buuren S (2012). "A simple calculation of
the target height." \emph{Archives of Disease in Childhood}, \bold{97}, 182.
}
\seealso{
\code{\link{hermanussencole}}
}
