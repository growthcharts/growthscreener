% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_screening_doms.R
\name{calculate_screening_doms}
\alias{calculate_screening_doms}
\title{Extract measurements that can function as date zero}
\usage{
calculate_screening_doms(tgt, ynames = c("hgt", "wgt", "hdc"), na.omit = TRUE)
}
\arguments{
\item{tgt}{A list with elements \code{psn} and \code{xyz}}

\item{ynames}{Character vector identifying the measures to be
screened. By default, \code{ynames = c("hgt", "wgt", "hdc")}.}

\item{na.omit}{A logical indicating whether records with a missing
\code{x} (age) or \code{y} (yname) should be removed. Defaults to
\code{TRUE}.}
}
\value{
A list with \code{length(ynames)} elements. Each list element
is another \code{list} with elements \code{dom0} (back-calculated
dates of measurement, vector, reverse time), \code{age0} (decimal age),
\code{y0} (measurement), \code{z0} (Z-score equivalent) and
\itemize{
\item for weight - \code{h0}, as well as similar quantities \code{dom1},
\code{age1}, \code{y1}, \code{z1} and \code{h1} for the upper part
of the pair.
}
}
\description{
This function scans the \code{tgt} object, and finds the observation
pairs used by the \code{calculate_advice_xxx()} functions.
}
\details{
This function implements the \code{"x1_last"} strategy for
choosing pairs. This strategy selects the last observation as
\code{x1} and forms pairs with every earlier observation.
}
\note{
Internal function. Not to be called directly.
}
\examples{
\dontrun{
growthscreener:::calculate_screening_doms(target)
}
}
