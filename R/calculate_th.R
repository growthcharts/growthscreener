#' Calculate target height SDS for a single child
#'
#' `calculate_th()` calculates the target height of a child based on the height
#' of the biological parents, using the method proposed by Hermanussen & Cole
#' (2003) with a simplified formula for the Dutch population. The function is
#' not vectorized, so it only works for a single child at a time. Optionally,
#' it can calculate the target height based on the height of the mother only,
#' although this only works for the Dutch population.
#'
#' For children with Down syndrome (`etn = "DS"`), the target height formula
#' is taken from Van Gameren-Oosterom et al. (2012), fitted directly on
#' final height of Dutch children with Down syndrome rather than derived
#' from parent-offspring/assortative-mating correlations. That paper does
#' not report a prediction error distinct from the final-height SD used in
#' the SDS formula, so `th_pe` and the denominator of `th_z` both use that
#' same SD (6.2 boys / 7.3 girls).
#'
#' @param hgtf  Length of biological father (cm)
#' @param hgtm  Length of biological mother (cm)
#' @param sex   Character, either `"male"` or `"female"`. If omitted, the
#' functions returns c(`NA`, `NA`).
#' @param etn   Ethnicity, one of `"NL"` (dutch), `"TU"` (turkish), `"MA"`
#'   (moroccan), `"HS"` (hindustani) or `"DS"` (Down syndrome). If omitted, the
#' functions returns c(`NA`, `NA`).
#' @param support_missing_hgtf  Logical, if `TRUE` and `hgtf` is missing, the
#'  function will calculate the target height based on `hgtm` only. This will
#'  work only for the Dutch population. If `hgtm` is missing, the function will
#'  always return `NA`. The default is `support_missing_hgt = FALSE`.
#' @param dec   Integer vector, length 2, indicating rounding for th and th_z,
#'   respectively
#' @return      Numeric, length 3: 1) target height (cm), 2) target height
#'   prediction error and 3) target height SDS.
#' @author      Stef van Buuren, 2019, 2024
#' @references
#' Van Gameren-Oosterom HBM, Van Dommelen P, Oudesluys-Murphy AM,
#' Buitendijk SE, Van Buuren S, Van Wouwe JP (2012) Healthy Growth in
#' Children with Down Syndrome. PLoS ONE 7(2): e31079.
#' @examples
#' calculate_th(180, 170, "male", "NL")
#' calculate_th(NA, 170, "male", "NL", support_missing_hgtf = TRUE)
#' calculate_th(180, 170, "male", "DS")
#' @export
calculate_th <- function(hgtf, hgtm,
												 sex = NULL, etn = NULL,
												 support_missing_hgtf = FALSE,
												 dec = c(1L, 3L, 3L)) {
	# safety trim of inputs to scalar
	hgtf <- as.numeric(hgtf[1L])
	hgtm <- as.numeric(hgtm[1L])
	sex <- sex[1L]
	etn <- etn[1L]

	# don't calculate if we're missing proper sex or etn
	if (is.null(sex) | is.null(etn)) return(c(NA, NA))
	if (!sex %in% c("male", "female")) return(c(NA, NA))
	if (!etn %in% c("NL", "TU", "MA", "HS", "DS")) return(c(NA, NA))

	# calculate target height, assuming valid hgtf and hgtm
	# recalculate target height if we're missing hgtf
	# calculate Z-score
	if (sex == "male") {
		th <- switch(EXPR = etn,
								 "NL" = 44.5 + 0.376 * hgtf + 0.411 * hgtm,
								 "TU" = 29.6 + 0.441 * hgtf + 0.465 * hgtm,
								 "MA" = 22.4 + 0.439 * hgtf + 0.508 * hgtm,
								 "HS" = 43.6 + 0.366 * hgtf + 0.431 * hgtm,
								 "DS" = 41.8 + 0.328 * hgtf + 0.359 * hgtm,
								 NA)
		if (is.na(th) && support_missing_hgtf) {
			th <- switch(EXPR = etn,
									 "NL" = 99.9 + 0.492 * hgtm,
									 NA)
		}
		th_pe <- switch(EXPR = etn,
										"NL" = 7.1 * sqrt(1 - 0.58^2),
										"TU" = 6.8 * sqrt(1 - 0.58^2),
										"MA" = 7.7 * sqrt(1 - 0.58^2),
										"HS" = 7.0 * sqrt(1 - 0.58^2),
										"DS" = 6.2,
										NA)
		th_z <- switch(EXPR = etn,
									 "NL" = (th - 183.8) / 7.1,
									 "TU" = (th - 176.8) / 6.8,
									 "MA" = (th - 177.2) / 7.7,
									 "HS" = (th - 174.3) / 7.0,
									 "DS" = (th - 163.4) / 6.2,
									 NA)
	}

	if (sex == "female") {
		th <- switch(EXPR = etn,
								 "NL" = 47.1 + 0.334 * hgtf + 0.364 * hgtm,
								 "TU" = 32.8 + 0.389 * hgtf + 0.410 * hgtm,
								 "MA" = 32.1 + 0.370 * hgtf + 0.429 * hgtm,
								 "HS" = 49.4 + 0.308 * hgtf + 0.364 * hgtm,
								 "DS" = 8.64 + 0.387 * hgtf + 0.422 * hgtm,
								 NA)
		if (is.na(th) && support_missing_hgtf) {
			th <- switch(EXPR = etn,
									 "NL" = 96.3 + 0.436 * hgtm,
									 NA)
		}
		th_pe <- switch(EXPR = etn,
										"NL" = 6.3 * sqrt(1 - 0.58^2),
										"TU" = 6.0 * sqrt(1 - 0.58^2),
										"MA" = 6.5 * sqrt(1 - 0.58^2),
										"HS" = 5.9 * sqrt(1 - 0.58^2),
										"DS" = 7.3,
										NA)
		th_z <- switch(EXPR = etn,
									 "NL" = (th - 170.7) / 6.3,
									 "TU" = (th - 162.6) / 6.0,
									 "MA" = (th - 162.8) / 6.5,
									 "HS" = (th - 159.6) / 5.9,
									 "DS" = (th - 151.8) / 7.3,
									 NA)
	}

	# return th and th_z
	return(c(round(th, dec[1]),
					 round(th_pe, dec[2]),
					 round(th_z, dec[3])))
}
