#' Pick reference for evaluating weight guidelines
#'
#' The function picks the reference for evaluation according to Dutch
#' referral guidelines for underweight and overweight.
#'
#' @details
#' The Z-score calculation relies on normative references. The exact reference
#' used depends on the age of the child, sex, gestational age and ethnicity.
#'
#' The underweight guideline prescribes \code{wgt} references for ages 0-1 years,
#' \code{wfh} references for ages 1-2 years. The overweight guidelines prescribes
#' \code{wfh} references for ages 0-2 years. For consistency and simplicity, the
#' suggestion of the overweight guidelines is followed for both under- and
#' overweight. For ages > 2.0,  international and ethnic
#' specific (for HS) BMI-cut offs are use.
#' For preterms (ga < 37) Dutch preterm \code{wgt} and \code{wfh} references
#' are used. The function does not return BMI references.
#'
#' Missing data policy: if \code{age} or \code{sex} are missing, the function
#' returns \code{NULL}. If \code{ga} is missing, the function assumes term birth.
#' If \code{etn} is missing the function assumes Dutch ethnicity.
#'
#' @param age   Scalar, most recent decimal age (in years).
#' @param sex   Character, either \code{"male"} or \code{"female"}
#' @param ga    Gestational age, completed weeks (Integer or character)
#' @param etn   Ethnicity, one of \code{"NL"} (dutch), \code{"TU"} (turkish),
#'   \code{"MA"} (moroccan) or \code{"HS"} (hindustani).
#' @author Stef van Buuren, 2020
#' @return A list or \code{NULL}. Element \code{call} contains
#' an executable string to the proper reference. Element \code{ty} is a function
#' to be applied to the measurement before calculating the Z-score. Element \code{yname}
#' is the type of measurement. The function returns \code{NULL} if it cannot
#' determine a proper reference (e.g. for missing age or sex).
#' @references
#' Dutch guideline overweight 2012:
#' \url{https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/overgewicht}
#'
#' Dutch guidline underweight 2019:
#' \url{https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/ondergewicht-2019}
#' @examples
#' growthscreener:::pick_reference_wgt(age = 0.5, sex = "male")
pick_reference_wgt <- function(age = NA,
                               sex = NA_character_,
                               ga = NA,
                               etn = NA_character_) {
  age <- as.numeric(age[1])
  sex <- as.character(sex[1])
  ga <- as.numeric(ga[1])
  etn <- as.character(etn[1])

  if (is.na(age) || age < 0) return(NULL)
  if (!sex %in% c("male", "female")) return(NULL)

  # The outcommented block below selects the weight for age references for ages 0-1 year, as
  # suggested by the underweight guideline. The overweight guideline, however, suggests to
  # use weight for age. For purposes of consistency, I decided that the
  # both guidelines should use identical references. For simplicity, I prefer the
  # weight for height guidelines for period 0-2 years throughout. SvB 13jul20.
  # first year: weight for age
  # if (age < 1.0) {
  #   if (!is.na(etn) && etn == "HS" && !is.na(ga) && ga >= 37) {
  #     if (sex == "male") return(list(call = "clopus::nlhs[['nl1976wgt.mwgtHS']]", ty = function(y) y, yname = "wgt"))
  #     if (sex == "female") return(list(call = "clopus::nlhs[['nl1976wgt.fwgtHS']]", ty = function(y) y, yname = "wgt"))
  #   }
  #   if (is.na(ga) || ga >= 37) {
  #     if (sex == "male") return(list(call = "clopus::nl1980[['nl1980.mwgt']]", ty = function(y) y, yname = "wgt"))
  #     if (sex == "female") return(list(call = "clopus::nl1980[['nl1980.fwgt']]", ty = function(y) y, yname = "wgt"))
  #   }
  #   if (ga < 25) ga <- 25
  #   if (ga > 36) ga <- 36
  #   if (sex == "male") return(list(call = paste0("clopus::preterm[['pt2012a.mwgt", ga, "']]"), ty = function(y) y, yname = "wgt"))
  #   if (sex == "female") return(list(call = paste0("clopus::preterm[['pt2012a.fwgt", ga, "']]"), ty = function(y) y, yname = "wgt"))
  #   return(NULL)
  # }

  # later years: weight for height
  if (!is.na(etn) && etn == "HS" && !is.na(ga) && ga >= 37) {
    if (sex == "male") return(list(call = "clopus::nlhs[['nl1976wfh.mwfhHS']]", ty = function(y) log10(y), yname = "wfh"))
    if (sex == "female") return(list(call = "clopus::nlhs[['nl1976wfh.fwfhHS']]", ty = function(y) log10(y), yname = "wfh"))
  }
  if (is.na(ga) || ga >= 37) {
    if (sex == "male") return(list(call = "clopus::nl1980[['nl1980.mwfhNLA']]", ty = function(y) log10(y), yname = "wfh"))
    if (sex == "female") return(list(call = "clopus::nl1980[['nl1980.fwfhNLA']]", ty = function(y) log10(y), yname = "wfh"))
  }
  if (sex == "male") return(list(call = "clopus::preterm[['pt2012wfh.mwfh']]", ty = function(y) log1p(y), yname = "wfh"))
  if (sex == "female") return(list(call = "clopus::preterm[['pt2012wfh.fwfh']]", ty = function(y) log1p(y), yname = "wfh"))
  return(NULL)
}
