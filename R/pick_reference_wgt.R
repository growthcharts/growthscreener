#' Pick reference for evaluating weight guidelines
#'
#' The function picks the reference for evaluation according to Dutch
#' referral guidelines for underweight and overweight.
#'
#' @details
#' The Z-score calculation relies on normative references. The exact reference
#' used depends on the age of the child, sex, gestational age and ethnicity.
#'
#' The underweight guideline prescribes `wgt` references for ages 0-1 years,
#' `wfh` references for ages 1-2 years. The overweight guidelines prescribes
#' `wfh` references for ages 0-2 years. For consistency and simplicity, the
#' suggestion of the overweight guidelines is followed for both under- and
#' overweight. For ages > 2.0,  international and ethnic
#' specific (for HS) BMI-cut offs are use.
#' For preterms (ga < 37) Dutch preterm `wgt` and `wfh` references
#' are used. The function does not return BMI references.
#'
#' Missing data policy: if `age` or `sex` are missing, the function
#' returns `NULL`. If `ga` is missing, the function assumes term birth.
#' If `etn` is missing the function assumes Dutch ethnicity.
#'
#' @param age   Scalar, most recent decimal age (in years).
#' @param sex   Character, either `"male"` or `"female"`
#' @param ga    Gestational age, completed weeks (Integer or character)
#' @param etn   Ethnicity, one of `"NL"` (dutch), `"TU"` (turkish),
#'   `"MA"` (moroccan) or `"HS"` (hindustani).
#' @author Stef van Buuren, 2020
#' @return A list or `NULL`. Element `call` contains
#' an executable string to the proper reference. Element `ty` is a function
#' to be applied to the measurement before calculating the Z-score. Element `yname`
#' is the type of measurement. The function returns `NULL` if it cannot
#' determine a proper reference (e.g. for missing age or sex).
#' @references
#' Dutch guideline overweight 2012:
#' <https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/overgewicht>
#'
#' Dutch guidline underweight 2019:
#' <https://www.ncj.nl/richtlijnen/alle-richtlijnen/richtlijn/ondergewicht-2019>
#' @examples
#' growthscreener:::pick_reference_wgt(age = 0.5, sex = "male")
pick_reference_wgt <- function(age = NA,
                               sex = NA_character_,
                               ga = NA,
                               etn = NA_character_) {
  age <- as.numeric(age[1L])
  sex <- as.character(sex[1L])
  ga <- as.numeric(ga[1L])
  etn <- as.character(etn[1L])

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
    return(paste("nl_1976_wfh", sex, "hs", sep = "_"))
  }
  if (is.na(ga) || ga >= 37) {
    return(paste("nl_1980_wfh", sex, "nla", sep = "_"))
  }

  # WFH until 120 cm
  paste("nl_2012_wfh", sex, "", sep = "_")
}
