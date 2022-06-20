#' Referral advice for language development
#'
#' This function traverses the decision tree "JGZ-Richtlijn taalontwikkeling
#' 2012", which is based on the van Wiechen questionnaire for child development.
#'
#' The observation corresponding to the oldest age is taken is the current
#' measurement.
#'
#' @param dob Date of birth (`ddmmYYYY`). Required if `dom` is supplied as a
#'   date string.
#' @param dom_vw41  Date of measurement for van Wiechen item 41. Either a vector
#'   of age in decimal years or a date in the format `ddmmYYYY`
#' @param vw41 Outcome of the van Wiechen item.
#' @return `calculate_advice_devlang` returns an integer, the `msgcode`, between
#'   4000-4999.
#' @author Arjan Huizing, Stef van Buuren, 2020
#' @rdname advice_devlang
#' @examples
#' msg(calculate_advice_devlang())
#' msgcode <- calculate_advice_devlang(dob = "01012020",
#'                                 dom_vw41 = "01012022", vw41 = 2,
#'                                 dom_vw42 = "01012022", vw42 = 1)
#' msg(msgcode)
#' @export
calculate_advice_devlang <- function(dob = NA_character_,
                                     dom_vw41 = NA, vw41 = NA,
                                     dom_vw42 = NA, vw42 = NA,
                                     dom_vw43 = NA, vw43 = NA,
                                     dom_vw44 = NA, vw44 = NA,
                                     dom_vw45 = NA, vw45 = NA,
                                     dom_vw46 = NA, vw46 = NA,
                                     verbose = FALSE, force = FALSE) {

  # convert ages - probably a more elegant way of doing this ..
  if (any(nchar(dom_vw41) >= 8 & !is.na(dom_vw41)))
    age_vw41 <- date2age(dob, dom_vw41) else age_vw41 <- dom_vw41
  if (any(nchar(dom_vw42) >= 8 & !is.na(dom_vw42)))
    age_vw42 <- date2age(dob, dom_vw42) else age_vw42 <- dom_vw42
  if (any(nchar(dom_vw43) >= 8 & !is.na(dom_vw43)))
    age_vw43 <- date2age(dob, dom_vw43) else age_vw43 <- dom_vw43
  if (any(nchar(dom_vw44) >= 8 & !is.na(dom_vw44)))
    age_vw44 <- date2age(dob, dom_vw44) else age_vw44 <- dom_vw44
  if (any(nchar(dom_vw45) >= 8 & !is.na(dom_vw45)))
    age_vw45 <- date2age(dob, dom_vw45) else age_vw45 <- dom_vw45
  if (any(nchar(dom_vw46) >= 8 & !is.na(dom_vw46)))
    age_vw46 <- date2age(dob, dom_vw46) else age_vw46 <- dom_vw46

  # create tibble with vW responses
  df <- tibble(dom = dom_vw41, age = age_vw41, vw41) %>%
    full_join(tibble(dom = dom_vw42, age = age_vw42, vw42), by = c("dom", "age")) %>%
    full_join(tibble(dom = dom_vw43, age = age_vw43, vw43), by = c("dom", "age")) %>%
    full_join(tibble(dom = dom_vw44, age = age_vw44, vw44), by = c("dom", "age")) %>%
    full_join(tibble(dom = dom_vw45, age = age_vw45, vw45), by = c("dom", "age")) %>%
    full_join(tibble(dom = dom_vw46, age = age_vw46, vw46), by = c("dom", "age"))


  # return early if data are insufficient
  if (all(is.na(df$dom))) return(4015)
  if (all(nchar(df$dom) >= 8) & is.na(dob)) return(4016)
  if (all(is.na(df$age))) return(4015)

  age1 <- max(df$age, na.rm = TRUE)
  if (age1 < 1.9166) return(4021)
  if (age1 > 3.3333) return(4022)
  if ((age1 >= 2.3333 && age1 < 2.4166) | (age1 >= 2.8333 && age1 < 2.9166)) return(4023)

  # vW to dichotomous variable
  df <- df %>%
    mutate_at(vars(starts_with("vw")), function(x) ifelse(x == "2", 0, ifelse(x %in% c("1", "3"), 1, NA_real_)))


  # start the sieve
  if (age1 >= 1.9166 && age1 < 2.3333) {
    # age 2
    df <- df %>%
      filter(age >= 1.9166 & age < 2.3333 & !is.na(age)) %>%
      arrange(-age) %>%
      fill(vw41, vw42, .direction = "up") %>%
      slice(1) %>%
      mutate(score = vw41 + vw42)
    if(is.na(df$score)) return(4011)
    if(df$score == 0) return(4041)
    if(df$score == 1) return(4042)
  }


  if (age1 >= 2.4166 && age1 < 2.8333) {
    # age 2.5 - check if neccesary
    score_2 <- df %>%
      filter(age >= 1.9166 & age < 2.3333 & !is.na(age)) %>%
      arrange(-age) %>%
      fill(vw41, vw42, .direction = "up") %>%
      slice(1) %>%
      transmute(score_2 = vw41 + vw42) %>%
      unlist

    if(is.na(score_2) || length(score_2) < 1) return(4012)

    # age 2.5
    if(score_2 < 2) {
      df <- df %>%
        filter(age >= 2.4166 & age < 2.8333 & !is.na(age)) %>%
        arrange(-age) %>%
        fill(vw41, vw43, vw44, .direction = "up") %>%
        slice(1) %>%
        mutate(score = vw41 + vw43 + vw44)
      if(is.na(df$score)) return(4011)
      if(df$score < 3) return(4041)
    }
  }

  if (age1 >= 2.8333 && age1 < 3.3333) {
    # age 3
    df <- df %>%
      filter(age >= 2.8333 & age < 3.3333 & !is.na(age)) %>%
      arrange(-age) %>%
      fill(vw45, vw46, .direction = "up") %>%
      slice(1) %>%
      mutate(score = vw45 + vw46)
    if(is.na(df$score)) return(4011)
    if(df$score == 0) return(4041)
    if(df$score == 1) return(4043)
  }
  # signal everything is alright
  return(4031)
}