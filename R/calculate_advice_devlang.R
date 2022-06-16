#' Referral advice for language development
#'
#' This function traverses a decision tree for language development based on the
#' van Wiechen questionnaire for child development.
#'
#'
#' @param y1    Head circumference at last measurement (cm)
#' @param y0    Head circumference at previous measurement (cm)
#' @inheritParams calculate_advice_hgt
#' @return `calculate_advice_devlang` returns an integer, the `msgcode`, between
#'   3000-3999.
#' @author Arjan Huizing, Stef van Buuren, 2020
#' @seealso calculate_helpers
#' @rdname advice_devlang
#' @examples
#' msg(calculate_advice_devlang())
#' msgcode <- calculate_advice_devlang(dob = "01012020",
#'                                 dom_vw41 = "01012022", vw41 = 2,
#'                                 dom_vw42 = "01012022", vw42 = 1)
#' msg(msgcode)
#' @export
calculate_advice_devlang <- function(dob = NA_integer_,
                                     dom_vw41 = NA, vw41 = NA,
                                     dom_vw42 = NA, vw42 = NA,
                                     dom_vw43 = NA, vw43 = NA,
                                     dom_vw44 = NA, vw44 = NA,
                                     dom_vw45 = NA, vw45 = NA,
                                     dom_vw46 = NA, vw46 = NA,
                                     verbose = FALSE, force = FALSE) {

  # convert ages - probably a more elegant way of doing this ..
  age_vw41 <- ifelse(all(nchar(dom_vw41) >= 8), date2age(dob, dom_vw41), dom_vw41)
  age_vw42 <- ifelse(all(nchar(dom_vw42) >= 8), date2age(dob, dom_vw42), dom_vw42)
  age_vw43 <- ifelse(all(nchar(dom_vw43) >= 8), date2age(dob, dom_vw43), dom_vw43)
  age_vw44 <- ifelse(all(nchar(dom_vw44) >= 8), date2age(dob, dom_vw44), dom_vw44)
  age_vw45 <- ifelse(all(nchar(dom_vw45) >= 8), date2age(dob, dom_vw45), dom_vw45)
  age_vw46 <- ifelse(all(nchar(dom_vw46) >= 8), date2age(dob, dom_vw46), dom_vw46)

  # create data.frame with vW responses
  df <- data.frame(age = age_vw41, vw41) %>%
    full_join(data.frame(age = age_vw42, vw42), by = "age") %>%
    full_join(data.frame(age = age_vw43, vw43), by = "age") %>%
    full_join(data.frame(age = age_vw44, vw44), by = "age") %>%
    full_join(data.frame(age = age_vw45, vw45), by = "age") %>%
    full_join(data.frame(age = age_vw46, vw46), by = "age")

  # return early if data is insufficient
  if (is.na(dob)) return(4001)
  if (all(is.na(df$date))) return(4002)

  age1 <- max(df$age, na.rm = TRUE)
  if (age1 < 1.9166) return(4003)
  if (age1 > 3.3333) return(4004)
  if ((age1 >= 2.3333 && age1 < 2.4166) | (age1 >= 2.8333 && age1 < 2.9166)) return(4005)

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
      head(1) %>%
      mutate(score = vw41 + vw42)
    if(is.na(df$score)) return(4011)
    if(df$score == 0) return(4041)
    if(df$score == 1) return(4042)
  }


  if (age1 >= 2.4166 && age1 < 2.8333) {
    # age 2.5 - check if neccesary
    df <- df %>%
      filter(age >= 1.9166 & age < 2.3333 & !is.na(age)) %>%
      arrange(-age) %>%
      fill(vw41, vw42, .direction = "up") %>%
      head(1) %>%
      mutate(score_2 = vw41 + vw42)

    if(is.na(df$score)) return(4012)

    # age 2.5
    if(score_2 < 2) {
      df <- df %>%
        filter(age >= 2.4166 & age < 2.8333 & !is.na(age)) %>%
        arrange(-age) %>%
        fill(vw41, vw43, vw44, .direction = "up") %>%
        head(1) %>%
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
      head(1) %>%
      mutate(score = vw45 + vw46)
    if(is.na(df$score)) return(4011)
    if(df$score == 0) return(4041)
    if(df$score == 1) return(4043)
  }
  # signal everything is alright
  return(4031)
}