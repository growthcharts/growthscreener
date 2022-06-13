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
#'                                 dat_vw41 = "01012022", vw41 = 2,
#'                                 dat_vw42 = "01012022", vw42 = 1)
#' msg(msgcode)
#' @export
calculate_advice_devlang <- function(dob = NA_integer_,
                                     dat_vw41 = NA, vw41 = NA,
                                     dat_vw42 = NA, vw42 = NA,
                                     dat_vw43 = NA, vw43 = NA,
                                     dat_vw44 = NA, vw44 = NA,
                                     dat_vw45 = NA, vw45 = NA,
                                     dat_vw46 = NA, vw46 = NA,
                                     verbose = FALSE, force = FALSE) {

  dob <- as.Date(gsub("(-)|(/)", "", dob), "%d%m%Y")

  # create data.frame with vW responses
  df <- data.frame(date = as.Date(gsub("(-)|(/)", "", dat_vw41), "%d%m%Y"), vw41) %>%
    full_join(data.frame(date = as.Date(gsub("(-)|(/)", "", dat_vw42), "%d%m%Y"), vw42), by = "date") %>%
    full_join(data.frame(date = as.Date(gsub("(-)|(/)", "", dat_vw43), "%d%m%Y"), vw43), by = "date") %>%
    full_join(data.frame(date = as.Date(gsub("(-)|(/)", "", dat_vw44), "%d%m%Y"), vw44), by = "date") %>%
    full_join(data.frame(date = as.Date(gsub("(-)|(/)", "", dat_vw45), "%d%m%Y"), vw45), by = "date") %>%
    full_join(data.frame(date = as.Date(gsub("(-)|(/)", "", dat_vw46), "%d%m%Y"), vw46), by = "date") %>%
    mutate(age = as.numeric(round((date - dob)/365.25, 4)))

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