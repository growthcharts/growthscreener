#' Referral advice for language development
#'
#' This function traverses a decision tree for language development based on the
#' van Wiechen questionnaire.
#'
#'
#' @param y1    Head circumference at last measurement (cm)
#' @param y0    Head circumference at previous measurement (cm)
#' @inheritParams calculate_advice_hgt
#' @return `calculate_advice_hdc` returns an integer, the `msgcode`, between
#'   3000-3999.
#' @author Arjan Huizing, Stef van Buuren, 2020
#' @seealso calculate_helpers
#' @rdname advice_hdc
#' @examples
#' msg(calculate_advice_hdc())
#' msgcode <- calculate_advice_hdc(sex = "female",
#'                                 dom1 = 243, y1 = 40,
#'                                 dom0 = 91, y0 = 36)
#' msg(msgcode)
#' @export
calculate_advice_devlang <- function(dob = NA_integer_,
                                     dat_vw41 = NA, vw41 = NA,
                                     dat_vw42 = NA, vw42 = NA,
                                     dat_vw43 = NA, vw43 = NA,
                                     dat_vw44 = NA, vw44 = NA,
                                     dat_vw45 = NA, vw45 = NA,
                                     dat_vw46 = NA, vw46 = NA,
                                     verbose = FALSE) {

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
  if (is.na(dob)) return(3001)
  if (all(is.na(df$date))) return(3002)

  age1 <- max(df$age, na.rm = TRUE)
  if (age1 < 1.9166) return(3003)
  if (age1 > 3.3333) return(3004)
  if ((age1 >= 2.3333 && age1 < 2.4166) | (age1 >= 2.8333 && age1 < 2.9166)) return(3005)

  # determine vW moment
  df <- df %>%
    select(case_when(
      age1 >= 1.9166 && age < 2.3333 ~ c("age", "vw41", "vw42"),
      age1 >= 2.4166 && age < 2.8333 ~ c("age", "vw41", "vw42", "vw43", "vw44"),
      age1 >= 2.9166 && age < 3.3333 ~ c("age", "vw45", "vw46"),
      TRUE ~ c("age")
    ))

  # start the sieve
}