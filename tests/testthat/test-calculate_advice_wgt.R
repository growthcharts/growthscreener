#sex = NA_character_,
#ga = NA, etn = NA,
#date = NA_integer_, y = NA,
#date_hgt = NA_integer_, hgt = NA,
#dob = NA_character_,

# create dates from age in days
dob <- "01-01-2020"
calculate_date <- function(dob, date) {
  x <- as.Date(unlist(dob), format = "%d-%m-%Y") + date
  format(x, format = "%d-%m-%Y")
}

kids <- tibble(
  dob = dob,
  date = list(
    NA, NA, NA, c(NA, 134), c(NA, 134), 499, 499, 1230, 1230, c(NA, 1230)
  ),
  y = list(
    NA, NA, NA, c(5.4, 6.0), c(5.4, 5.5), 13, 10, 20, 13, c(17, 16)
  ),
  date_hgt = list(
    NA, NA, NA, c(NA, 134), c(NA, 134), c(NA, 499), c(NA, 499), c(NA, 1230), c(NA, 1230), c(NA, 1230)
  ),
  hgt = list(
    NA, NA, NA, c(59, 60), c(59, 65), c(NA, 85), c(NA, 85), c(NA, 105), c(NA, 105), c(102, 105)
  ),
  #dom1 = c(NA, NA, NA, 134, 134, 499, 499, 1230, 1230, 1230),
  #dom0 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  #y1   = c(NA, NA, NA, 6.0, 5.5, 13, 10, 20, 13, 16),
  #y0   = c(NA, NA, NA, 5.4, 5.4, NA, NA, NA, NA, 17),
  #hgt0 = c(NA, NA, NA, 59, 59, NA, NA, NA, NA, 102),
  #hgt1 = c(NA, NA, NA, 60, 65, 85, 85, 105, 105, 105),
  sex  = c(NA_character_, "male", "male", "female", "female", "female",
           "female", "female", "female", "female"),
  ga   = c(NA, NA, NA, NA, NA, 33, 33, NA, NA, NA),
  code = c(2019, 2015, 2015, 2031, 2074, 2073, 2011, 2045, 2074, 2075),
  stringsAsFactors = FALSE)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_wgt(sex  = unlist(kids[k, "sex"]),
                                ga   = kids[k, "ga"],
                                dob = kids[k, "dob"],
                                date = calculate_date(kids[k, "dob"],
                                                      unlist(kids[k, "date"])),
                                y   = unlist(kids[k, "y"]),
                                date_hgt = calculate_date(kids[k, "dob"],
                                                      unlist(kids[k, "date_hgt"])),
                                hgt   = unlist(kids[k, "hgt"]))
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

