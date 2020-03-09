context("calculate_advice_wgt")

kids <- data.frame(
  dob  = c(NA, NA, as.Date("2018-07-31"), as.Date("2018-07-31"),
           as.Date("2018-07-31"), as.Date("2018-07-31"),
           as.Date("2015-07-31"), as.Date("2015-07-31"), as.Date("2015-07-31")),
  dom1 = c(NA, NA, NA, as.Date("2018-12-12"), as.Date("2019-12-12"),
           as.Date("2019-12-12"), as.Date("2018-12-12"),
           as.Date("2018-12-12"), as.Date("2018-12-12")),
  dom0 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
  y1   = c(NA, NA, NA, 6.0, 13, 10, 20, 13, 16),
  y0   = c(NA, NA, NA, 5.4, NA, NA, NA, NA, 17),
  sex  = c(NA_character_, "male", "male", "female", "female",
           "female", "female", "female", "female"),
  ga   = c(NA, NA, NA, NA, 33, 33, NA, NA, NA),
  hgt0 = c(NA, NA, NA, 59, NA, NA, NA, NA, 102),
  hgt1 = c(NA, NA, NA, 60, 85, 85, 105, 105, 105),
  code = c(2019, 2016, 2015, 2041, 2043, 2012, 2045, 2052, 2053),
  etn = c("NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL"),
  stringsAsFactors = FALSE)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_wgt(sex  = kids[k, "sex"],
                                dob  = kids[k, "dob"],
                                ga   = kids[k, "ga"],
                                etn  = kids[k, "etn"],
                                hgt0 = kids[k, "hgt0"],
                                hgt1 = kids[k, "hgt1"],
                                dom1 = kids[k, "dom1"],
                                y1   = kids[k, "y1"],
                                dom0 = kids[k, "dom0"],
                                y0   = kids[k, "y0"])
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

