context("calculate_advice_hdc")

kids <- data.frame(
  dob  = c(NA, NA, as.Date("2018-07-31"), as.Date("2018-07-31"),
           as.Date("2018-07-31"), as.Date("2018-07-31"),
           as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31")),
  dom1 = c(NA, NA, NA, as.Date("2019-12-12"), as.Date("2018-12-12"),
           as.Date("2018-12-12"), as.Date("2018-12-12"),
           as.Date("2018-12-12"), as.Date("2018-12-12")),
  dom0 = c(NA, NA, NA, NA, NA, NA, NA, as.Date("2018-11-12"), as.Date("2018-11-12")),
  y1   = c(NA, NA, NA, 43, 47, 35, 43, 43, 43),
  y0   = c(NA, NA, NA, NA, NA, NA, 41, 41, 42),
  sex  = c(NA_character_, "male", "male", "female", "female",
           "female", "female", "female", "female"),
  ga   = c(NA, NA, NA, NA, 33, 33, NA, NA, NA),
  code = c(3019, 3016, 3015, 3021, 3041, 3043, 3022, 3044, 3031),
  etn = c("NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL", "NL"),
  stringsAsFactors = FALSE)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_hdc(sex  = kids[k, "sex"],
                                dob  = kids[k, "dob"],
                                ga   = kids[k, "ga"],
                                etn  = kids[k, "etn"],
                                dom1 = kids[k, "dom1"],
                                y1   = kids[k, "y1"],
                                dom0 = kids[k, "dom0"],
                                y0   = kids[k, "y0"])
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

