context("calculate_advice_hgt")

kids <- data.frame(
  dob  = c(as.Date("2018-07-31"), NA, NA, NA, as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"),
           as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31")),
  dom1 = c(as.Date("2018-12-12"), NA, NA, NA, NA, as.Date("2018-12-12"), as.Date("2018-12-12"), as.Date("2018-12-12"), as.Date("2018-12-12"), as.Date("2018-12-12"),
           as.Date("2018-12-12"), as.Date("2018-12-12"), as.Date("2018-12-12")),
  dom0 = c(as.Date("2019-03-01"), NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA, NA),
  y1   = c(64, NA, NA, NA, NA, NA, 64, 64, 40, 40,
           40, 40, 75),
  y0   = c(60, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA, NA),
  sex  = c("male", NA_character_, "X", "female", "female", "female", "female", "female", "female", "female",
           "female", "female", "female"),
  bw   = c(3000, NA, NA, NA, NA, NA, NA, NA, NA, 3000,
           2000, 1400, 2000),
  bl   = c(50, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA, NA),
  ga   = c(40, NA, NA, NA, NA, NA, NA, NA, NA, 40,
           36, 36, NA),
  etn  = c("NL", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "NL", "NL", "NL",
           "NL", "NL", "NL"),
  hgtf = c(180, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA, NA),
  hgtm = c(170, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA, NA),
  code = c(1031, 1019, 1019, 1016, 1015, 1018, 1025, 1025, 1013, 1045,
           1061, 1025, 1048),
  stringsAsFactors = FALSE)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_hgt(sex  = kids[k, "sex"],
                                dob  = kids[k, "dob"],
                                bw   = kids[k, "bw"],
                                bl   = kids[k, "bl"],
                                ga   = kids[k, "ga"],
                                etn  = kids[k, "etn"],
                                hgtf = kids[k, "hgtf"],
                                hgtm = kids[k, "hgtm"],
                                dom1 = kids[k, "dom1"],
                                y1   = kids[k, "y1"],
                                dom0 = kids[k, "dom0"],
                                y0   = kids[k, "y0"])
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

