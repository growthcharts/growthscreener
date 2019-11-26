context("calculate_advice_hgt")

kids <- data.frame(
  dob  = c(as.Date("2018-07-31"), NA, NA, NA, as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"), as.Date("2018-07-31"),
           as.Date("2018-07-31"), as.Date("2018-07-31")),
  dom1 = c(as.Date("2018-12-12"), NA, NA, NA, NA, as.Date("2018-12-12"), as.Date("2018-12-12"), as.Date("2018-12-12"), as.Date("2018-12-12"), as.Date("2018-12-12"),
           as.Date("2018-12-12"), as.Date("2018-12-12")),
  dom0 = c(as.Date("2019-03-01"), NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA),
  y1   = c(64, NA, NA, NA, NA, NA, 64, 64, 40, 40,
           40, 75),
  y0   = c(60, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA),
  sex  = c("M", NA_character_, "X", "F", "F", "F", "F", "F", "F", "F",
           "F", "F"),
  bw   = c(3000, NA, NA, NA, NA, NA, NA, NA, NA, 3000,
           2000, 2000),
  bl   = c(50, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA),
  ga   = c(40, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA),
  etn  = c("N", NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, "N", "N", "N",
           "N", "N"),
  hgtf = c(180, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA),
  hgtm = c(170, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, NA),
  code = c(31, 19, 19, 16, 15, 18, 20, 31, 13, 45,
           31, 48),
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

