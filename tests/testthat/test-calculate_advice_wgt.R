kids <- data.frame(
  dom1 = c(NA, NA, NA, 134, 134, 499, 499, 1230, 1230, 1230),
  dom0 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  y1   = c(NA, NA, NA, 6.0, 5.5, 13, 10, 20, 13, 16),
  y0   = c(NA, NA, NA, 5.4, 5.4, NA, NA, NA, NA, 17),
  sex  = c(NA_character_, "male", "male", "female", "female", "female",
           "female", "female", "female", "female"),
  ga   = c(NA, NA, NA, NA, NA, 33, 33, NA, NA, NA),
  hgt0 = c(NA, NA, NA, 59, 59, NA, NA, NA, NA, 102),
  hgt1 = c(NA, NA, NA, 60, 65, 85, 85, 105, 105, 105),
  code = c(2019, 2015, 2015, 2031, 2074, 2073, 2011, 2045, 2074, 2075),
  stringsAsFactors = FALSE)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_wgt(sex  = kids[k, "sex"],
                                ga   = kids[k, "ga"],
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

