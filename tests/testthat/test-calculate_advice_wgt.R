# create tibble with observations and expected outcome
kids <- tibble(
  dob = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "01012020"),
  dom = list(
    NA, NA, NA, c(NA, 0.3669), c(NA, 0.3669),
    1.3662, 1.3662, 3.3676, 3.3676, c(NA, 3.3676)
  ),
  y = list(
    NA, NA, NA, c(5.4, 6.0), c(5.4, 5.5), 13, 10, 20, 13, c(17, 16)
  ),
  dom_hgt = list(
    NA, NA, NA, c(NA, 0.3669), c(NA, 0.3669), c(NA, 1.3662), c(NA, 1.3662),
    c(NA, 3.3676), c(NA, 3.3676), c(NA, 3.3676)
  ),
  hgt = list(
    NA, NA, NA, c(59, 60), c(59, 65), c(NA, 85), c(NA, 85), c(NA, 105), c(NA, 105), c(102, 105)
  ),
  sex  = c(NA_character_, "male", "male", "female", "female", "female",
           "female", "female", "female", "female"),
  ga   = c(NA, NA, NA, NA, NA, 33, 33, NA, NA, NA),
  code = c(2019, 2015, 2015, 2031, 2074, 2073, 2012, 2045, 2074, 2075)
  )

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_wgt(sex  = unlist(kids[k, "sex"]),
                                ga   = kids[k, "ga"],
                                dob = kids[k, "dob"],
                                dom = unlist(kids[k, "dom"]),
                                y   = unlist(kids[k, "y"]),
                                dom_hgt = unlist(kids[k, "dom_hgt"]),
                                hgt   = unlist(kids[k, "hgt"]))
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

