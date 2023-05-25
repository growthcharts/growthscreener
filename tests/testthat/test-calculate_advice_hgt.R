# create tibble with observations and expected outcome
kids <- tibble(
  dob = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "20200101"),
  dom = list(
    c(0.3669, 0.5832),
    NA, NA, NA, NA, 0.3669, 0.3669, 0.3669, 0.3669, 0.3669, 0.3669, 0.3669, "20200514"
  ),
  y = list(
    c(60, 64),
    NA, NA, NA, NA, NA, 64, 64, 40, 40, 40, 40, 75
  ),
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
  code = c(1031, 1019, 1019, 1015, 1015, 1018, 1025, 1025, 1013, 1045,
           1061, 1025, 1048),
  stringsAsFactors = FALSE)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_hgt(sex  = kids[k, "sex"],
                                bw   = kids[k, "bw"],
                                bl   = kids[k, "bl"],
                                ga   = kids[k, "ga"],
                                etn  = unlist(kids[k, "etn"]),
                                hgtf = kids[k, "hgtf"],
                                hgtm = kids[k, "hgtm"],
                                dob = kids[k, "dob"],
                                dom = unlist(kids[k, "dom"]),
                                y   = unlist(kids[k, "y"]))
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

