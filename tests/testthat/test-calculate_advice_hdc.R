kids <- tibble(
  dom = list(
    NA, NA, NA, 1.3662, 0.3669, 0.3669, 0.3669, c(0.2847, 0.3669), c(0.2847, 0.3669)
  ),
  y = list(
    NA, NA, NA, 43, 47, 35, c(41, 43), c(41, 43), c(42, 43)
  ),
  sex  = c(NA_character_, "male", "male", "female", "female",
           "female", "female", "female", "female"),
  ga   = c(NA, NA, NA, NA, 33, 33, NA, NA, NA),
  code = c(3019, 3015, 3015, 3021, 3041, 3043, 3022, 3044, 3031),
  stringsAsFactors = FALSE)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_hdc(sex  = kids[k, "sex"],
                                ga   = kids[k, "ga"],
                                dom = unlist(kids[k, "dom"]),
                                y   = unlist(kids[k, "y"]))
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})
