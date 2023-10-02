# create tibble with observations and expected outcome
kids <- tibble(
  age = c(0, 0.5, 1, 1, 4),
  bds_df = list(data.frame(bds = numeric(0), time = numeric(0)),
  							data.frame(bds = c(855, 390, 392, 398), time = 0.45),
  							data.frame(bds = 855, time = c(0.45, 0.9)),
  							data.frame(bds = numeric(0), time = numeric(0)),
  							data.frame(bds = 855, time = 0.45)),
  expected = c(21, 0, 30.4375, 30.4375, 0)
  )


# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- ((growthscreener:::calculate_advice_date(age = unlist(kids[k, "age"]),
  																								 bds_df = kids[k, "bds_df"][[1]]
  )$window %>% unlist ) - as.numeric(Sys.Date()))[1]
  results[k, ] <- c(k, kids$expected[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

