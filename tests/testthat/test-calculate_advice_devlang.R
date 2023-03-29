# create tibble with observations and expected outcome
kids <- tibble(
  dob = c(NA, NA, NA, NA, NA, "01012020"),
  vw41 = list(
    1, NA, 2, NA, c(2, 3), NA
  ),
  vw42 = c(1, 2, 1, NA, 2, NA),
  vw43 = c(NA, NA, NA, 1, 1, NA),
  vw44 = c(NA, NA, NA, 1, 1, NA),
  vw45 = c(NA, NA, NA, NA, NA, 1),
  vw46 = c(NA, NA, NA, NA, NA, 2),
  dom_vw41 = list(
    2, NA, 2, NA, c(2.3, 2.5), NA
  ),
  dom_vw42 = c(2, NA, 2, NA, 2.3, NA),
  dom_vw43 = c(NA, NA, NA, 2.5, 2.5, NA),
  dom_vw44 = c(NA, NA, NA, 2.5, 2.5, NA),
  dom_vw45 = c(NA, NA, NA, NA, NA, 3),
  dom_vw46 = c(NA, NA, NA, NA, NA, 3),
  code = c(4031, 4015, 4042, 4012, 4031, 4043)
)

# apply algorithm to kids
results <- matrix(NA, nrow = nrow(kids), ncol = 3)
colnames(results) <- c("k", "expected", "found")
for (k in 1:nrow(kids)) {
  found <- calculate_advice_devlang(dob = kids[k, "dob"],
                                    vw41 = unlist(kids[k, "vw41"]),
                                    dom_vw41 = unlist(kids[k, "dom_vw41"]),
                                    vw42 = unlist(kids[k, "vw42"]),
                                    dom_vw42 = unlist(kids[k, "dom_vw42"]),
                                    vw43 = unlist(kids[k, "vw43"]),
                                    dom_vw43 = unlist(kids[k, "dom_vw43"]),
                                    vw44 = unlist(kids[k, "vw44"]),
                                    dom_vw44 = unlist(kids[k, "dom_vw44"]),
                                    vw45 = unlist(kids[k, "vw45"]),
                                    dom_vw45 = unlist(kids[k, "dom_vw45"]),
                                    vw46 = unlist(kids[k, "vw46"]),
                                    dom_vw46 = unlist(kids[k, "dom_vw46"])
                                    )
  results[k, ] <- c(k, kids$code[k], found)
}

test_that("expected equals found", {
  expect_equal(results[, "expected"], results[, "found"])
})

