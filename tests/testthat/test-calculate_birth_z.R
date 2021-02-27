sex <- "male"
bw  <- 3000
ga <- 40
yname <- "bw"

test_that("functions as intended", {
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga),
               c(-2.882, -1.245))
})

test_that("sets extreme bw to NA", {
  expect_equal(is.na(calculate_birth_z(c(NA, -1000, NaN, NULL), sex, ga)),
               rep(TRUE, 3))
})

test_that("takes bw as character", {
  expect_equal(calculate_birth_z(c("2000", "0", "-1000"), sex, ga),
               c(-2.882, -5.833, NA))
})

test_that("sets extreme bw to NA", {
  expect_equal(is.na(calculate_birth_z(c("NULL", "NA", "", "-", "!"), sex, ga)),
               rep(TRUE, 5))
})

test_that("sets invalid sex to NA", {
  expect_equal(calculate_birth_z(c(2000, 3000), sex = "X", ga),
               rep(NA, 2))
  expect_equal(calculate_birth_z(c(2000, 3000), sex = "", ga),
               rep(NA, 2))
  expect_equal(calculate_birth_z(c(2000, 3000), sex = TRUE, ga),
               rep(NA, 2))
  expect_equal(calculate_birth_z(c(2000, 3000), sex = 0, ga),
               rep(NA, 2))
})

test_that("sets invalid ga to NA", {
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = 25),
               c(5.301, 10.657))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = 25.8),
               c(5.301, 10.657))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = 24),
               rep(NA, 2))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = 43),
               rep(NA, 2))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = "25"),
               c(5.301, 10.657))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = "25.8"),
               c(5.301, 10.657))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = "24"),
               rep(NA, 2))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = "43"),
               rep(NA, 2))
  expect_equal(calculate_birth_z(c(2000, 3000), sex, ga = ""),
               rep(NA, 2))
})

