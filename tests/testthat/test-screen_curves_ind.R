context("screen_curves_ind")

# client3.json
fn  <- system.file("extdata", "allegrosultum", "client3.json", package = "jamestest")
js  <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)
ind <- minihealth::convert_bds_individual(txt = js)
test_that("screens client3.json", {
  expect_silent(y <- screen_curves_ind(ind))})


# 2 problematic json files identified by Allegro Sultum - Feb 2020
fn  <- system.file("extdata", "test", "not_a_vector.json", package = "jamestest")
js  <- jsonlite::toJSON(jsonlite::fromJSON(fn), auto_unbox = TRUE)
ind <- minihealth::convert_bds_individual(txt = js)
test_that("screens client3.json", {
  expect_silent(y <- screen_curves_ind(ind))})
