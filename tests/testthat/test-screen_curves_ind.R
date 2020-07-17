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

# test battery
# path <- system.file("extdata", package = "jamestest")
# libs <- c("allegrosultum", "test", "smocc", "terneuzen", "preterm", "graham")
# for (lib in libs) {
#   files <- list.files(path = file.path(path, lib), pattern = ".json", full.names = TRUE)
#   for (file in files) {
#     cat("File ", file, "\n")
#     if (file == "/Users/buurensv/Library/R/4.0/library/jamestest/extdata/test/test14.json") next
#     if (file == "/Users/buurensv/Library/R/4.0/library/jamestest/extdata/test/test8.json") next
#     js  <- jsonlite::toJSON(jsonlite::fromJSON(file), auto_unbox = TRUE)
#     ind <- suppressMessages(minihealth::convert_bds_individual(txt = js))
#     test_that(paste(file, "passes"), {
#       expect_silent(y <- screen_curves_ind(ind))
#     })
#   }
# }
