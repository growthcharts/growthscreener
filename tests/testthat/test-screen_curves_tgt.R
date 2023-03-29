# client3.json
fn  <- system.file("extdata", "allegrosultum", "client3.json", package = "jamesdemodata")
tgt <- bdsreader::read_bds(fn)
test_that("screens client3.json", {
  expect_silent(screen_curves_tgt(tgt))
})


# 2 problematic json files identified by Allegro Sultum - Feb 2020
fn  <- system.file("extdata", "bds_v1.0", "test", "not_a_vector.json", package = "jamesdemodata")
tgt <- bdsreader::read_bds(fn)
test_that("screens not_a_vector.json", {
  expect_silent(screen_curves_tgt(tgt))
})

# test battery
path <- system.file("extdata", "bds_v1.0", package = "jamesdemodata")
libs <- c("allegrosultum", "test", "smocc", "terneuzen", "preterm", "graham")
for (lib in libs) {
  files <- list.files(path = file.path(path, lib), pattern = ".json", full.names = TRUE)
  for (file in files) {
    # cat("File ", file, "\n")
    if (file == file.path(path, "allegrosultum/antwoordbericht.json")) next
    if (file == file.path(path, "test/test14.json")) next
    if (file == file.path(path, "test/test8.json")) next
    if (file == file.path(path, "test/http400.json")) next
    if (is.na(file)) next
    tgt <- bdsreader::read_bds(file)
    test_that(paste(file, "passes"), {
      expect_silent(screen_curves_tgt(tgt))
    })
  }
}
