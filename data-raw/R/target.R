fn <- system.file("examples", "Laura_S.json", package = "bdsreader")
target <- bdsreader::read_bds(fn)
usethis::use_data(target, overwrite = TRUE)
