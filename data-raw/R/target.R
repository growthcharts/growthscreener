fn <- system.file("extdata/bds_v2.0/smocc/Laura_S.json", package = "jamesdemodata")
target <- bdsreader::read_bds(fn)
usethis::use_data(target, overwrite = TRUE)
