library(readxl)
library(AGD)
library(usethis)

path <- path.expand("~/Package/growthscreener/growthscreener/data-raw")

source(file.path(path, "R", "create_ref.nl5def.R"))
source(file.path(path, "R", "create_ref.nl5defSGAgewicht.R"))
source(file.path(path, "R", "create_ref.nl5defSGAlengte.R"))

ref.nl5defSGAgewicht <- create_ref.nl5defSGAgewicht()
ref.nl5defSGAlengte <- create_ref.nl5defSGAlengte()

ref.nl5def <- create_ref.nl5def()
nl45.hgt <- rbind(nl4.hgt, ref.nl5def)
nl5.hgt <- nl45.hgt[nl45.hgt[, 1] == "nl5", ]

fn <- file.path(path, "data", "Hindo.xlsx")
Hindo.hgt <- data.frame(read_excel(fn))
nl5.hgt <- rbind(nl5.hgt, Hindo.hgt)

usethis::use_data(nl5.hgt, ref.nl5defSGAgewicht, ref.nl5defSGAlengte,
                  overwrite = TRUE)
