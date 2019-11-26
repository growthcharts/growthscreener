path <- path.expand("~/Package/growthscreener/growthscreener")

# Added message 20 by hand on 2019-08-12, so we need to work for
# this file now.
messages_hgt <- read.table(file = file.path(path, "data-raw", "data", "messages_hgt.txt"),
                  sep = "\t", header = TRUE, stringsAsFactors = FALSE)
usethis::use_data(messages_hgt, overwrite = TRUE)
