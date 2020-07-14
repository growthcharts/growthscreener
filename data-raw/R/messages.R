messages <- read.table(file = file.path("data-raw", "data", "messages.txt"),
                  sep = "\t", header = TRUE, stringsAsFactors = FALSE)
usethis::use_data(messages, overwrite = TRUE)
