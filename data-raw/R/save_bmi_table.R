bmi_table <- read.table(file = "data-raw/data/cutoffs.txt", header = TRUE)
usethis::use_data(bmi_table, overwrite = TRUE)
