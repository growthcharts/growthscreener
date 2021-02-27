
library(clopus)
individual <- minihealth::donordata_to_individual(dnr = "smocc", id = 10022)

usethis::use_data(individual)
