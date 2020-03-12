create_bmi_table <- function() {
  male <- read.table("data-raw/data/IOTF_male.txt", header = TRUE, stringsAsFactors = FALSE)
  female <- read.table("data-raw/data/IOTF_female.txt", header = TRUE, stringsAsFactors = FALSE)
  bmi_table <- rbind(male, female)
  bmi_table$sex <- rep(c("male", "female"), each = 33)
  colnames(bmi_table)[2] <- "age"
  bmi_table
}
