#' Pick ideal date for next visitation
#'
#' The function picks the best date for a next visitation in the JGZ based on
#' what has already been done and what still needs doing.
calculate_advice_date <- function(age, bds_df) {
	# check incoming data
	if(age < 0 | age > 18) return("")
	if(!is.data.frame(bds_df)) return("")
	if(!"bds" %in% colnames(bds_df) | !"time" %in% colnames(bds_df)) return("")

	# filter what's relevant
	now <- reminder_table %>% filter(time_l <= age & age <= time_r) %>% mutate(done = FALSE)
	later <- reminder_table %>% filter(age < time_l) %>% arrange(time_l)

	# filter out topics from now that have already been done
	# for (i in 1:nrow(now)) {
	# 	# check for empty measurements
	# 	if (nrow(bds_df) == 0L) break
	#
	# 	# has the i-th measurement been done?
	# 	bds <- now[i, "bds"]
	#
	#
	# 	if (any(done[i, "time_l"] <= done & done < done[i, "time_r"])) {
	# 		# remove first matching measured time
	# 		done <- done[-which(doing[i, "time_l"] <= done & done < doing[i, "time_r"])[1]]
	# 	} else {
	# 		remind_ausc <- doing[i, "time_r"]
	# 		break
	# 	}
	# }
	# if (is.na(remind_ausc)) remind_ausc <- -999
}

# voorbeeld data
bds_df <- data.frame(
	bds = c(885),
	time = c(0.8)
)

# helper functions
wks <- function(x) {(x*7)/365.25}
mnths <- function(x) {(x*(365.25 / 12))/365.25}