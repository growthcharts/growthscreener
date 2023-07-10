#' Pick ideal date for next visitation
#'
#' The function picks the best date for a next visitation in the JGZ based on
#' what has already been done and what still needs doing.
#'
#' @param age     Age of child in decimal years
#' @param bds_df  \code{data.frame} object with the named columns \code{bds}
#'   containing bds numbers and \code{time} containing decimal years.
#' @author Arjan Huizing, 2023
#' @rdname advice_date
calculate_advice_date <- function(age, bds_df = NULL) {
	reminder_table <- growthscreener::reminder_table
	if(is.null(bds_df)) bds_df <- data.frame(bds = numeric(0), time = numeric(0))
	# check incoming data
	if(age < 0 | age > 18) return("")
	if(!is.data.frame(bds_df)) bds_df <- data.frame(bds = numeric(0), time = numeric(0))
	if(!"bds" %in% colnames(bds_df) | !"time" %in% colnames(bds_df))  bds_df <- data.frame(bds = numeric(0), time = numeric(0))

	# arrange bds_df by time
	bds_df <- bds_df %>% arrange("time")

	# filter what's relevant
	now <- reminder_table %>% filter(.data$time_l <= age & age <= .data$time_r) %>% mutate(done = FALSE)
	later <- reminder_table %>% filter(age < .data$time_l) %>% arrange(.data$time_l)

	# filter out topics from now that have already been done
	for (i in 1:nrow(now)) {
		# check for empty measurements
		if (nrow(bds_df) == 0L) break
		if (nrow(now) == 0L) break

		# has the i-th measurement been done?
		if (any(now[i, "time_l"] <= bds_df$time & bds_df$time < now[i, "time_r"] & bds_df$bds == now[i, "bds"])) {
			# remove first matching measured time
			bds_df <- bds_df[-which(now[i, "time_l"] <= bds_df$time & bds_df$time < now[i, "time_r"] & bds_df$bds == now[i, "bds"]), ]
			now[i, "done"] <- TRUE
		}
	}

	# check dates
	suggest <- rbind(now %>% filter(!.data$done) %>% select(-'done'), later) %>%
		filter(.data$time_l < min(.data$time_r)) %>% # < not <=; we want to avoid suggesting a singular day on which topic A starts and B ends.
		distinct(.data$topic, .keep_all = TRUE)
	windows <- suggest %>%
		summarise(window_l = as.Date((max(.data$time_l) - age)*365.25, origin = Sys.Date()),
							window_r = as.Date((min(.data$time_r) - age)*365.25, origin = Sys.Date()))

	if (windows$window_l < Sys.Date()) windows$window_l <- Sys.Date()

	# write message
	msg <- paste0("Een vervolgafspraak kan het best ingepland worden tussen ", windows$window_l," en ", windows$window_r, ".")

	return(
		list(
			msg = msg,
			topics = suggest$topic,
			window = windows,
			done = now %>% filter(.data$done)
			)
		)
	# end of function
}

# helper function for the SRM
create_bds_df <- function(list = list()) {
	bds_df <- data.frame(bds = numeric(0), time = numeric(0))

	if("ausc" %in% names(list)) {
		df <- list$ausc
		df <- data.frame(bds = 855, time = df[df$ausc == "1" & !is.na(df$ausc), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("fem" %in% names(list)) {
		df <- list$fem
		df <- data.frame(bds = 146, time = df[!is.na(df$fem_l) | !is.na(df$fem_r), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("spleen" %in% names(list)) {
		df <- list$spleen
		df <- data.frame(bds = 207, time = df[!is.na(df$spleen), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("liver" %in% names(list)) {
		df <- list$liver
		df <- data.frame(bds = 206, time = df[!is.na(df$liver), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("nst" %in% names(list)) {
		df <- list$nst
		df <- data.frame(bds = 806, time = df[!is.na(df$nst), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("prefpos" %in% names(list)) {
		df <- list$prefpos
		df <- data.frame(bds = 167, time = df[df$prefpos == "1" & !is.na(df$prefpos), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("eyecheck" %in% names(list)) {
		df <- list$eyecheck
		df <- data.frame(bds = 1379, time = df[df$eyecheck == "1" & !is.na(df$eyecheck), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("fundus" %in% names(list)) {
		df <- list$fundus
		df <- data.frame(bds = 840, time = df[!is.na(df$red_l) | !is.na(df$red_r), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("reflexbeeld" %in% names(list)) {
		df <- list$reflexbeeld
		df <- data.frame(bds = 390, time = df[!is.na(df$reflexbeeld_l) | !is.na(df$reflexbeeld_r), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("instel" %in% names(list)) {
		df <- list$instel
		df <- data.frame(bds = 392, time = df[!is.na(df$instel_l) | !is.na(df$instel_r), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("volgbeweging" %in% names(list)) {
		df <- list$volgbeweging
		df <- data.frame(bds = 398, time = df[!is.na(df$volgbeweging_l) | !is.na(df$volgbeweging_r), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("visus" %in% names(list)) {
		df <- list$visus
		df <- data.frame(bds = 408, time = df[!is.na(df$visus), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("vwo1" %in% names(list)) {
		df <- list$vwo1
		df <- data.frame(bds = 879, time = df[!is.na(df$vwo1), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("vwo2" %in% names(list)) {
		df <- list$vwo2
		df <- data.frame(bds = 881, time = df[!is.na(df$vwo2), "age"])
		bds_df <- rbind(bds_df, df)
	}

	if("vwo4" %in% names(list)) {
		df <- list$vwo4
		df <- data.frame(bds = 886, time = df[!is.na(df$vwo4), "age"])
		bds_df <- rbind(bds_df, df)
	}

	return(bds_df)
}
