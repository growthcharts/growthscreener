wks <- function(x) {(x*7)/365.25}
mnths <- function(x) {(x*(365.25 / 12))/365.25}

create_reminder_table <- function() {
	# heart
	ausc_heart <- data.frame(topic = "auscultation heart",
													 bds = 855,
													 time_l = c(wks(3), wks(6),  mnths(4), mnths(10)),
													 time_r = c(wks(5), wks(16), mnths(10), mnths(15)))

	palp_heart <- data.frame(topic = c("palpation a.fem"),
													 bds = 146,
													 time_l = c(wks(3), wks(6)),
													 time_r = c(wks(5), wks(16)))

	spleen_df <- data.frame(topic = "spleen",
													bds = 207,
													time_l = c(wks(3), wks(6)),
													time_r = c(wks(5), wks(16)))

	liver_df <- data.frame(topic = "liver",
												 bds = 206,
												 time_l = c(wks(3), wks(6)),
												 time_r = c(wks(5), wks(16)))


	# NST
	nst_df <- data.frame(topic = "niet-scrotale testis",
											 bds = 806,
											 time_l = c(wks(3), mnths(2), mnths(7)),
											 time_r = c(wks(7), mnths(6), mnths(11)))

	# pref. pos.
	prefpos_df <- data.frame(topic = "voorkeurshouding",
													 bds = 167,
													 time_l = c(wks(3), wks(6), wks(11), wks(16)),
													 time_r = c(wks(6), wks(10), wks(15), wks(26)))

	# eyes
	eyecheck_df <- data.frame(topic = "ooginspectie",
														bds = 1379,
														time_l = c(wks(3), wks(6), wks(11), mnths(5), mnths(13), mnths(36), mnths(42)),
														time_r = c(wks(5), wks(10), wks(15), mnths(9), mnths(24), mnths(36.99), mnths(48)))

	red_reflex <- data.frame(topic = c("fundus"),
													 bds = 840,
													 time_l = c(wks(3), wks(11)),
													 time_r = c(wks(5), wks(15)))

	reflexbeeld <- data.frame(topic = c("reflexbeeld"),
														bds = 390,
														time_l = c(mnths(5), mnths(13)),
														time_r = c(mnths(9), mnths(24)))

	instel_beweging <- data.frame(topic = c("instel"),
																bds = 392,
																time_l = c(mnths(5), mnths(13)),
																time_r = c(mnths(9), mnths(24)))

	volgbeweging <- data.frame(topic = c("volgbeweging"),
														 bds = 398,
														 time_l = c(mnths(5), mnths(13)),
														 time_r = c(mnths(9), mnths(24)))

	visus_df <- data.frame(topic = "visus",
												 bds = 408,
												 time_l = c(mnths(42), mnths(54)),
												 time_r = c(mnths(48), mnths(66)))

	# VWO
	vwo1_df <- data.frame(topic = "vwo1",
												bds = 879,
												time_l = c(wks(3)),
												time_r = c(wks(5)))

	vwo2_df <- data.frame(topic = c("vwo2"),
												bds = 881,
												time_l = c(wks(6)),
												time_r = c(wks(10)))

	vwo4_df <- data.frame(topic = "vwo4",
												bds = 886,
												time_l = c(wks(11)),
												time_r = c(wks(15)))

	vax_df <- data.frame(topic = "vax",
											 bds = 0,
											 time_l = c(mnths(2.5), mnths(4.5), mnths(10.5), mnths(13), mnths(45)),
											 time_r = c(mnths(3.5), mnths(5.5), mnths(11.5), mnths(15), mnths(52)))

	# combine

	reminder_table <- do.call("rbind", list(ausc_heart, palp_heart,
																					spleen_df, liver_df, nst_df, prefpos_df,
																					eyecheck_df, red_reflex, reflexbeeld,
																					instel_beweging, volgbeweging,
																					visus_df, vwo1_df, vwo2_df, vwo4_df))
	return(reminder_table)
}