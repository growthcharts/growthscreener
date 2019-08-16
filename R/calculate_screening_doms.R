calculate_screening_doms <- function(ind) {
  # internal function to extract all doms that could function
  # dom0. Currently restricted to height.

  xyz <- get_xyz(ind, yname = "hgt")
  xorder <- order(xyz$x)
  x <- xyz$x[xorder]
  y <- xyz$y[xorder]
  if (length(x) == 0L) {
    age1 <- NA
    age0 <- NA
    y1 <- NA
    y0 <- NA
  }
  if (length(x) == 1L) {
    age1 <- x
    age0 <- NA
    y1 <- y
    y0 <- NA
  }
  if (length(x) >= 2L) {
    age1 <- x[length(x)]
    age0 <- rev(x[-length(x)])
    y1 <- y[length(x)]
    y0 <- rev(y[-length(x)])
  }

  # transform to dom's
  dob <- as.Date(get_dob(ind), format = "%Y%m%d")
  dom1 <- get_dob(ind)  + round(age1 * 365.25)
  dom0 <- get_dob(ind)  + round(age0 * 365.25)

  return(list(dom1 = dom1, age1 = age1, y1 = y1,
              dom0 = dom0, age0 = age0, y0 = y0))
}
