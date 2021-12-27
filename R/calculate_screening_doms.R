#' Extract measurements that can function as date zero
#'
#' This function scans the `tgt` object, and finds the observation
#' pairs used by the `calculate_advice_xxx()` functions.
#' @inheritParams screen_curves_tgt
#' @return A list with `length(ynames)` elements. Each list element
#' is another `list` with elements `dom0` (back-calculated
#' dates of measurement, vector, reverse time), `age0` (decimal age),
#' `y0` (measurement), `z0` (Z-score equivalent) and
#' - for weight - `h0`, as well as similar quantities `dom1`,
#' `age1`, `y1`, `z1` and `h1` for the upper part
#' of the pair.
#' @details This function implements the `"x1_last"` strategy for
#' choosing pairs. This strategy selects the last observation as
#' `x1` and forms pairs with every earlier observation.
#' @note Internal function. Not to be called directly.
#' @examples
#' growthscreener:::calculate_screening_doms(target)
calculate_screening_doms <- function(tgt,
                                     ynames = c("hgt", "wgt", "hdc"),
                                     na.omit = TRUE) {
  stopifnot(is.list(tgt))
  time <- timedata(tgt)

  # prepare output
  if (!length(ynames)) return(NULL)
  result <- vector("list", length(ynames))
  names(result) <- ynames

  # loop over ynames
  for (yname in ynames) {

    # extract measures
    d <- time %>%
      filter(.data$yname == !!yname & .data$xname == "age") %>%
      select(all_of(c("x", "y", "z")))

    # for wgt, we also need hgt0 and hgt1
    if (yname == "wgt") {
      h <- time %>%
        filter(.data$yname == "hgt" & .data$xname == "age") %>%
        rename(hgt = "y") %>%
        select(all_of(c("x", "hgt")))
      d <- left_join(d, h, by = "x")
    } else {
      d <- d %>%
        mutate(hgt = NA_real_)
    }

    # remove NA's and sort
    if (na.omit) d <- filter(d, !(is.na(.data$x) | is.na(.data$y)))
    d <- arrange(d, .data$x)

    # assign elements
    nr <- nrow(d)
    if (!nr) {
      age1 <- age0 <- y1 <- y0 <- z1 <- z0 <- h1 <- h0 <- numeric(0)
    } else if (nr == 1L) {
      age1 <- d$x
      age0 <- NA_real_
      y1 <- d$y
      y0 <- NA_real_
      z1 <- d$z
      z0 <- NA_real_
      h1 <- d$hgt
      h0 <- NA_real_
    } else if (nr >= 2L) {
      age1 <- d$x[nr]
      age0 <- rev(d$x[-nr])
      y1 <- d$y[nr]
      y0 <- rev(d$y[-nr])
      z1 <- d$z[nr]
      z0 <- rev(d$z[-nr])
      h1 <- d$hgt[nr]
      h0 <- rev(d$hgt[-nr])
    }

    # transform to dom's (age in days)
    dom1 <- round(age1 * 365.25)
    dom0 <- round(age0 * 365.25)

    result[[yname]] <- list(dom0 = dom0, dom1 = dom1,
                            age0 = age0, age1 = age1,
                            y0 = y0, y1 = y1,
                            z0 = z0, z1 = z1,
                            h0 = h0, h1 = h1)
  }
  result
}
