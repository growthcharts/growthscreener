# Changelog

## growthscreener 1.26.0

- Adds a `prefix` argument to
  [`msg()`](https://growthcharts.org/growthscreener/reference/msg.md)
  (default `TRUE`, preserving current behaviour) so callers can request
  the bare advice text with `msg(msgcode, prefix = FALSE)`. Previously
  the lead-in “Het advies volgens de JGZ-richtlijn … is als volgt:” was
  hardcoded into `messages.txt` for advice messages, with no way to omit
  it.

## growthscreener 1.25.0

- Adds a `"DS"` (Down syndrome) option to
  [`calculate_th()`](https://growthcharts.org/growthscreener/reference/calculate_th.md),
  using the target height formula from Van Gameren-Oosterom et
  al. (2012). Previously
  [`calculate_th()`](https://growthcharts.org/growthscreener/reference/calculate_th.md)
  had no Down-syndrome case at all, so calling code (e.g.
  `chartplotter::plot_target_height()`) fell back to the Dutch (`"NL"`)
  formula for Down syndrome charts, silently giving the wrong target
  height.

## growthscreener 1.24.0

- Fixes issue in hgt advice and correct date in examples (#5)

## growthscreener 1.23.0

- Extends
  [`calculate_th()`](https://growthcharts.org/growthscreener/reference/calculate_th.md)
  with an extra output (prediction error) useful for calculating the
  target height range

## growthscreener 1.22.0

- Extends the
  [`calculate_th()`](https://growthcharts.org/growthscreener/reference/calculate_th.md)
  function with a `support_missing_hgtf` argument to allow for the
  calculation of target height from only the mother’s height. This is
  useful when height from the biological father height is missing. It
  currently works only for Dutch populations. If mother’s height is
  missing, the function will return `NA`, as before.
- Adds a silent safety trim of input arguments of
  [`calculate_th()`](https://growthcharts.org/growthscreener/reference/calculate_th.md)
  to scalar values. This is to prevent the function from breaking when
  the input is a vector of length \> 1. The function will now only use
  the first element of the input vector.
- Adds functions
  [`hermanussencole()`](https://growthcharts.org/growthscreener/reference/hermanussencole.md)
  and
  [`targetheight()`](https://growthcharts.org/growthscreener/reference/targetheight.md)
  from the `chartdesigner` package. In contrast to
  [`calculate_th()`](https://growthcharts.org/growthscreener/reference/calculate_th.md)
  these functions are vectorised, so more suited for group applications
  rather than individual application.

## growthscreener 1.21.0

- Merges `srm` branch updates (v1.19.3) into main
- Undo the choice for weight for height guidelines for period 0-2 years
  throughout (made SvB 13jul20) This choice causes issues in practice as
  our advice does not line up with what the professional sees on their
  own diagrams. Sticking with 2012 guidelines even if they are
  inconsistent.
- Makes
  [`list_screeners()`](https://growthcharts.org/growthscreener/reference/list_screeners.md)
  aware of the language development guidelines
- Introduces `lgd` as abbreviation for language development

## growthscreener 1.20.0

- Merges srm brnach into master
- Updates NEWS.md

## growthscreener 1.19.0

- Version increases 1.16.0 –\> 1.19.0 were made in srm branch
- Adds advice algorithm for future visit date (remind date guideline)
- Adds screener for language development, 2 yrs
- Changes all dates to `yyyymmdd`  
- Merges with master

## growthscreener 1.16.1

- Extrapolates the BMI cutoff table beyond 18 years to evade error when
  the child age is between 18 and 19 years
- Silences the Rm CMD CHECK error on global variable `age`
- Deprecates
  [`calculate_screening_doms()`](https://growthcharts.org/growthscreener/reference/calculate_screening_doms.md)
- Removes `lubridate` dependency
- Makes sure `ga` is in completed weeks

## growthscreener 1.16.0

- Merges the srm branch
- Generalises screening functions from two time points to any number of
  time points
- Assumes that the last time point is current
- Returns one advice for all time points combined (rather than m-1
  retrospective pairs)
- Adds a new screening algorithm for language based on six Van Wiechen
  items
- Contains numerous other improvements
- Updates to roxygen version to 7.2.3
- Updates GitHub actions

## growthscreener 1.15.0

- Update roxygen version to 7.2.1

## growthscreener 1.14.0

- Siplify data structure to list with elements `psn` and `xyz`

## growthscreener 1.13.0

- Adds support for the new `target` class of the `bdsreader` package

## growthscreener 1.12.0

- Adds `\dots` argument to
  [`screen_curves_ind()`](https://growthcharts.org/growthscreener/reference/screen_curves_ind.md),
  [`screen_curves_tgt()`](https://growthcharts.org/growthscreener/reference/screen_curves_tgt.md)
  and
  [`list_screeners()`](https://growthcharts.org/growthscreener/reference/list_screeners.md)
  for extendibility

## growthscreener 1.11.0

- Adds
  [`list_screeners()`](https://growthcharts.org/growthscreener/reference/list_screeners.md)
  to produce overview of screeners

## growthscreener 1.10.0

- Relies now on new `auto_format` functionality from `bdsreader`
- Solves some smaller bugs
- Define stricter dependencies

## growthscreener 1.9.0

- **Breaking changes**:
- Replaces the `minihealth` package by `bdsreader`
- Replaces the `jamestest` package by `jamesdemodata`

## growthscreener 1.8.0

- Uses `nlreferences` package

## growthscreener 1.7.0

- Major rewrite to replace the `clopus` by the `centile` package for
  clearer, easier and extendable reference selection and calculation

## growthscreener 1.6.1

- Explicit declaration of `clopus`

## growthscreener 1.6.0

- This version improves consistency in sieves and messages
- Adds checks on missing data for `z1` in some edge cases where needed
- Adds messages `**24` and `**25` for edge cases where the Z-score
  cannot be calculated, even if all information is available
- Adds a `test_gain` argument to screening function that we may use to
  disable any checking on gain scores `z1 - z0` (e.g. when there is only
  one observation for a child)
- Uses stricter scalar comparison operators `&&` and `||`
- Makes `SD` and `SDS` consistent in messages
- Replaces text “voorlaatste” to the more generic “eerdere”
- Adds test code that runs all `json` files in `jamestest`

## growthscreener 1.5.0

- Introduces
  [`pick_reference_wgt()`](https://growthcharts.org/growthscreener/reference/pick_reference_wgt.md)
  to select weight reference depending on sex, age, ga and etn
- Refreshes the sieve used for under- and overweight screening
- Solves a bug in
  [`calculate_screening_doms()`](https://growthcharts.org/growthscreener/reference/calculate_screening_doms.md)
  that prevented weight screening
- For clarity, reverts the weight screening code from `wfh` to `wgt`

## growthscreener 1.4.2

- Updates to `roxygen 7.1.1`
- Evades testing problem by adding `jamestest` to `suggests:`

## growthscreener 1.4.1

- Updates to `roxygen 7.1.0`

## growthscreener 1.4.0

- Renames `wgt` to the more appropriate `wfh` guidelines
- Generalises
  [`calculate_screening_doms()`](https://growthcharts.org/growthscreener/reference/calculate_screening_doms.md)
  to work with `wfh` and `hdc`
- Documents
  [`calculate_screening_doms()`](https://growthcharts.org/growthscreener/reference/calculate_screening_doms.md)
- Renumbers messages according to type of advice
- Adds a more detailed BMI-table and applies
  [`approx()`](https://rdrr.io/r/stats/approxfun.html) instead of
  [`floor()`](https://rdrr.io/r/base/Round.html)
- Adds an [`na.omit()`](https://rdrr.io/r/stats/na.fail.html) argument
  to
  [`screen_curves_ind()`](https://growthcharts.org/growthscreener/reference/screen_curves_ind.md)
  to avoid boring NA messages
- Adds a `recalculate_z` argument to
  [`screen_curves_ind()`](https://growthcharts.org/growthscreener/reference/screen_curves_ind.md)
  to avoid superfluous calculation (not yet used)
- Adds the package version number to the results

## growthscreener 1.3.1

- Remove etnicity from `wgt` and `hdc`
- Set default `etn = "NL"` for `hgt`

## growthscreener 1.3.0

- Restructures the computational flow to adapt to multiple screeners
- Renumbers the messages starting with 1000’s
- Adds new screeners for weight and head circumference
- Replaces the `AGD` dependency by `clopus`
- Uses the updated preterm WFH references (in `clopus 0.43.0`)
- Adds Arjan Huizing as author

## growthscreener 1.2.0

- Adds test for height screening

## growthscreener 1.1.0

- Added a `NEWS.md` file to track changes to the package.
- Added `convert_msgcode_json()`

## growthscreener 1.0.0

- First complete version for height screening.
- Define
  [`calculate_advice_hgt()`](https://growthcharts.org/growthscreener/reference/advice_hgt.md)
- Define `calculate_helpers_hgt()`
- Define message table `messages_hgt`
- Define
  [`msg()`](https://growthcharts.org/growthscreener/reference/msg.md)
  function
- Add datasets and helper functions
